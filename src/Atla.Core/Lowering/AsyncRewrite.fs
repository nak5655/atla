namespace Atla.Core.Lowering

open System.Reflection
open System.Threading.Tasks
open Atla.Core.Data
open Atla.Core.Semantics.Data

/// `async fn` のための ClosedHir レベル書き換え。
/// PR-3a の現状は「同期実装」: `await expr` を `expr.GetAwaiter().GetResult()` へ展開し、
/// 本体の最終値を `Task.CompletedTask` / `Task.FromResult<T>(value)` で包んで Task を返す。
/// .NET 互換の Task を返すため、await 結果は同期的にブロックする（真の非同期は PR-3b で対応）。
module AsyncRewrite =
    // ─────────────────────────────────────────────
    // .NET 反射のキャッシュ
    // ─────────────────────────────────────────────

    let private taskType : System.Type = typeof<Task>
    let private taskGenericType : System.Type = typedefof<Task<_>>
    let private taskAwaiterType : System.Type = typeof<System.Runtime.CompilerServices.TaskAwaiter>
    let private taskAwaiterGenericType : System.Type = typedefof<System.Runtime.CompilerServices.TaskAwaiter<_>>

    /// `Task.CompletedTask`（static get property）の `getter` MethodInfo を解決する。
    let private taskCompletedTaskGetter : MethodInfo =
        let prop = taskType.GetProperty("CompletedTask", BindingFlags.Public ||| BindingFlags.Static)
        prop.GetGetMethod()

    /// `Task.FromResult<T>` のジェネリック定義 MethodInfo。`MakeGenericMethod` で閉じる。
    let private taskFromResultDef : MethodInfo =
        taskType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.find (fun m ->
            m.Name = "FromResult"
            && m.IsGenericMethodDefinition
            && m.GetParameters().Length = 1)

    /// 非ジェネリック `Task.GetAwaiter() : TaskAwaiter`。
    let private taskGetAwaiter : MethodInfo =
        taskType.GetMethod("GetAwaiter", System.Type.EmptyTypes)

    /// 非ジェネリック `TaskAwaiter.GetResult() : void`。
    let private taskAwaiterGetResult : MethodInfo =
        taskAwaiterType.GetMethod("GetResult", System.Type.EmptyTypes)

    /// `Task<T>.GetAwaiter() : TaskAwaiter<T>` を T で閉じて返す。
    let private taskGenericGetAwaiterFor (t: System.Type) : MethodInfo =
        let closedTask = taskGenericType.MakeGenericType([| t |])
        closedTask.GetMethod("GetAwaiter", System.Type.EmptyTypes)

    /// `TaskAwaiter<T>.GetResult() : T` を T で閉じて返す。
    let private taskAwaiterGenericGetResultFor (t: System.Type) : MethodInfo =
        let closedAwaiter = taskAwaiterGenericType.MakeGenericType([| t |])
        closedAwaiter.GetMethod("GetResult", System.Type.EmptyTypes)

    // ─────────────────────────────────────────────
    // TypeId → System.Type の解決ヘルパー
    // ─────────────────────────────────────────────

    /// `TypeId.Name sid` をシンボルテーブル経由で `System.Type` へ解決する。
    /// インポートされた `Task` などはここを通る。
    let private tryResolveNameToSystemType (symbolTable: SymbolTable) (sid: SymbolId) : System.Type option =
        match symbolTable.Get(sid) with
        | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) }
            when not (obj.ReferenceEquals(sysType, null)) -> Some sysType
        | _ -> None

    /// `TypeId` から `System.Type` を解決する。`Name`/`Native`/`App(.., [..])` をカバー。
    let rec private tryResolveTypeToSystem (symbolTable: SymbolTable) (tid: TypeId) : System.Type option =
        match tid with
        | TypeId.Native t -> Some t
        | TypeId.Name sid -> tryResolveNameToSystemType symbolTable sid
        | TypeId.App (TypeId.Native head, args) when head = taskType ->
            // ユーザーが `Task T` と書いた場合、`Native typeof<Task>` をヘッドとして表現される。
            // .NET の実体は `Task<T>` なのでジェネリックを閉じる。
            args
            |> List.map (tryResolveTypeToSystem symbolTable)
            |> List.fold (fun acc opt ->
                match acc, opt with
                | Some xs, Some x -> Some (xs @ [ x ])
                | _ -> None) (Some [])
            |> Option.map (fun argTypes -> taskGenericType.MakeGenericType(List.toArray argTypes))
        | TypeId.App (head, args) ->
            match tryResolveTypeToSystem symbolTable head with
            | Some headSys ->
                // 一般のジェネリック型適用も同様に閉じる。
                args
                |> List.map (tryResolveTypeToSystem symbolTable)
                |> List.fold (fun acc opt ->
                    match acc, opt with
                    | Some xs, Some x -> Some (xs @ [ x ])
                    | _ -> None) (Some [])
                |> Option.bind (fun argTypes ->
                    if headSys.IsGenericTypeDefinition then
                        try Some (headSys.MakeGenericType(List.toArray argTypes))
                        with _ -> None
                    else
                        Some headSys)
            | None -> None
        | _ -> None

    /// 与えられた `TypeId` が `Task` または `Task<T>` のとき、
    /// `Some(awaitedType, isGeneric, taskSystemType, payloadSystemType option)` を返す。
    let private tryClassifyTaskType (symbolTable: SymbolTable) (tid: TypeId) : (TypeId * System.Type * System.Type option) option =
        let isTaskSystem (t: System.Type) : bool =
            not (obj.ReferenceEquals(t, null)) && t = taskType

        match tid with
        | TypeId.Native t when isTaskSystem t -> Some (TypeId.Unit, t, None)
        | TypeId.Name sid ->
            match tryResolveNameToSystemType symbolTable sid with
            | Some t when isTaskSystem t -> Some (TypeId.Unit, t, None)
            | _ -> None
        | TypeId.App (head, [ arg ]) ->
            let headSysOpt =
                match head with
                | TypeId.Native t -> Some t
                | TypeId.Name sid -> tryResolveNameToSystemType symbolTable sid
                | _ -> None
            match headSysOpt with
            | Some t when isTaskSystem t ->
                let payloadSys = tryResolveTypeToSystem symbolTable arg
                let closedTask =
                    payloadSys
                    |> Option.map (fun ps -> taskGenericType.MakeGenericType([| ps |]))
                    |> Option.defaultValue taskGenericType
                Some (arg, closedTask, payloadSys)
            | _ -> None
        | _ -> None

    // ─────────────────────────────────────────────
    // await の同期化
    // ─────────────────────────────────────────────

    /// `await operand` を `operand.GetAwaiter().GetResult()` の Call 連鎖に変換する。
    /// PR-3a の暫定実装: 同期的に Task の完了を待つ。
    let private buildSyncAwait (symbolTable: SymbolTable) (operand: ClosedHir.Expr) (resultTid: TypeId) (span: Span) : ClosedHir.Expr =
        let operandType = operand.typ
        match tryClassifyTaskType symbolTable operandType with
        | Some (_, _, None) ->
            // 非ジェネリック Task: TaskAwaiter.GetResult() : void
            let awaiterCall =
                ClosedHir.Expr.Call(
                    Hir.Callable.NativeMethod taskGetAwaiter,
                    Some operand,
                    [],
                    TypeId.Native taskAwaiterType,
                    span)
            ClosedHir.Expr.Call(
                Hir.Callable.NativeMethod taskAwaiterGetResult,
                Some awaiterCall,
                [],
                TypeId.Unit,
                span)
        | Some (_, _, Some payloadSys) ->
            // ジェネリック Task<T>: TaskAwaiter<T>.GetResult() : T
            let getAwaiterMi = taskGenericGetAwaiterFor payloadSys
            let getResultMi = taskAwaiterGenericGetResultFor payloadSys
            let awaiterTid = TypeId.Native (taskAwaiterGenericType.MakeGenericType([| payloadSys |]))
            let awaiterCall =
                ClosedHir.Expr.Call(
                    Hir.Callable.NativeMethod getAwaiterMi,
                    Some operand,
                    [],
                    awaiterTid,
                    span)
            ClosedHir.Expr.Call(
                Hir.Callable.NativeMethod getResultMi,
                Some awaiterCall,
                [],
                resultTid,
                span)
        | None ->
            // 型分類失敗。ExprError を返してエラーを上流へ伝える。
            ClosedHir.Expr.ExprError(
                "AsyncRewrite: await operand is not Task or Task<T>",
                resultTid,
                span)

    // ─────────────────────────────────────────────
    // 本体の Task ラップ
    // ─────────────────────────────────────────────

    /// 本体式を Task / Task<T> を返す形へ包む。
    /// - 戻り値型が `Task`（非ジェネリック）の場合:
    ///     `Block([ExprStmt(body)], Task.CompletedTask, Task, span)`
    /// - 戻り値型が `Task<T>` の場合:
    ///     `Task.FromResult<T>(body)`
    let private wrapBody
        (symbolTable: SymbolTable)
        (body: ClosedHir.Expr)
        (declaredRet: TypeId)
        (span: Span) : ClosedHir.Expr =
        match tryClassifyTaskType symbolTable declaredRet with
        | Some (_, taskSys, None) ->
            // Task.CompletedTask を MemberAccess(NativeProperty) として表現する。
            let completedTaskProp =
                taskType.GetProperty("CompletedTask", BindingFlags.Public ||| BindingFlags.Static)
            let completedTaskExpr =
                ClosedHir.Expr.MemberAccess(
                    Hir.Member.NativeProperty completedTaskProp,
                    None,
                    TypeId.Native taskSys,
                    span)
            ClosedHir.Expr.Block(
                [ ClosedHir.Stmt.ExprStmt(body, span) ],
                completedTaskExpr,
                TypeId.Native taskSys,
                span)
        | Some (_, closedTaskSys, Some payloadSys) ->
            // Task.FromResult<T>(body)
            let closedFromResult = taskFromResultDef.MakeGenericMethod([| payloadSys |])
            ClosedHir.Expr.Call(
                Hir.Callable.NativeMethod closedFromResult,
                None,
                [ body ],
                TypeId.Native closedTaskSys,
                span)
        | None ->
            // PR-2 の戻り値型検証で既に診断済みの想定。素通しでフォールバック。
            body

    // ─────────────────────────────────────────────
    // ClosedHir.Method の書き換え
    // ─────────────────────────────────────────────

    /// `isAsync` メソッドの本体を「await 同期化 + Task ラップ」で書き換える。
    /// 非 async メソッドはそのまま返す。
    let private rewriteMethod (symbolTable: SymbolTable) (method: ClosedHir.Method) : ClosedHir.Method =
        if not method.isAsync then
            method
        else
            let declaredRet =
                match method.typ with
                | TypeId.Fn (_, ret) -> ret
                | _ -> method.typ

            // 1. 全ての Await ノードを GetAwaiter().GetResult() の同期 Call 連鎖へ書き換える。
            let bodyAfterAwaitRewrite =
                ClosedHir.mapExpr
                    (fun e ->
                        match e with
                        | ClosedHir.Expr.Await (operand, resultTid, span) ->
                            buildSyncAwait symbolTable operand resultTid span
                        | _ -> e)
                    method.body

            // 2. 本体を Task / Task<T> で包む。
            let wrappedBody = wrapBody symbolTable bodyAfterAwaitRewrite declaredRet method.span

            ClosedHir.Method(
                method.sym,
                method.args,
                wrappedBody,
                method.typ,
                method.overrideTarget,
                method.isAsync,
                method.span)

    // ─────────────────────────────────────────────
    // ClosedHir.Assembly の書き換え
    // ─────────────────────────────────────────────

    /// `ClosedHir.Assembly` 全体を走査し、`isAsync = true` のメソッドを書き換える。
    let rewriteAssembly (symbolTable: SymbolTable) (asm: ClosedHir.Assembly) : ClosedHir.Assembly =
        let rewriteModule (modul: ClosedHir.Module) : ClosedHir.Module =
            let rewrittenMethods =
                modul.methods |> List.map (rewriteMethod symbolTable)
            let rewrittenTypes =
                modul.types
                |> List.map (fun t ->
                    let rewrittenTypeMethods =
                        t.methods |> List.map (rewriteMethod symbolTable)
                    ClosedHir.Type(
                        t.sym,
                        t.isInterface,
                        t.baseType,
                        t.typeParams,
                        t.fields,
                        rewrittenTypeMethods))
            ClosedHir.Module(
                modul.name,
                rewrittenTypes,
                modul.fields,
                rewrittenMethods,
                modul.scope,
                modul.closureInvokeMethods)

        let rewrittenModules = asm.modules |> List.map rewriteModule
        ClosedHir.Assembly(asm.name, rewrittenModules)
