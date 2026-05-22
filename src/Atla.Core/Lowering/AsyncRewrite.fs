namespace Atla.Core.Lowering

open System.Reflection
open System.Threading.Tasks
open Atla.Core.Data
open Atla.Core.Semantics.Data

/// `async fn` のための ClosedHir レベル書き換え。
///
/// 各 async メソッドに対して `IAsyncStateMachine` を実装するクラス（state machine）を
/// 新規生成し、引数・ローカルをそのフィールドへホイストする。元メソッドは
/// 「SM を生成・初期化し `builder.Start(ref sm)` で起動して `builder.Task` を return する」
/// kickoff シムへ置き換える。
///
/// await 境界で真に yield する。MoveNext 冒頭に `<>1__state` による
/// ディスパッチ（resume ジャンプ）を置き、各 await を
/// `awaiter = task.GetAwaiter(); if(!awaiter.IsCompleted){ state=N; builder.AwaitUnsafeOnCompleted(ref awaiter, ref this); leave END } resume_N: ... awaiter.GetResult()`
/// へ展開する。式中ネストの await は spilling により文レベルへ持ち上げ、await を跨いで生存する
/// 中間値は SM の spill フィールド（`<>s__N`）へ退避する（左→右の評価順を保つ）。
///
/// MoveNext 全体は try/catch で囲い、本体で発生した例外は `builder.SetException(ex)` で捕捉して
/// 返り Task を faulted にする（await 中断は try 内から `leave END` で SetResult を飛ばして抜ける）。
///
/// 既知の制約: SetResult は try 内の正常完了位置に置いているため、SetResult 自体が投げると
/// 二重完了例外になり得る（正常系では発生しない）。
module AsyncRewrite =
    // ─────────────────────────────────────────────
    // .NET 反射のキャッシュ
    // ─────────────────────────────────────────────

    let private taskType : System.Type = typeof<Task>
    let private taskGenericType : System.Type = typedefof<Task<_>>
    let private taskAwaiterType : System.Type = typeof<System.Runtime.CompilerServices.TaskAwaiter>
    let private taskAwaiterGenericType : System.Type = typedefof<System.Runtime.CompilerServices.TaskAwaiter<_>>

    let private builderType : System.Type = typeof<System.Runtime.CompilerServices.AsyncTaskMethodBuilder>
    let private builderGenericType : System.Type = typedefof<System.Runtime.CompilerServices.AsyncTaskMethodBuilder<_>>
    let private asyncStateMachineInterface : System.Type = typeof<System.Runtime.CompilerServices.IAsyncStateMachine>

    /// `IAsyncStateMachine.MoveNext()` の MethodInfo。
    let private iasmMoveNext : MethodInfo =
        asyncStateMachineInterface.GetMethod("MoveNext", System.Type.EmptyTypes)

    /// `IAsyncStateMachine.SetStateMachine(IAsyncStateMachine)` の MethodInfo。
    let private iasmSetStateMachine : MethodInfo =
        asyncStateMachineInterface.GetMethod("SetStateMachine", [| asyncStateMachineInterface |])

    /// 非ジェネリック `AsyncTaskMethodBuilder.Create()`（static）。
    let private builderCreate : MethodInfo =
        builderType.GetMethod("Create", BindingFlags.Public ||| BindingFlags.Static, null, System.Type.EmptyTypes, null)

    let private exceptionType : System.Type = typeof<System.Exception>

    /// 非ジェネリック `AsyncTaskMethodBuilder.SetResult()`。
    let private builderSetResult : MethodInfo =
        builderType.GetMethod("SetResult", System.Type.EmptyTypes)

    /// 非ジェネリック `AsyncTaskMethodBuilder.SetException(Exception)`。
    let private builderSetException : MethodInfo =
        builderType.GetMethod("SetException", [| exceptionType |])

    /// `AsyncTaskMethodBuilder<T>.SetException(Exception)` を T で閉じて返す。
    let private builderGenericSetExceptionFor (t: System.Type) : MethodInfo =
        let closed = builderGenericType.MakeGenericType([| t |])
        closed.GetMethod("SetException", [| exceptionType |])

    /// 非ジェネリック `AsyncTaskMethodBuilder.Task` getter（returns Task）。
    let private builderTaskGetter : MethodInfo =
        builderType.GetProperty("Task", BindingFlags.Public ||| BindingFlags.Instance).GetGetMethod()

    /// `AsyncTaskMethodBuilder<T>.Create()`（static）を T で閉じて返す。
    let private builderGenericCreateFor (t: System.Type) : MethodInfo =
        let closed = builderGenericType.MakeGenericType([| t |])
        closed.GetMethod("Create", BindingFlags.Public ||| BindingFlags.Static, null, System.Type.EmptyTypes, null)

    /// `AsyncTaskMethodBuilder<T>.SetResult(T)` を T で閉じて返す。
    let private builderGenericSetResultFor (t: System.Type) : MethodInfo =
        let closed = builderGenericType.MakeGenericType([| t |])
        closed.GetMethod("SetResult", [| t |])

    /// `AsyncTaskMethodBuilder<T>.Task` getter（returns Task<T>）を T で閉じて返す。
    let private builderGenericTaskGetterFor (t: System.Type) : MethodInfo =
        let closed = builderGenericType.MakeGenericType([| t |])
        closed.GetProperty("Task", BindingFlags.Public ||| BindingFlags.Instance).GetGetMethod()

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

    /// awaiter 型（`TaskAwaiter` / `TaskAwaiter<T>`）の `IsCompleted` getter。
    let private awaiterIsCompletedGetter (awaiterType: System.Type) : MethodInfo =
        awaiterType.GetProperty("IsCompleted", BindingFlags.Public ||| BindingFlags.Instance).GetGetMethod()

    /// builder 実体型（`AsyncTaskMethodBuilder` / `AsyncTaskMethodBuilder<T>`）から
    /// `AwaitUnsafeOnCompleted<TAwaiter, TStateMachine>` のジェネリックメソッド定義を得る。
    let private awaitUnsafeOnCompletedDef (builderRuntimeType: System.Type) : MethodInfo =
        builderRuntimeType.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
        |> Array.find (fun m ->
            m.Name = "AwaitUnsafeOnCompleted" && m.IsGenericMethodDefinition && m.GetParameters().Length = 2)

    /// builder 実体型から `Start<TStateMachine>(ref TStateMachine)` のジェネリックメソッド定義を得る。
    let private startDef (builderRuntimeType: System.Type) : MethodInfo =
        builderRuntimeType.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
        |> Array.find (fun m ->
            m.Name = "Start" && m.IsGenericMethodDefinition && m.GetParameters().Length = 1)

    // ─────────────────────────────────────────────
    // シンボル採番
    // ─────────────────────────────────────────────

    /// 新しい SymbolId を採番し、コンパイラ生成シンボルとして登録する。
    /// `name` は CIL のフィールド/メソッド名に使われる（Gen.fs が SymbolTable を参照する）。
    let private allocateNamedSymbol (symbolTable: SymbolTable) (name: string) (typ: TypeId) : SymbolId =
        let sid = symbolTable.NextId()
        symbolTable.Add(sid, { name = name; typ = typ; kind = SymbolKind.Local() })
        sid

    // ─────────────────────────────────────────────
    // TypeId → System.Type の解決ヘルパー
    // ─────────────────────────────────────────────

    /// `TypeId.Name sid` をシンボルテーブル経由で `System.Type` へ解決する。
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
        // プリミティブ（Int/Bool/Float/String/Unit）・配列・関数型などは汎用変換に委ねる。
        | _ -> TypeId.tryToRuntimeSystemType tid

    /// 与えられた戻り値 `TypeId` を分類する。
    /// `Task`         → `Some(None)`           （非ジェネリック）
    /// `Task<T>`      → `Some(Some payloadSys)`（ジェネリック、payload の System.Type）
    /// それ以外        → `None`
    let private tryClassifyTaskType (symbolTable: SymbolTable) (tid: TypeId) : System.Type option option =
        let isTaskSystem (t: System.Type) : bool =
            not (obj.ReferenceEquals(t, null)) && t = taskType

        match tid with
        | TypeId.Native t when isTaskSystem t -> Some None
        | TypeId.Name sid ->
            match tryResolveNameToSystemType symbolTable sid with
            | Some t when isTaskSystem t -> Some None
            | _ -> None
        | TypeId.App (head, [ arg ]) ->
            let headSysOpt =
                match head with
                | TypeId.Native t -> Some t
                | TypeId.Name sid -> tryResolveNameToSystemType symbolTable sid
                | _ -> None
            match headSysOpt with
            | Some t when isTaskSystem t ->
                match tryResolveTypeToSystem symbolTable arg with
                | Some payloadSys -> Some (Some payloadSys)
                | None -> None
            | _ -> None
        | _ -> None

    /// await operand 型から awaiter 関連の MethodInfo 群を解決する。
    /// `(awaiterSysType, GetAwaiter, IsCompleted getter, GetResult)` を返す。
    /// operand が `Task` / `Task<T>` でない場合は `None`。
    let private classifyAwaitOperand
        (symbolTable: SymbolTable)
        (operandType: TypeId) : (System.Type * MethodInfo * MethodInfo * MethodInfo) option =
        match tryClassifyTaskType symbolTable operandType with
        | Some None ->
            Some (taskAwaiterType, taskGetAwaiter, awaiterIsCompletedGetter taskAwaiterType, taskAwaiterGetResult)
        | Some (Some payloadSys) ->
            let awaiterSys = taskAwaiterGenericType.MakeGenericType([| payloadSys |])
            Some (awaiterSys, taskGenericGetAwaiterFor payloadSys, awaiterIsCompletedGetter awaiterSys, taskAwaiterGenericGetResultFor payloadSys)
        | None -> None

    // ─────────────────────────────────────────────
    // await の同期化（残存 await のフォールバック）
    // ─────────────────────────────────────────────

    /// `await operand` を `operand.GetAwaiter().GetResult()` の Call 連鎖に変換する。
    /// PR-3b-2 の暫定実装: MoveNext 内で同期的に Task の完了を待つ。
    let private buildSyncAwait (symbolTable: SymbolTable) (operand: ClosedHir.Expr) (resultTid: TypeId) (span: Span) : ClosedHir.Expr =
        match tryClassifyTaskType symbolTable operand.typ with
        | Some None ->
            // 非ジェネリック Task: TaskAwaiter.GetResult() : void
            let awaiterCall =
                ClosedHir.Expr.Call(
                    Hir.Callable.NativeMethod taskGetAwaiter, Some operand, [],
                    TypeId.Native taskAwaiterType, span)
            ClosedHir.Expr.Call(
                Hir.Callable.NativeMethod taskAwaiterGetResult, Some awaiterCall, [],
                TypeId.Unit, span)
        | Some (Some payloadSys) ->
            // ジェネリック Task<T>: TaskAwaiter<T>.GetResult() : T
            let getAwaiterMi = taskGenericGetAwaiterFor payloadSys
            let getResultMi = taskAwaiterGenericGetResultFor payloadSys
            let awaiterTid = TypeId.Native (taskAwaiterGenericType.MakeGenericType([| payloadSys |]))
            let awaiterCall =
                ClosedHir.Expr.Call(
                    Hir.Callable.NativeMethod getAwaiterMi, Some operand, [],
                    awaiterTid, span)
            ClosedHir.Expr.Call(
                Hir.Callable.NativeMethod getResultMi, Some awaiterCall, [],
                resultTid, span)
        | None ->
            ClosedHir.Expr.ExprError("AsyncRewrite: await operand is not Task or Task<T>", resultTid, span)

    // ─────────────────────────────────────────────
    // ローカル変数（Let 束縛）の収集
    // ─────────────────────────────────────────────

    /// 式・文の木を走査し、`Let` で束縛されたローカル変数 (sid, typ) を収集する。
    /// ホイスト対象フィールドの算出に使う。For 反復変数は対象外（3b-2 ではローカルのまま）。
    let rec private collectLetSids (expr: ClosedHir.Expr) : (SymbolId * TypeId) list =
        match expr with
        | ClosedHir.Expr.Block (stmts, body, _, _) ->
            (stmts |> List.collect collectLetSidsStmt) @ collectLetSids body
        | ClosedHir.Expr.Call (_, instance, args, _, _) ->
            (instance |> Option.map collectLetSids |> Option.defaultValue [])
            @ (args |> List.collect collectLetSids)
        | ClosedHir.Expr.MemberAccess (_, instance, _, _) ->
            instance |> Option.map collectLetSids |> Option.defaultValue []
        | ClosedHir.Expr.If (cond, thenB, elseB, _, _) ->
            collectLetSids cond @ collectLetSids thenB @ collectLetSids elseB
        | ClosedHir.Expr.Await (operand, _, _) -> collectLetSids operand
        | ClosedHir.Expr.AddrOf (target, _, _) -> collectLetSids target
        | _ -> []

    and private collectLetSidsStmt (stmt: ClosedHir.Stmt) : (SymbolId * TypeId) list =
        match stmt with
        | ClosedHir.Stmt.Let (sid, _, value, _) -> [ sid, value.typ ] @ collectLetSids value
        | ClosedHir.Stmt.Assign (_, value, _) -> collectLetSids value
        | ClosedHir.Stmt.ExprStmt (value, _) -> collectLetSids value
        | ClosedHir.Stmt.StoreField (instanceExpr, _, _, value, _) ->
            collectLetSids instanceExpr @ collectLetSids value
        | ClosedHir.Stmt.For (_, _, iterable, body, _) ->
            collectLetSids iterable @ (body |> List.collect collectLetSidsStmt)
        | ClosedHir.Stmt.If (cond, thenBody, elseBody, _) ->
            collectLetSids cond
            @ (thenBody |> List.collect collectLetSidsStmt)
            @ (elseBody |> List.collect collectLetSidsStmt)
        | ClosedHir.Stmt.TryCatch (tryBody, _, _, catchBody, _) ->
            (tryBody |> List.collect collectLetSidsStmt) @ (catchBody |> List.collect collectLetSidsStmt)
        | ClosedHir.Stmt.Label _ | ClosedHir.Stmt.Goto _ | ClosedHir.Stmt.Return _ | ClosedHir.Stmt.Leave _ -> []
        | ClosedHir.Stmt.ErrorStmt _ -> []

    // ─────────────────────────────────────────────
    // フィールドホイスト
    // ホイスト対象 sid への参照を `this.<field>` へ、Let/Assign を StoreField へ変換する。
    // ─────────────────────────────────────────────

    /// ホイスト書き換えの文脈。
    type private HoistCtx = {
        /// MoveNext の `this`（= SM インスタンス）を指す式。
        thisExpr: ClosedHir.Expr
        /// SM 型の SymbolId。
        smTypeSid: SymbolId
        /// ホイスト対象となる変数 sid の集合（引数 + Let ローカル）。
        hoisted: Set<int>
    }

    /// 式中のホイスト対象 `Id` を `this.<field>` 読み出しへ書き換える。Block 内の Let/Assign は
    /// `hoistStmt` で StoreField へ変換する。
    let rec private hoistExpr (ctx: HoistCtx) (expr: ClosedHir.Expr) : ClosedHir.Expr =
        match expr with
        | ClosedHir.Expr.Id (sid, tid, span) when ctx.hoisted.Contains sid.id ->
            ClosedHir.Expr.MemberAccess(
                Hir.Member.DataField(ctx.smTypeSid, sid), Some ctx.thisExpr, tid, span)
        | ClosedHir.Expr.Block (stmts, body, tid, span) ->
            ClosedHir.Expr.Block(stmts |> List.map (hoistStmt ctx), hoistExpr ctx body, tid, span)
        | ClosedHir.Expr.Call (func, instance, args, tid, span) ->
            ClosedHir.Expr.Call(func, instance |> Option.map (hoistExpr ctx), args |> List.map (hoistExpr ctx), tid, span)
        | ClosedHir.Expr.MemberAccess (mem, instance, tid, span) ->
            ClosedHir.Expr.MemberAccess(mem, instance |> Option.map (hoistExpr ctx), tid, span)
        | ClosedHir.Expr.If (cond, thenB, elseB, tid, span) ->
            ClosedHir.Expr.If(hoistExpr ctx cond, hoistExpr ctx thenB, hoistExpr ctx elseB, tid, span)
        | ClosedHir.Expr.Await (operand, tid, span) ->
            ClosedHir.Expr.Await(hoistExpr ctx operand, tid, span)
        | ClosedHir.Expr.AddrOf (target, tid, span) ->
            ClosedHir.Expr.AddrOf(hoistExpr ctx target, tid, span)
        // リテラル・非ホイスト Id・Null・ExprError・EnvFieldLoad・ClosureCreate・Lambda は素通し。
        | _ -> expr

    and private hoistStmt (ctx: HoistCtx) (stmt: ClosedHir.Stmt) : ClosedHir.Stmt =
        match stmt with
        | ClosedHir.Stmt.Let (sid, isMut, value, span) ->
            let value' = hoistExpr ctx value
            if ctx.hoisted.Contains sid.id then
                ClosedHir.Stmt.StoreField(ctx.thisExpr, ctx.smTypeSid, sid, value', span)
            else
                ClosedHir.Stmt.Let(sid, isMut, value', span)
        | ClosedHir.Stmt.Assign (sid, value, span) ->
            let value' = hoistExpr ctx value
            if ctx.hoisted.Contains sid.id then
                ClosedHir.Stmt.StoreField(ctx.thisExpr, ctx.smTypeSid, sid, value', span)
            else
                ClosedHir.Stmt.Assign(sid, value', span)
        | ClosedHir.Stmt.StoreField (instanceExpr, typeSid, fieldSid, value, span) ->
            ClosedHir.Stmt.StoreField(hoistExpr ctx instanceExpr, typeSid, fieldSid, hoistExpr ctx value, span)
        | ClosedHir.Stmt.ExprStmt (value, span) ->
            ClosedHir.Stmt.ExprStmt(hoistExpr ctx value, span)
        | ClosedHir.Stmt.For (sid, tid, iterable, body, span) ->
            ClosedHir.Stmt.For(sid, tid, hoistExpr ctx iterable, body |> List.map (hoistStmt ctx), span)
        | ClosedHir.Stmt.If (cond, thenBody, elseBody, span) ->
            ClosedHir.Stmt.If(hoistExpr ctx cond, thenBody |> List.map (hoistStmt ctx), elseBody |> List.map (hoistStmt ctx), span)
        | ClosedHir.Stmt.TryCatch (tryBody, catchType, catchVarSid, catchBody, span) ->
            ClosedHir.Stmt.TryCatch(tryBody |> List.map (hoistStmt ctx), catchType, catchVarSid, catchBody |> List.map (hoistStmt ctx), span)
        | ClosedHir.Stmt.Label _ | ClosedHir.Stmt.Goto _ | ClosedHir.Stmt.Return _ | ClosedHir.Stmt.Leave _ -> stmt
        | ClosedHir.Stmt.ErrorStmt _ -> stmt

    // ─────────────────────────────────────────────
    // 状態機械の生成
    // ─────────────────────────────────────────────

    /// 1 つの async メソッドに対する生成結果。
    type private StateMachineGen = {
        /// 元メソッドの新しい本体（kickoff シム）。
        kickoffBody: ClosedHir.Expr
        /// 生成された SM クラス型。
        smType: ClosedHir.Type
        /// kickoff メソッドの戻り値型。`Task` / `Task<T>` を確実に閉じた `Native` 型へ正規化したもの。
        /// （Atla の `Task T` は `App(Name task, [T])` 表現で、Gen の総称消去では非総称 `Task` に
        /// 落ちてしまうため、ここで実体の `Task<T>` に確定させる。）
        resolvedRetType: TypeId
    }

    /// async メソッド本体から状態機械を生成する。
    let private generateStateMachine
        (symbolTable: SymbolTable)
        (method: ClosedHir.Method)
        (declaredRet: TypeId) : StateMachineGen =
        let span = method.span
        let payloadOpt = tryClassifyTaskType symbolTable declaredRet |> Option.defaultValue None

        // 戻り値型を実体の `Task` / `Task<T>`（Native）へ正規化する。
        let resolvedRetType =
            match payloadOpt with
            | None -> TypeId.Native taskType
            | Some payloadSys -> TypeId.Native (taskGenericType.MakeGenericType([| payloadSys |]))

        // 1. builder のフィールド型と関連 MethodInfo を決定する。
        let builderFieldType, createMi, taskGetterMi =
            match payloadOpt with
            | None -> TypeId.Native builderType, builderCreate, builderTaskGetter
            | Some payloadSys ->
                let closed = builderGenericType.MakeGenericType([| payloadSys |])
                TypeId.Native closed, builderGenericCreateFor payloadSys, builderGenericTaskGetterFor payloadSys

        // 2. SM 型・メンバーの SymbolId を採番する。
        let smTypeSid = allocateNamedSymbol symbolTable "<>c__AsyncStateMachine" TypeId.Unit
        let stateFieldSid = allocateNamedSymbol symbolTable "<>1__state" TypeId.Int
        let builderFieldSid = allocateNamedSymbol symbolTable "<>t__builder" builderFieldType
        let moveNextSid = allocateNamedSymbol symbolTable "MoveNext" (TypeId.Fn([ TypeId.Name smTypeSid ], TypeId.Unit))
        let moveNextThisSid = allocateNamedSymbol symbolTable "this" (TypeId.Name smTypeSid)
        let setSmSid =
            allocateNamedSymbol symbolTable "SetStateMachine"
                (TypeId.Fn([ TypeId.Name smTypeSid; TypeId.Native asyncStateMachineInterface ], TypeId.Unit))
        let setSmThisSid = allocateNamedSymbol symbolTable "this" (TypeId.Name smTypeSid)
        let setSmParamSid = allocateNamedSymbol symbolTable "stateMachine" (TypeId.Native asyncStateMachineInterface)
        let smLocalSid = allocateNamedSymbol symbolTable "<>sm" (TypeId.Name smTypeSid)

        // ラベル払い出しカウンタ（For 展開・await の resume・try の END で共有する）。
        let mutable nextLabel = 0
        let freshLabelId () =
            let l = nextLabel
            nextLabel <- nextLabel + 1
            l

        // 2.5 For ループ展開: await を含む For を「明示イテレータ + Label/Goto」ループへ
        // ClosedHir レベルで展開する。イテレータ・ループ変数を Let 束縛にすることで、後段の
        // フィールドホイストで SM フィールド化され、await 中断を跨いで生存できるようになる。
        // （await を含まない For は Layout の効率的な実装に委ねるため展開しない。）
        let stmtHasAwait (s: ClosedHir.Stmt) : bool =
            ClosedHir.foldStmt (fun acc e -> acc || (match e with ClosedHir.Expr.Await _ -> true | _ -> false)) false s

        // iterable 型から列挙子の MoveNext() と Current getter を反射解決する（Layout と同じ手順）。
        let resolveEnumerator (iterableTyp: TypeId) : (MethodInfo * MethodInfo) option =
            match tryResolveTypeToSystem symbolTable iterableTyp with
            | None -> None
            | Some iterType ->
                let candidates = iterType :: (iterType.GetInterfaces() |> Array.toList)
                let moveNext =
                    candidates
                    |> List.tryPick (fun t ->
                        t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
                        |> Array.tryFind (fun m -> m.Name = "MoveNext" && m.GetParameters().Length = 0))
                let currentProp =
                    candidates
                    |> List.collect (fun t ->
                        t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
                        |> Array.filter (fun p -> p.Name = "Current")
                        |> Array.toList)
                    |> List.tryFind (fun p -> p.PropertyType <> typeof<obj>)
                    |> Option.orElseWith (fun () ->
                        candidates
                        |> List.tryPick (fun t ->
                            t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
                            |> Array.tryFind (fun p -> p.Name = "Current")))
                match moveNext, currentProp with
                | Some mn, Some cp when not (obj.ReferenceEquals(cp.GetMethod, null)) -> Some (mn, cp.GetMethod)
                | _ -> None

        let rec expandForExpr (e: ClosedHir.Expr) : ClosedHir.Expr =
            match e with
            | ClosedHir.Expr.Block (stmts, final, tid, bSpan) ->
                ClosedHir.Expr.Block(stmts |> List.collect expandForStmt, expandForExpr final, tid, bSpan)
            | ClosedHir.Expr.Call (f, inst, args, tid, sp) ->
                ClosedHir.Expr.Call(f, inst |> Option.map expandForExpr, args |> List.map expandForExpr, tid, sp)
            | ClosedHir.Expr.MemberAccess (m, inst, tid, sp) ->
                ClosedHir.Expr.MemberAccess(m, inst |> Option.map expandForExpr, tid, sp)
            | ClosedHir.Expr.If (c, t, el, tid, sp) ->
                ClosedHir.Expr.If(expandForExpr c, expandForExpr t, expandForExpr el, tid, sp)
            | ClosedHir.Expr.Await (op, tid, sp) -> ClosedHir.Expr.Await(expandForExpr op, tid, sp)
            | ClosedHir.Expr.AddrOf (t, tid, sp) -> ClosedHir.Expr.AddrOf(expandForExpr t, tid, sp)
            | _ -> e
        and expandForStmt (stmt: ClosedHir.Stmt) : ClosedHir.Stmt list =
            match stmt with
            | ClosedHir.Stmt.For (sid, tid, iterable, body, fSpan) when stmtHasAwait stmt ->
                let iterable' = expandForExpr iterable
                let body' = body |> List.collect expandForStmt
                match resolveEnumerator iterable'.typ with
                | Some (moveNextMi, currentGetterMi) ->
                    let itSid = allocateNamedSymbol symbolTable "<>for_it" iterable'.typ
                    let condSid = allocateNamedSymbol symbolTable "<>for_cond" TypeId.Bool
                    let loopStart = freshLabelId ()
                    let loopBody = freshLabelId ()
                    let loopEnd = freshLabelId ()
                    let itExpr = ClosedHir.Expr.Id(itSid, iterable'.typ, fSpan)
                    [ ClosedHir.Stmt.Let(itSid, false, iterable', fSpan)
                      ClosedHir.Stmt.Label(loopStart, fSpan)
                      ClosedHir.Stmt.Let(condSid, false, ClosedHir.Expr.Call(Hir.Callable.NativeMethod moveNextMi, Some itExpr, [], TypeId.Bool, fSpan), fSpan)
                      ClosedHir.Stmt.If(ClosedHir.Expr.Id(condSid, TypeId.Bool, fSpan), [ ClosedHir.Stmt.Goto(loopBody, fSpan) ], [ ClosedHir.Stmt.Goto(loopEnd, fSpan) ], fSpan)
                      ClosedHir.Stmt.Label(loopBody, fSpan)
                      ClosedHir.Stmt.Let(sid, false, ClosedHir.Expr.Call(Hir.Callable.NativeMethod currentGetterMi, Some itExpr, [], tid, fSpan), fSpan) ]
                    @ body'
                    @ [ ClosedHir.Stmt.Goto(loopStart, fSpan); ClosedHir.Stmt.Label(loopEnd, fSpan) ]
                | None ->
                    // 列挙子を解決できない場合は展開せず Layout に委ねる（await ループは未対応のまま）。
                    [ ClosedHir.Stmt.For(sid, tid, iterable', body', fSpan) ]
            | ClosedHir.Stmt.For (sid, tid, iterable, body, fSpan) ->
                [ ClosedHir.Stmt.For(sid, tid, expandForExpr iterable, body |> List.collect expandForStmt, fSpan) ]
            | ClosedHir.Stmt.If (cond, thenB, elseB, ifSpan) ->
                [ ClosedHir.Stmt.If(expandForExpr cond, thenB |> List.collect expandForStmt, elseB |> List.collect expandForStmt, ifSpan) ]
            | ClosedHir.Stmt.Let (sid, m, v, sp) -> [ ClosedHir.Stmt.Let(sid, m, expandForExpr v, sp) ]
            | ClosedHir.Stmt.Assign (sid, v, sp) -> [ ClosedHir.Stmt.Assign(sid, expandForExpr v, sp) ]
            | ClosedHir.Stmt.ExprStmt (v, sp) -> [ ClosedHir.Stmt.ExprStmt(expandForExpr v, sp) ]
            | ClosedHir.Stmt.StoreField (i, tSid, fSid, v, sp) -> [ ClosedHir.Stmt.StoreField(expandForExpr i, tSid, fSid, expandForExpr v, sp) ]
            | other -> [ other ]

        let expandedBody = expandForExpr method.body

        // 3. ホイスト対象（引数 + Let ローカル）を算出する。元の sid をそのままフィールド sid に再利用する。
        let argFields = method.args  // (sid, typ) list
        let localFields = collectLetSids expandedBody
        let hoistedSet =
            (argFields |> List.map (fun (sid, _) -> sid.id))
            @ (localFields |> List.map (fun (sid, _) -> sid.id))
            |> Set.ofList

        // builder 実体型（await/start のジェネリックメソッド定義取得用）。
        let builderRuntimeType =
            match payloadOpt with
            | None -> builderType
            | Some payloadSys -> builderGenericType.MakeGenericType([| payloadSys |])
        let awaitUnsafeDef = awaitUnsafeOnCompletedDef builderRuntimeType
        let startMethodDef = startDef builderRuntimeType

        // 4. ホイスト用 this 式とコンテキスト、フィールドアクセス補助。
        let moveNextThisExpr = ClosedHir.Expr.Id(moveNextThisSid, TypeId.Name smTypeSid, span)
        let hoistCtx = { thisExpr = moveNextThisExpr; smTypeSid = smTypeSid; hoisted = hoistedSet }
        let fieldLoad (fieldSid: SymbolId) (fieldTyp: TypeId) =
            ClosedHir.Expr.MemberAccess(Hir.Member.DataField(smTypeSid, fieldSid), Some moveNextThisExpr, fieldTyp, span)
        let fieldAddr (fieldSid: SymbolId) (fieldTyp: TypeId) =
            ClosedHir.Expr.AddrOf(fieldLoad fieldSid fieldTyp, TypeId.ByRef fieldTyp, span)
        let storeState (n: int) =
            ClosedHir.Stmt.StoreField(moveNextThisExpr, smTypeSid, stateFieldSid, ClosedHir.Expr.Int(n, span), span)

        // 5. 本体（For 展開済み）をホイストする（await ノードは保持し、operand の Id のみホイストされる）。
        let hoistedBody = hoistExpr hoistCtx expandedBody

        // 6. await の中断/再開シーケンスを生成しながら本体を線形化する。
        let awaiterFields = System.Collections.Generic.List<ClosedHir.Field>()
        let dispatchEntries = System.Collections.Generic.List<int * int>()  // (stateNum, resumeLabelId)
        // spill 用フィールド（await を跨いで生存する中間値）。式中ネストの await を文レベルへ展開する際に使う。
        let spillFields = System.Collections.Generic.List<ClosedHir.Field>()
        let mutable nextSpill = 0
        let mutable nextState = 0

        let freshSpillField (typ: TypeId) : SymbolId =
            let sid = allocateNamedSymbol symbolTable (sprintf "<>s__%d" nextSpill) typ
            nextSpill <- nextSpill + 1
            spillFields.Add(ClosedHir.Field(sid, typ, ClosedHir.Expr.Unit span, span))
            sid

        // try/catch 用: メソッド末尾の END ラベルと catch 変数（例外）を確保する。
        // await 中断は try 内から `Leave END` でメソッド末尾へ抜ける（SetResult を飛ばす）。
        let endLabelId = freshLabelId ()
        let exceptionVarSid = allocateNamedSymbol symbolTable "<>ex" (TypeId.Native exceptionType)

        // 1 つの await を「GetAwaiter→IsCompleted 判定→state設定→AwaitUnsafeOnCompleted→Return→
        // resume ラベル→GetResult」へ展開し、(設定文リスト, 結果式) を返す。
        let emitAwaitSeq (op: ClosedHir.Expr) (resultTid: TypeId) (awaitSpan: Span) : ClosedHir.Stmt list * ClosedHir.Expr =
            match classifyAwaitOperand symbolTable op.typ with
            | None ->
                // Task/Task<T> と判定できない operand は同期フォールバック。
                [], buildSyncAwait symbolTable op resultTid awaitSpan
            | Some (awaiterSys, getAwaiterMi, isCompletedMi, getResultMi) ->
                let stateNum = nextState
                nextState <- nextState + 1
                let resumeLbl = nextLabel
                nextLabel <- nextLabel + 1
                let uSid = allocateNamedSymbol symbolTable (sprintf "<>u__%d" stateNum) (TypeId.Native awaiterSys)
                awaiterFields.Add(ClosedHir.Field(uSid, TypeId.Native awaiterSys, ClosedHir.Expr.Unit span, span))
                dispatchEntries.Add((stateNum, resumeLbl))
                let awaiterTyp = TypeId.Native awaiterSys
                // this.<>u__N = op.GetAwaiter()
                let storeAwaiter =
                    ClosedHir.Stmt.StoreField(
                        moveNextThisExpr, smTypeSid, uSid,
                        ClosedHir.Expr.Call(Hir.Callable.NativeMethod getAwaiterMi, Some op, [], awaiterTyp, awaitSpan),
                        awaitSpan)
                // if (this.<>u__N.IsCompleted) goto resume
                let isCompletedCall =
                    ClosedHir.Expr.Call(Hir.Callable.NativeMethod isCompletedMi, Some (fieldAddr uSid awaiterTyp), [], TypeId.Bool, awaitSpan)
                let skipIf = ClosedHir.Stmt.If(isCompletedCall, [ ClosedHir.Stmt.Goto(resumeLbl, awaitSpan) ], [], awaitSpan)
                // builder.AwaitUnsafeOnCompleted<TAwaiter, TStateMachine>(ref this.builder, ref this.<>u__N, ref this)
                let awaitCall =
                    ClosedHir.Expr.Call(
                        Hir.Callable.NativeGenericMethod(awaitUnsafeDef, [ TypeId.Native awaiterSys; TypeId.Name smTypeSid ]),
                        None,
                        [ fieldAddr builderFieldSid builderFieldType
                          fieldAddr uSid awaiterTyp
                          ClosedHir.Expr.AddrOf(moveNextThisExpr, TypeId.ByRef (TypeId.Name smTypeSid), awaitSpan) ],
                        TypeId.Unit, awaitSpan)
                let setup =
                    [ storeAwaiter
                      skipIf
                      storeState stateNum
                      ClosedHir.Stmt.ExprStmt(awaitCall, awaitSpan)
                      // try 内からの中断脱出は ret/br ではなく leave で END へ。
                      ClosedHir.Stmt.Leave(endLabelId, awaitSpan)
                      ClosedHir.Stmt.Label(resumeLbl, awaitSpan)
                      storeState -1 ]
                let resultExpr =
                    ClosedHir.Expr.Call(Hir.Callable.NativeMethod getResultMi, Some (fieldAddr uSid awaiterTyp), [], resultTid, awaitSpan)
                setup, resultExpr

        // 式が（深さ問わず）await を含むか判定する。
        let rec containsAwait (e: ClosedHir.Expr) : bool =
            match e with
            | ClosedHir.Expr.Await _ -> true
            | ClosedHir.Expr.Call (_, inst, args, _, _) ->
                (inst |> Option.map containsAwait |> Option.defaultValue false) || (args |> List.exists containsAwait)
            | ClosedHir.Expr.MemberAccess (_, inst, _, _) -> inst |> Option.map containsAwait |> Option.defaultValue false
            | ClosedHir.Expr.If (c, t, el, _, _) -> containsAwait c || containsAwait t || containsAwait el
            | ClosedHir.Expr.Block (stmts, fin, _, _) -> (stmts |> List.exists containsAwaitStmt) || containsAwait fin
            | ClosedHir.Expr.AddrOf (t, _, _) -> containsAwait t
            | _ -> false
        and containsAwaitStmt (s: ClosedHir.Stmt) : bool =
            match s with
            | ClosedHir.Stmt.Let (_, _, v, _) | ClosedHir.Stmt.Assign (_, v, _) | ClosedHir.Stmt.ExprStmt (v, _) -> containsAwait v
            | ClosedHir.Stmt.StoreField (i, _, _, v, _) -> containsAwait i || containsAwait v
            | ClosedHir.Stmt.For (_, _, it, body, _) -> containsAwait it || (body |> List.exists containsAwaitStmt)
            | ClosedHir.Stmt.If (c, t, el, _) -> containsAwait c || (t |> List.exists containsAwaitStmt) || (el |> List.exists containsAwaitStmt)
            | _ -> false

        // 式中ネストの await を文レベルへ展開する（spilling）。
        // 返り値: (先行文, await を含まない残余式)。
        let rec spillExpr (e: ClosedHir.Expr) : ClosedHir.Stmt list * ClosedHir.Expr =
            match e with
            | ClosedHir.Expr.Await (op, resTid, awaitSpan) ->
                let pop, rop = spillExpr op
                let awaitSetup, getResultExpr = emitAwaitSeq rop resTid awaitSpan
                // GetResult は一度だけ評価し、結果を spill フィールドへ退避してから残余として読む。
                let spillSid = freshSpillField resTid
                let storeStmt = ClosedHir.Stmt.StoreField(moveNextThisExpr, smTypeSid, spillSid, getResultExpr, awaitSpan)
                pop @ awaitSetup @ [ storeStmt ], fieldLoad spillSid resTid
            | ClosedHir.Expr.Call (func, instance, args, tid, span) ->
                let operands = (Option.toList instance) @ args
                let prefix, residuals = spillOperands operands
                let resInstance, resArgs =
                    match instance with
                    | Some _ -> Some (List.head residuals), List.tail residuals
                    | None -> None, residuals
                prefix, ClosedHir.Expr.Call(func, resInstance, resArgs, tid, span)
            | ClosedHir.Expr.MemberAccess (mem, Some inst, tid, span) ->
                let p, r = spillExpr inst
                p, ClosedHir.Expr.MemberAccess(mem, Some r, tid, span)
            | ClosedHir.Expr.If (cond, thenB, elseB, tid, span) when containsAwait thenB || containsAwait elseB ->
                // 分岐内に await がある値 If は、結果を spill フィールドへ書き込む文レベル If へ降格する。
                let pc, rc = spillExpr cond
                let resultSid = freshSpillField tid
                let pThen, rThen = spillExpr thenB
                let pElse, rElse = spillExpr elseB
                let thenStmts = pThen @ [ ClosedHir.Stmt.StoreField(moveNextThisExpr, smTypeSid, resultSid, rThen, span) ]
                let elseStmts = pElse @ [ ClosedHir.Stmt.StoreField(moveNextThisExpr, smTypeSid, resultSid, rElse, span) ]
                pc @ [ ClosedHir.Stmt.If(rc, thenStmts, elseStmts, span) ], fieldLoad resultSid tid
            | ClosedHir.Expr.If (cond, thenB, elseB, tid, span) ->
                // 分岐に await なし: cond だけ spill。
                let pc, rc = spillExpr cond
                pc, ClosedHir.Expr.If(rc, thenB, elseB, tid, span)
            | ClosedHir.Expr.Block (stmts, final, _, _) ->
                let stmtSpilled = stmts |> List.collect spillStmt
                let pf, rf = spillExpr final
                stmtSpilled @ pf, rf
            | _ -> [], e   // リーフ（await を含まない想定）

        // オペランド列を左→右の評価順を保ったまま spill する。
        // 後続オペランドに await が含まれる場合、現オペランドの残余を temp フィールドへ退避する
        // （await 中断で評価スタックが破棄されるため、跨いで生存する値はフィールド化が必要）。
        and spillOperands (operands: ClosedHir.Expr list) : ClosedHir.Stmt list * ClosedHir.Expr list =
            let arr = List.toArray operands
            let n = arr.Length
            let suffixHasAwait = Array.zeroCreate n
            let mutable acc = false
            for i in (n - 1) .. -1 .. 0 do
                suffixHasAwait.[i] <- acc
                if containsAwait arr.[i] then acc <- true
            let prefix = System.Collections.Generic.List<ClosedHir.Stmt>()
            let residuals =
                arr |> Array.mapi (fun i op ->
                    let pop, rop = spillExpr op
                    prefix.AddRange(pop)
                    if suffixHasAwait.[i] then
                        let tmpSid = freshSpillField op.typ
                        prefix.Add(ClosedHir.Stmt.StoreField(moveNextThisExpr, smTypeSid, tmpSid, rop, op.span))
                        fieldLoad tmpSid op.typ
                    else rop)
            List.ofSeq prefix, List.ofArray residuals

        // 文の await を spill して文レベルへ展開する。
        and spillStmt (stmt: ClosedHir.Stmt) : ClosedHir.Stmt list =
            match stmt with
            // よくある形（standalone await / 単純な StoreField 値が await）は spill フィールドを介さず直接展開する。
            | ClosedHir.Stmt.ExprStmt (ClosedHir.Expr.Await (op, resTid, awaitSpan), _) ->
                let pop, rop = spillExpr op
                let setup, getResult = emitAwaitSeq rop resTid awaitSpan
                pop @ setup @ [ ClosedHir.Stmt.ExprStmt(getResult, awaitSpan) ]
            | ClosedHir.Stmt.StoreField (inst, tSid, fSid, ClosedHir.Expr.Await (op, resTid, awaitSpan), sfSpan)
                    when not (containsAwait inst) ->
                let pop, rop = spillExpr op
                let setup, getResult = emitAwaitSeq rop resTid awaitSpan
                pop @ setup @ [ ClosedHir.Stmt.StoreField(inst, tSid, fSid, getResult, sfSpan) ]
            | ClosedHir.Stmt.ExprStmt (e, span) ->
                let p, r = spillExpr e
                p @ [ ClosedHir.Stmt.ExprStmt(r, span) ]
            | ClosedHir.Stmt.StoreField (inst, tSid, fSid, v, span) ->
                match spillOperands [ inst; v ] with
                | prefix, [ rInst; rV ] -> prefix @ [ ClosedHir.Stmt.StoreField(rInst, tSid, fSid, rV, span) ]
                | _ -> [ stmt ]
            | ClosedHir.Stmt.Let (sid, isMut, v, span) ->
                let p, r = spillExpr v
                p @ [ ClosedHir.Stmt.Let(sid, isMut, r, span) ]
            | ClosedHir.Stmt.Assign (sid, v, span) ->
                let p, r = spillExpr v
                p @ [ ClosedHir.Stmt.Assign(sid, r, span) ]
            | ClosedHir.Stmt.If (cond, thenB, elseB, span) ->
                let pc, rc = spillExpr cond
                pc @ [ ClosedHir.Stmt.If(rc, thenB |> List.collect spillStmt, elseB |> List.collect spillStmt, span) ]
            | ClosedHir.Stmt.For (sid, tid, iter, body, span) ->
                let pi, ri = spillExpr iter
                pi @ [ ClosedHir.Stmt.For(sid, tid, ri, body |> List.collect spillStmt, span) ]
            | other -> [ other ]

        // 本体を (先行文, 最終値式) に分解し、await を spill して文レベルへ展開する。
        let bodyStmts, bodyFinal =
            match hoistedBody with
            | ClosedHir.Expr.Block (stmts, final, _, _) -> stmts, final
            | other -> [], other
        let linearizedStmts = bodyStmts |> List.collect spillStmt
        let finalStmts, finalValue =
            match bodyFinal with
            // 最終式が直接 await の場合は spill フィールドを介さず GetResult を結果値にする。
            | ClosedHir.Expr.Await (op, resTid, awaitSpan) ->
                let pop, rop = spillExpr op
                let setup, getResult = emitAwaitSeq rop resTid awaitSpan
                pop @ setup, getResult
            | other -> spillExpr other

        // 7. ディスパッチ（state スイッチ）: 各 await の resume ラベルへ分岐する。
        let stateLoad = fieldLoad stateFieldSid TypeId.Int
        let dispatchStmts =
            dispatchEntries
            |> Seq.map (fun (n, lbl) ->
                let eqCall =
                    ClosedHir.Expr.Call(
                        Hir.Callable.BuiltinOperator Builtins.Operators.OpEq,
                        None, [ stateLoad; ClosedHir.Expr.Int(n, span) ], TypeId.Bool, span)
                ClosedHir.Stmt.If(eqCall, [ ClosedHir.Stmt.Goto(lbl, span) ], [], span))
            |> List.ofSeq

        // 8. MoveNext 本体を組み立てる。
        //    try { dispatch → 本体 → (正常完了) state=-2; SetResult(...) }
        //    catch (Exception ex) { state=-2; builder.SetException(ex) }
        //    END:   // await 中断は try 内から Leave END で SetResult を飛ばしてここへ抜ける
        // 注意（既知の制約）: SetResult を try 内に置いているため、SetResult 自体が投げると
        // catch が拾って SetException を試み二重完了例外になり得る（正常系では発生しない）。
        let builderAddr = fieldAddr builderFieldSid builderFieldType

        // 正常完了時の SetResult シーケンス。
        let normalCompletion =
            match payloadOpt with
            | None ->
                let setResultCall =
                    ClosedHir.Expr.Call(Hir.Callable.NativeMethod builderSetResult, Some builderAddr, [], TypeId.Unit, span)
                [ ClosedHir.Stmt.ExprStmt(finalValue, span)
                  storeState -2
                  ClosedHir.Stmt.ExprStmt(setResultCall, span) ]
            | Some payloadSys ->
                let setResultMi = builderGenericSetResultFor payloadSys
                let setResultCall =
                    ClosedHir.Expr.Call(Hir.Callable.NativeMethod setResultMi, Some builderAddr, [ finalValue ], TypeId.Unit, span)
                [ storeState -2
                  ClosedHir.Stmt.ExprStmt(setResultCall, span) ]

        let tryBody = dispatchStmts @ linearizedStmts @ finalStmts @ normalCompletion

        // catch 節: state=-2 にして builder.SetException(ex)。
        let setExceptionMi =
            match payloadOpt with
            | None -> builderSetException
            | Some payloadSys -> builderGenericSetExceptionFor payloadSys
        let catchBody =
            [ storeState -2
              ClosedHir.Stmt.ExprStmt(
                  ClosedHir.Expr.Call(
                      Hir.Callable.NativeMethod setExceptionMi, Some builderAddr,
                      [ ClosedHir.Expr.Id(exceptionVarSid, TypeId.Native exceptionType, span) ], TypeId.Unit, span),
                  span) ]

        let moveNextBodyRaw =
            ClosedHir.Expr.Block(
                [ ClosedHir.Stmt.TryCatch(tryBody, exceptionType, exceptionVarSid, catchBody, span)
                  ClosedHir.Stmt.Label(endLabelId, span) ],
                ClosedHir.Expr.Unit span, TypeId.Unit, span)

        // 残存する（式内ネスト等の）await は同期フォールバックへ変換する。
        let moveNextBody =
            ClosedHir.mapExpr
                (fun e ->
                    match e with
                    | ClosedHir.Expr.Await (operand, resultTid, awaitSpan) -> buildSyncAwait symbolTable operand resultTid awaitSpan
                    | _ -> e)
                moveNextBodyRaw

        // 9. SM 型のフィールド群（state, builder, ホイスト引数/ローカル, awaiter）。
        let unitExpr = ClosedHir.Expr.Unit span
        let smFields =
            [ ClosedHir.Field(stateFieldSid, TypeId.Int, unitExpr, span)
              ClosedHir.Field(builderFieldSid, builderFieldType, unitExpr, span) ]
            @ (argFields |> List.map (fun (sid, typ) -> ClosedHir.Field(sid, typ, unitExpr, span)))
            @ (localFields |> List.map (fun (sid, typ) -> ClosedHir.Field(sid, typ, unitExpr, span)))
            @ (awaiterFields |> List.ofSeq)
            @ (spillFields |> List.ofSeq)

        let moveNextMethod =
            ClosedHir.Method(
                moveNextSid,
                [ moveNextThisSid, TypeId.Name smTypeSid ],
                moveNextBody,
                TypeId.Fn([ TypeId.Name smTypeSid ], TypeId.Unit),
                Some iasmMoveNext,
                false,
                span)

        // 6. SetStateMachine 本体（3b-2 では空スタブ）。
        let setSmMethod =
            ClosedHir.Method(
                setSmSid,
                [ setSmThisSid, TypeId.Name smTypeSid; setSmParamSid, TypeId.Native asyncStateMachineInterface ],
                ClosedHir.Expr.Unit span,
                TypeId.Fn([ TypeId.Name smTypeSid; TypeId.Native asyncStateMachineInterface ], TypeId.Unit),
                Some iasmSetStateMachine,
                false,
                span)

        // 7. SM 型（IAsyncStateMachine を実装するクラス）。
        let smType =
            ClosedHir.Type(
                smTypeSid,
                false,
                Some (TypeId.Native asyncStateMachineInterface),
                [],
                smFields,
                [ moveNextMethod; setSmMethod ])

        // 8. kickoff シム本体を構築する。
        let smLocalExpr = ClosedHir.Expr.Id(smLocalSid, TypeId.Name smTypeSid, span)
        // 8a. `<>sm = new SM()`
        let newSmStmt =
            ClosedHir.Stmt.Let(
                smLocalSid, false,
                ClosedHir.Expr.Call(
                    Hir.Callable.DataConstructor(smTypeSid, []), None, [], TypeId.Name smTypeSid, span),
                span)
        // 8b. `<>sm.<>t__builder = AsyncTaskMethodBuilder.Create()`
        let initBuilderStmt =
            ClosedHir.Stmt.StoreField(
                smLocalExpr, smTypeSid, builderFieldSid,
                ClosedHir.Expr.Call(Hir.Callable.NativeMethod createMi, None, [], builderFieldType, span),
                span)
        // 8c. `<>sm.<>1__state = -1`
        let initStateStmt =
            ClosedHir.Stmt.StoreField(
                smLocalExpr, smTypeSid, stateFieldSid, ClosedHir.Expr.Int(-1, span), span)
        // 8d. 各引数を SM フィールドへコピーする。
        let copyArgStmts =
            argFields
            |> List.map (fun (argSid, argTyp) ->
                ClosedHir.Stmt.StoreField(
                    smLocalExpr, smTypeSid, argSid,
                    ClosedHir.Expr.Id(argSid, argTyp, span), span))
        // 8e. `<>sm.<>t__builder.Start(ref <>sm)`
        // Start<TStateMachine> が ExecutionContext を捕捉し、内部で sm.MoveNext() を起動する。
        let startStmt =
            ClosedHir.Stmt.ExprStmt(
                ClosedHir.Expr.Call(
                    Hir.Callable.NativeGenericMethod(startMethodDef, [ TypeId.Name smTypeSid ]),
                    None,
                    [ ClosedHir.Expr.AddrOf(
                        ClosedHir.Expr.MemberAccess(
                            Hir.Member.DataField(smTypeSid, builderFieldSid), Some smLocalExpr, builderFieldType, span),
                        TypeId.ByRef builderFieldType, span)
                      ClosedHir.Expr.AddrOf(smLocalExpr, TypeId.ByRef (TypeId.Name smTypeSid), span) ],
                    TypeId.Unit, span),
                span)
        // 8f. `return <>sm.<>t__builder.Task`
        let builderAddrForTask =
            ClosedHir.Expr.AddrOf(
                ClosedHir.Expr.MemberAccess(
                    Hir.Member.DataField(smTypeSid, builderFieldSid), Some smLocalExpr, builderFieldType, span),
                TypeId.ByRef builderFieldType, span)
        let returnTaskExpr =
            ClosedHir.Expr.Call(
                Hir.Callable.NativeMethod taskGetterMi, Some builderAddrForTask, [], resolvedRetType, span)

        let kickoffBody =
            ClosedHir.Expr.Block(
                [ newSmStmt; initBuilderStmt; initStateStmt ] @ copyArgStmts @ [ startStmt ],
                returnTaskExpr,
                resolvedRetType,
                span)

        { kickoffBody = kickoffBody; smType = smType; resolvedRetType = resolvedRetType }

    // ─────────────────────────────────────────────
    // ClosedHir.Method の書き換え
    // ─────────────────────────────────────────────

    /// `isAsync` メソッドを状態機械化する。kickoff へ書き換えたメソッドと生成 SM 型を返す。
    /// 非 async メソッドはそのまま返し、SM 型は生成しない。
    let private rewriteMethod (symbolTable: SymbolTable) (method: ClosedHir.Method) : ClosedHir.Method * ClosedHir.Type option =
        if not method.isAsync then
            method, None
        else
            let declaredRet =
                match method.typ with
                | TypeId.Fn (_, ret) -> ret
                | _ -> method.typ

            let gen = generateStateMachine symbolTable method declaredRet
            // kickoff の戻り値型を実体 `Task` / `Task<T>` へ正規化する（Gen の総称消去対策）。
            // override メソッドの場合、親シグネチャと整合させるため overrideTarget の戻り値型も保持される。
            let kickoffType =
                match method.typ with
                | TypeId.Fn (argTypes, _) -> TypeId.Fn (argTypes, gen.resolvedRetType)
                | other -> other
            let kickoff =
                ClosedHir.Method(
                    method.sym,
                    method.args,
                    gen.kickoffBody,
                    kickoffType,
                    method.overrideTarget,
                    // kickoff 自体は通常メソッド（async 状態は SM に移動済み）。
                    false,
                    method.span)
            kickoff, Some gen.smType

    // ─────────────────────────────────────────────
    // ClosedHir.Assembly の書き換え
    // ─────────────────────────────────────────────

    /// `ClosedHir.Assembly` 全体を走査し、`isAsync = true` のメソッドを状態機械化する。
    /// 生成された SM 型は所属モジュールの types へ追加する。
    let rewriteAssembly (symbolTable: SymbolTable) (asm: ClosedHir.Assembly) : ClosedHir.Assembly =
        let rewriteModule (modul: ClosedHir.Module) : ClosedHir.Module =
            // モジュール直下メソッドを書き換え、生成 SM 型を収集する。
            let rewrittenMethods, moduleGenTypes =
                modul.methods
                |> List.fold (fun (accMethods, accTypes) m ->
                    let rewritten, smOpt = rewriteMethod symbolTable m
                    accMethods @ [ rewritten ], accTypes @ (Option.toList smOpt)) ([], [])

            // 各型のインスタンスメソッドを書き換え、生成 SM 型を収集する。
            let rewrittenTypes, typeGenTypes =
                modul.types
                |> List.fold (fun (accTypes, accGen) t ->
                    let rewrittenTypeMethods, genFromType =
                        t.methods
                        |> List.fold (fun (accM, accG) m ->
                            let rewritten, smOpt = rewriteMethod symbolTable m
                            accM @ [ rewritten ], accG @ (Option.toList smOpt)) ([], [])
                    let rewrittenType =
                        ClosedHir.Type(
                            t.sym, t.isInterface, t.baseType, t.typeParams, t.fields, rewrittenTypeMethods)
                    accTypes @ [ rewrittenType ], accGen @ genFromType) ([], [])

            ClosedHir.Module(
                modul.name,
                rewrittenTypes @ moduleGenTypes @ typeGenTypes,
                modul.fields,
                rewrittenMethods,
                modul.scope,
                modul.closureInvokeMethods)

        ClosedHir.Assembly(asm.name, asm.modules |> List.map rewriteModule)
