namespace Atla.Lang.Hir

open System
open System.IO
open System.Reflection
open System.Collections.Generic
open System.Linq

open Atla.Compiler.Hir
open Atla.Compiler.Mir
open Atla.Compiler.Types

// Problem / Severity / Span は元の定義に依存します
type Result<'T,'E> = 
    | Ok of 'T
    | Error of 'E

[<CLIMutable>]
type KNormal =
    { proc : seq<Mir.Ins>
      value : Mir.Value option } // void のとき None

type Callable =
    | MethodInfo of MethodInfo
    | Inline of Proc
    | Constructor of ConstructorInfo
    | InstanceMethod of proc: seq<Mir.Ins> * instance: Mir.Value * methodInfo: MethodInfo

type Trans() =

    // getCallable : Frame -> Hir.Scope -> Hir.Expr -> Result<Callable, Problem>
    member _.getCallable (frame: Frame) (scope: Hir.Scope) (hir: Hir.Expr) : Result<Callable, Problem> =
        match hir with
        | Hir.Expr.Id id ->
            let ty = hir.Type.pruned()
            match id.getSymbol scope with
            | Some sym ->
                match sym.kind with
                | SymbolKind.Method mi -> Ok (MethodInfo mi)
                | SymbolKind.InlineMethod proc -> Ok (Inline proc)
                | SymbolKind.Constructor ci -> Ok (Constructor ci)
                | _ ->
                    match ty with
                    | Atla.Lang.Type.Fn ->
                        let invoke = ty.ToSystemType().GetMethod("Invoke")
                        Ok (MethodInfo invoke)
                    | _ ->
                        Error (Problem(Severity.Error, sprintf "%A is not a function." hir, Span.zero()))
            | None ->
                Error (Problem(Severity.Error, sprintf "%A is not a function." hir, Span.zero()))

        | Hir.Expr.Member (expr, name) ->
            match _.trans frame scope expr with
            | Error e -> Error e
            | Ok kn ->
                match kn with
                | { proc = proc; value = Some value } ->
                    let instTy = expr.Type.pruned()
                    let _, m = instTy.findMember(name, hir.Type)
                    // プリミティブ型のインスタンス関数はアドレスを渡す
                    let inst =
                        if instTy.isPrimitive() then
                            match value with
                            | Mir.Value.Sym sym -> Mir.Value.Addr sym :> Mir.Value
                            | _ -> value
                        else value
                    match m with
                    | Some (Either.Right mi) -> Ok (InstanceMethod(proc, inst, mi))
                    | _ -> Error (Problem(Severity.Error, sprintf "%A has no members." instTy, expr.Span))
                | _ -> Error (Problem(Severity.Error, sprintf "%A has no value." expr, expr.Span))

        | Hir.Expr.StaticMember (clsName, name) ->
            let mutable ty = scope.resolveType clsName
            match ty.pruned() with
            | Type.Native nt ->
                match nt.findMember(name, hir.Type) with
                | _, Some (Either.Right mi) -> Ok (MethodInfo mi)
                | _ -> Error (Problem(Severity.Error, sprintf "%s has no member \"%s\"." clsName name, hir.Span))
            | _ ->
                // TODO: 他のケース
                Error (Problem(Severity.Error, sprintf "%s has no member \"%s\"." clsName name, hir.Span))

    member _.declareId (frame: Frame) (scope: Hir.Scope) (id: Hir.Expr.Id) : Result<Symbol, Problem> =
        match id.getSymbol scope with
        | Some sym ->
            if sym.isLocal() then
                frame.declareLocal sym
                Ok sym
            else
                Error (Problem(Severity.Error, sprintf "%s is not declared as local variable." id.Name, id.Span))
        | None ->
            Error (Problem(Severity.Error, sprintf "%s does not exist in scope." id.Name, id.Span))

    member _.findMethod (expr: Hir.Expr) (name: string) (typ: Type) : Result<MethodInfo, Problem> =
        match expr.Type.findMember(name, typ) with
        | _, Some (Either.Right mi) -> Ok mi
        | _ -> Error (Problem(Severity.Error, sprintf "%A has no member \"%s\"." expr.Type name, expr.Span))

    /// trans for statements
    member _.trans (frame: Frame) (scope: Hir.Scope) (hir: Hir.Stmt) : Result<seq<Mir.Ins>, Problem> =
        match hir with
        | Hir.Stmt.Expr e ->
            match _.trans frame scope e with
            | Error e -> Error e
            | Ok kn ->
                match kn with
                | { proc = proc } -> Ok proc
        | Hir.Stmt.Var (id, expr, _) ->
            match _.declareId frame scope id with
            | Error e -> Error e
            | Ok sym ->
                match _.trans frame scope expr with
                | Error e -> Error e
                | Ok kn ->
                    match kn with
                    | { proc = proc; value = Some res } ->
                        Ok (Seq.append proc (seq { yield Mir.Ins.Assign(sym, res) }))
                    | _ -> Error (Problem(Severity.Error, sprintf "%A has no value." expr, expr.Span))
        | Hir.Stmt.Assign (id, expr) ->
            match id.getSymbol scope with
            | None -> Error (Problem(Severity.Error, sprintf "%s does not exist in scope." id.Name, id.Span))
            | Some sym ->
                match _.trans frame scope expr with
                | Error e -> Error e
                | Ok kn ->
                    match kn with
                    | { proc = proc; value = Some res } ->
                        Ok (Seq.append proc (seq { yield Mir.Ins.Assign(sym, res) }))
                    | _ -> Error (Problem(Severity.Error, sprintf "%A has no value." expr, expr.Span))
        | Hir.Stmt.Return e ->
            match scope with
            | :? Hir.Scope.Block as bscope ->
                match _.trans frame scope e with
                | Error e -> Error e
                | Ok kn ->
                    match kn with
                    | { proc = proc; value = Some res } ->
                        Ok (Seq.append proc (seq { yield Mir.Ins.Assign(bscope.retSymbol, res); yield Mir.Ins.Jump(bscope.endLabel) }))
                    | { proc = proc } ->
                        Ok (Seq.append proc (seq { yield Mir.Ins.Jump(bscope.endLabel) }))
            | Hir.Scope.Fn ->
                match _.trans frame scope e with
                | Error e -> Error e
                | Ok kn ->
                    match kn with
                    | { proc = proc; value = Some res } ->
                        Ok (Seq.append proc (seq { yield Mir.Ins.RetValue(res) }))
                    | { proc = proc } ->
                        Ok (Seq.append proc (seq { yield Mir.Ins.Ret() }))
            | _ ->
                Error (Problem(Severity.Error, sprintf "Could not find the return destination. %A" hir, hir.Span))
        | Hir.Stmt.For st ->
            // 長い処理なので元コードのロジックに従って実装する（省略可）
            // ここでは元コードの流れを踏襲するため、trans(iter) を評価し、Iterator の型チェック、ループ本体の変換、Try/Finally の組み立てを行う
            // 実装は元コードを参照してプロジェクトに合わせて埋めてください
            Error (Problem(Severity.Error, "For-statement translation not implemented in this sample.", st.Span))

    /// trans for expressions
    member _.trans (frame: Frame) (scope: Hir.Scope) (hir: Hir.Expr) : Result<KNormal, Problem> =
        match hir with
        | Hir.Expr.Unit -> Ok { proc = Seq.empty; value = None }
        | Hir.Expr.Bool value -> Ok { proc = Seq.empty; value = Some (Mir.Value.Imm (Mir.Imm.Bool value)) }
        | Hir.Expr.Int value -> Ok { proc = Seq.empty; value = Some (Mir.Value.Imm (Mir.Imm.Int value)) }
        | Hir.Expr.Double value -> Ok { proc = Seq.empty; value = Some (Mir.Value.Imm (Mir.Imm.Double value)) }
        | Hir.Expr.String s -> Ok { proc = Seq.empty; value = Some (Mir.Value.Imm (Mir.Imm.String s)) }
        | Hir.Expr.Id (name, _) as id ->
            match id.getSymbol scope with
            | None -> Error (Problem(Severity.Error, sprintf "%s is not defined as %A type." name id.Type, id.Span))
            | Some sym ->
                match sym.kind with
                | SymbolKind.Arg ->
                    match frame.args |> Seq.tryFind (fun a -> a.name = sym.name) with
                    | Some arg -> Ok { proc = Seq.empty; value = Some (Mir.Value.Sym arg) }
                    | None -> Error (Problem(Severity.Error, sprintf "Could not find %s in arguments." name, id.Span))
                | SymbolKind.Local ->
                    match frame.locs |> Seq.tryFind (fun l -> l.name = sym.name && l.type.canUnify(sym.type)) with
                    | Some loc -> Ok { proc = Seq.empty; value = Some (Mir.Value.Sym loc) }
                    | None -> Error (Problem(Severity.Error, sprintf "Could not find %s in locals." name, id.Span))
                | SymbolKind.Field fi ->
                    let insts = scope.resolveVar("this", Type.Unknown())
                    if Seq.isEmpty insts |> not then
                        let thisSymbol = Seq.head insts
                        Ok { proc = Seq.empty; value = Some (Mir.Value.Field(thisSymbol, fi.fieldInfo)) }
                    else
                        Error (Problem(Severity.Error, "Could not find an instance named \"this\" in locals.", id.Span))
                | _ ->
                    Error (Problem(Severity.Error, sprintf "%s is not defined as %A type." name id.Type, id.Span))

        | Hir.Expr.Member (expr, name) ->
            match _.trans frame scope expr with
            | Error e -> Error e
            | Ok { proc = proc; value = Some valv } ->
                let ty, mfimi = expr.Type.findMember(name, hir.Type)
                match mfimi with
                | Some (Either.Left fi) ->
                    let sym = frame.declareTemp ty
                    if not (isNull fi) then
                        Ok { proc = Seq.append proc (seq { yield Mir.Ins.Assign(sym, valv) }); value = Some (Mir.Value.Field(sym, fi)) }
                    else
                        Error (Problem(Severity.Error, sprintf "Could not find field %s in %A." name ty, hir.Span))
                | Some (Either.Right mi) ->
                    let sym = frame.declareTemp (Type.fromSystemType mi.ReturnType)
                    Ok { proc = Seq.append proc (seq { yield Mir.Ins.CallAssign(sym, mi, [valv]) }); value = Some (Mir.Value.Sym sym) }
                | _ ->
                    Error (Problem(Severity.Error, sprintf "Could not find field %s in %A." name ty, hir.Span))
            | Ok _ -> Error (Problem(Severity.Error, sprintf "Could not interpret %A as a value." expr, expr.Span))

        | Hir.Expr.Apply (fn, args) ->
            // 引数を順に評価して K 正規化を行う
            let kArgRs = args |> Seq.map (fun a -> _.trans frame scope a) |> Seq.toArray
            // 失敗チェック
            match kArgRs |> Array.tryFind (function Error _ -> true | _ -> false) with
            | Some (Error e) -> Error e
            | _ ->
                let kArgs = kArgRs |> Array.map (function Ok v -> v | _ -> failwith "unreachable")
                let procArgs = kArgs |> Array.collect (fun k -> k.proc |> Seq.toArray) |> Seq.ofArray
                let resArgs = kArgs |> Array.map (fun k -> k.value) |> Array.choose id |> Array.toList

                match _.getCallable frame scope fn with
                | Error e -> Error e
                | Ok (MethodInfo m) ->
                    let procArgs', resArgs' =
                        if args.Length = 1 && args.[0].Type.isVoid() then Seq.empty, []
                        else procArgs, resArgs
                    if m.ReturnType = typeof<Void> then
                        Ok { proc = Seq.append procArgs' (seq { yield Mir.Ins.Call (Either.Left m, resArgs') }); value = None }
                    else
                        let tmp = frame.declareTemp m.ReturnType
                        Ok { proc = Seq.append procArgs' (seq { yield Mir.Ins.CallAssign(tmp, m, resArgs') }); value = Some (Mir.Value.Sym tmp) }

                | Ok (InstanceMethod (proc, inst, mi)) ->
                    let procArgs', resArgs' =
                        if args.Length = 1 && args.[0].Type.isVoid() then Seq.empty, []
                        else procArgs, resArgs
                    if mi.ReturnType = typeof<Void> then
                        Ok { proc = Seq.concat [procArgs'; proc; seq { yield Mir.Ins.Call (Either.Left mi, inst :: resArgs') }]; value = None }
                    else
                        let tmp = frame.declareTemp mi.ReturnType
                        Ok { proc = Seq.concat [procArgs'; proc; seq { yield Mir.Ins.CallAssign(tmp, mi, inst :: resArgs') }]; value = Some (Mir.Value.Sym tmp) }

                | Ok (Inline proc) ->
                    match fn.Type.pruned() with
                    | Atla.Lang.Type.Fn (_, ret) ->
                        let tmp = frame.declareTemp ret
                        // 元コードでは Proc.DstArg2 のようなパターンがある
                        let body =
                            match proc with
                            | Proc.DstArg2 body -> body(tmp, resArgs.[0], resArgs.[1])
                            | _ -> Seq.empty
                        Ok { proc = Seq.append procArgs body; value = Some (Mir.Value.Sym tmp) }
                    | _ -> Error (Problem(Severity.Error, sprintf "%A must be a function." fn, fn.Span))

                | Ok (Constructor ci) ->
                    match fn.Type.pruned() with
                    | Atla.Lang.Type.Fn (_, ret) ->
                        let tmp = frame.declareTemp ret
                        Ok { proc = Seq.append procArgs (seq { yield Mir.Ins.New(tmp, ci, resArgs) }); value = Some (Mir.Value.Sym tmp) }
                    | _ -> Error (Problem(Severity.Error, sprintf "%A must be a function." fn, fn.Span))

        | Hir.Expr.Block (blockScope, ss) ->
            // ブロックの帰り値を確保
            if not (blockScope.retType.isVoid()) then
                blockScope.retSymbol <- frame.declareTemp(blockScope.retType)

            let stmts = ss.ToNList()
            // 元コードのロジックに従って stmts を変換する（省略可能）
            // ここでは簡略化して、各文を順に trans して結合する実装例を示す
            let rec foldStmts acc idx =
                if idx >= stmts.Length then Ok acc
                else
                    match _.trans frame blockScope stmts.[idx] with
                    | Error e -> Error e
                    | Ok proc ->
                        foldStmts (Seq.append acc proc) (idx + 1)
            match foldStmts Seq.empty 0 with
            | Error e -> Error e
            | Ok body ->
                if blockScope.retType.isVoid() then Ok { proc = body; value = None }
                else Ok { proc = body; value = Some (Mir.Value.Sym blockScope.retSymbol) }

        | Hir.Expr.Fn ->
            failwith "Internal Error: Fn should have been removed by flattening"

        | Hir.Expr.Switch ents ->
            // 元コードのロジックに従って実装
            // ここでは概略のみ示す
            let mutable racc : Result<seq<Mir.Ins>, Problem> = Ok Seq.empty
            let endLabel = Mir.Label()
            let tmp = frame.declareTemp (hir.Type.pruned().ToSystemType())

            let bodies = ents |> Seq.map (fun ent -> (Mir.Label(), _.trans frame scope ent.body)) |> Seq.toArray

            // 分岐命令の組み立て
            for i in 0 .. ents.Count - 1 do
                let ent = ents.[i]
                let label, _ = bodies.[i]
                match _.trans frame scope ent.pred with
                | Error e -> return Error e
                | Ok { proc = p; value = Some res } ->
                    racc <- 
                        match racc with
                        | Error e -> Error e
                        | Ok acc -> Ok (Seq.append acc (Seq.append p (seq { yield Mir.Ins.JumpTrue(res, label) })))
                | Ok _ -> return Error (Problem(Severity.Error, sprintf "Switch Predicate %A is not a Value." ent.pred, ent.Span))

            // 各分岐先の追加（省略せずに実装する場合は元コードを参照）
            match racc with
            | Error e -> Error e
            | Ok r ->
                // 最後に endLabel をマークして tmp を返す
                Ok { proc = Seq.append r (seq { yield Mir.Ins.MarkLabel(endLabel) }); value = Some (Mir.Value.Sym tmp) }

    // 以下、implType / addType / addConstructor / implConstructor / addMethod / implMethod / generateModule / trans(asm) など
    // 元コードのロジックに従って同様に移植してください。
    // 省略した部分は元の Nemerle/C# コードを参照して F# に合わせて実装してください。
    //
    // 例: addType, addConstructor の移植は比較的直訳できます。
    //
    // 注意: Mir や Hir の補助関数（findMember, chooseMostConcretest など）は別モジュールとして移植してください。
    ()

module HirTransExtensions =

    open System.Reflection

    let chooseMostConcretest (methods: seq<MethodInfo>) : MethodInfo option =
        if Seq.isEmpty methods then None
        else
            let mutable res = Seq.head methods
            for m in methods |> Seq.skip 1 do
                if res.DeclaringType <> m.DeclaringType then
                    if TypeExtensions.chooseMoreConcretely(res.DeclaringType, m.DeclaringType) = res.DeclaringType then
                        ()
                    else
                        res <- m
                else
                    let mutable resres = res
                    let ps = res.GetParameters() |> Array.toList
                    let qs = m.GetParameters() |> Array.toList
                    for (a,b) in List.zip ps qs do
                        let at = a.ParameterType
                        let bt = b.ParameterType
                        if at <> bt then
                            if TypeExtensions.chooseMoreConcretely(at, bt) = at then
                                ()
                            else
                                resres <- m
                    res <- resres
            Some res

    let findMember (typ: Type) (name: string) (expected: Type) : Type * (Either<FieldInfo,MethodInfo> option) =
        match typ.pruned() with
        | Type.Define ty ->
            let fis = ty.fields |> Seq.filter (fun fi -> fi.name = name)
            if Seq.isEmpty fis then
                let fnType =
                    match expected.pruned() with
                    | Type.Fn (args, ret) -> Type.Fn(ty :: args, ret)
                    | _ -> Type.Fn([ty], expected)
                let mis = ty.methods |> Seq.filter (fun mi -> mi.name = name && mi.fn.type.canUnify(fnType))
                if Seq.isEmpty mis then (ty, None)
                else (ty, Some (Either.Right (Seq.head mis).info.Value))
            else
                (ty, Some (Either.Left (Seq.head fis).info))
        | Type.Native ty ->
            let fi = ty.type.GetField(name)
            if isNull fi then
                let methods =
                    ty.type.GetMethods()
                    |> Seq.filter (fun m -> m.Name = name)
                    |> Seq.append (ty.type.GetInterfaces() |> Seq.collect (fun i -> i.GetMethods() |> Seq.filter (fun m -> m.Name = name)))
                match expected.pruned() with
                | Type.Fn fnType ->
                    let args = fnType.args |> List.ofSeq
                    let args =
                        if args.Length > 0 && args.Head.canUnify(ty) then args.Tail else args
                    let args =
                        if args.Length = 1 && args.Head.isVoid() then [] else args
                    let methods =
                        methods
                        |> Seq.filter (fun m ->
                            let ps = m.GetParameters() |> Array.toList
                            ps.Length = args.Length &&
                            List.forall2 (fun p arg -> p.ParameterType.canAssignWith(arg.ToSystemType())) ps args)
                    match chooseMostConcretest methods with
                    | Some m -> (ty, Some (Either.Right m))
                    | None -> (ty, None)
                | _ ->
                    if Seq.length methods = 1 then (ty, Some (Either.Right (Seq.head methods)))
                    else (ty, None)
            else
                (ty, Some (Either.Left fi))
        | Type.Generic (b, ps) ->
            match b with
            | Type.Native t ->
                let gt = t.type.GetGenericTypeDefinition().MakeGenericType(ps |> Seq.map (fun p -> p.ToSystemType()) |> Seq.toArray)
                Type.Native(0, gt).findMember(name, expected)
            | _ -> b.findMember(name, expected)