namespace Atla.Core.Lowering

open Atla.Core.Semantics.Data

module ClosureConversion =
    /// 捕捉変数1件分のメタデータ。
    type private CapturedVarMetadata = {
        sid: int
        isMutable: bool
        typ: TypeId
    }

    /// 1つのラムダ式に対する捕捉情報メタデータ。
    type private LambdaCaptureMetadata = {
        ownerMethodSid: int
        span: Atla.Core.Data.Span
        captured: CapturedVarMetadata list
    }

    /// モジュール内で一意な SymbolId 採番を行う状態。
    type private ConversionState = {
        nextSymbolId: int
        generatedMethods: Hir.Method list
        generatedTypes: Hir.Type list
        // クロージャー invoke メソッドの (liftedMethodSid -> envTypeSid) マッピング。
        closureInvokeMethods: Map<int, int>
        diagnostics: Diagnostic list
        captureMetadata: LambdaCaptureMetadata list
    }

    /// 変換状態へ新しい SymbolId を採番して返す。
    let private allocateSymbolId (state: ConversionState) : SymbolId * ConversionState =
        let sid = SymbolId state.nextSymbolId
        sid, { state with nextSymbolId = state.nextSymbolId + 1 }

    /// 式内の自由変数集合を収集する（globalSymbols は捕捉対象から除外）。
    let rec private collectFreeVarsExpr (bound: Set<int>) (globalSymbols: Set<int>) (expr: Hir.Expr) : Set<int> =
        match expr with
        | Hir.Expr.Unit _
        | Hir.Expr.Int _
        | Hir.Expr.Float _
        | Hir.Expr.String _
        | Hir.Expr.Null _
        | Hir.Expr.ExprError _ -> Set.empty
        | Hir.Expr.Id (sid, _, _) ->
            if bound.Contains sid.id || globalSymbols.Contains sid.id then Set.empty else Set.singleton sid.id
        | Hir.Expr.MemberAccess (_, instance, _, _) ->
            instance
            |> Option.map (collectFreeVarsExpr bound globalSymbols)
            |> Option.defaultValue Set.empty
        | Hir.Expr.Call (_, instance, args, _, _) ->
            let instanceVars =
                instance
                |> Option.map (collectFreeVarsExpr bound globalSymbols)
                |> Option.defaultValue Set.empty
            let argVars = args |> List.map (collectFreeVarsExpr bound globalSymbols) |> List.fold Set.union Set.empty
            Set.union instanceVars argVars
        | Hir.Expr.Block (stmts, body, _, _) ->
            let freeInStmts, boundAfterStmts = collectFreeVarsStmts bound globalSymbols stmts
            let freeInBody = collectFreeVarsExpr boundAfterStmts globalSymbols body
            Set.union freeInStmts freeInBody
        | Hir.Expr.If (cond, thenBranch, elseBranch, _, _) ->
            [ collectFreeVarsExpr bound globalSymbols cond
              collectFreeVarsExpr bound globalSymbols thenBranch
              collectFreeVarsExpr bound globalSymbols elseBranch ]
            |> List.fold Set.union Set.empty
        | Hir.Expr.Lambda (args, _, body, _, _) ->
            // ラムダの自由変数判定では、外側スコープの束縛は「自由変数候補」として扱う。
            // ここではラムダ自身の引数のみを束縛集合へ入れる。
            let lambdaBound = args |> List.fold (fun (acc: Set<int>) arg -> acc.Add arg.sid.id) Set.empty
            collectFreeVarsExpr lambdaBound globalSymbols body
        // EnvFieldLoad は env インスタンス引数へのアクセスで、外側スコープの自由変数ではない。
        | Hir.Expr.EnvFieldLoad _ -> Set.empty
        // ClosureCreate が捕捉している変数 sid は外側スコープで参照が必要。
        | Hir.Expr.ClosureCreate (_, _, captured, _, _) ->
            captured |> List.map (fun (sid, _, _) -> sid.id) |> Set.ofList

    /// 文列を宣言順に走査し、自由変数集合と更新後 bound を返す。
    and private collectFreeVarsStmts (bound: Set<int>) (globalSymbols: Set<int>) (stmts: Hir.Stmt list) : Set<int> * Set<int> =
        stmts
        |> List.fold (fun (freeVars, currentBound) stmt ->
            let stmtFree, nextBound = collectFreeVarsStmt currentBound globalSymbols stmt
            Set.union freeVars stmtFree, nextBound) (Set.empty, bound)

    /// 単一文の自由変数集合と更新後 bound を返す。
    and private collectFreeVarsStmt (bound: Set<int>) (globalSymbols: Set<int>) (stmt: Hir.Stmt) : Set<int> * Set<int> =
        match stmt with
        | Hir.Stmt.Let (sid, _, value, _) ->
            collectFreeVarsExpr bound globalSymbols value, bound.Add sid.id
        | Hir.Stmt.Assign (_, value, _)
        | Hir.Stmt.ExprStmt (value, _) ->
            collectFreeVarsExpr bound globalSymbols value, bound
        | Hir.Stmt.For (sid, _, iterable, body, _) ->
            let iterableVars = collectFreeVarsExpr bound globalSymbols iterable
            // for 反復変数 sid はボディ内で束縛済みとして扱う。
            // ラムダがボディ内でこの変数を参照した場合は「捕捉」とみなされる（env-class 方式で処理される）。
            // これは C#互換の「反復ごと新規束縛」セマンティクスに対応している。
            let bodyVars, _ = collectFreeVarsStmts (bound.Add sid.id) globalSymbols body
            Set.union iterableVars bodyVars, bound
        | Hir.Stmt.ErrorStmt _ -> Set.empty, bound

    /// lifted invoke method 本体内の捕捉変数参照（Hir.Expr.Id）を EnvFieldLoad へ書き換える。
    let rec private rewriteCapturedRefs (envArgSid: SymbolId) (capturedSids: Set<int>) (expr: Hir.Expr) : Hir.Expr =
        match expr with
        | Hir.Expr.Id (sid, tid, span) when capturedSids.Contains sid.id ->
            // 捕捉変数参照を env インスタンスのフィールドアクセスへ変換する。
            Hir.Expr.EnvFieldLoad(envArgSid, sid, tid, span)
        | Hir.Expr.Unit _
        | Hir.Expr.Int _
        | Hir.Expr.Float _
        | Hir.Expr.String _
        | Hir.Expr.Null _
        | Hir.Expr.ExprError _
        | Hir.Expr.Id _ -> expr
        | Hir.Expr.EnvFieldLoad _ -> expr
        | Hir.Expr.ClosureCreate _ -> expr
        | Hir.Expr.MemberAccess (mem, instance, tid, span) ->
            Hir.Expr.MemberAccess(mem, instance |> Option.map (rewriteCapturedRefs envArgSid capturedSids), tid, span)
        | Hir.Expr.Call (func, instance, args, tid, span) ->
            Hir.Expr.Call(
                func,
                instance |> Option.map (rewriteCapturedRefs envArgSid capturedSids),
                args |> List.map (rewriteCapturedRefs envArgSid capturedSids),
                tid, span)
        | Hir.Expr.Block (stmts, body, tid, span) ->
            Hir.Expr.Block(
                stmts |> List.map (rewriteCapturedRefsStmt envArgSid capturedSids),
                rewriteCapturedRefs envArgSid capturedSids body,
                tid, span)
        | Hir.Expr.If (cond, thenBranch, elseBranch, tid, span) ->
            Hir.Expr.If(
                rewriteCapturedRefs envArgSid capturedSids cond,
                rewriteCapturedRefs envArgSid capturedSids thenBranch,
                rewriteCapturedRefs envArgSid capturedSids elseBranch,
                tid, span)
        | Hir.Expr.Lambda (args, ret, body, tid, span) ->
            // Lambda ノードは rewriteExpr で処理済みのはず（ここには到達しないはず）。
            Hir.Expr.Lambda(args, ret, rewriteCapturedRefs envArgSid capturedSids body, tid, span)

    /// 捕捉変数参照の書き換えを文に適用する。
    and private rewriteCapturedRefsStmt (envArgSid: SymbolId) (capturedSids: Set<int>) (stmt: Hir.Stmt) : Hir.Stmt =
        match stmt with
        | Hir.Stmt.Let (sid, isMutable, value, span) ->
            Hir.Stmt.Let(sid, isMutable, rewriteCapturedRefs envArgSid capturedSids value, span)
        | Hir.Stmt.Assign (sid, value, span) ->
            Hir.Stmt.Assign(sid, rewriteCapturedRefs envArgSid capturedSids value, span)
        | Hir.Stmt.ExprStmt (value, span) ->
            Hir.Stmt.ExprStmt(rewriteCapturedRefs envArgSid capturedSids value, span)
        | Hir.Stmt.For (sid, tid, iterable, body, span) ->
            Hir.Stmt.For(
                sid, tid,
                rewriteCapturedRefs envArgSid capturedSids iterable,
                body |> List.map (rewriteCapturedRefsStmt envArgSid capturedSids),
                span)
        | Hir.Stmt.ErrorStmt _ -> stmt

    /// lambda lifting のために式を再帰変換し、必要な生成メソッドを state に蓄積する。
    let rec private rewriteExpr (ownerMethod: Hir.Method) (bound: Set<int>) (bindings: Map<int, bool * TypeId>) (globalSymbols: Set<int>) (expr: Hir.Expr) (state: ConversionState) : Hir.Expr * ConversionState =
        match expr with
        | Hir.Expr.Unit _
        | Hir.Expr.Int _
        | Hir.Expr.Float _
        | Hir.Expr.String _
        | Hir.Expr.Null _
        | Hir.Expr.Id _
        | Hir.Expr.ExprError _
        // 変換済みノードはそのまま返す。
        | Hir.Expr.EnvFieldLoad _
        | Hir.Expr.ClosureCreate _ -> expr, state
        | Hir.Expr.MemberAccess (mem, instance, tid, span) ->
            let rewrittenInstance, nextState =
                match instance with
                | Some instExpr ->
                    let rewrittenExpr, rewrittenState = rewriteExpr ownerMethod bound bindings globalSymbols instExpr state
                    Some rewrittenExpr, rewrittenState
                | None -> None, state
            Hir.Expr.MemberAccess(mem, rewrittenInstance, tid, span), nextState
        | Hir.Expr.Call (func, instance, args, tid, span) ->
            let rewrittenInstance, instanceState =
                match instance with
                | Some instExpr ->
                    let rewrittenExpr, rewrittenState = rewriteExpr ownerMethod bound bindings globalSymbols instExpr state
                    Some rewrittenExpr, rewrittenState
                | None -> None, state
            let rewrittenArgs, finalState =
                args
                |> List.fold (fun (acc, st) arg ->
                    let rewrittenArg, nextState = rewriteExpr ownerMethod bound bindings globalSymbols arg st
                    acc @ [ rewrittenArg ], nextState) ([], instanceState)
            Hir.Expr.Call(func, rewrittenInstance, rewrittenArgs, tid, span), finalState
        | Hir.Expr.Block (stmts, body, tid, span) ->
            let rewrittenStmts, boundAfterStmts, bindingsAfterStmts, stateAfterStmts = rewriteStmts ownerMethod bound bindings globalSymbols stmts state
            let rewrittenBody, finalState = rewriteExpr ownerMethod boundAfterStmts bindingsAfterStmts globalSymbols body stateAfterStmts
            Hir.Expr.Block(rewrittenStmts, rewrittenBody, tid, span), finalState
        | Hir.Expr.If (cond, thenBranch, elseBranch, tid, span) ->
            let rewrittenCond, condState = rewriteExpr ownerMethod bound bindings globalSymbols cond state
            let rewrittenThen, thenState = rewriteExpr ownerMethod bound bindings globalSymbols thenBranch condState
            let rewrittenElse, finalState = rewriteExpr ownerMethod bound bindings globalSymbols elseBranch thenState
            Hir.Expr.If(rewrittenCond, rewrittenThen, rewrittenElse, tid, span), finalState
        | Hir.Expr.Lambda (args, ret, body, tid, span) ->
            // 捕捉判定は「ラムダ自身の引数」を束縛として扱う（外側束縛は捕捉対象）。
            let lambdaBound = args |> List.fold (fun (acc: Set<int>) arg -> acc.Add arg.sid.id) Set.empty
            let lambdaBindings =
                args
                |> List.fold (fun (acc: Map<int, bool * TypeId>) arg -> acc.Add(arg.sid.id, (false, arg.typ))) bindings
            let rewrittenBody, bodyState = rewriteExpr ownerMethod lambdaBound lambdaBindings globalSymbols body state
            let captured =
                collectFreeVarsExpr lambdaBound globalSymbols rewrittenBody
                |> Set.toList
                |> List.sort

            match captured with
            | [] ->
                // 非捕捉ラムダ: static delegate として lambda lifting する。
                let liftedSid, sidAllocatedState = allocateSymbolId bodyState
                let methodArgs = args |> List.map (fun arg -> arg.sid, arg.typ)
                let liftedMethodType = TypeId.Fn(methodArgs |> List.map snd, ret)
                let liftedMethod = Hir.Method(liftedSid, methodArgs, rewrittenBody, liftedMethodType, span)
                let updatedState = { sidAllocatedState with generatedMethods = sidAllocatedState.generatedMethods @ [ liftedMethod ] }
                Hir.Expr.Id(liftedSid, tid, span), updatedState
            | _ ->
                // 捕捉ラムダ: env-class 方式でクロージャーを生成する。
                let capturedWithBindings =
                    captured
                    |> List.map (fun sid ->
                        match bindings.TryFind sid with
                        | Some(isMutable, typ) -> Some { sid = sid; isMutable = isMutable; typ = typ }, None
                        | None -> None, Some sid)
                let unknownSids = capturedWithBindings |> List.choose snd
                let capturedMetadata = capturedWithBindings |> List.choose fst

                match unknownSids with
                | _ :: _ ->
                    // 型情報を持たない捕捉変数が存在する（不正な HIR）。
                    let unknownText = unknownSids |> List.map string |> String.concat ", "
                    let diagnostic =
                        Diagnostic.Error(
                            $"Closure conversion failed: captured variable(s) have no type information. sids=[{unknownText}], ownerMethodSid={ownerMethod.sym.id}",
                            ownerMethod.span)
                    { bodyState with diagnostics = bodyState.diagnostics @ [ diagnostic ] }
                    |> fun s -> Hir.Expr.Lambda(args, ret, rewrittenBody, tid, span), s
                | [] ->
                    // env クラスの SymbolId を採番する。
                    let envTypeSid, state1 = allocateSymbolId bodyState
                    // lifted invoke メソッドの SymbolId を採番する。
                    let liftedMethodSid, state2 = allocateSymbolId state1
                    // lifted method 内で env インスタンス引数として使う SymbolId を採番する。
                    let envArgSid, state3 = allocateSymbolId state2

                    // env クラスの Hir.Field エントリを生成（各捕捉変数に対して 1 フィールド）。
                    let envFields =
                        capturedMetadata
                        |> List.map (fun cm ->
                            // フィールドの body は Unit で代用（実際の値は外側スコープから格納される）。
                            Hir.Field(SymbolId cm.sid, cm.typ, Hir.Expr.Unit span, span))

                    // ラムダ本体内の捕捉変数参照を EnvFieldLoad へ書き換える。
                    let capturedSidSet = capturedMetadata |> List.map (fun cm -> cm.sid) |> Set.ofList
                    let rewrittenBodyWithEnv = rewriteCapturedRefs envArgSid capturedSidSet rewrittenBody

                    // lifted invoke method を生成する。
                    // 第一引数は env インスタンス（TypeId.Name envTypeSid）、残りはラムダ引数。
                    let methodArgs = (envArgSid, TypeId.Name envTypeSid) :: (args |> List.map (fun arg -> arg.sid, arg.typ))
                    let liftedMethodType = TypeId.Fn(methodArgs |> List.map snd, ret)
                    let liftedMethod = Hir.Method(liftedMethodSid, methodArgs, rewrittenBodyWithEnv, liftedMethodType, span)

                    // env-class の Hir.Type を生成する。
                    let envType = Hir.Type(envTypeSid, envFields)

                    // ClosureCreate 式を生成する。
                    let capturedForCreate = capturedMetadata |> List.map (fun cm -> SymbolId cm.sid, cm.typ, cm.isMutable)
                    let closureExpr = Hir.Expr.ClosureCreate(envTypeSid, liftedMethodSid, capturedForCreate, tid, span)

                    let captureInfo = { ownerMethodSid = ownerMethod.sym.id; span = ownerMethod.span; captured = capturedMetadata }
                    let updatedState =
                        { state3 with
                            generatedMethods = state3.generatedMethods @ [ liftedMethod ]
                            generatedTypes = state3.generatedTypes @ [ envType ]
                            closureInvokeMethods = state3.closureInvokeMethods |> Map.add liftedMethodSid.id envTypeSid.id
                            captureMetadata = state3.captureMetadata @ [ captureInfo ] }

                    closureExpr, updatedState

    /// lambda lifting のために文を再帰変換し、Let の束縛を逐次反映する。
    and private rewriteStmt (ownerMethod: Hir.Method) (bound: Set<int>) (bindings: Map<int, bool * TypeId>) (globalSymbols: Set<int>) (stmt: Hir.Stmt) (state: ConversionState) : Hir.Stmt * Set<int> * Map<int, bool * TypeId> * ConversionState =
        match stmt with
        | Hir.Stmt.Let (sid, isMutable, value, span) ->
            let rewrittenValue, nextState = rewriteExpr ownerMethod bound bindings globalSymbols value state
            let nextBindings = bindings.Add(sid.id, (isMutable, value.typ))
            Hir.Stmt.Let(sid, isMutable, rewrittenValue, span), bound.Add sid.id, nextBindings, nextState
        | Hir.Stmt.Assign (sid, value, span) ->
            let rewrittenValue, nextState = rewriteExpr ownerMethod bound bindings globalSymbols value state
            Hir.Stmt.Assign(sid, rewrittenValue, span), bound, bindings, nextState
        | Hir.Stmt.ExprStmt (value, span) ->
            let rewrittenValue, nextState = rewriteExpr ownerMethod bound bindings globalSymbols value state
            Hir.Stmt.ExprStmt(rewrittenValue, span), bound, bindings, nextState
        | Hir.Stmt.For (sid, tid, iterable, body, span) ->
            let rewrittenIterable, iterableState = rewriteExpr ownerMethod bound bindings globalSymbols iterable state
            // for 反復変数 sid はボディ内で束縛済みとして扱い、ラムダからの捕捉候補となる（C#互換: 反復ごと新規束縛）。
            let bodyBindings = bindings.Add(sid.id, (false, tid))
            let rewrittenBody, _, _, bodyState = rewriteStmts ownerMethod (bound.Add sid.id) bodyBindings globalSymbols body iterableState
            Hir.Stmt.For(sid, tid, rewrittenIterable, rewrittenBody, span), bound, bindings, bodyState
        | Hir.Stmt.ErrorStmt _ -> stmt, bound, bindings, state

    /// 文列を逐次変換し、最終 bound と state を返す。
    and private rewriteStmts (ownerMethod: Hir.Method) (bound: Set<int>) (bindings: Map<int, bool * TypeId>) (globalSymbols: Set<int>) (stmts: Hir.Stmt list) (state: ConversionState) : Hir.Stmt list * Set<int> * Map<int, bool * TypeId> * ConversionState =
        stmts
        |> List.fold (fun (rewrittenStmts, currentBound, currentBindings, currentState) stmt ->
            let rewrittenStmt, nextBound, nextBindings, nextState = rewriteStmt ownerMethod currentBound currentBindings globalSymbols stmt currentState
            rewrittenStmts @ [ rewrittenStmt ], nextBound, nextBindings, nextState) ([], bound, bindings, state)

    /// モジュール内の既存 SymbolId の最大値を収集する（採番開始位置に使用）。
    let rec private collectMaxSymbolIdExpr (expr: Hir.Expr) : int =
        let fromOption (value: Hir.Expr option) =
            value |> Option.map collectMaxSymbolIdExpr |> Option.defaultValue -1
        match expr with
        | Hir.Expr.Id (sid, _, _) -> sid.id
        | Hir.Expr.Call (_, instance, args, _, _) ->
            List.max ((fromOption instance) :: (args |> List.map collectMaxSymbolIdExpr))
        | Hir.Expr.MemberAccess (_, instance, _, _) -> fromOption instance
        | Hir.Expr.Block (stmts, body, _, _) ->
            List.max ((collectMaxSymbolIdExpr body) :: (stmts |> List.map collectMaxSymbolIdStmt))
        | Hir.Expr.If (cond, thenBranch, elseBranch, _, _) ->
            [ cond; thenBranch; elseBranch ] |> List.map collectMaxSymbolIdExpr |> List.max
        | Hir.Expr.Lambda (args, _, body, _, _) ->
            let maxArgSid = args |> List.map (fun arg -> arg.sid.id) |> List.fold max -1
            max maxArgSid (collectMaxSymbolIdExpr body)
        | Hir.Expr.EnvFieldLoad (envArgSid, capturedSid, _, _) ->
            max envArgSid.id capturedSid.id
        | Hir.Expr.ClosureCreate (envTypeSid, methodSid, captured, _, _) ->
            let maxCapturedSid = captured |> List.map (fun (sid, _, _) -> sid.id) |> List.fold max -1
            [ envTypeSid.id; methodSid.id; maxCapturedSid ] |> List.max
        | _ -> -1

    /// モジュール内の文に含まれる SymbolId の最大値を収集する。
    and private collectMaxSymbolIdStmt (stmt: Hir.Stmt) : int =
        match stmt with
        | Hir.Stmt.Let (sid, _, value, _) -> max sid.id (collectMaxSymbolIdExpr value)
        | Hir.Stmt.Assign (sid, value, _) -> max sid.id (collectMaxSymbolIdExpr value)
        | Hir.Stmt.ExprStmt (value, _) -> collectMaxSymbolIdExpr value
        | Hir.Stmt.For (sid, _, iterable, body, _) ->
            max sid.id (max (collectMaxSymbolIdExpr iterable) (body |> List.map collectMaxSymbolIdStmt |> List.fold max -1))
        | Hir.Stmt.ErrorStmt _ -> -1

    /// モジュール内 method を順序保持で変換し、生成メソッドと型を末尾追加した一覧を返す。
    let private rewriteMethods (hirModule: Hir.Module) : Hir.Method list * Hir.Type list * Map<int, int> * Diagnostic list =
        let globalSymbols = hirModule.methods |> List.map (fun methodInfo -> methodInfo.sym.id) |> Set.ofList
        let maxInModule =
            let maxMethodSid = hirModule.methods |> List.map (fun methodInfo -> methodInfo.sym.id) |> List.fold max -1
            let maxExprSid = hirModule.methods |> List.map (fun methodInfo -> collectMaxSymbolIdExpr methodInfo.body) |> List.fold max -1
            max maxMethodSid maxExprSid
        let initialState =
            { nextSymbolId = maxInModule + 1
              generatedMethods = []
              generatedTypes = []
              closureInvokeMethods = Map.empty
              diagnostics = []
              captureMetadata = [] }

        let rewrittenMethods, finalState =
            hirModule.methods
            |> List.fold (fun (accMethods, state) methodInfo ->
                let methodBound = methodInfo.args |> List.map fst |> List.fold (fun (acc: Set<int>) sid -> acc.Add sid.id) Set.empty
                let methodBindings =
                    methodInfo.args
                    |> List.fold (fun (acc: Map<int, bool * TypeId>) (sid, tid) -> acc.Add(sid.id, (false, tid))) Map.empty
                let rewrittenBody, bodyState = rewriteExpr methodInfo methodBound methodBindings globalSymbols methodInfo.body state
                let rewrittenMethod = Hir.Method(methodInfo.sym, methodInfo.args, rewrittenBody, methodInfo.typ, methodInfo.span)
                accMethods @ [ rewrittenMethod ], bodyState) ([], initialState)

        rewrittenMethods @ finalState.generatedMethods,
        finalState.generatedTypes,
        finalState.closureInvokeMethods,
        finalState.diagnostics

    /// クロージャー変換前処理を行い、ラムダを lambda lifting / env-class 方式で lower する。
    let preprocessAssembly (asm: Hir.Assembly) : PhaseResult<Hir.Assembly> =
        let rewrittenModules, diagnostics =
            asm.modules
            |> List.fold (fun (modules, allDiagnostics) hirModule ->
                let rewrittenMethods, generatedTypes, closureInvokeMethods, moduleDiagnostics = rewriteMethods hirModule
                let allTypes = hirModule.types @ generatedTypes
                let rewrittenModule =
                    Hir.Module(
                        hirModule.name,
                        allTypes,
                        hirModule.fields,
                        rewrittenMethods,
                        hirModule.scope,
                        closureInvokeMethods)
                modules @ [ rewrittenModule ], allDiagnostics @ moduleDiagnostics) ([], [])

        match diagnostics with
        | [] -> PhaseResult.succeeded (Hir.Assembly(asm.name, rewrittenModules)) []
        | _ -> PhaseResult.failed diagnostics
