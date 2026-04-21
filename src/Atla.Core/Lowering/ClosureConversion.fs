namespace Atla.Core.Lowering

open Atla.Core.Semantics.Data

module ClosureConversion =
    /// モジュール内で一意な SymbolId 採番を行う状態。
    type private ConversionState = {
        nextSymbolId: int
        generatedMethods: Hir.Method list
        diagnostics: Diagnostic list
    }

    /// 変換状態へ新しい SymbolId を採番して返す。
    let private allocateSymbolId (state: ConversionState) : SymbolId * ConversionState =
        let sid = SymbolId state.nextSymbolId
        sid, { state with nextSymbolId = state.nextSymbolId + 1 }

    /// 捕捉ラムダの診断を決定的フォーマットで追加する。
    let private addCapturedLambdaDiagnostic (ownerMethod: Hir.Method) (captured: int list) (state: ConversionState) : ConversionState =
        let capturedText = captured |> List.map string |> String.concat ", "
        let diagnostic =
            Diagnostic.Error(
                $"Closure conversion requires env-class lowering but backend support is incomplete. methodSid={ownerMethod.sym.id}, captured=[{capturedText}]",
                ownerMethod.span)
        { state with diagnostics = state.diagnostics @ [ diagnostic ] }

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
            let lambdaBound = args |> List.fold (fun (acc: Set<int>) arg -> acc.Add arg.sid.id) bound
            collectFreeVarsExpr lambdaBound globalSymbols body

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
            let bodyVars, _ = collectFreeVarsStmts (bound.Add sid.id) globalSymbols body
            Set.union iterableVars bodyVars, bound
        | Hir.Stmt.ErrorStmt _ -> Set.empty, bound

    /// lambda lifting のために式を再帰変換し、必要な生成メソッドを state に蓄積する。
    let rec private rewriteExpr (ownerMethod: Hir.Method) (bound: Set<int>) (globalSymbols: Set<int>) (expr: Hir.Expr) (state: ConversionState) : Hir.Expr * ConversionState =
        match expr with
        | Hir.Expr.Unit _
        | Hir.Expr.Int _
        | Hir.Expr.Float _
        | Hir.Expr.String _
        | Hir.Expr.Null _
        | Hir.Expr.Id _
        | Hir.Expr.ExprError _ -> expr, state
        | Hir.Expr.MemberAccess (mem, instance, tid, span) ->
            let rewrittenInstance, nextState =
                match instance with
                | Some instExpr ->
                    let rewrittenExpr, rewrittenState = rewriteExpr ownerMethod bound globalSymbols instExpr state
                    Some rewrittenExpr, rewrittenState
                | None -> None, state
            Hir.Expr.MemberAccess(mem, rewrittenInstance, tid, span), nextState
        | Hir.Expr.Call (func, instance, args, tid, span) ->
            let rewrittenInstance, instanceState =
                match instance with
                | Some instExpr ->
                    let rewrittenExpr, rewrittenState = rewriteExpr ownerMethod bound globalSymbols instExpr state
                    Some rewrittenExpr, rewrittenState
                | None -> None, state
            let rewrittenArgs, finalState =
                args
                |> List.fold (fun (acc, st) arg ->
                    let rewrittenArg, nextState = rewriteExpr ownerMethod bound globalSymbols arg st
                    acc @ [ rewrittenArg ], nextState) ([], instanceState)
            Hir.Expr.Call(func, rewrittenInstance, rewrittenArgs, tid, span), finalState
        | Hir.Expr.Block (stmts, body, tid, span) ->
            let rewrittenStmts, boundAfterStmts, stateAfterStmts = rewriteStmts ownerMethod bound globalSymbols stmts state
            let rewrittenBody, finalState = rewriteExpr ownerMethod boundAfterStmts globalSymbols body stateAfterStmts
            Hir.Expr.Block(rewrittenStmts, rewrittenBody, tid, span), finalState
        | Hir.Expr.If (cond, thenBranch, elseBranch, tid, span) ->
            let rewrittenCond, condState = rewriteExpr ownerMethod bound globalSymbols cond state
            let rewrittenThen, thenState = rewriteExpr ownerMethod bound globalSymbols thenBranch condState
            let rewrittenElse, finalState = rewriteExpr ownerMethod bound globalSymbols elseBranch thenState
            Hir.Expr.If(rewrittenCond, rewrittenThen, rewrittenElse, tid, span), finalState
        | Hir.Expr.Lambda (args, ret, body, tid, span) ->
            let lambdaBound = args |> List.fold (fun (acc: Set<int>) arg -> acc.Add arg.sid.id) bound
            let rewrittenBody, bodyState = rewriteExpr ownerMethod lambdaBound globalSymbols body state
            let captured =
                collectFreeVarsExpr lambdaBound globalSymbols rewrittenBody
                |> Set.toList
                |> List.sort

            match captured with
            | [] ->
                let liftedSid, sidAllocatedState = allocateSymbolId bodyState
                let methodArgs = args |> List.map (fun arg -> arg.sid, arg.typ)
                let liftedMethodType = TypeId.Fn(methodArgs |> List.map snd, ret)
                let liftedMethod = Hir.Method(liftedSid, methodArgs, rewrittenBody, liftedMethodType, span)
                let updatedState = { sidAllocatedState with generatedMethods = sidAllocatedState.generatedMethods @ [ liftedMethod ] }
                Hir.Expr.Id(liftedSid, tid, span), updatedState
            | _ ->
                let diagnosticState = addCapturedLambdaDiagnostic ownerMethod captured bodyState
                Hir.Expr.Lambda(args, ret, rewrittenBody, tid, span), diagnosticState

    /// lambda lifting のために文を再帰変換し、Let の束縛を逐次反映する。
    and private rewriteStmt (ownerMethod: Hir.Method) (bound: Set<int>) (globalSymbols: Set<int>) (stmt: Hir.Stmt) (state: ConversionState) : Hir.Stmt * Set<int> * ConversionState =
        match stmt with
        | Hir.Stmt.Let (sid, isMutable, value, span) ->
            let rewrittenValue, nextState = rewriteExpr ownerMethod bound globalSymbols value state
            Hir.Stmt.Let(sid, isMutable, rewrittenValue, span), bound.Add sid.id, nextState
        | Hir.Stmt.Assign (sid, value, span) ->
            let rewrittenValue, nextState = rewriteExpr ownerMethod bound globalSymbols value state
            Hir.Stmt.Assign(sid, rewrittenValue, span), bound, nextState
        | Hir.Stmt.ExprStmt (value, span) ->
            let rewrittenValue, nextState = rewriteExpr ownerMethod bound globalSymbols value state
            Hir.Stmt.ExprStmt(rewrittenValue, span), bound, nextState
        | Hir.Stmt.For (sid, tid, iterable, body, span) ->
            let rewrittenIterable, iterableState = rewriteExpr ownerMethod bound globalSymbols iterable state
            let rewrittenBody, _, bodyState = rewriteStmts ownerMethod (bound.Add sid.id) globalSymbols body iterableState
            Hir.Stmt.For(sid, tid, rewrittenIterable, rewrittenBody, span), bound, bodyState
        | Hir.Stmt.ErrorStmt _ -> stmt, bound, state

    /// 文列を逐次変換し、最終 bound と state を返す。
    and private rewriteStmts (ownerMethod: Hir.Method) (bound: Set<int>) (globalSymbols: Set<int>) (stmts: Hir.Stmt list) (state: ConversionState) : Hir.Stmt list * Set<int> * ConversionState =
        stmts
        |> List.fold (fun (rewrittenStmts, currentBound, currentState) stmt ->
            let rewrittenStmt, nextBound, nextState = rewriteStmt ownerMethod currentBound globalSymbols stmt currentState
            rewrittenStmts @ [ rewrittenStmt ], nextBound, nextState) ([], bound, state)

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

    /// モジュール内 method を順序保持で変換し、生成メソッドを末尾追加した method 一覧を返す。
    let private rewriteMethods (hirModule: Hir.Module) : Hir.Method list * Diagnostic list =
        let globalSymbols = hirModule.methods |> List.map (fun methodInfo -> methodInfo.sym.id) |> Set.ofList
        let maxInModule =
            let maxMethodSid = hirModule.methods |> List.map (fun methodInfo -> methodInfo.sym.id) |> List.fold max -1
            let maxExprSid = hirModule.methods |> List.map (fun methodInfo -> collectMaxSymbolIdExpr methodInfo.body) |> List.fold max -1
            max maxMethodSid maxExprSid
        let initialState = { nextSymbolId = maxInModule + 1; generatedMethods = []; diagnostics = [] }

        let rewrittenMethods, finalState =
            hirModule.methods
            |> List.fold (fun (accMethods, state) methodInfo ->
                let methodBound = methodInfo.args |> List.map fst |> List.fold (fun (acc: Set<int>) sid -> acc.Add sid.id) Set.empty
                let rewrittenBody, bodyState = rewriteExpr methodInfo methodBound globalSymbols methodInfo.body state
                let rewrittenMethod = Hir.Method(methodInfo.sym, methodInfo.args, rewrittenBody, methodInfo.typ, methodInfo.span)
                accMethods @ [ rewrittenMethod ], bodyState) ([], initialState)

        rewrittenMethods @ finalState.generatedMethods, finalState.diagnostics

    /// クロージャー変換前処理を行い、非捕捉ラムダを lambda lifting で lower する。
    let preprocessAssembly (asm: Hir.Assembly) : PhaseResult<Hir.Assembly> =
        let rewrittenModules, diagnostics =
            asm.modules
            |> List.fold (fun (modules, allDiagnostics) hirModule ->
                let rewrittenMethods, moduleDiagnostics = rewriteMethods hirModule
                let rewrittenModule = Hir.Module(hirModule.name, hirModule.types, hirModule.fields, rewrittenMethods, hirModule.scope)
                modules @ [ rewrittenModule ], allDiagnostics @ moduleDiagnostics) ([], [])

        match diagnostics with
        | [] -> PhaseResult.succeeded (Hir.Assembly(asm.name, rewrittenModules)) []
        | _ -> PhaseResult.failed diagnostics
