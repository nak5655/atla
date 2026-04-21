namespace Atla.Core.Lowering

open Atla.Core.Semantics.Data

module ClosureConversion =
    /// 束縛済み SymbolId 集合を使って、式内の自由変数 SymbolId 集合を収集する。
    let rec private collectFreeVarsExpr (bound: Set<int>) (expr: Hir.Expr) : Set<int> =
        match expr with
        | Hir.Expr.Unit _
        | Hir.Expr.Int _
        | Hir.Expr.Float _
        | Hir.Expr.String _
        | Hir.Expr.Null _
        | Hir.Expr.ExprError _ -> Set.empty
        | Hir.Expr.Id (sid, _, _) ->
            if bound.Contains sid.id then Set.empty else Set.singleton sid.id
        | Hir.Expr.MemberAccess (_, instance, _, _) ->
            instance
            |> Option.map (collectFreeVarsExpr bound)
            |> Option.defaultValue Set.empty
        | Hir.Expr.Call (_, instance, args, _, _) ->
            let instanceVars =
                instance
                |> Option.map (collectFreeVarsExpr bound)
                |> Option.defaultValue Set.empty
            let argVars = args |> List.map (collectFreeVarsExpr bound) |> List.fold Set.union Set.empty
            Set.union instanceVars argVars
        | Hir.Expr.Block (stmts, body, _, _) ->
            let stmtVars = stmts |> List.map (collectFreeVarsStmt bound) |> List.fold Set.union Set.empty
            let bodyVars = collectFreeVarsExpr bound body
            Set.union stmtVars bodyVars
        | Hir.Expr.If (cond, thenBranch, elseBranch, _, _) ->
            [ collectFreeVarsExpr bound cond
              collectFreeVarsExpr bound thenBranch
              collectFreeVarsExpr bound elseBranch ]
            |> List.fold Set.union Set.empty
        | Hir.Expr.Lambda (args, _, body, _, _) ->
            // Lambda 引数はこのラムダ本体内で束縛済みとして扱う。
            let lambdaBound =
                args
                |> List.fold (fun acc arg ->
                    // Arg は現状 name/type/span のみを持つため、Lambda 引数自体は自由変数にならない。
                    // ここでは名前だけを使う識別は行わず、Id 側 SymbolId 判定を優先する。
                    acc) bound
            collectFreeVarsExpr lambdaBound body

    /// 束縛済み SymbolId 集合を使って、文内の自由変数 SymbolId 集合を収集する。
    and private collectFreeVarsStmt (bound: Set<int>) (stmt: Hir.Stmt) : Set<int> =
        match stmt with
        | Hir.Stmt.Let (_, _, value, _)
        | Hir.Stmt.Assign (_, value, _)
        | Hir.Stmt.ExprStmt (value, _) ->
            collectFreeVarsExpr bound value
        | Hir.Stmt.For (sid, _, iterable, body, _) ->
            let iterableVars = collectFreeVarsExpr bound iterable
            let bodyBound = bound.Add sid.id
            let bodyVars = body |> List.map (collectFreeVarsStmt bodyBound) |> List.fold Set.union Set.empty
            Set.union iterableVars bodyVars
        | Hir.Stmt.ErrorStmt _ -> Set.empty

    /// ラムダの自由変数を列挙し、決定的な順序（昇順）で返す。
    let rec private collectCapturedLambdaVarsExpr (bound: Set<int>) (expr: Hir.Expr) : int list =
        let recurseExpr = collectCapturedLambdaVarsExpr bound
        let recurseStmt = collectCapturedLambdaVarsStmt bound
        match expr with
        | Hir.Expr.Call (_, instance, args, _, _) ->
            let instanceCaptured =
                instance
                |> Option.map recurseExpr
                |> Option.defaultValue []
            let argCaptured = args |> List.collect recurseExpr
            instanceCaptured @ argCaptured
        | Hir.Expr.MemberAccess (_, instance, _, _) ->
            instance |> Option.map recurseExpr |> Option.defaultValue []
        | Hir.Expr.Block (stmts, body, _, _) ->
            (stmts |> List.collect recurseStmt) @ recurseExpr body
        | Hir.Expr.If (cond, thenBranch, elseBranch, _, _) ->
            recurseExpr cond @ recurseExpr thenBranch @ recurseExpr elseBranch
        | Hir.Expr.Lambda (_, _, body, _, _) ->
            let captured =
                collectFreeVarsExpr bound body
                |> Set.toList
                |> List.sort
            let nestedCaptured = collectCapturedLambdaVarsExpr bound body
            captured @ nestedCaptured
        | _ -> []

    /// 文を走査して、自由変数ありラムダの捕捉変数 SymbolId を列挙する。
    and private collectCapturedLambdaVarsStmt (bound: Set<int>) (stmt: Hir.Stmt) : int list =
        match stmt with
        | Hir.Stmt.Let (sid, _, value, _) ->
            let current = collectCapturedLambdaVarsExpr bound value
            let _nextBound = bound.Add sid.id
            current
        | Hir.Stmt.Assign (_, value, _)
        | Hir.Stmt.ExprStmt (value, _) ->
            collectCapturedLambdaVarsExpr bound value
        | Hir.Stmt.For (sid, _, iterable, body, _) ->
            let iterableCaptured = collectCapturedLambdaVarsExpr bound iterable
            let bodyBound = bound.Add sid.id
            iterableCaptured @ (body |> List.collect (collectCapturedLambdaVarsStmt bodyBound))
        | Hir.Stmt.ErrorStmt _ -> []

    /// 単一メソッド内の自由変数ありラムダ捕捉を、重複排除済み・昇順で返す。
    let private collectCapturedLambdaVarsMethod (hirMethod: Hir.Method) : int list =
        let methodBound =
            hirMethod.args
            |> List.map fst
            |> List.fold (fun (acc: Set<int>) (sid: SymbolId) -> acc.Add sid.id) Set.empty
        collectCapturedLambdaVarsExpr methodBound hirMethod.body
        |> List.distinct
        |> List.sort

    /// クロージャー変換前処理を行い、未対応の自由変数ありラムダを明示診断に変換する。
    let preprocessAssembly (asm: Hir.Assembly) : PhaseResult<Hir.Assembly> =
        let diagnostics =
            asm.modules
            |> List.collect (fun modul ->
                modul.methods
                |> List.choose (fun hirMethod ->
                    let captured = collectCapturedLambdaVarsMethod hirMethod
                    if List.isEmpty captured then
                        None
                    else
                        let capturedText = captured |> List.map string |> String.concat ", "
                        Some(
                            Diagnostic.Error(
                                $"Closure conversion requires env-class lowering but backend support is incomplete. methodSid={hirMethod.sym.id}, captured=[{capturedText}]",
                                hirMethod.span))))

        match diagnostics with
        | [] -> PhaseResult.succeeded asm []
        | _ -> PhaseResult.failed diagnostics
