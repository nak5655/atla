namespace Atla.Core.Semantics

open Atla.Core.Semantics.Data

module Infer =
    let rec private inferExpr (typeSubst: TypeSubst) (expr: Hir.Expr) : Hir.Expr =
        let inferType tid = Type.resolve typeSubst tid

        match expr with
        | Hir.Expr.Unit _
        | Hir.Expr.Int _
        | Hir.Expr.Float _
        | Hir.Expr.String _ -> expr
        | Hir.Expr.Id (sid, tid, span) -> Hir.Expr.Id(sid, inferType tid, span)
        | Hir.Expr.Call (func, instance, args, tid, span) ->
            let inferredInstance = instance |> Option.map (inferExpr typeSubst)
            let inferredArgs = args |> List.map (inferExpr typeSubst)
            Hir.Expr.Call(func, inferredInstance, inferredArgs, inferType tid, span)
        | Hir.Expr.Lambda (args, ret, body, tid, span) ->
            let inferredArgs = args |> List.map (fun arg -> Hir.Arg(arg.name, inferType arg.typ, arg.span))
            let inferredRet = inferType ret
            let inferredBody = inferExpr typeSubst body
            Hir.Expr.Lambda(inferredArgs, inferredRet, inferredBody, inferType tid, span)
        | Hir.Expr.MemberAccess (mem, instance, tid, span) ->
            let inferredInstance = instance |> Option.map (inferExpr typeSubst)
            Hir.Expr.MemberAccess(mem, inferredInstance, inferType tid, span)
        | Hir.Expr.Block (stmts, body, tid, span) ->
            let inferredStmts = stmts |> List.map (inferStmt typeSubst)
            let inferredBody = inferExpr typeSubst body
            Hir.Expr.Block(inferredStmts, inferredBody, inferType tid, span)
        | Hir.Expr.If (cond, thenBranch, elseBranch, tid, span) ->
            let inferredCond = inferExpr typeSubst cond
            let inferredThen = inferExpr typeSubst thenBranch
            let inferredElse = inferExpr typeSubst elseBranch
            Hir.Expr.If(inferredCond, inferredThen, inferredElse, inferType tid, span)
        | Hir.Expr.ExprError (message, errTyp, span) ->
            Hir.Expr.ExprError(message, inferType errTyp, span)

    and private inferStmt (typeSubst: TypeSubst) (stmt: Hir.Stmt) : Hir.Stmt =
        match stmt with
        | Hir.Stmt.Let (sid, isMutable, value, span) ->
            Hir.Stmt.Let(sid, isMutable, inferExpr typeSubst value, span)
        | Hir.Stmt.Assign (sid, value, span) ->
            Hir.Stmt.Assign(sid, inferExpr typeSubst value, span)
        | Hir.Stmt.ExprStmt (expr, span) ->
            Hir.Stmt.ExprStmt(inferExpr typeSubst expr, span)
        | Hir.Stmt.For (sid, tid, iterable, body, span) ->
            let inferredTid = Type.resolve typeSubst tid
            let inferredIterable = inferExpr typeSubst iterable
            let inferredBody = body |> List.map (inferStmt typeSubst)
            Hir.Stmt.For(sid, inferredTid, inferredIterable, inferredBody, span)
        | Hir.Stmt.ErrorStmt _ -> stmt

    let inferModule (typeSubst: TypeSubst, hirModule: Hir.Module) : Result<Hir.Module, Error list> =
        let inferredFields =
            hirModule.fields
            |> List.map (fun field -> Hir.Field(field.sym, Type.resolve typeSubst field.typ, inferExpr typeSubst field.body, field.span))

        let inferredMethods =
            hirModule.methods
            |> List.map (fun meth -> Hir.Method(meth.sym, inferExpr typeSubst meth.body, Type.resolve typeSubst meth.typ, meth.span))

        let inferredTypes =
            hirModule.types
            |> List.map (fun typ ->
                let inferredTypeFields =
                    typ.fields
                    |> List.map (fun field -> Hir.Field(field.sym, Type.resolve typeSubst field.typ, inferExpr typeSubst field.body, field.span))
                Hir.Type(typ.sym, inferredTypeFields))

        let typedModule = Hir.Module(hirModule.name, inferredTypes, inferredFields, inferredMethods, hirModule.scope)

        match typedModule.getErrors with
        | [] -> Result.Ok typedModule
        | diagnostics -> Result.Error diagnostics
