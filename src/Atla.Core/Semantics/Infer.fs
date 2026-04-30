namespace Atla.Core.Semantics

open Atla.Core.Semantics.Data

module Infer =
    let rec private inferExpr (typeSubst: TypeSubst) (expr: Hir.Expr) : Hir.Expr =
        let inferType tid = Type.resolve typeSubst tid

        match expr with
        | Hir.Expr.Unit _
        | Hir.Expr.Bool _
        | Hir.Expr.Int _
        | Hir.Expr.Float _
        | Hir.Expr.String _ -> expr
        | Hir.Expr.Null (tid, span) -> Hir.Expr.Null(inferType tid, span)
        | Hir.Expr.Id (sid, tid, span) -> Hir.Expr.Id(sid, inferType tid, span)
        | Hir.Expr.Call (func, instance, args, tid, span) ->
            let inferredInstance = instance |> Option.map (inferExpr typeSubst)
            let inferredArgs = args |> List.map (inferExpr typeSubst)
            Hir.Expr.Call(func, inferredInstance, inferredArgs, inferType tid, span)
        | Hir.Expr.Lambda (args, ret, body, tid, span) ->
            let inferredArgs = args |> List.map (fun arg -> Hir.Arg(arg.sid, arg.name, inferType arg.typ, arg.span))
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

    let inferModule (typeSubst: TypeSubst, hirModule: Hir.Module) : Result<Hir.Module, Diagnostic list> =
        let inferredFields =
            hirModule.fields
            |> List.map (fun field -> Hir.Field(field.sym, Type.resolve typeSubst field.typ, inferExpr typeSubst field.body, field.span))

        let inferredMethods =
            hirModule.methods
            |> List.map (fun meth ->
                // 引数の型も型変数が解決された具体的な型に置き換える。
                let inferredArgs = meth.args |> List.map (fun (sid, tid) -> (sid, Type.resolve typeSubst tid))
                Hir.Method(meth.sym, inferredArgs, inferExpr typeSubst meth.body, Type.resolve typeSubst meth.typ, meth.span))

        let inferredTypes =
            hirModule.types
            |> List.map (fun typ ->
                let inferredTypeFields =
                    typ.fields
                    |> List.map (fun field -> Hir.Field(field.sym, Type.resolve typeSubst field.typ, inferExpr typeSubst field.body, field.span))
                let inferredTypeMethods =
                    typ.methods
                    |> List.map (fun meth ->
                        let inferredArgs = meth.args |> List.map (fun (sid, tid) -> (sid, Type.resolve typeSubst tid))
                        Hir.Method(meth.sym, inferredArgs, inferExpr typeSubst meth.body, Type.resolve typeSubst meth.typ, meth.span))
                let inferredBaseType = typ.baseType |> Option.map (Type.resolve typeSubst)
                Hir.Type(typ.sym, inferredBaseType, inferredTypeFields, inferredTypeMethods))

        let typedModule = Hir.Module(hirModule.name, inferredTypes, inferredFields, inferredMethods, hirModule.scope)

        let diagnostics = typedModule.getDiagnostics
        match diagnostics |> List.filter (fun diagnostic -> diagnostic.isError) with
        | [] -> Result.Ok typedModule
        | errors -> Result.Error errors
