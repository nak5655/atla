namespace Atla.Compiler.Lowering

open Atla.Compiler.Types
open Atla.Compiler.Ast
open Atla.Compiler.Hir

module Desugar =
    let rec desugarExpr (expr: Ast.Expr) : Hir.Expr =
        match expr with
        | :? Ast.Expr.Unit as unitExpr -> Hir.Expr.Unit(unitExpr.span)
        | :? Ast.Expr.Int as intExpr -> Hir.Expr.Int(intExpr.value, intExpr.span)
        | :? Ast.Expr.Float as floatExpr -> Hir.Expr.Float(floatExpr.value, floatExpr.span)
        | :? Ast.Expr.String as stringExpr -> Hir.Expr.String(stringExpr.value, stringExpr.span)
        | :? Ast.Expr.Id as idExpr -> Hir.Expr.Id(idExpr.name, idExpr.span)
        | :? Ast.Expr.Block as blockExpr ->
            let stmts = blockExpr.stmts |> List.map desugarStmt
            match List.last stmts with
            | Hir.Stmt.ExprStmt (lastExpr, _) -> Hir.Expr.Block(List.take(stmts.Length - 1) stmts, lastExpr, blockExpr.span)
            | _ -> Hir.Expr.Block(stmts, Hir.Expr.Unit(blockExpr.span), blockExpr.span) // ブロックの最後のステートメントが式でない場合は Unit を返す
        | :? Ast.Expr.Apply as applyExpr ->
            let func = desugarExpr applyExpr.func
            let args = applyExpr.args |> List.map desugarExpr
            Hir.Expr.Apply(func, args, applyExpr.span)
        | :? Ast.Expr.MemberAccess as memberAccessExpr ->
            let receiver = desugarExpr memberAccessExpr.receiver
            let memberName = memberAccessExpr.memberName
            Hir.Expr.MemberAccess(receiver, memberName, memberAccessExpr.span)
        | _ -> failwith "Unsupported expression type"

    and desugarStmt (stmt: Ast.Stmt) : Hir.Stmt =
        match stmt with
        | :? Ast.Stmt.Let as letStmt ->
            Hir.Stmt.Let(letStmt.name, false, desugarExpr letStmt.value, letStmt.span)
        | :? Ast.Stmt.Var as varStmt ->
            Hir.Stmt.Let(varStmt.name, true, desugarExpr varStmt.value, varStmt.span)
        | :? Ast.Stmt.Assign as assignStmt ->
            Hir.Stmt.Assign(assignStmt.name, desugarExpr assignStmt.value, assignStmt.span)
        | :? Ast.Stmt.ExprStmt as exprStmt ->
            Hir.Stmt.ExprStmt(desugarExpr exprStmt.expr, exprStmt.span)
        | _ -> failwith "Unsupported statement type"

    let rec desugarTypeExpr (typeExpr: Ast.TypeExpr) : Hir.TypeExpr =
        match typeExpr with
        | :? Ast.TypeExpr.Id as idTypeExpr -> Hir.TypeExpr.Id(idTypeExpr.name, idTypeExpr.span)
        | _ -> failwith "Unsupported type expression type"

    let rec desugarFnArg (fnArg: Ast.FnArg) : Hir.FnArg =
        match fnArg with
        | :? Ast.FnArg.Unit as unitArg -> Hir.FnArg.Unit(unitArg.span)
        | :? Ast.FnArg.Named as namedArg -> Hir.FnArg.Named(namedArg.name, desugarTypeExpr namedArg.typeExpr, namedArg.span)

    let rec desugarDecl (decl: Ast.Decl) : Hir.Decl =
        match decl with
        | :? Ast.Decl.Import as importDecl ->
            if importDecl.path.Length = 0 then
                Hir.Decl.DeclError("Import path cannot be empty", importDecl.span)
            else
                Hir.Decl.TypeDef(List.last importDecl.path, Hir.TypeExpr.Import(importDecl.path, importDecl.span), importDecl.span)
        | :? Ast.Decl.Fn as fnDecl ->
            let args = fnDecl.args |> List.map desugarFnArg
            let ret = desugarTypeExpr fnDecl.ret
            let body = desugarExpr fnDecl.body
            Hir.Decl.Def(fnDecl.name, Hir.Expr.Fn(args, ret, body, fnDecl.span), fnDecl.span)
        | _ -> failwith "Unsupported declaration type"

    let rec desugarModule (moduleAst: Ast.Module) : Hir.Module =
        let decls = moduleAst.decls |> List.map desugarDecl
        Hir.Module(decls)