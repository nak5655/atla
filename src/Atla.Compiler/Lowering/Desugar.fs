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
        | _ -> failwith "Unsupported expression type"

    let rec desugarStmt (stmt: Ast.Stmt) : Hir.Stmt =
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
        | :? Ast.TypeExpr.Id as idTypeExpr -> Hir.TypeExpr.IdType(idTypeExpr.name, idTypeExpr.span)
        | _ -> failwith "Unsupported type expression type"

    let rec desugarDataItem (dataItem: Ast.DataItem) : Hir.DataItem =
        match dataItem with
        | :? Ast.DataItem.Field as field -> Hir.DataItem.Field(field.name, desugarTypeExpr field.typeExpr, field.span)
        | _ -> failwith "Unsupported data item type"

    let rec desugarDecl (decl: Ast.Decl) : Hir.Decl =
        match decl with
        | :? Ast.Decl.Import as importDecl ->
            Hir.Decl.Import(importDecl.path, importDecl.span)
        | :? Ast.Decl.Data as dataDecl ->
            let items = dataDecl.items |> List.map desugarDataItem
            Hir.Decl.Data(dataDecl.name, items, dataDecl.span)
        | _ -> failwith "Unsupported declaration type"

    let rec desugarModule (moduleAst: Ast.Module) : Hir.Module =
        let decls = moduleAst.decls |> List.map desugarDecl
        let stmts = moduleAst.stmts |> List.map desugarStmt
        Hir.Module(decls, stmts)