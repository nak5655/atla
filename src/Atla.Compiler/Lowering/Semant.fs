namespace Atla.Compiler.Lowering

open Atla.Compiler.Ast
open Atla.Compiler.Hir

module Semant =
    let rec analyzeExpr (scope: Scope) (expr: Ast.Expr) : Hir.Expr =
        match expr with
        | :? Ast.Expr.Unit as unitExpr -> Hir.Expr.Unit(unitExpr.span)
        | :? Ast.Expr.Int as intExpr -> Hir.Expr.Int(intExpr.value, intExpr.span)
        | :? Ast.Expr.Float as floatExpr -> Hir.Expr.Float(floatExpr.value, floatExpr.span)
        | :? Ast.Expr.String as stringExpr -> Hir.Expr.String(stringExpr.value, stringExpr.span)
        | :? Ast.Expr.Id as idExpr -> Hir.Expr.Id(idExpr.name, idExpr.span)
        | :? Ast.Expr.Block as blockExpr ->
            let blockScope = Scope(Some scope)
            let stmts = blockExpr.stmts |> List.map (analyzeStmt blockScope)
            Hir.Expr.Block(stmts, blockScope, blockExpr.span)
        | :? Ast.Expr.Apply as applyExpr ->
            let func = analyzeExpr scope applyExpr.func
            let args = applyExpr.args |> List.map (analyzeExpr scope)
            Hir.Expr.Apply(func, args, applyExpr.span)
        | :? Ast.Expr.MemberAccess as memberAccessExpr ->
            let receiver = analyzeExpr scope memberAccessExpr.receiver
            let memberName = memberAccessExpr.memberName
            Hir.Expr.MemberAccess(receiver, memberName, memberAccessExpr.span)
        | :? Ast.Expr.If as ifExpr ->
            let rec analyzeIfBranches (branches: (Ast.IfBranch) list) : Hir.Expr =
                match List.head branches with
                | :? Ast.IfBranch.Then as thenBranch ->
                    let cond = analyzeExpr scope thenBranch.cond
                    let body = analyzeExpr scope thenBranch.body
                    Hir.Expr.If(cond, body, analyzeIfBranches (List.tail branches), { left = thenBranch.span.left; right = (List.last branches).span.right }) :> Hir.Expr
                | :? Ast.IfBranch.Else as elseBranch ->
                    analyzeExpr scope elseBranch.body
                | _ -> failwith "Unsupported if branch type"
            analyzeIfBranches ifExpr.branches
        | _ -> failwith "Unsupported expression type"

    and analyzeStmt (scope: Scope) (stmt: Ast.Stmt) : Hir.Stmt =
        match stmt with
        | :? Ast.Stmt.Let as letStmt ->
            Hir.Stmt.Let(letStmt.name, false, analyzeExpr scope letStmt.value, letStmt.span)
        | :? Ast.Stmt.Var as varStmt ->
            Hir.Stmt.Let(varStmt.name, true, analyzeExpr scope varStmt.value, varStmt.span)
        | :? Ast.Stmt.Assign as assignStmt ->
            Hir.Stmt.Assign(assignStmt.name, analyzeExpr scope assignStmt.value, assignStmt.span)
        | :? Ast.Stmt.ExprStmt as exprStmt ->
            Hir.Stmt.ExprStmt(analyzeExpr scope exprStmt.expr, exprStmt.span)
        | _ -> failwith "Unsupported statement type"

    let rec analyzeTypeExpr (typeExpr: Ast.TypeExpr) : Hir.TypeExpr =
        match typeExpr with
        | :? Ast.TypeExpr.Unit as unitTypeExpr -> Hir.TypeExpr.Unit(unitTypeExpr.span)
        | :? Ast.TypeExpr.Id as idTypeExpr -> Hir.TypeExpr.Id(idTypeExpr.name, idTypeExpr.span)
        | _ -> failwith "Unsupported type expression type"

    let rec analyzeFnArg (fnArg: Ast.FnArg) : Hir.FnArg =
        match fnArg with
        | :? Ast.FnArg.Unit as unitArg -> Hir.FnArg.Unit(unitArg.span)
        | :? Ast.FnArg.Named as namedArg -> Hir.FnArg.Named(namedArg.name, analyzeTypeExpr namedArg.typeExpr, namedArg.span)

    let rec analyzeDecl (moduleScope: Scope) (decl: Ast.Decl) : Hir.Decl =
        match decl with
        | :? Ast.Decl.Import as importDecl ->
            if importDecl.path.Length = 0 then
                Hir.Decl.DeclError("Import path cannot be empty", importDecl.span)
            else
                Hir.Decl.TypeDef(List.last importDecl.path, Hir.TypeExpr.Import(importDecl.path, importDecl.span), importDecl.span)
        | :? Ast.Decl.Fn as fnDecl ->
            let fnScope = Scope(Some moduleScope)
            let args = fnDecl.args |> List.map analyzeFnArg
            let ret = analyzeTypeExpr fnDecl.ret
            let body = analyzeExpr fnScope fnDecl.body
            Hir.Decl.Fn(fnDecl.name, args, ret, body, Scope(Some moduleScope), fnDecl.span)
        | _ -> failwith "Unsupported declaration type"

    let rec analyzeModule (moduleName: string, moduleAst: Ast.Module, globalScope: Scope) : Hir.Module =
        let moduleScope = Scope(Some globalScope)
        let decls = moduleAst.decls |> List.map (analyzeDecl moduleScope)
        Hir.Module(moduleName, decls, moduleScope)