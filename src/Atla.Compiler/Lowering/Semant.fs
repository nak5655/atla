namespace Atla.Compiler.Lowering

open Atla.Compiler.Ast
open Atla.Compiler.Hir

module Semant =
    let rec analyzeExpr (expr: Ast.Expr) : Hir.Expr =
        match expr with
        | :? Ast.Expr.Unit as unitExpr -> Hir.Expr.Unit(unitExpr.span)
        | :? Ast.Expr.Int as intExpr -> Hir.Expr.Int(intExpr.value, intExpr.span)
        | :? Ast.Expr.Float as floatExpr -> Hir.Expr.Float(floatExpr.value, floatExpr.span)
        | :? Ast.Expr.String as stringExpr -> Hir.Expr.String(stringExpr.value, stringExpr.span)
        | :? Ast.Expr.Id as idExpr -> Hir.Expr.Id(idExpr.name, idExpr.span)
        | :? Ast.Expr.Block as blockExpr ->
            let stmts = blockExpr.stmts |> List.map analyzeStmt
            Hir.Expr.Block(stmts, blockExpr.span)
        | :? Ast.Expr.Apply as applyExpr ->
            let func = analyzeExpr applyExpr.func
            let args = applyExpr.args |> List.map analyzeExpr
            Hir.Expr.Apply(func, args, applyExpr.span)
        | :? Ast.Expr.MemberAccess as memberAccessExpr ->
            let receiver = analyzeExpr memberAccessExpr.receiver
            let memberName = memberAccessExpr.memberName
            Hir.Expr.MemberAccess(receiver, memberName, memberAccessExpr.span)
        | :? Ast.Expr.If as ifExpr ->
            let rec analyzeIfBranches (branches: (Ast.IfBranch) list) : Hir.Expr =
                match List.head branches with
                | :? Ast.IfBranch.Then as thenBranch ->
                    let cond = analyzeExpr thenBranch.cond
                    let body = analyzeExpr thenBranch.body
                    Hir.Expr.If(cond, body, analyzeIfBranches (List.tail branches), { left = thenBranch.span.left; right = (List.last branches).span.right }) :> Hir.Expr
                | :? Ast.IfBranch.Else as elseBranch ->
                    analyzeExpr elseBranch.body
                | _ -> failwith "Unsupported if branch type"
            analyzeIfBranches ifExpr.branches
        | _ -> failwith "Unsupported expression type"

    and analyzeStmt (stmt: Ast.Stmt) : Hir.Stmt =
        match stmt with
        | :? Ast.Stmt.Let as letStmt ->
            Hir.Stmt.Let(letStmt.name, false, analyzeExpr letStmt.value, letStmt.span)
        | :? Ast.Stmt.Var as varStmt ->
            Hir.Stmt.Let(varStmt.name, true, analyzeExpr varStmt.value, varStmt.span)
        | :? Ast.Stmt.Assign as assignStmt ->
            Hir.Stmt.Assign(assignStmt.name, analyzeExpr assignStmt.value, assignStmt.span)
        | :? Ast.Stmt.ExprStmt as exprStmt ->
            Hir.Stmt.ExprStmt(analyzeExpr exprStmt.expr, exprStmt.span)
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

    let rec analyzeDecl (decl: Ast.Decl) : Hir.Decl =
        match decl with
        | :? Ast.Decl.Import as importDecl ->
            if importDecl.path.Length = 0 then
                Hir.Decl.DeclError("Import path cannot be empty", importDecl.span)
            else
                Hir.Decl.TypeDef(List.last importDecl.path, Hir.TypeExpr.Import(importDecl.path, importDecl.span), importDecl.span)
        | :? Ast.Decl.Fn as fnDecl ->
            let args = fnDecl.args |> List.map analyzeFnArg
            let ret = analyzeTypeExpr fnDecl.ret
            let body = analyzeExpr fnDecl.body
            Hir.Decl.Fn(fnDecl.name, args, ret, body, fnDecl.span)
        | _ -> failwith "Unsupported declaration type"

    let rec analyzeModule (moduleName: string, moduleAst: Ast.Module) : Hir.Module =
        let decls = moduleAst.decls |> List.map analyzeDecl
        Hir.Module(moduleName, decls)