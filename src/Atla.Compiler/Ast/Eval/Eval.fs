namespace Atla.Compiler.Ast.Eval

open Atla.Compiler.Types
open Atla.Compiler.Ast

module Eval =
    let evalExpr (scope: Scope) (expr: Ast.Expr) : Value =
        match expr with
        | :? Ast.Expr.Unit as unitExpr -> Value.Unit
        | :? Ast.Expr.Int as intExpr -> Value.Int(intExpr.value)
        | :? Ast.Expr.Float as floatExpr -> Value.Float(floatExpr.value)
        | :? Ast.Expr.String as stringExpr -> Value.String(stringExpr.value)
        | _ -> failwith "Unsupported expression type"

    let evalStmt (scope: Scope) (stmt: Ast.Stmt) : unit =
        match stmt with
            | :? Ast.Stmt.Let as letStmt ->
                let value = evalExpr scope letStmt.value
                scope.SetVar(letStmt.name, value)
            | :? Ast.Stmt.Var as varStmt ->
                let value = evalExpr scope varStmt.value
                scope.SetVar(varStmt.name, value)
            | :? Ast.Stmt.ExprStmt as exprStmt ->
                ignore (evalExpr scope exprStmt.expr)

    let evalDecl (scope: Scope) (decl: Ast.Decl) : unit =
        let tryGetDotnetType (path: string list) : System.Type option =
            System.AppDomain.CurrentDomain.GetAssemblies()
            |> Array.choose (fun asm -> asm.GetType(String.concat "." path) |> Option.ofObj)
            |> Array.tryHead

        match decl with
        | :? Ast.Decl.Import as importDecl ->
            match tryGetDotnetType importDecl.path with
            | Some dotnetType ->
                scope.SetType(List.last importDecl.path, Type.Native dotnetType)
            | None -> failwithf "Failed to import type: %s" (String.concat "." importDecl.path)
        | _ -> failwith "Unsupported declaration type"

    let evalModule (scope: Scope) (moduleDecl: Ast.Module) : unit =
        for decl in moduleDecl.decls do
            evalDecl scope decl