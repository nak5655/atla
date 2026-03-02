namespace Atla.Compiler.Ast.Eval

open Atla.Compiler.Types
open Atla.Compiler.Ast

module Eval =
    let rec evalExpr (scope: Scope) (expr: Ast.Expr) : Value =
        match expr with
        | :? Ast.Expr.Unit as unitExpr -> Value.Unit
        | :? Ast.Expr.Int as intExpr -> Value.Int(intExpr.value)
        | :? Ast.Expr.Float as floatExpr -> Value.Float(floatExpr.value)
        | :? Ast.Expr.String as stringExpr -> Value.String(stringExpr.value)
        | :? Ast.Expr.Id as idExpr ->
            match scope.GetVar(idExpr.name) with
            | Some variable -> variable.value
            | None -> failwithf "Variable '%s' is not defined in this scope" idExpr.name
        | :? Ast.Expr.Apply as applyExpr ->
            let funcValue = evalExpr scope applyExpr.func
            let argValues = applyExpr.args |> List.map (evalExpr scope)
            match funcValue with
            | Value.Function func -> func argValues
            | _ -> failwith "Attempted to apply a non-function value"
        | _ -> failwith "Unsupported expression type"

    let evalStmt (scope: Scope) (stmt: Ast.Stmt) : unit =
        match stmt with
            | :? Ast.Stmt.Let as letStmt ->
                let value = evalExpr scope letStmt.value
                if scope.HasVar(letStmt.name) then
                    failwithf "Variable '%s' is already defined in this scope" letStmt.name
                scope.SetVar(letStmt.name, { value = value; isMutable = false })
            | :? Ast.Stmt.Var as varStmt ->
                let value = evalExpr scope varStmt.value
                if scope.HasVar(varStmt.name) then
                    failwithf "Variable '%s' is already defined in this scope" varStmt.name
                scope.SetVar(varStmt.name, { value = value; isMutable = true })
            | :? Ast.Stmt.Assign as assignStmt ->
                let value = evalExpr scope assignStmt.value
                match scope.GetVar(assignStmt.name) with
                    | Some variable when variable.isMutable -> scope.SetVar(assignStmt.name, { value = value; isMutable = true })
                    | Some _ -> failwithf "Variable '%s' is immutable and cannot be assigned to" assignStmt.name
                    | None -> failwithf "Variable '%s' is not defined in this scope" assignStmt.name

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

        for stmt in moduleDecl.stmts do
            evalStmt scope stmt