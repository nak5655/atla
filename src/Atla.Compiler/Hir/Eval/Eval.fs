namespace Atla.Compiler.Hir.Eval

open Atla.Compiler.Types
open Atla.Compiler.Hir

module Eval =
    let rec evalExpr (scope: Scope) (expr: Hir.Expr) : Value =
        match expr with
        | Hir.Expr.Unit _ -> Value.Unit
        | Hir.Expr.Int (v, _) -> Value.Int v
        | Hir.Expr.Float (v, _) -> Value.Float v
        | Hir.Expr.String (v, _) -> Value.String v
        | Hir.Expr.Id (name, _) ->
            match scope.GetVar(name) with
            | Some variable -> variable.value
            | None -> failwithf "Variable '%s' is not defined in this scope" name
        | Hir.Expr.Apply (funcExpr, args, _) ->
            let funcValue = evalExpr scope funcExpr
            let argValues = args |> List.map (evalExpr scope)
            match funcValue with
            | Value.Function func -> func argValues
            | _ -> failwith "Attempted to apply a non-function value"
        | _ -> failwith "Unsupported expression type"

    let evalStmt (scope: Scope) (stmt: Hir.Stmt) : unit =
        match stmt with
        | Hir.Stmt.Let (name, mut, valueExpr, _) ->
            let value = evalExpr scope valueExpr
            if scope.HasVar(name) then
                failwithf "Variable '%s' is already defined in this scope" name
            scope.SetVar(name, { value = value; isMutable = mut })
        | Hir.Stmt.Assign (name, valueExpr, _) ->
            let value = evalExpr scope valueExpr
            match scope.GetVar(name) with
            | Some variable when variable.isMutable -> scope.SetVar(name, { value = value; isMutable = true })
            | Some _ -> failwithf "Variable '%s' is immutable and cannot be assigned to" name
            | None -> failwithf "Variable '%s' is not defined in this scope" name
        | Hir.Stmt.ExprStmt (expr, _) ->
            ignore (evalExpr scope expr)
        | Hir.Stmt.ErrorStmt (msg, span) -> failwith msg

    let evalDecl (scope: Scope) (decl: Hir.Decl) : unit =
        let tryGetDotnetType (path: string list) : System.Type option =
            System.AppDomain.CurrentDomain.GetAssemblies()
            |> Array.choose (fun asm -> asm.GetType(String.concat "." path) |> Option.ofObj)
            |> Array.tryHead

        match decl with
        | Hir.Decl.Import (path, _) ->
            match tryGetDotnetType path with
            | Some dotnetType ->
                scope.SetType(List.last path, Type.Native dotnetType)
            | None -> failwithf "Failed to import type: %s" (String.concat "." path)
        | _ -> failwith "Unsupported declaration type"

    let evalModule (scope: Scope) (moduleDecl: Hir.Module) : unit =
        for decl in moduleDecl.decls do
            evalDecl scope decl

        for stmt in moduleDecl.stmts do
            evalStmt scope stmt