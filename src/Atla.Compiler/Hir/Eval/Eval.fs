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
            | None -> match scope.GetType(name) with
                      | Some typ -> Value.TypeRef typ
                      | _ -> failwithf "Variable '%s' is not defined in this scope" name
        | Hir.Expr.Apply (funcExpr, args, _) ->
            let funcValue = evalExpr scope funcExpr
            let argValues = args |> List.map (evalExpr scope)
            match funcValue with
            | Value.Function func -> func argValues
            | _ -> failwith "Attempted to apply a non-function value"
        | Hir.Expr.MemberAccess (receiver, memberName, _) ->
            match evalExpr scope receiver with
            | Value.TypeRef (Type.Native dotnetType) ->
                let methodsWithName = dotnetType.GetMethods() |> Array.filter (fun m -> m.Name = memberName)
                if methodsWithName.Length = 0 then failwithf "Member '%s' not found on type '%s'" memberName dotnetType.FullName
                Value.Function (fun args ->
                    let argValues = args |> List.map (function
                        | Value.Int v -> box v
                        | Value.Float v -> box v
                        | Value.String s -> box s
                        | Value.Unit -> box ()
                        | Value.TypeRef t -> box t
                        | Value.Native o -> o
                        | _ -> failwith "Unsupported argument type for .NET method call")
                    let argArr = argValues |> List.toArray
                    // 絞り込み: 引数の数が一致するオーバーロード
                    let candidates = methodsWithName |> Array.filter (fun m -> m.GetParameters().Length = argArr.Length)
                    let matchesByType (m: System.Reflection.MethodInfo) : bool =
                        let ps = m.GetParameters()
                        ps |> Array.mapi (fun i p ->
                            let a = argArr.[i]
                            if a = null then true
                            else p.ParameterType.IsInstanceOfType(a)) |> Array.forall id
                    match candidates |> Array.tryFind matchesByType with
                    | Some (:? System.Reflection.MethodInfo as methodInfo) ->
                        if not methodInfo.IsStatic then failwithf "Only static methods supported for type access: %s" memberName
                        let result = methodInfo.Invoke(null, argArr)
                        match result with
                        | null -> Value.Unit
                        | :? int as intResult -> Value.Int intResult
                        | :? float as floatResult -> Value.Float floatResult
                        | :? string as stringResult -> Value.String stringResult
                        | _ -> Value.Native result
                    | None -> failwithf "No overload of '%s' on '%s' matches %d arguments" memberName dotnetType.FullName argArr.Length)
            | _ -> failwith "Member access is only supported on .NET types in this implementation"
        | Hir.Expr.Block (stmts, expr, _) ->
            let blockScope = Scope(Some scope)
            stmts |> List.iter (evalStmt blockScope)
            evalExpr blockScope expr
        | _ -> failwith "Unsupported expression type"

    and evalStmt (scope: Scope) (stmt: Hir.Stmt) : unit =
        match stmt with
        | Hir.Stmt.Let (name, mut, valueExpr, _) ->
            let value = evalExpr scope valueExpr
            if scope.HasVar(name) then
                failwithf "Variable '%s' is already defined in this scope" name
            scope.SetVar(name, Variable(value, mut))
        | Hir.Stmt.Assign (name, valueExpr, _) ->
            let value = evalExpr scope valueExpr
            match scope.GetVar(name) with
            | Some variable when variable.isMutable -> scope.SetVar(name, Variable(value, true))
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
        | Hir.Decl.Fn (name, args, body, _) ->
            let fnValue = Value.Function (fun argValues ->
                if List.length argValues <> List.length args then
                    failwithf "Function '%s' expected %d arguments but got %d" name (List.length args) (List.length argValues)
                let fnScope = Scope(Some scope)
                List.zip args argValues
                |> List.iter (fun (arg, value) -> 
                    match arg with
                    | Hir.FnArg.Unit _ -> () // Unit 引数は値をバインドしない
                    | Hir.FnArg.Named (argName, _, _) -> fnScope.SetVar(argName, Variable(value, false)))
                evalExpr fnScope body
            )
            scope.SetVar(name, Variable(fnValue, false))
        | _ -> failwith "Unsupported declaration type"

    let evalModule (scope: Scope) (moduleDecl: Hir.Module) : unit =
        for decl in moduleDecl.decls do
            evalDecl scope decl