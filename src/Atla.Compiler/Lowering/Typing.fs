namespace Atla.Compiler.Lowering

open System
open Atla.Compiler.Types
open Atla.Compiler.Hir

module Typing =
    let evalTypeExpr (scope: Scope) (typeExpr: Hir.TypeExpr) : TypeCray =
        match typeExpr with
        | Hir.TypeExpr.Id (name, span) ->
            match scope.ResolveType(name) with
            | Some t -> t
            | None -> TypeCray.Error (sprintf "Undefined type '%s' at %A" name span)
        | Hir.TypeExpr.Import (path, span) ->
            let fullName = String.Join(".", path)
            let maybeType =
                AppDomain.CurrentDomain.GetAssemblies()
                |> Array.choose (fun asm ->
                    match asm.GetType(fullName) with
                    | null -> None
                    | t -> Some t)
                |> Array.tryHead
            match maybeType with
            | Some t -> TypeCray.System t
            | _ -> TypeCray.Error (sprintf "Undefined type '%s' at %A" fullName span)


    let rec typingExpr (scope: Scope) (expr: Hir.Expr) (expect: TypeCray) =
        match expr with
        | :? Hir.Expr.Unit as unitExpr ->
            if expect <> TypeCray.Unit then
               (unitExpr :> Hir.Expr).typ <- TypeCray.Error (sprintf "Expected type %A but got Unit at %A" expect unitExpr.span)
        | :? Hir.Expr.Int as intExpr ->
            if expect <> TypeCray.Int then
                (intExpr :> Hir.Expr).typ <- TypeCray.Error (sprintf "Expected type %A but got Int at %A" expect intExpr.span)
        | :? Hir.Expr.Float as floatExpr ->
            if expect <> TypeCray.Float then
                (floatExpr :> Hir.Expr).typ <- TypeCray.Error (sprintf "Expected type %A but got Float at %A" expect floatExpr.span)
        | :? Hir.Expr.String as stringExpr ->
            if expect <> TypeCray.String then
                (stringExpr :> Hir.Expr).typ <- TypeCray.Error (sprintf "Expected type %A but got String at %A" expect stringExpr.span)
        | :? Hir.Expr.Id as idExpr ->
            match scope.ResolveVar(idExpr.name) with
            | Some varType ->
                (idExpr :> Hir.Expr).typ <- varType.Unify(expect)
            | None ->
                (idExpr :> Hir.Expr).typ <- TypeCray.Error (sprintf "Undefined variable '%s' at %A" idExpr.name idExpr.span)
        | :? Hir.Expr.Apply as applyExpr ->
            let argTypes: TypeCray list = applyExpr.args |> List.map (fun arg -> typingExpr scope arg TypeCray.Unknown
                                                                                 arg.typ)
            typingExpr scope applyExpr.func (TypeCray.Function(argTypes, expect))
        | :? Hir.Expr.Fn as fnExpr ->
            let (argTypes, retType) =
                match expect with
                | TypeCray.Function(argTypes, retType) -> (argTypes, retType)
                | TypeCray.Unknown -> (List.replicate fnExpr.args.Length TypeCray.Unknown, TypeCray.Unknown)
                | _ -> (List.replicate fnExpr.args.Length (TypeCray.Error "Type mismatch in function argument"), TypeCray.Error "Type mismatch in function return type")
            let bodyScope = Scope(Some scope)
            for (arg, expectedArgType) in List.zip fnExpr.args argTypes do
                match arg with
                | Hir.FnArg.Unit _ -> ()
                | Hir.FnArg.Named (argName, typeExpr, _) ->
                    let typ = evalTypeExpr scope typeExpr
                    bodyScope.DeclareVar(argName, expectedArgType.Unify(typ))
            typingExpr bodyScope fnExpr.body retType
        | :? Hir.Expr.Block as blockExpr ->
            // TODO: infer return statement types and unify with expect
            let blockScope = Scope(Some scope)
            let mutable lastType = TypeCray.Unit
            for stmt in blockExpr.stmts do
                typingStmt blockScope stmt
            if blockExpr.stmts.Length > 0 then
                match List.last blockExpr.stmts with
                | Hir.Stmt.ExprStmt (lastExpr, _) -> lastType <- lastExpr.typ
                | _ -> lastType <- TypeCray.Unit
            (blockExpr :> Hir.Expr).typ <- lastType.Unify(expect)

    and typingStmt (scope: Scope) (stmt: Hir.Stmt) =
        match stmt with
        | Hir.Stmt.Let (name, _, value, span) ->
            typingExpr scope value TypeCray.Unknown
            scope.DeclareVar(name, value.typ)
        | Hir.Stmt.Assign (name, value, span) ->
            match scope.ResolveVar(name) with
            | Some typ ->
                typingExpr scope value typ
            | None -> failwithf "Undefined variable '%s' at %A" name span
        | Hir.Stmt.ExprStmt (expr, span) ->
            typingExpr scope expr TypeCray.Unknown
        | Hir.Stmt.ErrorStmt (message, span) ->
            () // エラーステートメントは型推論の対象外

    let typingModule (scope: Scope) (moduleDecl: Hir.Module) =
        // iterate declarations in the module
        for decl in moduleDecl.decls do
            match decl with
            | Hir.Decl.Def (name, expr, span) ->
                typingExpr scope expr TypeCray.Unknown
                scope.DeclareVar(name, expr.typ)
            | Hir.Decl.TypeDef (name, typeExpr, span) ->
                let typ = evalTypeExpr scope typeExpr
                scope.DeclareType(name, typ)
            | Hir.Decl.DeclError _ -> ()
