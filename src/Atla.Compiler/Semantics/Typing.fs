namespace Atla.Compiler.Semantics

open System
open Atla.Compiler.Semantics.Data

module Typing =
    let evalTypeExpr (scope: Scope) (typeExpr: Hir.TypeExpr) : TypeId =
        match typeExpr with
        | Hir.TypeExpr.Unit (_) -> TypeId.System typeof<Void>
        | Hir.TypeExpr.Id (name, span) ->
            match scope.ResolveType(name) with
            | Some t -> t
            | None -> TypeId.Error (sprintf "Undefined type '%s' at %A" name span)
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
            | Some t -> TypeId.System t
            | _ -> TypeId.Error (sprintf "Undefined type '%s' at %A" fullName span)


    let rec typingExpr (scope: Scope) (expr: Hir.Expr) (expect: TypeId) =
        match expr with
        | :? Hir.Expr.Unit as unitExpr ->
            (unitExpr :> Hir.Expr).typ <- TypeId.System(typeof<Void>).Unify(expect)
        | :? Hir.Expr.Int as intExpr ->
            (intExpr :> Hir.Expr).typ <- TypeId.System(typeof<int>).Unify(expect)
        | :? Hir.Expr.Float as floatExpr ->
            (floatExpr :> Hir.Expr).typ <- TypeId.System(typeof<float>).Unify(expect)
        | :? Hir.Expr.String as stringExpr ->
            (stringExpr :> Hir.Expr).typ <- TypeId.System(typeof<string>).Unify(expect)
        | :? Hir.Expr.Id as idExpr ->
            match scope.ResolveVar(idExpr.name, (idExpr :> Hir.Expr).typ) with
            | [sym] ->
                idExpr.symbol.info <- sym
                (idExpr :> Hir.Expr).typ <- sym.typ.Unify(expect)
            | [] ->
                match scope.ResolveType(idExpr.name) with
                | Some typeItem ->
                    (idExpr :> Hir.Expr).typ <- typeItem.Unify(expect)
                | None ->
                    (idExpr :> Hir.Expr).typ <- TypeId.Error (sprintf "Undefined variable or type '%s' at %A" idExpr.name idExpr.span)
            | _ -> failwithf "Ambiguous variable '%s' at %A" idExpr.name idExpr.span
        | :? Hir.Expr.MemberAccess as memberAccess ->
            typingExpr scope memberAccess.receiver TypeId.Unknown
            match memberAccess.receiver.typ.Compress() with
            | TypeId.System sysType ->
                let mutable members = sysType.GetMember(memberAccess.memberName) |> Array.map (fun mi ->
                    match mi with
                    | mi when mi.MemberType = System.Reflection.MemberTypes.Method ->
                        let methodInfo = mi :?> System.Reflection.MethodInfo
                        TypeId.Fn(methodInfo.GetParameters() |> Array.map (fun p -> TypeId.System p.ParameterType) |> List.ofArray, TypeId.System methodInfo.ReturnType).Unify(expect)
                    | mi when mi.MemberType = System.Reflection.MemberTypes.Field ->
                        let fieldInfo = mi :?> System.Reflection.FieldInfo
                        (TypeId.System fieldInfo.FieldType).Unify(expect)
                    | _ -> failwithf "Unsupported member type '%A' for member '%s' at %A" mi.MemberType memberAccess.memberName memberAccess.span
                )
                members <- members |> Array.filter (fun t -> not (t.HasError()))
                if members.Length <> 1 then
                    (memberAccess :> Hir.Expr).typ <- TypeId.Error (sprintf "Member '%s' not found or ambiguous in type '%s' at %A" memberAccess.memberName sysType.FullName memberAccess.span)
                else
                    let typ = members.[0]
                    (memberAccess :> Hir.Expr).typ <- typ
                
            | _ -> failwithf "Member access on non-system type at %A" memberAccess.span

        | :? Hir.Expr.Apply as applyExpr ->
            let argTypes: TypeId list = applyExpr.args |> List.map (fun arg -> typingExpr scope arg TypeId.Unknown
                                                                                 arg.typ)
            typingExpr scope applyExpr.func (TypeId.Fn(argTypes, expect))
            match applyExpr.func.typ.Compress() with
            | TypeId.Fn (params, ret) ->
                (applyExpr :> Hir.Expr).typ <- ret.Unify(expect)
            | _ -> (applyExpr :> Hir.Expr).typ <- TypeId.Error (sprintf "Attempting to call a non-function type at %A" applyExpr.span)
        | :? Hir.Expr.Lambda as fnExpr ->
            let (argTypes, retType) =
                match expect with
                | TypeId.Fn(argTypes, retType) -> (argTypes, retType)
                | TypeId.Unknown -> (List.replicate fnExpr.args.Length TypeId.Unknown, TypeId.Unknown)
                | _ -> (List.replicate fnExpr.args.Length (TypeId.Error "Type mismatch in function argument"), TypeId.Error "Type mismatch in function return type")
            for (arg, expectedArgType) in List.zip fnExpr.args argTypes do
                match arg with
                | :? Hir.FnArg.Unit -> ()
                | :? Hir.FnArg.Named as namedArg ->
                    let typ = evalTypeExpr scope namedArg.typeExpr
                    fnExpr.scope.DeclareVar(Symbol.Arg(namedArg.name, expectedArgType.Unify(typ)))
                | _ -> failwith "Unsupported function argument type"
            typingExpr fnExpr.scope fnExpr.body retType
        | :? Hir.Expr.Block as blockExpr ->
            // TODO: infer return statement types and unify with expect
            let mutable lastType = TypeId.System(typeof<Void>)
            for stmt in blockExpr.stmts do
                typingStmt blockExpr.scope stmt
            match List.last blockExpr.stmts with
            | :? Hir.Stmt.ExprStmt as lastExprStmt ->
                lastType <- lastExprStmt.expr.typ
            | _ -> lastType <- TypeId.System(typeof<Void>)
            (blockExpr :> Hir.Expr).typ <- lastType.Unify(expect)
        | :? Hir.Expr.If as ifExpr ->
            typingExpr scope ifExpr.cond (TypeId.System(typeof<bool>))
            typingExpr scope ifExpr.thenBranch expect
            typingExpr scope ifExpr.elseBranch expect
            (ifExpr :> Hir.Expr).typ <- ifExpr.thenBranch.typ.Unify(ifExpr.elseBranch.typ)

    and typingStmt (scope: Scope) (stmt: Hir.Stmt) =
        match stmt with
        | :? Hir.Stmt.Let as letStmt ->
            typingExpr scope letStmt.value TypeId.Unknown
            scope.DeclareVar(Symbol.Local(letStmt.name, letStmt.value.typ))
        | :? Hir.Stmt.Assign as assignStmt ->
            match scope.ResolveVar(assignStmt.name, assignStmt.value.typ) with
            | [sym] -> typingExpr scope assignStmt.value sym.typ
            | [] -> failwithf "Undefined variable '%s' at %A" assignStmt.name assignStmt.span
            | _ -> failwithf "Ambiguous variable '%s' at %A" assignStmt.name assignStmt.span
        | :? Hir.Stmt.ExprStmt as exprStmt ->
            typingExpr scope exprStmt.expr TypeId.Unknown
        | :? Hir.Stmt.ErrorStmt ->
            () // エラーステートメントは型推論の対象外

    let typingModule (moduleDecl: Hir.Module) =
        // iterate declarations in the module
        for decl in moduleDecl.decls do
            match decl with
            | Hir.Decl.Fn (name, args, ret, body, scope, span) ->
                let retType = evalTypeExpr scope ret
                let argTypes = args |> List.map (fun arg ->
                    match arg with
                    | :? Hir.FnArg.Named as namedArg ->
                        namedArg.typ <- evalTypeExpr scope namedArg.typeExpr
                        scope.DeclareVar(Symbol.Arg(namedArg.name, namedArg.typ))
                        Some(namedArg.typ)
                    | _ -> None) |> List.filter Option.isSome |> List.map Option.get
                scope.DeclareVar(Symbol.Method(name, TypeCray.Function(argTypes, retType), None))
                typingExpr scope body retType
            | Hir.Decl.TypeDef (name, typeExpr, span) ->
                let typ = evalTypeExpr moduleDecl.scope typeExpr
                moduleDecl.scope.DeclareType(name, typ)
            | Hir.Decl.DeclError _ -> ()
