namespace Atla.Compiler.Lowering

open System
open Atla.Compiler.Hir

module Typing =
    let evalTypeExpr (scope: Scope) (typeExpr: Hir.TypeExpr) : TypeCray =
        match typeExpr with
        | Hir.TypeExpr.Unit (_) -> TypeCray.System typeof<Void>
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
            (unitExpr :> Hir.Expr).typ <- TypeCray.System(typeof<Void>).Unify(expect)
        | :? Hir.Expr.Int as intExpr ->
            (intExpr :> Hir.Expr).typ <- TypeCray.System(typeof<int>).Unify(expect)
        | :? Hir.Expr.Float as floatExpr ->
            (floatExpr :> Hir.Expr).typ <- TypeCray.System(typeof<float>).Unify(expect)
        | :? Hir.Expr.String as stringExpr ->
            (stringExpr :> Hir.Expr).typ <- TypeCray.System(typeof<string>).Unify(expect)
        | :? Hir.Expr.Id as idExpr ->
            match scope.ResolveVar(idExpr.name) with
            | Some sym ->
                (idExpr :> Hir.Expr).typ <- sym.typ.Unify(expect)
            | None ->
                match scope.ResolveType(idExpr.name) with
                | Some typeItem ->
                    (idExpr :> Hir.Expr).typ <- typeItem.Unify(expect)
                | None ->
                    (idExpr :> Hir.Expr).typ <- TypeCray.Error (sprintf "Undefined variable or type '%s' at %A" idExpr.name idExpr.span)
        | :? Hir.Expr.MemberAccess as memberAccess ->
            typingExpr scope memberAccess.receiver TypeCray.Unknown
            match memberAccess.receiver.typ.Compress() with
            | TypeCray.System sysType ->
                let mutable members = sysType.GetMember(memberAccess.memberName) |> Array.map (fun mi ->
                    match mi with
                    | mi when mi.MemberType = System.Reflection.MemberTypes.Method ->
                        let methodInfo = mi :?> System.Reflection.MethodInfo
                        TypeCray.Function(methodInfo.GetParameters() |> Array.map (fun p -> TypeCray.System p.ParameterType) |> List.ofArray, TypeCray.System methodInfo.ReturnType).Unify(expect)
                    | mi when mi.MemberType = System.Reflection.MemberTypes.Field ->
                        let fieldInfo = mi :?> System.Reflection.FieldInfo
                        (TypeCray.System fieldInfo.FieldType).Unify(expect)
                    | _ -> failwithf "Unsupported member type '%A' for member '%s' at %A" mi.MemberType memberAccess.memberName memberAccess.span
                )
                members <- members |> Array.filter (fun t -> not (t.HasError()))
                if members.Length <> 1 then
                    (memberAccess :> Hir.Expr).typ <- TypeCray.Error (sprintf "Member '%s' not found or ambiguous in type '%s' at %A" memberAccess.memberName sysType.FullName memberAccess.span)
                else
                    let typ = members.[0]
                    (memberAccess :> Hir.Expr).typ <- typ
                
            | _ -> failwithf "Member access on non-system type at %A" memberAccess.span

        | :? Hir.Expr.Apply as applyExpr ->
            let argTypes: TypeCray list = applyExpr.args |> List.map (fun arg -> typingExpr scope arg TypeCray.Unknown
                                                                                 arg.typ)
            typingExpr scope applyExpr.func (TypeCray.Function(argTypes, expect))
            match applyExpr.func.typ.Compress() with
            | TypeCray.Function (params, ret) ->
                (applyExpr :> Hir.Expr).typ <- ret.Unify(expect)
            | _ -> (applyExpr :> Hir.Expr).typ <- TypeCray.Error (sprintf "Attempting to call a non-function type at %A" applyExpr.span)
        | :? Hir.Expr.Fn as fnExpr ->
            let (argTypes, retType) =
                match expect with
                | TypeCray.Function(argTypes, retType) -> (argTypes, retType)
                | TypeCray.Unknown -> (List.replicate fnExpr.args.Length TypeCray.Unknown, TypeCray.Unknown)
                | _ -> (List.replicate fnExpr.args.Length (TypeCray.Error "Type mismatch in function argument"), TypeCray.Error "Type mismatch in function return type")
            for (arg, expectedArgType) in List.zip fnExpr.args argTypes do
                match arg with
                | :? Hir.FnArg.Unit -> ()
                | :? Hir.FnArg.Named as namedArg ->
                    let typ = evalTypeExpr scope namedArg.typeExpr
                    fnExpr.scope.DeclareVar(namedArg.name, Symbol.Arg(expectedArgType.Unify(typ)))
                | _ -> failwith "Unsupported function argument type"
            typingExpr fnExpr.scope fnExpr.body retType
        | :? Hir.Expr.Block as blockExpr ->
            // TODO: infer return statement types and unify with expect
            let mutable lastType = TypeCray.System(typeof<Void>)
            for stmt in blockExpr.stmts do
                typingStmt blockExpr.scope stmt
            match List.last blockExpr.stmts with
            | :? Hir.Stmt.ExprStmt as lastExprStmt ->
                lastType <- lastExprStmt.expr.typ
            | _ -> lastType <- TypeCray.System(typeof<Void>)
            (blockExpr :> Hir.Expr).typ <- lastType.Unify(expect)
        | :? Hir.Expr.If as ifExpr ->
            typingExpr scope ifExpr.cond (TypeCray.System(typeof<bool>))
            typingExpr scope ifExpr.thenBranch expect
            typingExpr scope ifExpr.elseBranch expect
            (ifExpr :> Hir.Expr).typ <- ifExpr.thenBranch.typ.Unify(ifExpr.elseBranch.typ)

    and typingStmt (scope: Scope) (stmt: Hir.Stmt) =
        match stmt with
        | :? Hir.Stmt.Let as letStmt ->
            typingExpr scope letStmt.value TypeCray.Unknown
            scope.DeclareVar(letStmt.name, Symbol.Local(letStmt.value.typ))
        | :? Hir.Stmt.Assign as assignStmt ->
            match scope.ResolveVar(assignStmt.name) with
            | Some sym ->
                typingExpr scope assignStmt.value sym.typ
            | None -> failwithf "Undefined variable '%s' at %A" assignStmt.name assignStmt.span
        | :? Hir.Stmt.ExprStmt as exprStmt ->
            typingExpr scope exprStmt.expr TypeCray.Unknown
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
                        scope.DeclareVar(namedArg.name, Symbol.Arg(namedArg.typ))
                        Some(namedArg.typ)
                    | _ -> None) |> List.filter Option.isSome |> List.map Option.get
                scope.DeclareVar(name, Symbol.Method(TypeCray.Function(argTypes, retType)))
                typingExpr scope body retType
            | Hir.Decl.TypeDef (name, typeExpr, span) ->
                let typ = evalTypeExpr moduleDecl.scope typeExpr
                moduleDecl.scope.DeclareType(name, typ)
            | Hir.Decl.DeclError _ -> ()
