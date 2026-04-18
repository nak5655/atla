namespace Atla.Core.Semantics

open System.Reflection
open System.Collections.Generic
open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data

module Analyze =
    type NameEnv(symbolTable: SymbolTable, scope: Scope) =
        member this.symbolTable = symbolTable
        member this.scope = scope

        // TypeExprをTypeIdへ解決する。
        member this.resolveTypeExpr (typeExpr: Ast.TypeExpr) : TypeId =
            let resolveNamedType (name: string) (span: Atla.Core.Data.Span) : TypeId =
                match scope.ResolveType(name) with
                | Some typ -> typ
                | _ -> TypeId.Error (sprintf "Undefined type '%s' at %A" name span)

            match typeExpr with
            | :? Ast.TypeExpr.Unit -> TypeId.Unit
            | :? Ast.TypeExpr.Id as idTypeExpr ->
                resolveNamedType idTypeExpr.name idTypeExpr.span
            | :? Ast.TypeExpr.Apply as applyTypeExpr ->
                let resolveArgs () =
                    let resolvedArgs = applyTypeExpr.args |> List.map this.resolveTypeExpr
                    let firstArgError =
                        resolvedArgs
                        |> List.tryPick (fun argType ->
                            match argType with
                            | TypeId.Error message -> Some message
                            | _ -> None)
                    resolvedArgs, firstArgError

                match applyTypeExpr.head with
                | :? Ast.TypeExpr.Id as headId when headId.name = "Array" ->
                    let resolvedArgs, firstArgError = resolveArgs ()
                    match firstArgError, resolvedArgs with
                    | Some message, _ -> TypeId.Error message
                    | None, [elemType] -> TypeId.Array elemType
                    | None, _ -> TypeId.Error(sprintf "Array type expects exactly one type argument at %A" applyTypeExpr.span)
                | _ ->
                    let resolvedHead = this.resolveTypeExpr applyTypeExpr.head
                    let resolvedArgs, firstArgError = resolveArgs ()
                    match resolvedHead, firstArgError with
                    | TypeId.Error message, _ -> TypeId.Error message
                    | _, Some message -> TypeId.Error message
                    | _, None -> TypeId.App(resolvedHead, resolvedArgs)
            | _ -> TypeId.Error (sprintf "Unsupported type expression type at %A" typeExpr.span)

        member this.resolveArgType (arg: Ast.FnArg) : TypeId =
            match arg with
            | :? Ast.FnArg.Named as namedArg -> this.resolveTypeExpr namedArg.typeExpr
            | :? Ast.FnArg.Unit -> TypeId.Unit
            | _ -> TypeId.Error (sprintf "Unsupported function argument type at %A" arg.span)

        member this.declareLocal (name: string) (tid: TypeId) : SymbolId =
            let sid = symbolTable.NextId()
            let symInfo = { name = name; typ = tid; kind = SymbolKind.Local() }
            symbolTable.Add(sid, symInfo)
            scope.DeclareVar(name, sid)
            sid

        member this.declareArg (name: string) (tid: TypeId) : SymbolId =
            let sid = symbolTable.NextId()
            let symInfo = { name = name; typ = tid; kind = SymbolKind.Arg() }
            symbolTable.Add(sid, symInfo)
            scope.DeclareVar(name, sid)
            sid

        member this.resolveVar (name: string) (tid: TypeId) : SymbolId list =
            scope.ResolveVar(name, tid)

        member this.resolveSymType (sid: SymbolId) : TypeId =
            match symbolTable.Get(sid) with
            | Some(symInfo) -> symInfo.typ
            | _ -> TypeId.Error (sprintf "Undefined symbol '%A'" sid)

        member this.resolveSym(sid: SymbolId) : SymbolInfo option =
            symbolTable.Get(sid)

        member this.sub(): NameEnv =
            let blockScope = Scope(Some this.scope)
            NameEnv(this.symbolTable, blockScope)

    type TypeEnv(typSubst: TypeSubst, metaFactory: TypeMetaFactory) =
        member this.typSubst = typSubst

        member this.unifyTypes (tid1: TypeId) (tid2: TypeId) : Result<unit, UnifyError> =
            Type.unify typSubst tid1 tid2 |> Result.map ignore

        member this.resolveType (tid: TypeId) : TypeId =
            Type.resolve typSubst tid

        member this.canUnify (tid1: TypeId) (tid2: TypeId) : bool =
            Type.canUnify typSubst tid1 tid2

        member this.freshMeta() : TypeId =
            TypeId.Meta(metaFactory.Fresh())

    let private resolveTyp (nameEnv: NameEnv) (typeEnv: TypeEnv) (tid: TypeId) : SymbolInfo option =
        match typeEnv.resolveType tid with
        | TypeId.Name sid -> nameEnv.resolveSym sid
        | _ -> None

    let private resolveNativeMember (typeEnv: TypeEnv) (memberInfos: MemberInfo list) (tid: TypeId) : (MemberInfo * TypeId) list =
        let exactResult = List<MemberInfo * TypeId>()
        let optionalResult = List<MemberInfo * TypeId>()
        for memberInfo in memberInfos do
            match memberInfo with
            | :? MethodInfo as methodInfo ->
                let parameterTypes = [ for p in methodInfo.GetParameters() -> TypeId.fromSystemType p.ParameterType ]
                let returnType = TypeId.fromSystemType methodInfo.ReturnType
                let methodType = TypeId.Fn(parameterTypes, returnType)
                match tid with
                | TypeId.Fn(expectedArgs, expectedRet) ->
                    let parameters = methodInfo.GetParameters() |> Array.toList
                    let requiredParamCount = parameters |> List.filter (fun p -> not p.IsOptional) |> List.length
                    if
                        expectedArgs.Length >= requiredParamCount
                        && expectedArgs.Length <= parameters.Length
                        && List.forall2 (fun expectedArg (parameterInfo: ParameterInfo) -> typeEnv.canUnify expectedArg (TypeId.fromSystemType parameterInfo.ParameterType)) expectedArgs (parameters |> List.take expectedArgs.Length)
                        && typeEnv.canUnify expectedRet returnType then
                        if expectedArgs.Length = parameters.Length then
                            exactResult.Add(memberInfo, TypeId.Fn(expectedArgs, returnType))
                        else
                            optionalResult.Add(memberInfo, TypeId.Fn(expectedArgs, returnType))
                | _ ->
                    if typeEnv.canUnify methodType tid then
                        exactResult.Add(memberInfo, methodType)
            | :? FieldInfo as fieldInfo ->
                let fieldType = TypeId.fromSystemType fieldInfo.FieldType
                if typeEnv.canUnify fieldType tid then
                    exactResult.Add(memberInfo, fieldType)
            | :? PropertyInfo as propertyInfo ->
                let propertyType = TypeId.fromSystemType propertyInfo.PropertyType
                if typeEnv.canUnify propertyType tid then
                    exactResult.Add(memberInfo, propertyType)
            | _ -> ()
        if exactResult.Count > 0 then
            Seq.toList exactResult
        else
            Seq.toList optionalResult

    type private EnumeratorMembers =
        { iteratorType: System.Type
          moveNext: MethodInfo
          current: PropertyInfo }

    let private tryResolveEnumeratorMembers (iterableSystemType: System.Type) : EnumeratorMembers option =
        let allTypes = iterableSystemType :: (iterableSystemType.GetInterfaces() |> Array.toList)
        let moveNextMethod =
            allTypes
            |> List.tryPick (fun t ->
                t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
                |> Array.tryFind (fun methodInfo -> methodInfo.Name = "MoveNext" && methodInfo.GetParameters().Length = 0))
        let currentProperty =
            allTypes
            |> List.collect (fun t ->
                t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
                |> Array.filter (fun propertyInfo -> propertyInfo.Name = "Current")
                |> Array.toList)
            |> List.tryFind (fun propertyInfo -> propertyInfo.PropertyType <> typeof<obj>)
            |> Option.orElseWith (fun () ->
                allTypes
                |> List.tryPick (fun t ->
                    t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
                    |> Array.tryFind (fun propertyInfo -> propertyInfo.Name = "Current")))

        match moveNextMethod, currentProperty with
        | Some moveNext, Some current ->
            Some { iteratorType = iterableSystemType; moveNext = moveNext; current = current }
        | _ -> None

    let private tryGetEnumerator (iterableSystemType: System.Type) : MethodInfo option =
        (iterableSystemType :: (iterableSystemType.GetInterfaces() |> Array.toList))
        |> List.tryPick (fun t ->
            t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
            |> Array.tryFind (fun methodInfo -> methodInfo.Name = "GetEnumerator" && methodInfo.GetParameters().Length = 0))

    let private errorExpr (tid: TypeId) (span: Atla.Core.Data.Span) (message: string) : Hir.Expr =
        Hir.Expr.ExprError(message, tid, span)

    let private resultToExpr (tid: TypeId) (span: Atla.Core.Data.Span) (resolved: Result<Hir.Expr, string>) : Hir.Expr =
        match resolved with
        | Result.Ok expr -> expr
        | Result.Error message -> errorExpr tid span message

    let private unresolvedImportedSystemTypeMessage (typeName: string) (span: Atla.Core.Data.Span) : string =
        sprintf
            "Imported system type '%s' was not found at %A. If this type is provided by a dependency, check dependency loading diagnostics."
            typeName
            span

    let private isNativeVoid (typeEnv: TypeEnv) (tid: TypeId) : bool =
        match typeEnv.resolveType tid with
        | TypeId.Native runtimeType when runtimeType = typeof<System.Void> -> true
        | _ -> false

    let private isUnitContext (typeEnv: TypeEnv) (tid: TypeId) : bool =
        match typeEnv.resolveType tid with
        | TypeId.Unit -> true
        | _ -> false

    let private unifyOrError (typeEnv: TypeEnv) (expected: TypeId) (actual: TypeId) (span: Atla.Core.Data.Span) : Result<unit, Hir.Expr> =
        if isNativeVoid typeEnv actual && not (isUnitContext typeEnv expected) then
            Result.Error(errorExpr expected span (UnifyError.toMessage (UnifyError.CannotUnify(expected, actual))))
        else
            match typeEnv.unifyTypes expected actual with
            | Result.Ok _ -> Result.Ok ()
            | Result.Error err -> Result.Error(errorExpr expected span (UnifyError.toMessage err))

    let private tryDefaultArgExpr (parameterInfo: ParameterInfo) (span: Atla.Core.Data.Span) : Hir.Expr option =
        if not parameterInfo.IsOptional then
            None
        elif not parameterInfo.HasDefaultValue then
            None
        else
            let parameterType = parameterInfo.ParameterType
            let defaultValue = parameterInfo.DefaultValue
            if parameterType.IsEnum then
                Some(Hir.Expr.Int(System.Convert.ToInt32(defaultValue), span))
            elif parameterType = typeof<int> then
                Some(Hir.Expr.Int(unbox<int> defaultValue, span))
            elif parameterType = typeof<bool> then
                Some(Hir.Expr.Int((if unbox<bool> defaultValue then 1 else 0), span))
            elif parameterType = typeof<float> then
                Some(Hir.Expr.Float(unbox<float> defaultValue, span))
            elif parameterType = typeof<string> then
                Some(Hir.Expr.String((if obj.ReferenceEquals(defaultValue, null) then "" else unbox<string> defaultValue), span))
            else
                None

    let private tryResolveIndexerMethod (receiverType: System.Type) : MethodInfo option =
        let tryResolveSingleDimArrayIndexer () =
            if receiverType.IsArray && receiverType.GetArrayRank() = 1 then
                receiverType.GetMethod("GetValue", BindingFlags.Public ||| BindingFlags.Instance, null, [| typeof<int> |], null)
                |> Option.ofObj
            else
                None

        let allTypes = receiverType :: (receiverType.GetInterfaces() |> Array.toList)
        let candidates =
            allTypes
            |> List.collect (fun t ->
                t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
                |> Array.filter (fun methodInfo ->
                    let ps = methodInfo.GetParameters()
                    ps.Length = 1 && ps.[0].ParameterType = typeof<int>)
                |> Array.toList)
        let pickByName name =
            candidates |> List.tryFind (fun methodInfo -> methodInfo.Name = name)

        tryResolveSingleDimArrayIndexer ()
        |> Option.orElseWith (fun () -> pickByName "get_Item")
        |> Option.orElseWith (fun () -> pickByName "get_Chars")

    let rec private exprAsCallable (nameEnv: NameEnv) (typeEnv: TypeEnv) (expr: Hir.Expr): Hir.Callable option =
        match expr with
        | Hir.Expr.Id (sid, _, _) ->
            match nameEnv.resolveSym sid with
            | Some symInfo ->
                match symInfo.kind with
                | SymbolKind.External(ExternalBinding.NativeMethodGroup methodInfos) -> Some (Hir.Callable.NativeMethodGroup methodInfos)
                | SymbolKind.External(ExternalBinding.ConstructorGroup ctorInfos) -> Some (Hir.Callable.NativeConstructorGroup ctorInfos)
                | SymbolKind.BuiltinOperator op -> Some (Hir.Callable.BuiltinOperator op)
                | SymbolKind.Local ()
                | SymbolKind.Arg () ->
                    match typeEnv.resolveType symInfo.typ with
                    | TypeId.Fn _ -> Some (Hir.Callable.Fn sid)
                    | _ -> None
                | _ -> None
            | None -> None
        | Hir.Expr.MemberAccess (memberInfo, _, _, _) ->
            match memberInfo with
            | Hir.Member.NativeMethod methodInfo -> Some (Hir.Callable.NativeMethod methodInfo)
            | _ -> None
        | _ -> None

    and private analyzeExpr (nameEnv: NameEnv) (typeEnv: TypeEnv) (expr: Ast.Expr) (tid: TypeId) : Hir.Expr =
        match expr with
        | :? Ast.Expr.Unit as unitExpr ->
            match unifyOrError typeEnv tid TypeId.Unit unitExpr.span with
            | Result.Ok _ -> Hir.Expr.Unit(unitExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Int as intExpr ->
            match unifyOrError typeEnv tid TypeId.Int intExpr.span with
            | Result.Ok _ -> Hir.Expr.Int(intExpr.value, intExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Float as floatExpr ->
            match unifyOrError typeEnv tid TypeId.Float floatExpr.span with
            | Result.Ok _ -> Hir.Expr.Float(floatExpr.value, floatExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.String as stringExpr ->
            match unifyOrError typeEnv tid TypeId.String stringExpr.span with
            | Result.Ok _ -> Hir.Expr.String(stringExpr.value, stringExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Id as idExpr ->
            match nameEnv.resolveVar idExpr.name tid with
            | [sid] ->
                match unifyOrError typeEnv tid (nameEnv.resolveSymType sid) idExpr.span with
                | Result.Ok _ -> Hir.Expr.Id(sid, nameEnv.resolveSymType sid, idExpr.span)
                | Result.Error exprErr -> exprErr
            | [] -> Hir.Expr.ExprError(sprintf "Undefined variable '%s' at %A" idExpr.name idExpr.span, tid, idExpr.span)
            | _ -> Hir.Expr.ExprError(sprintf "Ambiguous variable '%s' at %A" idExpr.name idExpr.span, tid, idExpr.span)
        | :? Ast.Expr.Block as blockExpr ->
            let blockNameEnv = nameEnv.sub()
            let stmts = blockExpr.stmts |> List.map (analyzeStmt blockNameEnv typeEnv)
            match List.last stmts with
            | Hir.Stmt.ExprStmt (expr, _) ->
                match unifyOrError typeEnv tid expr.typ blockExpr.span with
                | Result.Ok _ ->
                    let leadingStmts = stmts |> List.take (stmts.Length - 1)
                    Hir.Expr.Block(leadingStmts, expr, tid, blockExpr.span)
                | Result.Error exprErr -> exprErr
            | _ ->
                let unitExpr = Hir.Expr.Unit ({ left = blockExpr.span.right; right = blockExpr.span.right })
                Hir.Expr.Block(stmts, unitExpr, tid, blockExpr.span)
        | :? Ast.Expr.Apply as applyExpr ->
            let analyzedArgs = applyExpr.args |> List.map (fun arg -> analyzeExpr nameEnv typeEnv arg (typeEnv.freshMeta()))
            let normalizedArgs =
                match analyzedArgs with
                | [Hir.Expr.Unit _] -> []
                | _ -> analyzedArgs
            let callRetType = typeEnv.freshMeta()
            let funcType = normalizedArgs |> List.map (fun arg -> arg.typ) |> fun argTypes -> TypeId.Fn(argTypes, callRetType)
            let analyzedFunc = analyzeExpr nameEnv typeEnv applyExpr.func funcType
            let callable = exprAsCallable nameEnv typeEnv analyzedFunc
            let instanceArgs =
                match analyzedFunc with
                | Hir.Expr.MemberAccess (_, Some instance, _, _) -> [ instance ]
                | _ -> []
            match callable with
            | Some resolvedCallable ->
                let allArgs = instanceArgs @ normalizedArgs
                let suppliedParameterCount (methodInfo: MethodInfo) =
                    let instanceOffset = if methodInfo.IsStatic then 0 else 1
                    max 0 (allArgs.Length - instanceOffset)

                let canApplyWithOptionalDefaults (methodInfo: MethodInfo) =
                    let parameters = methodInfo.GetParameters()
                    let suppliedCount = suppliedParameterCount methodInfo
                    if suppliedCount > parameters.Length then
                        false
                    else
                        parameters
                        |> Array.skip suppliedCount
                        |> Array.forall (fun p -> p.IsOptional && (tryDefaultArgExpr p applyExpr.span |> Option.isSome))

                let resolvedCall =
                    match resolvedCallable with
                    | Hir.Callable.NativeMethodGroup methods ->
                        let exactArity =
                            methods
                            |> List.filter (fun methodInfo -> methodInfo.GetParameters().Length = suppliedParameterCount methodInfo)

                        let optionalArity =
                            methods
                            |> List.filter (fun methodInfo -> methodInfo.GetParameters().Length > suppliedParameterCount methodInfo)
                            |> List.filter canApplyWithOptionalDefaults

                        match exactArity, optionalArity with
                        | [methodInfo], _ -> Some (Hir.Callable.NativeMethod methodInfo, TypeId.fromSystemType methodInfo.ReturnType)
                        | [], [methodInfo] -> Some (Hir.Callable.NativeMethod methodInfo, TypeId.fromSystemType methodInfo.ReturnType)
                        | _ -> None
                    | Hir.Callable.NativeConstructorGroup ctors ->
                        match ctors |> List.filter (fun ctorInfo -> ctorInfo.GetParameters().Length = allArgs.Length) with
                        | [ctorInfo] -> Some (Hir.Callable.NativeConstructor ctorInfo, TypeId.fromSystemType ctorInfo.DeclaringType)
                        | _ -> None
                    | Hir.Callable.NativeMethod methodInfo -> Some (resolvedCallable, TypeId.fromSystemType methodInfo.ReturnType)
                    | Hir.Callable.NativeConstructor ctorInfo -> Some (resolvedCallable, TypeId.fromSystemType ctorInfo.DeclaringType)
                    | _ -> Some (resolvedCallable, typeEnv.resolveType callRetType)

                match resolvedCall with
                | Some (callableExpr, callRetType) ->
                    let callArgs =
                        match callableExpr with
                        | Hir.Callable.NativeMethod methodInfo when methodInfo.GetParameters().Length > suppliedParameterCount methodInfo ->
                            let parameters = methodInfo.GetParameters()
                            let suppliedCount = suppliedParameterCount methodInfo
                            let missingDefaults =
                                parameters
                                |> Array.skip suppliedCount
                                |> Array.toList
                                |> List.map (fun p -> tryDefaultArgExpr p applyExpr.span)
                            allArgs @ (missingDefaults |> List.choose id)
                        | _ -> allArgs
                    match unifyOrError typeEnv tid callRetType applyExpr.span with
                    | Result.Ok _ -> Hir.Expr.Call(callableExpr, None, callArgs, callRetType, applyExpr.span)
                    | Result.Error exprErr -> exprErr
                | None ->
                    Hir.Expr.ExprError(sprintf "No overload matched argument count %d at %A" allArgs.Length applyExpr.span, tid, applyExpr.span)
            | None ->
                Hir.Expr.ExprError(sprintf "Expression is not callable at %A" applyExpr.span, tid, applyExpr.span)
        | :? Ast.Expr.IndexAccess as indexAccessExpr ->
            let receiver = analyzeExpr nameEnv typeEnv indexAccessExpr.receiver (typeEnv.freshMeta ())
            let indexExpr = analyzeExpr nameEnv typeEnv indexAccessExpr.index TypeId.Int
            let resolvedReceiverType = typeEnv.resolveType receiver.typ

            let resolvedIndexResult =
                match TypeId.tryToRuntimeSystemType resolvedReceiverType with
                | Some systemType ->
                    match tryResolveIndexerMethod systemType with
                    | Some methodInfo ->
                        let returnType = TypeId.fromSystemType methodInfo.ReturnType
                        match unifyOrError typeEnv tid returnType indexAccessExpr.span with
                        | Result.Ok _ ->
                            Result.Ok(Hir.Expr.Call(Hir.Callable.NativeMethod methodInfo, None, [ receiver; indexExpr ], returnType, indexAccessExpr.span))
                        | Result.Error errExpr -> Result.Error errExpr
                    | None ->
                        Result.Error(Hir.Expr.ExprError(sprintf "Type '%A' does not support single-index access at %A" systemType indexAccessExpr.span, tid, indexAccessExpr.span))
                | None ->
                    Result.Error(Hir.Expr.ExprError(sprintf "Type '%A' is not a runtime indexable type at %A" resolvedReceiverType indexAccessExpr.span, tid, indexAccessExpr.span))

            match resolvedIndexResult with
            | Result.Ok resolvedExpr -> resolvedExpr
            | Result.Error errExpr -> errExpr
        | :? Ast.Expr.MemberAccess as memberAccessExpr ->
            let resolveMemberFromSystemTypeResult (sysType: System.Type) : Result<Hir.Expr, string> =
                let memberInfos =
                    sysType.GetMembers(BindingFlags.Public ||| BindingFlags.Static)
                    |> Seq.filter (fun m -> m.Name = memberAccessExpr.memberName)
                    |> Seq.toList

                match resolveNativeMember typeEnv memberInfos tid with
                | [memberInfo, resolvedTid] ->
                    match memberInfo with
                    | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, None, resolvedTid, memberAccessExpr.span))
                    | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, None, resolvedTid, memberAccessExpr.span))
                    | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, None, resolvedTid, memberAccessExpr.span))
                    | _ -> Result.Error (sprintf "Unsupported member type for '%s' at %A" memberAccessExpr.memberName memberAccessExpr.span)
                | [] -> Result.Error (sprintf "Undefined member '%s' for type '%A' at %A" memberAccessExpr.memberName sysType memberAccessExpr.span)
                | _ -> Result.Error (sprintf "Ambiguous member '%s' for type '%A' at %A" memberAccessExpr.memberName sysType memberAccessExpr.span)

            let resolveFromSymInfo (typeName: obj) (symInfo: SymbolInfo) : Result<Hir.Expr, string> =
                match symInfo.kind with
                | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) ->
                    resolveMemberFromSystemTypeResult sysType
                | SymbolKind.External(ExternalBinding.SystemTypeRef _) ->
                    Result.Error (unresolvedImportedSystemTypeMessage (string typeName) memberAccessExpr.span)
                | _ ->
                    Result.Error (sprintf "Type '%A' does not support member access at %A" symInfo.typ memberAccessExpr.span)

            let resolvedMemberResult =
                match memberAccessExpr.receiver with
                | :? Ast.Expr.Id as receiverId ->
                    match nameEnv.scope.ResolveType(receiverId.name) with
                    | Some (TypeId.Name sid) ->
                        match nameEnv.resolveSym sid with
                        | Some symInfo ->
                            match symInfo.kind with
                            | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) -> resolveMemberFromSystemTypeResult sysType
                            | SymbolKind.External(ExternalBinding.SystemTypeRef _) -> Result.Error (unresolvedImportedSystemTypeMessage receiverId.name memberAccessExpr.span)
                            | _ -> Result.Error (sprintf "Type '%s' is not a system type at %A" receiverId.name memberAccessExpr.span)
                        | None -> Result.Error (sprintf "Undefined type symbol '%s' at %A" receiverId.name memberAccessExpr.span)
                    | _ ->
                        let receiver = analyzeExpr nameEnv typeEnv memberAccessExpr.receiver (typeEnv.freshMeta())
                        let receiverType = typeEnv.resolveType receiver.typ
                        match TypeId.tryToRuntimeSystemType receiverType with
                        | Some systemType ->
                            let memberInfos =
                                systemType.GetMembers(BindingFlags.Public ||| BindingFlags.Instance)
                                |> Seq.filter (fun m -> m.Name = memberAccessExpr.memberName)
                                |> Seq.toList
                            match resolveNativeMember typeEnv memberInfos tid with
                            | [memberInfo, resolvedTid] ->
                                match memberInfo with
                                | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                                | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                                | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                                | _ -> Result.Error (sprintf "Unsupported member type for '%s' at %A" memberAccessExpr.memberName memberAccessExpr.span)
                            | [] ->
                                if systemType.IsValueType && memberAccessExpr.memberName = "ToString" then
                                    let convertMethod = typeof<System.Convert>.GetMethod("ToString", [| systemType |])
                                    if obj.ReferenceEquals(convertMethod, null) then
                                        Result.Error (sprintf "Undefined member '%s' for type '%A' at %A" memberAccessExpr.memberName systemType memberAccessExpr.span)
                                    else
                                        Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod convertMethod, Some receiver, TypeId.Fn([TypeId.fromSystemType systemType], TypeId.String), memberAccessExpr.span))
                                else
                                    Result.Error (sprintf "Undefined member '%s' for type '%A' at %A" memberAccessExpr.memberName systemType memberAccessExpr.span)
                            | _ -> Result.Error (sprintf "Ambiguous member '%s' for type '%A' at %A" memberAccessExpr.memberName systemType memberAccessExpr.span)
                        | None ->
                            match resolveTyp nameEnv typeEnv receiver.typ with
                            | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                            | None -> Result.Error (sprintf "Undefined type '%A' at %A" receiver.typ memberAccessExpr.span)
                | _ ->
                    let receiver = analyzeExpr nameEnv typeEnv memberAccessExpr.receiver (typeEnv.freshMeta())
                    let receiverType = typeEnv.resolveType receiver.typ
                    match TypeId.tryToRuntimeSystemType receiverType with
                    | Some systemType ->
                        let memberInfos =
                            systemType.GetMembers(BindingFlags.Public ||| BindingFlags.Instance)
                            |> Seq.filter (fun m -> m.Name = memberAccessExpr.memberName)
                            |> Seq.toList
                        match resolveNativeMember typeEnv memberInfos tid with
                        | [memberInfo, resolvedTid] ->
                            match memberInfo with
                            | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                            | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                            | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                            | _ -> Result.Error (sprintf "Unsupported member type for '%s' at %A" memberAccessExpr.memberName memberAccessExpr.span)
                        | [] ->
                            if systemType.IsValueType && memberAccessExpr.memberName = "ToString" then
                                let convertMethod = typeof<System.Convert>.GetMethod("ToString", [| systemType |])
                                if obj.ReferenceEquals(convertMethod, null) then
                                    Result.Error (sprintf "Undefined member '%s' for type '%A' at %A" memberAccessExpr.memberName systemType memberAccessExpr.span)
                                else
                                    Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod convertMethod, Some receiver, TypeId.Fn([TypeId.fromSystemType systemType], TypeId.String), memberAccessExpr.span))
                            else
                                Result.Error (sprintf "Undefined member '%s' for type '%A' at %A" memberAccessExpr.memberName systemType memberAccessExpr.span)
                        | _ -> Result.Error (sprintf "Ambiguous member '%s' for type '%A' at %A" memberAccessExpr.memberName systemType memberAccessExpr.span)
                    | None ->
                        match resolveTyp nameEnv typeEnv receiver.typ with
                        | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                        | None -> Result.Error (sprintf "Undefined type '%A' at %A" receiver.typ memberAccessExpr.span)

            resultToExpr tid memberAccessExpr.span resolvedMemberResult
        | :? Ast.Expr.StaticAccess as staticAccessExpr ->
            let resolvedStaticResult =
                match nameEnv.scope.ResolveType(staticAccessExpr.typeName) with
                | Some(TypeId.Name sid) ->
                    match nameEnv.resolveSym sid with
                    | Some symInfo ->
                        match symInfo.kind with
                        | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) ->
                            if obj.ReferenceEquals(sysType, null) then
                                Result.Error (unresolvedImportedSystemTypeMessage staticAccessExpr.typeName staticAccessExpr.span)
                            else
                                let memberInfos =
                                    sysType.GetMembers(BindingFlags.Public ||| BindingFlags.Static)
                                    |> Seq.filter (fun m -> m.Name = staticAccessExpr.memberName)
                                    |> Seq.toList
                                match resolveNativeMember typeEnv memberInfos tid with
                                | [memberInfo, memberType] ->
                                    match memberInfo with
                                    | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, None, memberType, staticAccessExpr.span))
                                    | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, None, memberType, staticAccessExpr.span))
                                    | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, None, memberType, staticAccessExpr.span))
                                    | _ -> Result.Error (sprintf "Unsupported member type for '%s.%s' at %A" staticAccessExpr.typeName staticAccessExpr.memberName staticAccessExpr.span)
                                | [] -> Result.Error (sprintf "Undefined member '%s' for type '%s' at %A" staticAccessExpr.memberName staticAccessExpr.typeName staticAccessExpr.span)
                                | _ -> Result.Error (sprintf "Ambiguous member '%s' for type '%s' at %A" staticAccessExpr.memberName staticAccessExpr.typeName staticAccessExpr.span)
                        | _ -> Result.Error (sprintf "Type '%s' is not a system type at %A" staticAccessExpr.typeName staticAccessExpr.span)
                    | None -> Result.Error (sprintf "Undefined type symbol '%s' at %A" staticAccessExpr.typeName staticAccessExpr.span)
                | Some _ -> Result.Error (sprintf "Unsupported type id for '%s' at %A" staticAccessExpr.typeName staticAccessExpr.span)
                | None -> Result.Error (sprintf "Undefined type '%s' at %A" staticAccessExpr.typeName staticAccessExpr.span)

            resultToExpr tid staticAccessExpr.span resolvedStaticResult
        | :? Ast.Expr.If as ifExpr ->
            let rec analyzeIfBranches (branches: Ast.IfBranch list) : Hir.Expr =
                match List.head branches with
                | :? Ast.IfBranch.Then as thenBranch ->
                    let cond = analyzeExpr nameEnv typeEnv thenBranch.cond TypeId.Bool
                    let body = analyzeExpr nameEnv typeEnv thenBranch.body tid
                    Hir.Expr.If(cond, body, analyzeIfBranches (List.tail branches), tid, { left = thenBranch.span.left; right = (List.last branches).span.right })
                | :? Ast.IfBranch.Else as elseBranch -> analyzeExpr nameEnv typeEnv elseBranch.body tid
                | _ -> errorExpr tid (List.head branches).span "Unsupported if branch type"
            analyzeIfBranches ifExpr.branches
        | _ -> errorExpr tid expr.span "Unsupported expression type"

    and private analyzeStmt (nameEnv: NameEnv) (typeEnv: TypeEnv) (stmt: Ast.Stmt) : Hir.Stmt =
        match stmt with
        | :? Ast.Stmt.Let as letStmt ->
            let tid = typeEnv.freshMeta ()
            let rhs = analyzeExpr nameEnv typeEnv letStmt.value tid
            let sid = nameEnv.declareLocal letStmt.name tid
            Hir.Stmt.Let(sid, false, rhs, letStmt.span)
        | :? Ast.Stmt.Var as varStmt ->
            let tid = typeEnv.freshMeta ()
            let rhs = analyzeExpr nameEnv typeEnv varStmt.value tid
            let sid = nameEnv.declareLocal varStmt.name tid
            Hir.Stmt.Let(sid, true, rhs, varStmt.span)
        | :? Ast.Stmt.Assign as assignStmt ->
            let tid = typeEnv.freshMeta ()
            let rhs = analyzeExpr nameEnv typeEnv assignStmt.value tid
            match nameEnv.resolveVar assignStmt.name (typeEnv.freshMeta ()) with
            | [sid] ->
                match unifyOrError typeEnv (nameEnv.resolveSymType sid) rhs.typ assignStmt.span with
                | Result.Ok _ -> Hir.Stmt.Assign(sid, rhs, assignStmt.span)
                | Result.Error exprErr -> Hir.Stmt.ExprStmt(exprErr, assignStmt.span)
            | [] -> Hir.Stmt.ExprStmt(Hir.Expr.ExprError(sprintf "Undefined variable '%s' at %A" assignStmt.name assignStmt.span, TypeId.Error (sprintf "Undefined variable '%s'" assignStmt.name), assignStmt.span), assignStmt.span)
            | _ ->
                Hir.Stmt.ExprStmt(
                    Hir.Expr.ExprError(
                        sprintf "Ambiguous variable '%s' at %A" assignStmt.name assignStmt.span,
                        TypeId.Error(sprintf "Ambiguous variable '%s'" assignStmt.name),
                        assignStmt.span),
                    assignStmt.span)
        | :? Ast.Stmt.ExprStmt as exprStmt ->
            let expr = analyzeExpr nameEnv typeEnv exprStmt.expr TypeId.Unit
            Hir.Stmt.ExprStmt(expr, exprStmt.span)
        | :? Ast.Stmt.For as forStmt ->
            let iterable = analyzeExpr nameEnv typeEnv forStmt.iterable (typeEnv.freshMeta ())
            let resolvedIterableType = typeEnv.resolveType iterable.typ
            match TypeId.tryToRuntimeSystemType resolvedIterableType with
            | None ->
                Hir.Stmt.ErrorStmt(sprintf "For iterable is not a supported runtime type: %A at %A" resolvedIterableType forStmt.span, forStmt.span)
            | Some iterableSystemType ->
                let iteratorResolution =
                    match tryResolveEnumeratorMembers iterableSystemType with
                    | Some members -> Some(iterable, members)
                    | None ->
                        match tryGetEnumerator iterableSystemType with
                        | Some getEnumeratorMethod ->
                            let iteratorType = getEnumeratorMethod.ReturnType
                            tryResolveEnumeratorMembers iteratorType
                            |> Option.map (fun members ->
                                let getEnumeratorExpr =
                                    Hir.Expr.Call(
                                        Hir.Callable.NativeMethod getEnumeratorMethod,
                                        None,
                                        [ iterable ],
                                        TypeId.fromSystemType iteratorType,
                                        forStmt.span)
                                getEnumeratorExpr, members)
                        | None -> None

                match iteratorResolution with
                | Some (resolvedIteratorExpr, resolvedIteratorType) ->
                    let loopNameEnv = nameEnv.sub()
                    let itemType = TypeId.fromSystemType resolvedIteratorType.current.PropertyType
                    let loopVarSid = loopNameEnv.declareLocal forStmt.varName itemType
                    let bodyStmts = forStmt.body |> List.map (analyzeStmt loopNameEnv typeEnv)
                    Hir.Stmt.For(loopVarSid, itemType, resolvedIteratorExpr, bodyStmts, forStmt.span)
                | None ->
                    Hir.Stmt.ErrorStmt(sprintf "For iterable type '%A' does not define MoveNext/Current or GetEnumerator() at %A" iterableSystemType forStmt.span, forStmt.span)
        | _ -> Hir.Stmt.ErrorStmt("Unsupported statement type", stmt.span)

    let private analyzeMethod (nameEnv: NameEnv) (typeEnv: TypeEnv) (fnDecl: Ast.Decl.Fn) : Hir.Method =
        let bodyNameEnv = nameEnv.sub()
        let retType = nameEnv.resolveTypeExpr fnDecl.ret
        let rawArgTypes = fnDecl.args |> List.map bodyNameEnv.resolveArgType
        let argTypes =
            match fnDecl.args, rawArgTypes with
            | [ (:? Ast.FnArg.Unit) ], [ TypeId.Unit ] -> []
            | _ -> rawArgTypes

        fnDecl.args
        |> List.iteri (fun index arg ->
            match arg with
            | :? Ast.FnArg.Named as namedArg ->
                let argType = argTypes.[index]
                bodyNameEnv.declareArg namedArg.name argType |> ignore
            | :? Ast.FnArg.Unit -> ()
            | _ -> ())

        let tid = TypeId.Fn(argTypes, retType)
        let sid = nameEnv.declareLocal fnDecl.name tid
        let body = analyzeExpr bodyNameEnv typeEnv fnDecl.body retType

        Hir.Method(sid, body, tid, fnDecl.span)

    let analyzeModule (symbolTable: SymbolTable, typeSubst: TypeSubst, moduleName: string, moduleAst: Ast.Module) : PhaseResult<Hir.Module> =
        match Resolve.resolveModule (symbolTable, moduleName, moduleAst) with
        | { succeeded = false; diagnostics = diagnostics } -> PhaseResult.failed diagnostics
        | { value = Some resolvedModule } ->
            let nameEnv = NameEnv(symbolTable, resolvedModule.moduleScope)
            let typeEnv = TypeEnv(typeSubst, TypeMetaFactory())

            let fields = List<Hir.Field>()
            let methods = List<Hir.Method>()
            let types = List<Hir.Type>()

            resolvedModule.fnDecls
            |> List.iter (fun fnDecl -> methods.Add(analyzeMethod nameEnv typeEnv fnDecl))

            let untypedHirModule =
                Hir.Module(
                    resolvedModule.moduleName,
                    types |> Seq.toList,
                    fields |> Seq.toList,
                    methods |> Seq.toList,
                    resolvedModule.moduleScope
                )

            match Infer.inferModule (typeSubst, untypedHirModule) with
            | Result.Ok hir -> PhaseResult.succeeded hir []
            | Result.Error diagnostics -> PhaseResult.failed diagnostics
        | _ -> PhaseResult.failed [ Diagnostic.Error("Unknown analyze module failure", Atla.Core.Data.Span.Empty) ]
