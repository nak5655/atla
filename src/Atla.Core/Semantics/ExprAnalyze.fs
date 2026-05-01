namespace Atla.Core.Semantics

open System.Reflection
open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open Atla.Core.Semantics.Data.AnalyzeEnv

module ExprAnalyze =

    let private resolveTyp (nameEnv: NameEnv) (typeEnv: TypeEnv) (tid: TypeId) : SymbolInfo option =
        match typeEnv.resolveType tid with
        | TypeId.Name sid -> nameEnv.resolveSym sid
        | _ -> None

    let private errorExpr (tid: TypeId) (span: Atla.Core.Data.Span) (message: string) : Hir.Expr =
        Hir.Expr.ExprError(message, tid, span)

    let private resultToExpr (tid: TypeId) (span: Atla.Core.Data.Span) (resolved: Result<Hir.Expr, string>) : Hir.Expr =
        match resolved with
        | Result.Ok expr -> expr
        | Result.Error message -> errorExpr tid span message

    let private unresolvedImportedSystemTypeMessage (typeName: string) (_span: Atla.Core.Data.Span) : string =
        sprintf
            "Imported system type '%s' was not found. If this type is provided by a dependency, check dependency loading diagnostics."
            typeName

    let private isNativeVoid (typeEnv: TypeEnv) (tid: TypeId) : bool =
        match typeEnv.resolveType tid with
        | TypeId.Native runtimeType when runtimeType = typeof<System.Void> -> true
        | _ -> false

    let private isUnitContext (typeEnv: TypeEnv) (tid: TypeId) : bool =
        match typeEnv.resolveType tid with
        | TypeId.Unit -> true
        | _ -> false

    // Format a TypeId into a user-friendly, human-readable type name.
    let rec private formatTypeForDisplay (nameEnv: NameEnv) (typeEnv: TypeEnv) (tid: TypeId) : string =
        match typeEnv.resolveType tid with
        | TypeId.Unit -> "unit"
        | TypeId.Bool -> "bool"
        | TypeId.Int -> "int"
        | TypeId.Float -> "float"
        | TypeId.String -> "string"
        | TypeId.Native t -> t.FullName
        | TypeId.Fn (args, ret) ->
            let argsStr = args |> List.map (formatTypeForDisplay nameEnv typeEnv) |> String.concat " -> "
            sprintf "(%s -> %s)" argsStr (formatTypeForDisplay nameEnv typeEnv ret)
        | TypeId.App (head, args) ->
            let argsStr = args |> List.map (formatTypeForDisplay nameEnv typeEnv) |> String.concat ", "
            sprintf "%s<%s>" (formatTypeForDisplay nameEnv typeEnv head) argsStr
        | TypeId.Name sid ->
            // Retrieve the type name from the symbol table; if unavailable, return a generic placeholder.
            match nameEnv.resolveSym sid with
            | Some symInfo -> symInfo.name
            | None -> "<named type>"
        | TypeId.Meta _ -> "unknown"
        | TypeId.Error _ -> "<error>"

    // UnifyError を人間が読みやすいメッセージへ変換する。
    // formatTypeForDisplay を使い、型変数（Meta）を "unknown" に、既知の型を短い名前で表示する。
    let private formatUnifyError (nameEnv: NameEnv) (typeEnv: TypeEnv) (err: UnifyError) : string =
        match err with
        | UnifyError.DifferentFunctionArity (leftArity, rightArity) ->
            sprintf "Cannot unify function types with different number of arguments: %d vs %d" leftArity rightArity
        | UnifyError.OccursCheckFailed (meta, actual) ->
            sprintf "Occurs check failed: %A occurs in %s" meta (formatTypeForDisplay nameEnv typeEnv actual)
        | UnifyError.CannotUnify (left, right) ->
            sprintf "Cannot unify types: %s and %s"
                (formatTypeForDisplay nameEnv typeEnv left)
                (formatTypeForDisplay nameEnv typeEnv right)

    // 型の単一化を試み、失敗時は ExprError を返す。
    let private unifyOrError (nameEnv: NameEnv) (typeEnv: TypeEnv) (expected: TypeId) (actual: TypeId) (span: Atla.Core.Data.Span) : Result<unit, Hir.Expr> =
        let resolvedExpected = typeEnv.resolveType expected
        let resolvedActual = typeEnv.resolveType actual
        let dataDefsBySid =
            nameEnv.dataTypeDefs
            |> Map.toSeq
            |> Seq.map (fun (_, def) -> def.typeSid, def)
            |> Map.ofSeq

        let tryResolveNamedSystemType (sid: SymbolId) : System.Type option =
            match nameEnv.resolveSym sid with
            | Some symInfo ->
                match symInfo.kind with
                | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) -> Some sysType
                | _ -> None
            | None -> None

        let rec isSubtypeOfSystemType (candidateSid: SymbolId) (expectedSystemType: System.Type) (visited: Set<int>) : bool =
            if visited |> Set.contains candidateSid.id then
                false
            else
                match tryResolveNamedSystemType candidateSid with
                | Some candidateSystemType -> expectedSystemType.IsAssignableFrom(candidateSystemType)
                | None ->
                    match dataDefsBySid |> Map.tryFind candidateSid with
                    | Some candidateDef ->
                        match candidateDef.baseTypeSid with
                        | Some baseSid -> isSubtypeOfSystemType baseSid expectedSystemType (visited |> Set.add candidateSid.id)
                        | None -> false
                    | None -> false

        let subtypeSatisfied =
            match resolvedExpected, resolvedActual with
            | TypeId.Name expectedSid, TypeId.Name actualSid -> nameEnv.isSubtype actualSid expectedSid
            | TypeId.Native expectedSystemType, TypeId.Name actualSid -> isSubtypeOfSystemType actualSid expectedSystemType Set.empty
            | TypeId.Native expectedSystemType, TypeId.Native actualSystemType -> expectedSystemType.IsAssignableFrom(actualSystemType)
            | _ -> false

        if isNativeVoid typeEnv actual && not (isUnitContext typeEnv expected) then
            Result.Error(errorExpr expected span (formatUnifyError nameEnv typeEnv (UnifyError.CannotUnify(expected, actual))))
        elif subtypeSatisfied then
            Result.Ok ()
        else
            match typeEnv.unifyTypes expected actual with
            | Result.Ok _ -> Result.Ok ()
            | Result.Error err -> Result.Error(errorExpr expected span (formatUnifyError nameEnv typeEnv err))

    /// data 型インスタンスの継承チェーンを辿ってメンバー（field/instance method）を探索する。
    /// 直接見つからない場合は `impl ... by <field>` の委譲先フィールドも探索する。
    let private tryResolveDataInstanceMember
        (nameEnv: NameEnv)
        (typeEnv: TypeEnv)
        (expectedType: TypeId)
        (receiverTypeSid: SymbolId)
        (memberName: string)
        (span: Atla.Core.Data.Span)
        (receiverExpr: Hir.Expr)
        : Result<Hir.Expr, string> =
        let bySid =
            nameEnv.dataTypeDefs
            |> Map.toSeq
            |> Seq.map (fun (_, def) -> def.typeSid, def)
            |> Map.ofSeq

        let rec loop (currentSid: SymbolId) (visited: Set<int>) (currentReceiverExpr: Hir.Expr) =
            if visited |> Set.contains currentSid.id then
                Result.Error("Cyclic subtype relation detected while resolving data member")
            else
                match bySid |> Map.tryFind currentSid with
                | None -> Result.Error("Undefined data type while resolving member")
                | Some currentDef ->
                    match currentDef.fields |> List.tryFind (fun fieldDef -> fieldDef.name = memberName) with
                    | Some fieldDef ->
                        Result.Ok(Hir.Expr.MemberAccess(Hir.Member.DataField(currentDef.typeSid, fieldDef.sid), Some currentReceiverExpr, fieldDef.typ, span))
                    | None ->
                        match currentDef.methods |> Map.tryFind memberName with
                        | Some (methodSid, methodType, isStatic) when not isStatic ->
                            let boundMethodType =
                                match methodType with
                                | TypeId.Fn(_ :: remainingArgs, retType) -> TypeId.Fn(remainingArgs, retType)
                                | _ -> methodType
                            Result.Ok(Hir.Expr.MemberAccess(Hir.Member.DataMethod(currentDef.typeSid, methodSid), Some currentReceiverExpr, boundMethodType, span))
                        | Some _ ->
                            Result.Error(sprintf "Undefined instance member '%s' for data type" memberName)
                        | None ->
                            match currentDef.baseTypeSid with
                            | Some baseSid ->
                                match loop baseSid (visited |> Set.add currentSid.id) currentReceiverExpr with
                                | Result.Ok resolved -> Result.Ok resolved
                                | Result.Error _ -> tryResolveFromDelegatedField currentDef
                            | None -> tryResolveFromDelegatedField currentDef

        and tryResolveFromDelegatedField (currentDef: DataTypeDef) : Result<Hir.Expr, string> =
            match currentDef.delegatedByFieldName with
            | None -> Result.Error(sprintf "Undefined member '%s' for data type" memberName)
            | Some delegateFieldName ->
                match currentDef.fields |> List.tryFind (fun fieldDef -> fieldDef.name = delegateFieldName) with
                | None ->
                    Result.Error(sprintf "Delegate field '%s' not found on data type while resolving member" delegateFieldName)
                | Some delegateFieldDef ->
                    let delegatedReceiver =
                        Hir.Expr.MemberAccess(Hir.Member.DataField(currentDef.typeSid, delegateFieldDef.sid), Some receiverExpr, delegateFieldDef.typ, span)

                    match typeEnv.resolveType delegateFieldDef.typ with
                    | TypeId.Name delegateTypeSid ->
                        if bySid |> Map.containsKey delegateTypeSid then
                            loop delegateTypeSid Set.empty delegatedReceiver
                        else
                            match nameEnv.resolveSym delegateTypeSid with
                            | Some symInfo ->
                                match symInfo.kind with
                                | SymbolKind.External(ExternalBinding.SystemTypeRef systemType) when not (obj.ReferenceEquals(systemType, null)) ->
                                    let memberInfos =
                                        NativeInterop.getPublicInstanceMembersIncludingInterfaces systemType
                                        |> Seq.filter (fun memberInfo -> memberInfo.Name = memberName)
                                        |> Seq.toList

                                    match NativeInterop.resolveNativeMember typeEnv memberInfos expectedType with
                                    | [memberInfo, resolvedMemberType] ->
                                        match memberInfo with
                                        | :? MethodInfo as methodInfo ->
                                            Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeMethod(methodInfo), Some delegatedReceiver, resolvedMemberType, span))
                                        | :? FieldInfo as fieldInfo ->
                                            Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeField(fieldInfo), Some delegatedReceiver, resolvedMemberType, span))
                                        | :? PropertyInfo as propertyInfo ->
                                            Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeProperty(propertyInfo), Some delegatedReceiver, resolvedMemberType, span))
                                        | _ ->
                                            Result.Error(sprintf "Unsupported delegated member type for '%s'" memberName)
                                    | [] -> Result.Error(sprintf "Undefined member '%s' for delegated type '%s'" memberName systemType.FullName)
                                    | members ->
                                        let methodInfos =
                                            members
                                            |> List.choose (fun (memberInfo, _) ->
                                                match memberInfo with
                                                | :? MethodInfo as methodInfo -> Some methodInfo
                                                | _ -> None)

                                        if methodInfos.Length = members.Length then
                                            Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeMethodGroup methodInfos, Some delegatedReceiver, expectedType, span))
                                        else
                                            Result.Error(sprintf "Ambiguous delegated member '%s' for type '%s'" memberName systemType.FullName)
                                | _ ->
                                    Result.Error(sprintf "Delegate field '%s' type does not support delegated member lookup" delegateFieldName)
                            | None ->
                                Result.Error(sprintf "Undefined delegate field type for '%s'" delegateFieldName)
                    | TypeId.Native systemType ->
                        let memberInfos =
                            NativeInterop.getPublicInstanceMembersIncludingInterfaces systemType
                            |> Seq.filter (fun memberInfo -> memberInfo.Name = memberName)
                            |> Seq.toList

                        match NativeInterop.resolveNativeMember typeEnv memberInfos expectedType with
                        | [memberInfo, resolvedMemberType] ->
                            match memberInfo with
                            | :? MethodInfo as methodInfo ->
                                Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeMethod(methodInfo), Some delegatedReceiver, resolvedMemberType, span))
                            | :? FieldInfo as fieldInfo ->
                                Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeField(fieldInfo), Some delegatedReceiver, resolvedMemberType, span))
                            | :? PropertyInfo as propertyInfo ->
                                Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeProperty(propertyInfo), Some delegatedReceiver, resolvedMemberType, span))
                            | _ ->
                                Result.Error(sprintf "Unsupported delegated member type for '%s'" memberName)
                        | [] -> Result.Error(sprintf "Undefined member '%s' for delegated type '%s'" memberName systemType.FullName)
                        | members ->
                            let methodInfos =
                                members
                                |> List.choose (fun (memberInfo, _) ->
                                    match memberInfo with
                                    | :? MethodInfo as methodInfo -> Some methodInfo
                                    | _ -> None)

                            if methodInfos.Length = members.Length then
                                Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeMethodGroup methodInfos, Some delegatedReceiver, expectedType, span))
                            else
                                Result.Error(sprintf "Ambiguous delegated member '%s' for type '%s'" memberName systemType.FullName)
                    | unsupportedType ->
                        Result.Error(sprintf "Type '%s' does not support delegation lookup for member '%s'" (string unsupportedType) memberName)

        loop receiverTypeSid Set.empty receiverExpr

    /// data 型定義（型名アクセス）から static メソッドを探索する。
    let private tryResolveDataStaticMember
        (nameEnv: NameEnv)
        (receiverTypeSid: SymbolId)
        (memberName: string)
        (span: Atla.Core.Data.Span)
        : Result<Hir.Expr, string> =
        let bySid =
            nameEnv.dataTypeDefs
            |> Map.toSeq
            |> Seq.map (fun (_, def) -> def.typeSid, def)
            |> Map.ofSeq

        let rec loop (currentSid: SymbolId) (visited: Set<int>) =
            if visited |> Set.contains currentSid.id then
                Result.Error("Cyclic subtype relation detected while resolving data static member")
            else
                match bySid |> Map.tryFind currentSid with
                | None -> Result.Error("Undefined data type while resolving static member")
                | Some currentDef ->
                    match currentDef.methods |> Map.tryFind memberName with
                    | Some (methodSid, methodType, isStatic) when isStatic ->
                        Result.Ok(Hir.Expr.MemberAccess(Hir.Member.DataMethod(currentDef.typeSid, methodSid), None, methodType, span))
                    | Some _ ->
                        Result.Error(sprintf "Undefined static member '%s' for data type" memberName)
                    | None ->
                        match currentDef.baseTypeSid with
                        | Some baseSid -> loop baseSid (visited |> Set.add currentSid.id)
                        | None -> Result.Error(sprintf "Undefined static member '%s' for data type" memberName)

        loop receiverTypeSid Set.empty

    // Generate an overload error message with available candidates when argument count does not match.
    let private noOverloadMessage (callable: Hir.Callable) (argCount: int) : string =
        match callable with
        | Hir.Callable.NativeMethodGroup methods when not methods.IsEmpty ->
            let name = (List.head methods).Name
            let overloads =
                methods
                |> List.map (fun mi ->
                    let ps = mi.GetParameters() |> Array.map (fun p -> p.ParameterType.FullName) |> String.concat ", "
                    sprintf "  %s(%s)" mi.Name ps)
                |> String.concat "\n"
            sprintf "No overload of '%s' accepts %d argument(s). Available overloads:\n%s" name argCount overloads
        | Hir.Callable.NativeConstructorGroup ctors when not ctors.IsEmpty ->
            let typeName = (List.head ctors).DeclaringType.FullName
            let overloads =
                ctors
                |> List.map (fun ci ->
                    let ps = ci.GetParameters() |> Array.map (fun p -> p.ParameterType.FullName) |> String.concat ", "
                    sprintf "  new(%s)" ps)
                |> String.concat "\n"
            sprintf "No overload of '%s' constructor accepts %d argument(s). Available overloads:\n%s" typeName argCount overloads
        | _ -> sprintf "No overload matched argument count %d" argCount

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
            | Hir.Member.NativeMethodGroup methodInfos -> Some (Hir.Callable.NativeMethodGroup methodInfos)
            | Hir.Member.DataMethod (_, methodSid) -> Some (Hir.Callable.Fn methodSid)
            | _ -> None
        | _ -> None

    and private analyzeExpr (nameEnv: NameEnv) (typeEnv: TypeEnv) (expr: Ast.Expr) (tid: TypeId) : Hir.Expr =
        match expr with
        | :? Ast.Expr.Error as errorExpr ->
            // Parser 由来の式エラーはここで失わず、HIR 診断ノードへ明示的に伝播する。
            Hir.Expr.ExprError(errorExpr.message, tid, errorExpr.span)
        | :? Ast.Expr.Unit as unitExpr ->
            match unifyOrError nameEnv typeEnv tid TypeId.Unit unitExpr.span with
            | Result.Ok _ -> Hir.Expr.Unit(unitExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Int as intExpr ->
            match unifyOrError nameEnv typeEnv tid TypeId.Int intExpr.span with
            | Result.Ok _ -> Hir.Expr.Int(intExpr.value, intExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Bool as boolExpr ->
            match unifyOrError nameEnv typeEnv tid TypeId.Bool boolExpr.span with
            | Result.Ok _ -> Hir.Expr.Bool(boolExpr.value, boolExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Float as floatExpr ->
            match unifyOrError nameEnv typeEnv tid TypeId.Float floatExpr.span with
            | Result.Ok _ -> Hir.Expr.Float(floatExpr.value, floatExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.String as stringExpr ->
            match unifyOrError nameEnv typeEnv tid TypeId.String stringExpr.span with
            | Result.Ok _ -> Hir.Expr.String(stringExpr.value, stringExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.DataInit as dataInitExpr ->
            match Map.tryFind dataInitExpr.typeName nameEnv.dataTypeDefs with
            | None ->
                Hir.Expr.ExprError(sprintf "Undefined data type '%s'" dataInitExpr.typeName, tid, dataInitExpr.span)
            | Some dataTypeDef ->
                let fieldMap = dataTypeDef.fields |> List.map (fun fieldDef -> fieldDef.name, fieldDef) |> Map.ofList
                let initFields =
                    dataInitExpr.fields
                    |> List.choose (fun field ->
                        match field with
                        | :? Ast.DataInitField.Field as namedField -> Some namedField
                        | _ -> None)

                let duplicateFieldName =
                    initFields
                    |> List.fold
                        (fun (seen, dup) field ->
                            match dup with
                            | Some _ -> seen, dup
                            | None when Set.contains field.name seen -> seen, Some field.name
                            | None -> Set.add field.name seen, None)
                        (Set.empty, None)
                    |> snd

                match duplicateFieldName with
                | Some duplicatedName ->
                    Hir.Expr.ExprError(sprintf "Duplicate field initializer '%s'" duplicatedName, tid, dataInitExpr.span)
                | None ->
                    let unknownFieldName =
                        initFields
                        |> List.tryPick (fun field ->
                            if Map.containsKey field.name fieldMap then None else Some field.name)
                    match unknownFieldName with
                    | Some unknownName ->
                        Hir.Expr.ExprError(sprintf "Unknown field '%s' for data type '%s'" unknownName dataInitExpr.typeName, tid, dataInitExpr.span)
                    | None ->
                        let providedFieldNames = initFields |> List.map (fun field -> field.name) |> Set.ofList
                        let missingField =
                            dataTypeDef.fields
                            |> List.tryFind (fun fieldDef -> not (Set.contains fieldDef.name providedFieldNames))
                        match missingField with
                        | Some missing ->
                            Hir.Expr.ExprError(sprintf "Missing required field '%s' for data type '%s'" missing.name dataInitExpr.typeName, tid, dataInitExpr.span)
                        | None ->
                            let initFieldMap =
                                initFields
                                |> List.map (fun field -> field.name, field.value)
                                |> Map.ofList

                            let typedArgsResult =
                                dataTypeDef.fields
                                |> List.fold
                                    (fun acc fieldDef ->
                                        match acc with
                                        | Result.Error exprErr -> Result.Error exprErr
                                        | Result.Ok typedArgs ->
                                            match Map.tryFind fieldDef.name initFieldMap with
                                            | None ->
                                                Result.Error(Hir.Expr.ExprError(sprintf "Missing required field '%s' for data type '%s'" fieldDef.name dataInitExpr.typeName, tid, dataInitExpr.span))
                                            | Some valueExpr ->
                                                let typedExpr = analyzeExpr nameEnv typeEnv valueExpr fieldDef.typ
                                                match typedExpr with
                                                | Hir.Expr.ExprError _ as errExpr -> Result.Error errExpr
                                                | _ -> Result.Ok (typedArgs @ [ typedExpr ]))
                                    (Result.Ok [])

                            match typedArgsResult with
                            | Result.Error exprErr -> exprErr
                            | Result.Ok typedArgs ->
                                let dataType = TypeId.Name dataTypeDef.typeSid
                                match unifyOrError nameEnv typeEnv tid dataType dataInitExpr.span with
                                | Result.Error exprErr -> exprErr
                                | Result.Ok _ ->
                                    Hir.Expr.Call(
                                        Hir.Callable.DataConstructor(dataTypeDef.typeSid, dataTypeDef.fields |> List.map (fun fieldDef -> fieldDef.sid)),
                                        None,
                                        typedArgs,
                                        dataType,
                                        dataInitExpr.span)
        | :? Ast.Expr.Id as idExpr ->
            match nameEnv.resolveVar idExpr.name with
            | [sid] ->
                match nameEnv.resolveSym sid with
                | Some symInfo ->
                    match symInfo.kind with
                    | SymbolKind.External(ExternalBinding.NativeMethodGroup _)
                    | SymbolKind.External(ExternalBinding.ConstructorGroup _) ->
                        Hir.Expr.Id(sid, tid, idExpr.span)
                    | _ ->
                        match unifyOrError nameEnv typeEnv tid symInfo.typ idExpr.span with
                        | Result.Ok _ -> Hir.Expr.Id(sid, symInfo.typ, idExpr.span)
                        | Result.Error exprErr -> exprErr
                | None ->
                    Hir.Expr.ExprError(sprintf "Undefined symbol for '%s'" idExpr.name, tid, idExpr.span)
            | [] ->
                // 変数として未登録だが型スペースに存在する場合、import されたが未解決の型である可能性がある。
                // その場合は依存関係の設定を促す具体的なメッセージを返す。
                let errorMessage =
                    match nameEnv.scope.ResolveType(idExpr.name) with
                    | Some (TypeId.Name sid) ->
                        match nameEnv.resolveSym sid with
                        | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) } when isNull sysType ->
                            sprintf
                                "Type '%s' is imported but could not be resolved from loaded assemblies. Check that the dependency providing this type is listed in atla.yaml."
                                idExpr.name
                        | _ -> sprintf "Undefined variable '%s'" idExpr.name
                    | _ -> sprintf "Undefined variable '%s'" idExpr.name

                Hir.Expr.ExprError(errorMessage, tid, idExpr.span)
            | sids ->
                // 同名候補が複数ある場合は期待型との単一化可否で候補を絞り込む。
                // これにより、同名ビルトイン演算子（Int/Float）の解決を決定的に行う。
                let typedCandidates =
                    sids
                    |> List.choose (fun sid ->
                        match nameEnv.resolveSym sid with
                        | Some symInfo when typeEnv.canUnify tid symInfo.typ -> Some (sid, symInfo)
                        | _ -> None)

                match typedCandidates with
                | [sid, symInfo] ->
                    match symInfo.kind with
                    | SymbolKind.External(ExternalBinding.NativeMethodGroup _)
                    | SymbolKind.External(ExternalBinding.ConstructorGroup _) ->
                        Hir.Expr.Id(sid, tid, idExpr.span)
                    | _ ->
                        match unifyOrError nameEnv typeEnv tid symInfo.typ idExpr.span with
                        | Result.Ok _ -> Hir.Expr.Id(sid, symInfo.typ, idExpr.span)
                        | Result.Error exprErr -> exprErr
                | [] ->
                    Hir.Expr.ExprError(sprintf "No overload matched for '%s'" idExpr.name, tid, idExpr.span)
                | _ ->
                    Hir.Expr.ExprError(sprintf "Ambiguous variable '%s'" idExpr.name, tid, idExpr.span)
        | :? Ast.Expr.GenericApply as genericApplyExpr ->
            let genericArgResolutionResult = NativeInterop.resolveGenericTypeArgs nameEnv genericApplyExpr
            match genericArgResolutionResult with
            | Result.Error message ->
                Hir.Expr.ExprError(message, tid, genericApplyExpr.span)
            | Result.Ok genericArgTypes ->
                let analyzedTarget = analyzeExpr nameEnv typeEnv genericApplyExpr.func tid
                let buildGenericMemberExpr (instanceOpt: Hir.Expr option) (methodInfo: MethodInfo) =
                    let resolvedType = NativeInterop.memberMethodType instanceOpt methodInfo
                    Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, instanceOpt, resolvedType, genericApplyExpr.span)
                match analyzedTarget with
                | Hir.Expr.ExprError _ ->
                    // func 式が既にエラーの場合、元のエラーをそのまま伝播して汎用メッセージで隠さない。
                    analyzedTarget
                | Hir.Expr.MemberAccess (Hir.Member.NativeMethod methodInfo, instanceOpt, _, _) ->
                    match NativeInterop.closeGenericMethod genericArgTypes methodInfo with
                    | Some closedMethodInfo -> buildGenericMemberExpr instanceOpt closedMethodInfo
                    | None -> Hir.Expr.ExprError(sprintf "Generic method arity mismatch for '%s'" methodInfo.Name, tid, genericApplyExpr.span)
                | Hir.Expr.Id (sid, _, _) ->
                    match nameEnv.resolveSym sid with
                    | Some symInfo ->
                        match symInfo.kind with
                        | SymbolKind.External(ExternalBinding.NativeMethodGroup methodInfos) ->
                            let matchedMethods = methodInfos |> List.choose (NativeInterop.closeGenericMethod genericArgTypes)
                            match matchedMethods with
                            | [closedMethodInfo] -> buildGenericMemberExpr None closedMethodInfo
                            | [] -> Hir.Expr.ExprError(sprintf "No generic overload matched for '%s'" symInfo.name, tid, genericApplyExpr.span)
                            | _ -> Hir.Expr.ExprError(sprintf "Ambiguous generic overload for '%s'" symInfo.name, tid, genericApplyExpr.span)
                        | _ ->
                            Hir.Expr.ExprError("Expression is not a generic callable target", tid, genericApplyExpr.span)
                    | None ->
                        Hir.Expr.ExprError("Undefined symbol in generic apply", tid, genericApplyExpr.span)
                | _ ->
                    Hir.Expr.ExprError("Expression is not a generic callable target", tid, genericApplyExpr.span)
        | :? Ast.Expr.Block as blockExpr ->
            let blockNameEnv = nameEnv.sub()
            // 末尾 ExprStmt は block の期待型 tid で解析する。
            // analyzeStmt が常に unit を期待型として使うため、DataInit のような具体型を返す式が
            // 末尾に来ると "Cannot unify: unit and T" エラーになる不具合を回避する。
            match List.last blockExpr.stmts with
            | :? Ast.Stmt.ExprStmt as lastExprStmt ->
                let leadingStmts =
                    blockExpr.stmts
                    |> List.take (blockExpr.stmts.Length - 1)
                    |> List.map (analyzeStmt blockNameEnv typeEnv)
                // 末尾式を block の期待型で解析する。エラーはそのまま伝播させ、
                // 根本原因以外の "Cannot unify" が余分に報告されるのを防ぐ。
                let lastExpr = analyzeExpr blockNameEnv typeEnv lastExprStmt.expr tid
                Hir.Expr.Block(leadingStmts, lastExpr, tid, blockExpr.span)
            | _ ->
                let stmts = blockExpr.stmts |> List.map (analyzeStmt blockNameEnv typeEnv)
                let unitExpr = Hir.Expr.Unit ({ left = blockExpr.span.right; right = blockExpr.span.right })
                Hir.Expr.Block(stmts, unitExpr, tid, blockExpr.span)
        | :? Ast.Expr.Apply as applyExpr ->
            let analyzedArgs = applyExpr.args |> List.map (fun arg -> analyzeExpr nameEnv typeEnv arg (typeEnv.freshMeta()))
            let callRetType = typeEnv.freshMeta()
            let funcType = analyzedArgs |> List.map (fun arg -> arg.typ) |> fun argTypes -> TypeId.Fn(argTypes, callRetType)
            let analyzedFunc = analyzeExpr nameEnv typeEnv applyExpr.func funcType
            let callable = exprAsCallable nameEnv typeEnv analyzedFunc
            let callInstance =
                match analyzedFunc with
                | Hir.Expr.MemberAccess (_, Some instance, _, _) -> Some instance
                | _ -> None
            match callable with
            | Some resolvedCallable ->
                let allArgs = analyzedArgs
                let suppliedParameterCount (_methodInfo: MethodInfo) =
                    allArgs.Length

                // 呼び出し時の実引数に対応する論理パラメータ型列を返す。
                // インスタンスメソッドは先頭に receiver 型を補う。
                let suppliedParameterTypes (methodInfo: MethodInfo) : TypeId list =
                    let parameterTypes =
                        methodInfo.GetParameters()
                        |> Array.toList
                        |> List.map (fun p -> TypeId.fromSystemType p.ParameterType)

                    let suppliedCount = min parameterTypes.Length allArgs.Length
                    parameterTypes |> List.take suppliedCount

                let canApplyWithOptionalDefaults (methodInfo: MethodInfo) =
                    let parameters = methodInfo.GetParameters()
                    let suppliedCount = suppliedParameterCount methodInfo
                    if suppliedCount > parameters.Length then
                        false
                    else
                        parameters
                        |> Array.skip suppliedCount
                        |> Array.forall (fun p -> p.IsOptional && (NativeInterop.tryDefaultArgExpr p applyExpr.span |> Option.isSome))

                // Name（インポート型）と Native（リフレクション型）の間のサブタイプ互換性を判定する。
                // typeEnv.canUnify は Name/Native をすべて互換とするため、
                // delegation チェーンを辿って実際の .NET 型との互換性を厳密に検証する。
                let isSubtypeCompatible (actualType: TypeId) (expectedType: TypeId) : bool =
                    let resolvedActual = typeEnv.resolveType actualType
                    let resolvedExpected = typeEnv.resolveType expectedType
                    // シンボルIDから .NET SystemType を解決するヘルパー。
                    let tryResolveSysType (sid: SymbolId) =
                        match nameEnv.resolveSym sid with
                        | Some symInfo ->
                            match symInfo.kind with
                            | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) -> Some sysType
                            | _ -> None
                        | None -> None
                    // delegation チェーンを辿り、actualSid の実体が expectedSysType の派生型か判定する。
                    let dataDefsBySidLocal =
                        nameEnv.dataTypeDefs
                        |> Map.toSeq
                        |> Seq.map (fun (_, def) -> def.typeSid, def)
                        |> Map.ofSeq
                    let rec isNameSubtypeOfNative (sid: SymbolId) (expectedSysType: System.Type) (vis: Set<int>) =
                        if vis.Contains sid.id then false
                        else
                            match tryResolveSysType sid with
                            | Some sysType -> expectedSysType.IsAssignableFrom(sysType)
                            | None ->
                                match dataDefsBySidLocal |> Map.tryFind sid with
                                | Some def ->
                                    match def.baseTypeSid with
                                    | Some bs -> isNameSubtypeOfNative bs expectedSysType (vis.Add sid.id)
                                    | None -> false
                                | None -> false
                    match resolvedExpected, resolvedActual with
                    | _ when resolvedActual = resolvedExpected -> true
                    | TypeId.Name expectedSid, TypeId.Name actualSid ->
                        expectedSid = actualSid || nameEnv.isSubtype actualSid expectedSid
                    | TypeId.Native expectedSysType, TypeId.Name actualSid ->
                        isNameSubtypeOfNative actualSid expectedSysType Set.empty
                    | TypeId.Native expectedSysType, TypeId.Native actualSysType ->
                        expectedSysType.IsAssignableFrom(actualSysType)
                    | _ -> typeEnv.canUnify actualType expectedType

                // method 候補が実引数型と期待戻り型に適合するか判定する。
                // canUnify ではなく isSubtypeCompatible を使うことで、不適切なオーバーロードを排除する。
                let methodMatchesTypes (methodInfo: MethodInfo) : bool =
                    let suppliedTypes = suppliedParameterTypes methodInfo
                    if suppliedTypes.Length <> allArgs.Length then
                        false
                    else
                        let argsMatch =
                            List.zip allArgs suppliedTypes
                            |> List.forall (fun (actualArg, expectedType) -> isSubtypeCompatible actualArg.typ expectedType)

                        if not argsMatch then
                            false
                        else
                            let expectedReturnType = TypeId.fromSystemType methodInfo.ReturnType
                            typeEnv.canUnify callRetType expectedReturnType

                // 最もスコアの高い候補を返し、同スコアが複数ある場合はパラメータ型の特化度で決定する。
                // これにより、サブタイプが複数のインターフェイスを実装するケースで曖昧さを解消できる。
                let tryPickBestMethod (methods: MethodInfo list) : MethodInfo option =
                    let scoreMethod (methodInfo: MethodInfo) =
                        let suppliedTypes = suppliedParameterTypes methodInfo
                        List.zip allArgs suppliedTypes
                        |> List.sumBy (fun (actualArg, expectedType) ->
                            if typeEnv.resolveType actualArg.typ = typeEnv.resolveType expectedType then 2 else 1)

                    let scored = methods |> List.map (fun methodInfo -> methodInfo, scoreMethod methodInfo)
                    match scored with
                    | [] -> None
                    | _ ->
                        let bestScore = scored |> List.maxBy snd |> snd
                        let bestMethods = scored |> List.filter (fun (_, score) -> score = bestScore) |> List.map fst
                        match bestMethods with
                        | [methodInfo] -> Some methodInfo
                        | [] -> None
                        | _ ->
                            // 同スコアが複数ある場合、より特化したオーバーロードを選択する。
                            // m1 が m2 より特化している: 各引数型で m1 のパラメータ型が m2 のパラメータ型の
                            // サブタイプであり、かつ少なくとも一つは真に派生型である。
                            let isMoreSpecific (m1: MethodInfo) (m2: MethodInfo) =
                                let params1 = m1.GetParameters()
                                let params2 = m2.GetParameters()
                                if params1.Length <> params2.Length then false
                                else
                                    let zipped = Array.zip params1 params2
                                    (zipped |> Array.forall (fun (p1, p2) ->
                                        p2.ParameterType.IsAssignableFrom(p1.ParameterType)))
                                    && (zipped |> Array.exists (fun (p1, p2) ->
                                        p2.ParameterType.IsAssignableFrom(p1.ParameterType)
                                        && not (p1.ParameterType.IsAssignableFrom(p2.ParameterType))))
                            // 他のいずれの候補にも支配されないオーバーロードを残す。
                            let nonDominated =
                                bestMethods
                                |> List.filter (fun m ->
                                    not (bestMethods |> List.exists (fun other ->
                                        not (obj.ReferenceEquals(other, m)) && isMoreSpecific other m)))
                            match nonDominated with
                            | [methodInfo] -> Some methodInfo
                            | _ -> None

                let resolvedCall =
                    match resolvedCallable with
                    | Hir.Callable.NativeMethodGroup methods ->
                        let typeCompatible =
                            methods
                            |> List.filter (fun methodInfo ->
                                let suppliedCount = suppliedParameterCount methodInfo
                                let parameterCount = methodInfo.GetParameters().Length
                                (parameterCount = suppliedCount || (parameterCount > suppliedCount && canApplyWithOptionalDefaults methodInfo))
                                && methodMatchesTypes methodInfo)

                        match tryPickBestMethod typeCompatible with
                        | Some methodInfo -> Some (Hir.Callable.NativeMethod methodInfo, TypeId.fromSystemType methodInfo.ReturnType)
                        | None -> None
                    | Hir.Callable.NativeConstructorGroup ctors ->
                        let exactArity =
                            ctors
                            |> List.filter (fun ctorInfo -> ctorInfo.GetParameters().Length = allArgs.Length)
                            |> List.filter (fun ctorInfo -> not ctorInfo.DeclaringType.IsAbstract)
                        let optionalArity =
                            ctors
                            |> List.filter (fun ctorInfo -> ctorInfo.GetParameters().Length > allArgs.Length)
                            |> List.filter (fun ctorInfo ->
                                not ctorInfo.DeclaringType.IsAbstract
                                && ctorInfo.GetParameters()
                                   |> Array.skip allArgs.Length
                                   |> Array.forall (fun p -> p.IsOptional && (NativeInterop.tryDefaultArgExpr p applyExpr.span |> Option.isSome)))
                        match exactArity, optionalArity with
                        // callRetType メタを宣言型に事前束縛することで、let 束縛後の変数が
                        // runtime 型を持てるようにする。その上で callRetType（Meta）を返すことで、
                        // 宣言戻り型が Name（インポート型名）の場合でも単一化が成功する。
                        | [ctorInfo], _ ->
                            typeEnv.unifyTypes callRetType (TypeId.fromSystemType ctorInfo.DeclaringType) |> ignore
                            Some (Hir.Callable.NativeConstructor ctorInfo, callRetType)
                        | [], [ctorInfo] ->
                            typeEnv.unifyTypes callRetType (TypeId.fromSystemType ctorInfo.DeclaringType) |> ignore
                            Some (Hir.Callable.NativeConstructor ctorInfo, callRetType)
                        | _ -> None
                    | Hir.Callable.NativeMethod methodInfo ->
                        if methodInfo.GetParameters().Length = suppliedParameterCount methodInfo || canApplyWithOptionalDefaults methodInfo then
                            Some (resolvedCallable, TypeId.fromSystemType methodInfo.ReturnType)
                        else
                            None
                    | Hir.Callable.Fn sid ->
                        match nameEnv.resolveSym sid with
                        | Some symInfo ->
                            match typeEnv.resolveType symInfo.typ with
                            | TypeId.Fn(expectedArgs, expectedRet) when expectedArgs.Length = allArgs.Length ->
                                let argsMatch =
                                    List.zip allArgs expectedArgs
                                    |> List.forall (fun (actualArg, expectedArg) -> typeEnv.canUnify actualArg.typ expectedArg)
                                if argsMatch then
                                    Some (resolvedCallable, expectedRet)
                                else
                                    None
                            | _ -> None
                        | None -> None
                    | Hir.Callable.NativeConstructor ctorInfo ->
                        if ctorInfo.DeclaringType.IsAbstract then
                            None
                        else
                            let ctorParams = ctorInfo.GetParameters()
                            // callRetType メタを宣言型に事前束縛して runtime 型解決を可能にする。
                            if ctorParams.Length = allArgs.Length then
                                typeEnv.unifyTypes callRetType (TypeId.fromSystemType ctorInfo.DeclaringType) |> ignore
                                Some (resolvedCallable, callRetType)
                            elif ctorParams.Length > allArgs.Length && ctorParams |> Array.skip allArgs.Length |> Array.forall (fun p -> p.IsOptional && (NativeInterop.tryDefaultArgExpr p applyExpr.span |> Option.isSome)) then
                                typeEnv.unifyTypes callRetType (TypeId.fromSystemType ctorInfo.DeclaringType) |> ignore
                                Some (resolvedCallable, callRetType)
                            else
                                None
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
                                |> List.map (fun p -> NativeInterop.tryDefaultArgExpr p applyExpr.span)
                            allArgs @ (missingDefaults |> List.choose id)
                        | Hir.Callable.NativeConstructor ctorInfo when ctorInfo.GetParameters().Length > allArgs.Length ->
                            let missingDefaults =
                                ctorInfo.GetParameters()
                                |> Array.skip allArgs.Length
                                |> Array.toList
                                |> List.map (fun p -> NativeInterop.tryDefaultArgExpr p applyExpr.span)
                            allArgs @ (missingDefaults |> List.choose id)
                        | _ -> allArgs

                    // .NET メソッドへ関数値を渡す際に、引数の型を具体的なデリゲート型へ特殊化する。
                    // これにより Layout フェーズで正しいデリゲート型（Action<>/Func<> 等）が使用される。
                    let specializedCallArgs =
                        match callableExpr with
                        | Hir.Callable.NativeMethod mi ->
                            let mParams = mi.GetParameters()
                            callArgs |> List.mapi (fun i arg ->
                                let paramIdx = i
                                if paramIdx >= 0 && paramIdx < mParams.Length then
                                    let nativeParamType = TypeId.fromSystemType mParams.[paramIdx].ParameterType
                                    match arg, nativeParamType with
                                    // 関数型の引数を .NET デリゲート型で注釈し直す。
                                    | Hir.Expr.Id(sid, TypeId.Fn _, span), TypeId.Native t when TypeId.isDelegateType t ->
                                        Hir.Expr.Id(sid, nativeParamType, span)
                                    | _ -> arg
                                else arg)
                        | _ -> callArgs

                    match unifyOrError nameEnv typeEnv tid callRetType applyExpr.span with
                    | Result.Ok _ -> Hir.Expr.Call(callableExpr, callInstance, specializedCallArgs, callRetType, applyExpr.span)
                    | Result.Error exprErr -> exprErr
                | None ->
                    Hir.Expr.ExprError(noOverloadMessage resolvedCallable allArgs.Length, tid, applyExpr.span)
            | None ->
                // If the target expression is already an error, suppress additional diagnostics and propagate the error.
                match analyzedFunc with
                | Hir.Expr.ExprError _ -> analyzedFunc
                | _ ->
                    let resolvedType = typeEnv.resolveType analyzedFunc.typ
                    let message =
                        match resolvedType with
                        | TypeId.Meta _ -> "Expression could not be resolved to a callable"
                        | _ -> sprintf "Value of type '%s' is not callable" (formatTypeForDisplay nameEnv typeEnv resolvedType)
                    Hir.Expr.ExprError(message, tid, applyExpr.span)
        | :? Ast.Expr.IndexAccess as indexAccessExpr ->
            let receiver = analyzeExpr nameEnv typeEnv indexAccessExpr.receiver (typeEnv.freshMeta ())
            let indexExpr = analyzeExpr nameEnv typeEnv indexAccessExpr.index TypeId.Int
            let resolvedReceiverType = typeEnv.resolveType receiver.typ

            let resolvedIndexResult =
                match NativeInterop.resolveRuntimeSystemType nameEnv typeEnv resolvedReceiverType with
                | Some systemType ->
                    match NativeInterop.tryResolveIndexerMethod systemType with
                    | Some methodInfo ->
                        let returnType = TypeId.fromSystemType methodInfo.ReturnType
                        match unifyOrError nameEnv typeEnv tid returnType indexAccessExpr.span with
                        | Result.Ok _ ->
                            Result.Ok(Hir.Expr.Call(Hir.Callable.NativeMethod methodInfo, None, [ receiver; indexExpr ], returnType, indexAccessExpr.span))
                        | Result.Error errExpr -> Result.Error errExpr
                    | None ->
                        Result.Error(Hir.Expr.ExprError(sprintf "Type '%s' does not support single-index access" systemType.FullName, tid, indexAccessExpr.span))
                | None ->
                    Result.Error(Hir.Expr.ExprError(sprintf "Type '%s' does not support indexing" (formatTypeForDisplay nameEnv typeEnv resolvedReceiverType), tid, indexAccessExpr.span))

            match resolvedIndexResult with
            | Result.Ok resolvedExpr -> resolvedExpr
            | Result.Error errExpr -> errExpr
        | :? Ast.Expr.MemberAccess as memberAccessExpr ->
            let resolveMemberFromSystemTypeResult (sysType: System.Type) : Result<Hir.Expr, string> =
                let memberInfos =
                    sysType.GetMembers(BindingFlags.Public ||| BindingFlags.Static)
                    |> Seq.filter (fun m -> m.Name = memberAccessExpr.memberName)
                    |> Seq.toList

                match NativeInterop.resolveNativeMember typeEnv memberInfos tid with
                | [memberInfo, resolvedTid] ->
                    match memberInfo with
                    | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, None, resolvedTid, memberAccessExpr.span))
                    | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, None, resolvedTid, memberAccessExpr.span))
                    | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, None, resolvedTid, memberAccessExpr.span))
                    | _ -> Result.Error (sprintf "Unsupported member type for '%s'" memberAccessExpr.memberName)
                | [] -> Result.Error (sprintf "Undefined member '%s' for type '%s'" memberAccessExpr.memberName sysType.FullName)
                | members ->
                    let methodInfos =
                        members
                        |> List.choose (fun (memberInfo, _) ->
                            match memberInfo with
                            | :? MethodInfo as methodInfo -> Some methodInfo
                            | _ -> None)

                    if methodInfos.Length = members.Length then
                        Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethodGroup methodInfos, None, tid, memberAccessExpr.span))
                    else
                        Result.Error (sprintf "Ambiguous member '%s' for type '%s'" memberAccessExpr.memberName sysType.FullName)

            let resolveFromSymInfo (typeName: obj) (symInfo: SymbolInfo) : Result<Hir.Expr, string> =
                match symInfo.kind with
                | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) ->
                    resolveMemberFromSystemTypeResult sysType
                | SymbolKind.External(ExternalBinding.SystemTypeRef _) ->
                    Result.Error (unresolvedImportedSystemTypeMessage (string typeName) memberAccessExpr.span)
                | _ ->
                    Result.Error (sprintf "Type '%s' does not support member access" (formatTypeForDisplay nameEnv typeEnv symInfo.typ))

            // 型名レシーバー（`Type'member`）で data 型 static メソッドを優先探索する。
            let resolveMemberFromTypeName (receiverTypeSid: SymbolId) (receiverTypeName: string) : Result<Hir.Expr, string> =
                match tryResolveDataStaticMember nameEnv receiverTypeSid memberAccessExpr.memberName memberAccessExpr.span with
                | Result.Ok resolvedMember -> Result.Ok resolvedMember
                | Result.Error _ ->
                    match nameEnv.resolveSym receiverTypeSid with
                    | Some symInfo ->
                        match symInfo.kind with
                        | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) -> resolveMemberFromSystemTypeResult sysType
                        | SymbolKind.External(ExternalBinding.SystemTypeRef _) -> Result.Error (unresolvedImportedSystemTypeMessage receiverTypeName memberAccessExpr.span)
                        | _ -> Result.Error (sprintf "Type '%s' has no static member '%s'" receiverTypeName memberAccessExpr.memberName)
                    | None -> Result.Error (sprintf "Undefined type symbol '%s'" receiverTypeName)

            let resolvedMemberResult =
                match memberAccessExpr.receiver with
                | :? Ast.Expr.Id as receiverId ->
                    match nameEnv.tryResolveImportedModuleMember receiverId.name memberAccessExpr.memberName with
                    | Some moduleMember ->
                        Result.Ok(Hir.Expr.Id(moduleMember.symbolId, moduleMember.typ, memberAccessExpr.span))
                    | None ->
                        match nameEnv.scope.ResolveType(receiverId.name) with
                        | Some (TypeId.Name sid) ->
                            resolveMemberFromTypeName sid receiverId.name
                        | _ ->
                        let receiver = analyzeExpr nameEnv typeEnv memberAccessExpr.receiver (typeEnv.freshMeta())
                        let receiverType = typeEnv.resolveType receiver.typ
                        match NativeInterop.resolveRuntimeSystemType nameEnv typeEnv receiverType with
                        | Some systemType ->
                            let memberInfos =
                                NativeInterop.getPublicInstanceMembersIncludingInterfaces systemType
                                |> Seq.filter (fun m -> m.Name = memberAccessExpr.memberName)
                                |> Seq.toList
                            match NativeInterop.resolveNativeMember typeEnv memberInfos tid with
                            | [memberInfo, resolvedTid] ->
                                match memberInfo with
                                | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                                | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                                | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                                | _ -> Result.Error (sprintf "Unsupported member type for '%s'" memberAccessExpr.memberName)
                            | [] ->
                                let extensionMemberInfos = NativeInterop.findExtensionMethodCandidates systemType memberAccessExpr.memberName
                                match NativeInterop.resolveExtensionMember typeEnv extensionMemberInfos tid with
                                | [methodInfo, resolvedTid] ->
                                    Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                                | [] when systemType.IsValueType && memberAccessExpr.memberName = "ToString" ->
                                    let convertMethod = typeof<System.Convert>.GetMethod("ToString", [| systemType |])
                                    if obj.ReferenceEquals(convertMethod, null) then
                                        Result.Error (sprintf "Undefined member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                                    else
                                        Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod convertMethod, Some receiver, TypeId.Fn([TypeId.fromSystemType systemType], TypeId.String), memberAccessExpr.span))
                                | [] ->
                                    Result.Error (sprintf "Undefined member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                                | _ -> Result.Error (sprintf "Ambiguous extension member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                            | members ->
                                let methodInfos =
                                    members
                                    |> List.choose (fun (memberInfo, _) ->
                                        match memberInfo with
                                        | :? MethodInfo as methodInfo -> Some methodInfo
                                        | _ -> None)
                                if methodInfos.Length = members.Length then
                                    Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethodGroup methodInfos, Some receiver, tid, memberAccessExpr.span))
                                else
                                    Result.Error (sprintf "Ambiguous member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                        | None ->
                            match typeEnv.resolveType receiver.typ with
                            | TypeId.Name receiverTypeSid ->
                                match tryResolveDataInstanceMember nameEnv typeEnv tid receiverTypeSid memberAccessExpr.memberName memberAccessExpr.span receiver with
                                | Result.Ok resolvedMember -> Result.Ok resolvedMember
                                | Result.Error _ ->
                                    match resolveTyp nameEnv typeEnv receiver.typ with
                                    | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                                    | None -> Result.Error (sprintf "Undefined type '%s'" (formatTypeForDisplay nameEnv typeEnv receiver.typ))
                            | _ ->
                                match resolveTyp nameEnv typeEnv receiver.typ with
                                | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                                | None -> Result.Error (sprintf "Undefined type '%s'" (formatTypeForDisplay nameEnv typeEnv receiver.typ))
                | _ ->
                    let receiver = analyzeExpr nameEnv typeEnv memberAccessExpr.receiver (typeEnv.freshMeta())
                    let receiverType = typeEnv.resolveType receiver.typ
                    match TypeId.tryToRuntimeSystemType receiverType with
                    | Some systemType ->
                        let memberInfos =
                            NativeInterop.getPublicInstanceMembersIncludingInterfaces systemType
                            |> Seq.filter (fun m -> m.Name = memberAccessExpr.memberName)
                            |> Seq.toList
                        match NativeInterop.resolveNativeMember typeEnv memberInfos tid with
                        | [memberInfo, resolvedTid] ->
                            match memberInfo with
                            | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                            | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                            | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                            | _ -> Result.Error (sprintf "Unsupported member type for '%s'" memberAccessExpr.memberName)
                        | [] ->
                            let extensionMemberInfos = NativeInterop.findExtensionMethodCandidates systemType memberAccessExpr.memberName
                            match NativeInterop.resolveExtensionMember typeEnv extensionMemberInfos tid with
                            | [methodInfo, resolvedTid] ->
                                Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, Some receiver, resolvedTid, memberAccessExpr.span))
                            | [] when systemType.IsValueType && memberAccessExpr.memberName = "ToString" ->
                                let convertMethod = typeof<System.Convert>.GetMethod("ToString", [| systemType |])
                                if obj.ReferenceEquals(convertMethod, null) then
                                    Result.Error (sprintf "Undefined member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                                else
                                    Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod convertMethod, Some receiver, TypeId.Fn([TypeId.fromSystemType systemType], TypeId.String), memberAccessExpr.span))
                            | [] ->
                                Result.Error (sprintf "Undefined member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                            | _ -> Result.Error (sprintf "Ambiguous extension member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                        | members ->
                            let methodInfos =
                                members
                                |> List.choose (fun (memberInfo, _) ->
                                    match memberInfo with
                                    | :? MethodInfo as methodInfo -> Some methodInfo
                                    | _ -> None)
                            if methodInfos.Length = members.Length then
                                Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethodGroup methodInfos, Some receiver, tid, memberAccessExpr.span))
                            else
                                Result.Error (sprintf "Ambiguous member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                    | None ->
                        match typeEnv.resolveType receiver.typ with
                        | TypeId.Name receiverTypeSid ->
                            match tryResolveDataInstanceMember nameEnv typeEnv tid receiverTypeSid memberAccessExpr.memberName memberAccessExpr.span receiver with
                            | Result.Ok resolvedMember -> Result.Ok resolvedMember
                            | Result.Error _ ->
                                match resolveTyp nameEnv typeEnv receiver.typ with
                                | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                                | None -> Result.Error (sprintf "Undefined type '%s'" (formatTypeForDisplay nameEnv typeEnv receiver.typ))
                        | _ ->
                            match resolveTyp nameEnv typeEnv receiver.typ with
                            | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                            | None -> Result.Error (sprintf "Undefined type '%s'" (formatTypeForDisplay nameEnv typeEnv receiver.typ))

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
                                match NativeInterop.resolveNativeMember typeEnv memberInfos tid with
                                | [memberInfo, memberType] ->
                                    match memberInfo with
                                    | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, None, memberType, staticAccessExpr.span))
                                    | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, None, memberType, staticAccessExpr.span))
                                    | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, None, memberType, staticAccessExpr.span))
                                    | _ -> Result.Error (sprintf "Unsupported member type for '%s.%s'" staticAccessExpr.typeName staticAccessExpr.memberName)
                                | [] -> Result.Error (sprintf "Undefined member '%s' for type '%s'" staticAccessExpr.memberName staticAccessExpr.typeName)
                                | members ->
                                    let methodInfos =
                                        members
                                        |> List.choose (fun (memberInfo, _) ->
                                            match memberInfo with
                                            | :? MethodInfo as methodInfo -> Some methodInfo
                                            | _ -> None)
                                    if methodInfos.Length = members.Length then
                                        Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethodGroup methodInfos, None, tid, staticAccessExpr.span))
                                    else
                                        Result.Error (sprintf "Ambiguous member '%s' for type '%s'" staticAccessExpr.memberName staticAccessExpr.typeName)
                        | _ -> Result.Error (sprintf "Type '%s' is not a system type" staticAccessExpr.typeName)
                    | None -> Result.Error (sprintf "Undefined type symbol '%s'" staticAccessExpr.typeName)
                | Some _ -> Result.Error (sprintf "Unsupported type id for '%s'" staticAccessExpr.typeName)
                | None -> Result.Error (sprintf "Undefined type '%s'" staticAccessExpr.typeName)

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
        | :? Ast.Expr.Lambda as lambdaExpr ->
            // ラムダ本体専用のスコープを作成し、引数束縛を外側と分離する。
            let lambdaNameEnv = nameEnv.sub()

            // 期待型が関数型で引数個数が一致する場合は、その型情報を優先する。
            // 一致しない場合は fresh meta を割り当て、後段の単一化で整合性を確定する。
            let expectedArgTypes, expectedRetType =
                match typeEnv.resolveType tid with
                | TypeId.Fn(argTypes, retType) when argTypes.Length = lambdaExpr.args.Length ->
                    argTypes, retType
                | _ ->
                    let argTypes = lambdaExpr.args |> List.map (fun _ -> typeEnv.freshMeta())
                    let retType = typeEnv.freshMeta()
                    argTypes, retType

            // 引数名重複は明示的に診断として返す（AST 直構築ケースも考慮）。
            let duplicateArgName =
                lambdaExpr.args
                |> List.fold (fun (seen: Set<string>, dup: string option) name ->
                    match dup with
                    | Some _ -> seen, dup
                    | None ->
                        if seen.Contains name then
                            seen, Some name
                        else
                            seen.Add name, None) (Set.empty, None)
                |> snd

            match duplicateArgName with
            | Some dupName ->
                Hir.Expr.ExprError(sprintf "Duplicate lambda parameter '%s'" dupName, tid, lambdaExpr.span)
            | None ->
                // 引数を宣言順に Arg シンボルとして登録し、HIR 引数ノードを構築する。
                let hirArgs =
                    lambdaExpr.args
                    |> List.mapi (fun index argName ->
                        let argType = expectedArgTypes.[index]
                        let argSid = lambdaNameEnv.declareArg argName argType
                        Hir.Arg(argSid, argName, argType, lambdaExpr.span))

                // 本体は期待戻り型で解析し、関数型全体を期待型へ単一化する。
                let analyzedBody = analyzeExpr lambdaNameEnv typeEnv lambdaExpr.body expectedRetType
                let lambdaType = TypeId.Fn(expectedArgTypes, expectedRetType)
                match unifyOrError nameEnv typeEnv tid lambdaType lambdaExpr.span with
                | Result.Ok _ ->
                    let resolvedArgs =
                        hirArgs
                        |> List.map (fun arg -> Hir.Arg(arg.sid, arg.name, typeEnv.resolveType arg.typ, arg.span))
                    let resolvedRetType = typeEnv.resolveType expectedRetType
                    let resolvedLambdaType = typeEnv.resolveType lambdaType
                    Hir.Expr.Lambda(resolvedArgs, resolvedRetType, analyzedBody, resolvedLambdaType, lambdaExpr.span)
                | Result.Error exprErr -> exprErr
        | _ -> errorExpr tid expr.span "Unsupported expression type"

    and private analyzeStmt (nameEnv: NameEnv) (typeEnv: TypeEnv) (stmt: Ast.Stmt) : Hir.Stmt =
        match stmt with
        | :? Ast.Stmt.Error as errorStmt ->
            // Parser 由来の文エラーは ErrorStmt として後段へ渡す。
            Hir.Stmt.ErrorStmt(errorStmt.message, errorStmt.span)
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
            // 代入は左辺の形状ごとに lower する。
            // - 識別子: 既存の Hir.Stmt.Assign を利用
            // - メンバーアクセス: setter 呼び出しへ lower（現在はプロパティ setter のみ許可）
            match assignStmt.target with
            | :? Ast.Expr.Id as idTarget ->
                let tid = typeEnv.freshMeta ()
                let rhs = analyzeExpr nameEnv typeEnv assignStmt.value tid
                match nameEnv.resolveVar idTarget.name with
                | [sid] ->
                    match unifyOrError nameEnv typeEnv (nameEnv.resolveSymType sid) rhs.typ assignStmt.span with
                    | Result.Ok _ -> Hir.Stmt.Assign(sid, rhs, assignStmt.span)
                    | Result.Error exprErr -> Hir.Stmt.ExprStmt(exprErr, assignStmt.span)
                | [] ->
                    let message = sprintf "Undefined variable '%s'" idTarget.name
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                | _ ->
                    let message = sprintf "Ambiguous variable '%s'" idTarget.name
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
            | :? Ast.Expr.MemberAccess as memberTarget ->
                let analyzedTarget = analyzeExpr nameEnv typeEnv memberTarget (typeEnv.freshMeta ())
                match analyzedTarget with
                | Hir.Expr.MemberAccess (Hir.Member.NativeProperty propertyInfo, instanceOpt, _, _) ->
                    match propertyInfo.SetMethod with
                    | null ->
                        let message = sprintf "Property '%s' is read-only" propertyInfo.Name
                        Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                    | setMethod ->
                        let rhs = analyzeExpr nameEnv typeEnv assignStmt.value (TypeId.fromSystemType propertyInfo.PropertyType)
                        let callExpr =
                            Hir.Expr.Call(
                                Hir.Callable.NativeMethod setMethod,
                                instanceOpt,
                                [ rhs ],
                                TypeId.Unit,
                                assignStmt.span)
                        Hir.Stmt.ExprStmt(callExpr, assignStmt.span)
                | Hir.Expr.MemberAccess (Hir.Member.NativeField fieldInfo, _, _, _) ->
                    let message = sprintf "Field assignment is not supported for member '%s'" fieldInfo.Name
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                | Hir.Expr.MemberAccess (Hir.Member.DataField _, _, _, _) ->
                    let message = "Field assignment is not supported for data fields"
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                | Hir.Expr.MemberAccess (Hir.Member.NativeMethod methodInfo, _, _, _) ->
                    let message = sprintf "Method '%s' is not assignable" methodInfo.Name
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                | Hir.Expr.MemberAccess (Hir.Member.NativeMethodGroup methodInfos, _, _, _) ->
                    let methodName =
                        match methodInfos with
                        | head :: _ -> head.Name
                        | [] -> memberTarget.memberName
                    let message = sprintf "Method group '%s' is not assignable" methodName
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                | Hir.Expr.MemberAccess (Hir.Member.DataMethod _, _, _, _) ->
                    let message = "Method is not assignable"
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                | Hir.Expr.ExprError (message, errTyp, span) ->
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, errTyp, span), assignStmt.span)
                | _ ->
                    let message = "Invalid assignment target"
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
            | _ ->
                let message = "Invalid assignment target"
                Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
        | :? Ast.Stmt.ExprStmt as exprStmt ->
            let expr = analyzeExpr nameEnv typeEnv exprStmt.expr TypeId.Unit
            Hir.Stmt.ExprStmt(expr, exprStmt.span)
        | :? Ast.Stmt.For as forStmt ->
            let iterable = analyzeExpr nameEnv typeEnv forStmt.iterable (typeEnv.freshMeta ())
            let resolvedIterableType = typeEnv.resolveType iterable.typ
            match NativeInterop.resolveRuntimeSystemType nameEnv typeEnv resolvedIterableType with
            | None ->
                Hir.Stmt.ErrorStmt(sprintf "Value of type '%s' is not iterable" (formatTypeForDisplay nameEnv typeEnv resolvedIterableType), forStmt.span)
            | Some iterableSystemType ->
                // for 反復変数はボディ専用のサブスコープへ束縛する。
                // これにより、クロージャー変換時に「ループ変数を外側へ漏らさない」前提を維持する。
                let iteratorResolution =
                    match NativeInterop.tryResolveEnumeratorMembers iterableSystemType with
                    | Some members -> Some(iterable, members)
                    | None ->
                        match NativeInterop.tryGetEnumerator iterableSystemType with
                        | Some getEnumeratorMethod ->
                            let iteratorType = getEnumeratorMethod.ReturnType
                            NativeInterop.tryResolveEnumeratorMembers iteratorType
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
                    Hir.Stmt.ErrorStmt(sprintf "Type '%s' does not define MoveNext/Current or GetEnumerator()" iterableSystemType.FullName, forStmt.span)
        | _ -> Hir.Stmt.ErrorStmt("Unsupported statement type", stmt.span)

    let analyzeMethodCore (nameEnv: NameEnv) (typeEnv: TypeEnv) (sid: SymbolId) (fnDecl: Ast.Decl.Fn) : Hir.Method =
        let bodyNameEnv = nameEnv.sub()
        let retType = nameEnv.resolveTypeExpr fnDecl.ret
        let rawArgTypes = fnDecl.args |> List.map bodyNameEnv.resolveArgType
        let argTypes =
            match fnDecl.args, rawArgTypes with
            | [ (:? Ast.FnArg.Unit) ], [ TypeId.Unit ] -> []
            | _ -> rawArgTypes

        // 引数を宣言順に bodyNameEnv へ登録し、SymbolId を収集する。
        let argSids =
            fnDecl.args
            |> List.mapi (fun index arg ->
                match arg with
                | :? Ast.FnArg.Named as namedArg ->
                    let argType = argTypes.[index]
                    let sid = bodyNameEnv.declareArg namedArg.name argType
                    Some (sid, argType)
                | :? Ast.FnArg.Unit -> None
                | _ -> None)
            |> List.choose id

        let tid = TypeId.Fn(argTypes, retType)
        let body = analyzeExpr bodyNameEnv typeEnv fnDecl.body retType

        Hir.Method(sid, argSids, body, tid, fnDecl.span)

    let analyzeMethod (nameEnv: NameEnv) (typeEnv: TypeEnv) (fnDecl: Ast.Decl.Fn) : Hir.Method =
        let argTypes =
            fnDecl.args
            |> List.map nameEnv.resolveArgType
            |> (fun raw ->
                match fnDecl.args, raw with
                | [ (:? Ast.FnArg.Unit) ], [ TypeId.Unit ] -> []
                | _ -> raw)
        let retType = nameEnv.resolveTypeExpr fnDecl.ret
        let sid = nameEnv.declareLocal fnDecl.name (TypeId.Fn(argTypes, retType))
        analyzeMethodCore nameEnv typeEnv sid fnDecl
