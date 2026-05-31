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

    /// `tid` が `System.Threading.Tasks.Task` または `Task T` 形（`App(Name task, [t])`）のとき、
    /// 待機結果型（Task → Unit, Task<T> → T）を `Some` で返す。それ以外は `None`。
    /// `import System'Threading'Tasks'Task` 経由でインポートされた Task は `Name sid` として現れ、
    /// シンボル表の `SystemTypeRef` を経由して `typeof<Task>` 等価性で判定する。
    let private tryUnwrapTaskType (nameEnv: NameEnv) (typeEnv: TypeEnv) (tid: TypeId) : TypeId option =
        let taskType = typeof<System.Threading.Tasks.Task>

        let resolveSidToSystemType (sid: SymbolId) : System.Type option =
            match nameEnv.resolveSym sid with
            | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) }
                when not (obj.ReferenceEquals(sysType, null)) -> Some sysType
            | _ -> None

        let isTaskSystemType (t: System.Type) : bool =
            not (obj.ReferenceEquals(t, null)) && t = taskType

        let resolveHeadToSystemType (head: TypeId) : System.Type option =
            match typeEnv.resolveType head with
            | TypeId.Native t -> Some t
            | TypeId.Name sid -> resolveSidToSystemType sid
            | _ -> None

        match typeEnv.resolveType tid with
        | TypeId.Native t when isTaskSystemType t -> Some TypeId.Unit
        | TypeId.Name sid ->
            match resolveSidToSystemType sid with
            | Some t when isTaskSystemType t -> Some TypeId.Unit
            | _ -> None
        | TypeId.App (head, [ arg ]) ->
            match resolveHeadToSystemType head with
            | Some t when isTaskSystemType t -> Some arg
            | _ -> None
        | _ -> None

    // Format a TypeId into a user-friendly, human-readable type name.
    let rec private formatTypeForDisplay (nameEnv: NameEnv) (typeEnv: TypeEnv) (tid: TypeId) : string =
        match typeEnv.resolveType tid with
        | TypeId.Unit -> "unit"
        | TypeId.Bool -> "bool"
        | TypeId.Int -> "int"
        | TypeId.Float -> "float"
        | TypeId.Double -> "double"
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
        // 型パラメータ（TypeVar）は型パラメータ名をそのまま表示する。
        | TypeId.TypeVar name -> name
        | TypeId.VarargFn(fixedArgs, elemType, ret) ->
            let fixedStr = fixedArgs |> List.map (formatTypeForDisplay nameEnv typeEnv) |> String.concat " -> "
            let prefix = if fixedArgs.IsEmpty then "" else fixedStr + " -> "
            sprintf "(%s%s... -> %s)" prefix
                (formatTypeForDisplay nameEnv typeEnv elemType)
                (formatTypeForDisplay nameEnv typeEnv ret)
        | TypeId.ByRef inner -> sprintf "ref %s" (formatTypeForDisplay nameEnv typeEnv inner)

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
                        match candidateDef.baseType with
                        | Some (TypeId.Name baseSid) -> isSubtypeOfSystemType baseSid expectedSystemType (visited |> Set.add candidateSid.id)
                        // `impl X as DotNetBase`: X の直接 .NET 基底型との互換チェック。
                        | Some (TypeId.Native baseSysType) -> expectedSystemType.IsAssignableFrom(baseSysType)
                        | _ -> false
                    | None -> false

        let subtypeSatisfied =
            match resolvedExpected, resolvedActual with
            | TypeId.Name expectedSid, TypeId.Name actualSid -> nameEnv.isSubtype actualSid expectedSid
            | TypeId.Native expectedSystemType, TypeId.Name actualSid -> isSubtypeOfSystemType actualSid expectedSystemType Set.empty
            | TypeId.Native expectedSystemType, TypeId.Native actualSystemType -> expectedSystemType.IsAssignableFrom(actualSystemType)
            // Atla プリミティブ型（String, Bool, Int, Float など）を .NET ランタイム型へ変換してサブタイプチェックを行う。
            // 例: button'Content = label （Content: System.Object, label: String）→ typeof<string>.IsAssignableFrom(typeof<obj>) = true。
            | TypeId.Native expectedSystemType, _ ->
                match TypeId.tryToRuntimeSystemType resolvedActual with
                | Some actualSystemType -> expectedSystemType.IsAssignableFrom(actualSystemType)
                | None -> false
            | _ -> false

        if isNativeVoid typeEnv actual && not (isUnitContext typeEnv expected) then
            Result.Error(errorExpr expected span (formatUnifyError nameEnv typeEnv (UnifyError.CannotUnify(expected, actual))))
        elif subtypeSatisfied then
            Result.Ok ()
        else
            match typeEnv.unifyTypes expected actual with
            | Result.Ok _ -> Result.Ok ()
            | Result.Error err -> Result.Error(errorExpr expected span (formatUnifyError nameEnv typeEnv err))

    /// 式を返しつつ期待型 tid と式の型を単一化する。
    /// member/static アクセス解決後に呼び出し、let 束縛などで型変数を具体型に確定させる。
    let private unifyAndReturn (nameEnv: NameEnv) (typeEnv: TypeEnv) (tid: TypeId) (span: Atla.Core.Data.Span) (expr: Hir.Expr) : Hir.Expr =
        match unifyOrError nameEnv typeEnv tid expr.typ span with
        | Result.Ok _ -> expr
        | Result.Error errExpr -> errExpr

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
                            match currentDef.baseType with
                            | Some (TypeId.Name baseSid) ->
                                match loop baseSid (visited |> Set.add currentSid.id) currentReceiverExpr with
                                | Result.Ok resolved -> Result.Ok resolved
                                | Result.Error _ -> tryResolveFromDelegatedField currentDef
                            | Some (TypeId.Native sysType) ->
                                // `impl X as DotNetBase` のケース: .NET 継承チェーンのメンバーを直接探索する。
                                // CIL 上は X が DotNetBase のサブクラスなので currentReceiverExpr をそのまま receiver として使用できる。
                                let memberInfos =
                                    NativeInterop.getPublicInstanceMembersIncludingInterfaces sysType
                                    |> Seq.filter (fun memberInfo -> memberInfo.Name = memberName)
                                    |> Seq.toList
                                match NativeInterop.resolveNativeMember typeEnv memberInfos expectedType with
                                | [memberInfo, resolvedMemberType] ->
                                    match memberInfo with
                                    | :? MethodInfo as methodInfo ->
                                        Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeMethod(methodInfo), Some currentReceiverExpr, resolvedMemberType, span))
                                    | :? FieldInfo as fieldInfo ->
                                        Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeField(fieldInfo), Some currentReceiverExpr, resolvedMemberType, span))
                                    | :? PropertyInfo as propertyInfo ->
                                        Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeProperty(propertyInfo), Some currentReceiverExpr, resolvedMemberType, span))
                                    | _ ->
                                        Result.Error(sprintf "Unsupported member type for '%s'" memberName)
                                | [] -> tryResolveFromDelegatedField currentDef
                                | members ->
                                    let methodInfos =
                                        members
                                        |> List.choose (fun (memberInfo, _) ->
                                            match memberInfo with
                                            | :? MethodInfo as methodInfo -> Some methodInfo
                                            | _ -> None)
                                    if methodInfos.Length = members.Length then
                                        Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeMethodGroup methodInfos, Some currentReceiverExpr, expectedType, span))
                                    else
                                        Result.Error(sprintf "Ambiguous member '%s' for type '%s'" memberName sysType.FullName)
                            | _ -> tryResolveFromDelegatedField currentDef

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
                        match currentDef.baseType with
                        | Some (TypeId.Name baseSid) -> loop baseSid (visited |> Set.add currentSid.id)
                        | _ -> Result.Error(sprintf "Undefined static member '%s' for data type" memberName)

        loop receiverTypeSid Set.empty

    /// 型 SID から Atla の data / enum 型メタデータを検索する。
    let private tryFindTypeDefBySid (nameEnv: NameEnv) (typeSid: SymbolId) : DataTypeDef option =
        nameEnv.dataTypeDefs
        |> Map.toSeq
        |> Seq.tryPick (fun (_, def) ->
            if def.typeSid.id = typeSid.id then Some def else None)

    /// struct フィールドへの代入が `val`（不可変）でないことを検証する。
    /// 対応する DataFieldDef が見つからない場合は許容する（後段で別エラーが出る）。
    let private tryFindImmutableFieldName (nameEnv: NameEnv) (typeSid: SymbolId) (fieldSid: SymbolId) : string option =
        tryFindTypeDefBySid nameEnv typeSid
        |> Option.bind (fun def ->
            def.fields
            |> List.tryFind (fun fieldDef -> fieldDef.sid.id = fieldSid.id)
            |> Option.bind (fun fieldDef ->
                if fieldDef.isMutable then None else Some fieldDef.name))

    /// ジェネリック enum ルート型を期待型 tid から具体化する。
    /// tid = App(Name enumRootSid, concreteArgs) の場合はその型を返す。
    /// それ以外は typeParams 数分の新規メタを割り当てた App 型を返す。
    /// 型パラメータが空の場合は裸の Name 型を返す。
    let private instantiateEnumRootType (typeEnv: TypeEnv) (enumRootDef: DataTypeDef) (tid: TypeId) : TypeId =
        match typeEnv.resolveType tid with
        | TypeId.App(TypeId.Name sid, concreteArgs) when sid.id = enumRootDef.typeSid.id ->
            TypeId.App(TypeId.Name enumRootDef.typeSid, concreteArgs)
        | _ when List.isEmpty enumRootDef.typeParams ->
            TypeId.Name enumRootDef.typeSid
        | TypeId.Name sid when sid.id = enumRootDef.typeSid.id ->
            // 型消去後の non-App 型（impl Opt T の self など）に対しては fresh meta の代わりに
            // TypeVar を使い、Meta が CIL 生成フェーズまで伝播しないようにする。
            let typeVarArgs = enumRootDef.typeParams |> List.map TypeId.TypeVar
            TypeId.App(TypeId.Name enumRootDef.typeSid, typeVarArgs)
        | _ ->
            let metaArgs = enumRootDef.typeParams |> List.map (fun _ -> typeEnv.freshMeta())
            TypeId.App(TypeId.Name enumRootDef.typeSid, metaArgs)

    /// 型パラメータ名 → 具体型の置換マップを使い、型中の TypeVar を再帰的に置換する。
    let rec private substituteTypeVars (subst: Map<string, TypeId>) (tid: TypeId) : TypeId =
        match tid with
        | TypeId.TypeVar name -> subst |> Map.tryFind name |> Option.defaultValue tid
        | TypeId.App(head, args) -> TypeId.App(substituteTypeVars subst head, args |> List.map (substituteTypeVars subst))
        | TypeId.Fn(args, ret) -> TypeId.Fn(args |> List.map (substituteTypeVars subst), substituteTypeVars subst ret)
        | _ -> tid

    /// ジェネリック enum の rootType から型パラメータ → 具体型の置換マップを構築する。
    /// rootType = App(Name enumRootSid, concreteArgs) のとき typeParams[i] -> concreteArgs[i] を構築する。
    let private buildTypeVarSubst (typeEnv: TypeEnv) (enumRootDef: DataTypeDef) (rootType: TypeId) : Map<string, TypeId> =
        /// フィールド型に現れる型変数名を出現順で収集する。
        let rec collectTypeVars (tid: TypeId) (acc: string list) : string list =
            match tid with
            | TypeId.TypeVar name when not (List.contains name acc) -> acc @ [ name ]
            | TypeId.App(head, args) ->
                let withHead = collectTypeVars head acc
                args |> List.fold (fun s arg -> collectTypeVars arg s) withHead
            | TypeId.Fn(args, ret) ->
                let withArgs = args |> List.fold (fun s arg -> collectTypeVars arg s) acc
                collectTypeVars ret withArgs
            | _ -> acc

        match typeEnv.resolveType rootType with
        | TypeId.App(_, concreteArgs) when concreteArgs.Length = enumRootDef.typeParams.Length ->
            List.zip enumRootDef.typeParams concreteArgs |> Map.ofList
        | TypeId.App(_, concreteArgs) when enumRootDef.typeParams.IsEmpty ->
            // インポート時に typeParams が欠落している場合でも、enum case フィールドの TypeVar 名から
            // 最小限の置換マップを構築して具体型を伝播させる。
            let inferredParams =
                enumRootDef.enumInfo
                |> Option.map (fun enumDef ->
                    enumDef.cases
                    |> List.collect (fun caseDef -> caseDef.fields |> List.collect (fun fieldDef -> collectTypeVars fieldDef.typ [])))
                |> Option.defaultValue []
                |> List.distinct
            if inferredParams.Length = concreteArgs.Length then
                List.zip inferredParams concreteArgs |> Map.ofList
            else
                Map.empty
        | _ -> Map.empty

    /// 型 ID から enum のルート型 SID を取得する。Name と App(Name, args) の両方を受け入れる。
    let private tryGetRootTypeSid (typeEnv: TypeEnv) (tid: TypeId) : SymbolId option =
        match typeEnv.resolveType tid with
        | TypeId.Name sid -> Some sid
        | TypeId.App(TypeId.Name sid, _) -> Some sid
        | _ -> None

    /// ResolveType の結果からルート型 SID を抽出する（Name / App(Name, _) を許容）。
    let private tryGetTypeSidFromResolvedType (resolvedType: TypeId) : SymbolId option =
        match resolvedType with
        | TypeId.Name sid -> Some sid
        | TypeId.App(TypeId.Name sid, _) -> Some sid
        | _ -> None

    /// enum case コンストラクターを DataConstructor 呼び出しへ lower する。
    /// rootType には呼び出しコンテキストで具体化された enum 型（例: Opt<SpriteBatch>）を渡す。
    let private lowerEnumCaseConstructor
        (enumRootDef: DataTypeDef)
        (enumDef: EnumTypeDef)
        (caseDef: EnumCaseDef)
        (rootType: TypeId)
        (payloadArgs: Hir.Expr list)
        (span: Atla.Core.Data.Span)
        : Hir.Expr =
        let tagExpr = Hir.Expr.Int(caseDef.tag, span)

        match caseDef.payloadTypeSid, caseDef.payloadFieldSid, caseDef.fields with
        | None, None, [] ->
            Hir.Expr.Call(
                Hir.Callable.DataConstructor(enumRootDef.typeSid, [ enumDef.hiddenTagField.sid ]),
                None,
                [ tagExpr ],
                rootType,
                span)
        | Some payloadTypeSid, Some payloadFieldSid, caseFields ->
            let payloadExpr =
                Hir.Expr.Call(
                    Hir.Callable.DataConstructor(payloadTypeSid, caseFields |> List.map (fun fieldDef -> fieldDef.sid)),
                    None,
                    payloadArgs,
                    TypeId.Name payloadTypeSid,
                    span)
            Hir.Expr.Call(
                Hir.Callable.DataConstructor(enumRootDef.typeSid, [ enumDef.hiddenTagField.sid; payloadFieldSid ]),
                None,
                [ tagExpr; payloadExpr ],
                rootType,
                span)
        | _ ->
            Hir.Expr.ExprError(sprintf "Enum case '%s' has inconsistent payload metadata" caseDef.name, rootType, span)

    // Generate an overload error message with available candidates when argument count does not match.
    let private noOverloadMessage (callable: Hir.Callable) (argCount: int) : string =
        let formatMethodOverloads (methods: MethodInfo list) =
            let name = (List.head methods).Name
            let overloads =
                methods
                |> List.map (fun mi ->
                    let ps = mi.GetParameters() |> Array.map (fun p -> p.ParameterType.FullName) |> String.concat ", "
                    sprintf "  %s(%s)" mi.Name ps)
                |> String.concat "\n"
            sprintf "No overload of '%s' accepts %d argument(s). Available overloads:\n%s" name argCount overloads
        match callable with
        | Hir.Callable.NativeMethodGroup methods when not methods.IsEmpty ->
            formatMethodOverloads methods
        | Hir.Callable.NativeBaseMethodGroup methods when not methods.IsEmpty ->
            formatMethodOverloads methods
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

    let private instantiateTypeScheme (typeEnv: TypeEnv) (typ: TypeId) : TypeId =
        let varMap = System.Collections.Generic.Dictionary<string, TypeId>()
        let rec go t =
            match t with
            | TypeId.TypeVar name ->
                match varMap.TryGetValue(name) with
                | true, meta -> meta
                | false, _ ->
                    let meta = typeEnv.freshMeta()
                    varMap[name] <- meta
                    meta
            | TypeId.VarargFn(fixedArgs, e, r) ->
                TypeId.VarargFn(fixedArgs |> List.map go, go e, go r)
            | TypeId.App(head, args) -> TypeId.App(go head, args |> List.map go)
            | TypeId.Fn(args, ret) -> TypeId.Fn(args |> List.map go, go ret)
            | _ -> t
        go typ

    let rec private exprAsCallable (nameEnv: NameEnv) (typeEnv: TypeEnv) (expr: Hir.Expr): Hir.Callable option =
        match expr with
        | Hir.Expr.Id (sid, _, _) ->
            match nameEnv.resolveSym sid with
            | Some symInfo ->
                match symInfo.kind with
                | SymbolKind.External(ExternalBinding.NativeMethodGroup methodInfos) -> Some (Hir.Callable.NativeMethodGroup methodInfos)
                | SymbolKind.External(ExternalBinding.ConstructorGroup ctorInfos) -> Some (Hir.Callable.NativeConstructorGroup ctorInfos)
                | SymbolKind.BuiltinOperator op -> Some (Hir.Callable.BuiltinOperator op)
                | SymbolKind.BuiltinFn Builtins.BuiltinFunctions.Array -> Some (Hir.Callable.BuiltinArray)
                | SymbolKind.BuiltinFn Builtins.BuiltinFunctions.List -> Some (Hir.Callable.BuiltinList)
                | SymbolKind.BuiltinFn Builtins.BuiltinFunctions.ToFloat -> Some (Hir.Callable.BuiltinConvert TypeId.Float)
                | SymbolKind.BuiltinFn Builtins.BuiltinFunctions.ToDouble -> Some (Hir.Callable.BuiltinConvert TypeId.Double)
                | SymbolKind.BuiltinFn Builtins.BuiltinFunctions.ToInt -> Some (Hir.Callable.BuiltinConvert TypeId.Int)
                | SymbolKind.BuiltinFn Builtins.BuiltinFunctions.Range -> Some (Hir.Callable.BuiltinRange)
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
            | Hir.Member.NativeBaseMethod methodInfo -> Some (Hir.Callable.NativeBaseMethod methodInfo)
            | Hir.Member.NativeBaseMethodGroup methodInfos -> Some (Hir.Callable.NativeBaseMethodGroup methodInfos)
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
        | :? Ast.Expr.Double as doubleExpr ->
            match unifyOrError nameEnv typeEnv tid TypeId.Double doubleExpr.span with
            | Result.Ok _ -> Hir.Expr.Double(doubleExpr.value, doubleExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.String as stringExpr ->
            match unifyOrError nameEnv typeEnv tid TypeId.String stringExpr.span with
            | Result.Ok _ -> Hir.Expr.String(stringExpr.value, stringExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.RecordLit as recordLitExpr ->
            // 連想配列リテラル `{ k = v, ... }` は単独では型を決められない。
            // struct コンストラクタ `{...} TypeName.` の引数位置でのみ意味を持ち、
            // その場合は Apply 解析の専用パスで構築される。
            Hir.Expr.ExprError(
                "Associative array literal '{ ... }' must be passed to a struct type constructor (e.g. `{ ... } TypeName.`)",
                tid,
                recordLitExpr.span)
        | :? Ast.Expr.EnumInit as enumInitExpr ->
            match Map.tryFind enumInitExpr.typeName nameEnv.dataTypeDefs with
            | None ->
                Hir.Expr.ExprError(sprintf "Undefined enum type '%s'" enumInitExpr.typeName, tid, enumInitExpr.span)
            | Some enumRootDef ->
                match enumRootDef.enumInfo with
                | None ->
                    match enumRootDef.unionInfo with
                    | Some unionDef ->
                        // union バリアント構築: `Union'Variant { field = value, ... }`。
                        // 提供フィールドはバリアント自身のフィールドと union 共有フィールドの和に対応する。
                        match unionDef.variants |> List.tryFind (fun v -> v.name = enumInitExpr.caseName) with
                        | None ->
                            Hir.Expr.ExprError(sprintf "Unknown union variant '%s' for type '%s'" enumInitExpr.caseName enumInitExpr.typeName, tid, enumInitExpr.span)
                        | Some variantInfo ->
                            let qualifiedName = sprintf "%s'%s" enumInitExpr.typeName variantInfo.name
                            match nameEnv.dataTypeDefs |> Map.tryFind qualifiedName with
                            | None ->
                                Hir.Expr.ExprError(sprintf "Union variant type '%s' is not defined" qualifiedName, tid, enumInitExpr.span)
                            | Some variantDef ->
                                // コンストラクタ引数の順序: バリアント自身のフィールド → union 共有フィールド。
                                // Gen 側のバリアント ctor も同順でフィールドを受け取り、継承フィールドを含めて stfld する。
                                let allFieldDefs = variantDef.fields @ enumRootDef.fields
                                let initFields =
                                    enumInitExpr.fields
                                    |> List.choose (fun field ->
                                        match field with
                                        | :? Ast.DataInitField.Field as namedField -> Some namedField
                                        | _ -> None)
                                let initFieldMap = initFields |> List.map (fun f -> f.name, f.value) |> Map.ofList
                                let allFieldNames = allFieldDefs |> List.map (fun fd -> fd.name) |> Set.ofList
                                // 重複・未知・欠落フィールドを検査する。
                                let duplicateName =
                                    initFields
                                    |> List.fold (fun (seen, dup) f ->
                                        match dup with
                                        | Some _ -> seen, dup
                                        | None when Set.contains f.name seen -> seen, Some f.name
                                        | None -> Set.add f.name seen, None) (Set.empty, None)
                                    |> snd
                                let unknownName = initFields |> List.tryFind (fun f -> not (Set.contains f.name allFieldNames)) |> Option.map (fun f -> f.name)
                                let missingField = allFieldDefs |> List.tryFind (fun fd -> not (Map.containsKey fd.name initFieldMap))
                                match duplicateName, unknownName, missingField with
                                | Some dup, _, _ ->
                                    Hir.Expr.ExprError(sprintf "Duplicate field initializer '%s'" dup, tid, enumInitExpr.span)
                                | _, Some unknown, _ ->
                                    Hir.Expr.ExprError(sprintf "Unknown field '%s' for union variant '%s'" unknown variantInfo.name, tid, enumInitExpr.span)
                                | _, _, Some missing ->
                                    Hir.Expr.ExprError(sprintf "Missing required field '%s' for union variant '%s'" missing.name variantInfo.name, tid, enumInitExpr.span)
                                | None, None, None ->
                                    let variantType = TypeId.Name variantDef.typeSid
                                    // 各フィールド値を期待型で解析する。エラーは伝播。
                                    let typedArgsResult =
                                        allFieldDefs
                                        |> List.fold (fun acc fieldDef ->
                                            match acc with
                                            | Result.Error _ -> acc
                                            | Result.Ok args ->
                                                match Map.tryFind fieldDef.name initFieldMap with
                                                | None -> Result.Error(Hir.Expr.ExprError(sprintf "Missing required field '%s'" fieldDef.name, tid, enumInitExpr.span))
                                                | Some valueExpr ->
                                                    let typedExpr = analyzeExpr nameEnv typeEnv valueExpr fieldDef.typ
                                                    match typedExpr with
                                                    | Hir.Expr.ExprError _ as e -> Result.Error e
                                                    | _ -> Result.Ok(args @ [ typedExpr ])) (Result.Ok [])
                                    match typedArgsResult with
                                    | Result.Error e -> e
                                    | Result.Ok typedArgs ->
                                        // バリアント型 tid と統一（サブタイプ: variant <: union を許容）。
                                        match unifyOrError nameEnv typeEnv tid variantType enumInitExpr.span with
                                        | Result.Error e -> e
                                        | Result.Ok _ ->
                                            let allFieldSids = allFieldDefs |> List.map (fun fd -> fd.sid)
                                            Hir.Expr.Call(
                                                Hir.Callable.DataConstructor(variantDef.typeSid, allFieldSids),
                                                None,
                                                typedArgs,
                                                variantType,
                                                enumInitExpr.span)
                    | None ->
                        Hir.Expr.ExprError(sprintf "Type '%s' is not an enum" enumInitExpr.typeName, tid, enumInitExpr.span)
                | Some enumDef ->
                    match enumDef.cases |> List.tryFind (fun caseDef -> caseDef.name = enumInitExpr.caseName) with
                    | None ->
                        Hir.Expr.ExprError(sprintf "Unknown enum case '%s' for type '%s'" enumInitExpr.caseName enumInitExpr.typeName, tid, enumInitExpr.span)
                    | Some caseDef ->
                        let initFields =
                            enumInitExpr.fields
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
                            Hir.Expr.ExprError(sprintf "Duplicate field initializer '%s'" duplicatedName, tid, enumInitExpr.span)
                        | None ->
                            let caseFieldMap = caseDef.fields |> List.map (fun fieldDef -> fieldDef.name, fieldDef) |> Map.ofList
                            let unknownFieldName =
                                initFields
                                |> List.tryPick (fun field ->
                                    if Map.containsKey field.name caseFieldMap then None else Some field.name)
                            match unknownFieldName with
                            | Some unknownName ->
                                Hir.Expr.ExprError(sprintf "Unknown field '%s' for enum case '%s'" unknownName enumInitExpr.caseName, tid, enumInitExpr.span)
                            | None ->
                                let providedFieldNames = initFields |> List.map (fun field -> field.name) |> Set.ofList
                                let missingField =
                                    caseDef.fields
                                    |> List.tryFind (fun fieldDef -> not (Set.contains fieldDef.name providedFieldNames))
                                match missingField with
                                | Some missing ->
                                    Hir.Expr.ExprError(sprintf "Missing required field '%s' for enum case '%s'" missing.name enumInitExpr.caseName, tid, enumInitExpr.span)
                                | None ->
                                    match caseDef.payloadTypeSid, caseDef.payloadFieldSid, caseDef.fields with
                                    | None, None, [] ->
                                        // ペイロードなし case: 期待型から具体化した rootType を使い型統一する。
                                        let enumType = instantiateEnumRootType typeEnv enumRootDef tid
                                        match unifyOrError nameEnv typeEnv tid enumType enumInitExpr.span with
                                        | Result.Error exprErr -> exprErr
                                        | Result.Ok _ -> lowerEnumCaseConstructor enumRootDef enumDef caseDef enumType [] enumInitExpr.span
                                    | Some _, Some _, caseFields ->
                                        // ペイロードあり case: 期待型から型パラメータ置換マップを構築してフィールド型を解決する。
                                        let enumType = instantiateEnumRootType typeEnv enumRootDef tid
                                        let typeVarSubst = buildTypeVarSubst typeEnv enumRootDef enumType
                                        let initFieldMap =
                                            initFields
                                            |> List.map (fun field -> field.name, field.value)
                                            |> Map.ofList

                                        let typedArgsResult =
                                            caseFields
                                            |> List.fold
                                                (fun acc fieldDef ->
                                                    match acc with
                                                    | Result.Error exprErr -> Result.Error exprErr
                                                    | Result.Ok typedArgs ->
                                                        match Map.tryFind fieldDef.name initFieldMap with
                                                        | None ->
                                                            Result.Error(Hir.Expr.ExprError(sprintf "Missing required field '%s' for enum case '%s'" fieldDef.name enumInitExpr.caseName, tid, enumInitExpr.span))
                                                        | Some valueExpr ->
                                                            // 型パラメータ（TypeVar）を具体型で置換してフィールドの期待型を確定する。
                                                            let expectedFieldType = substituteTypeVars typeVarSubst fieldDef.typ
                                                            let typedExpr = analyzeExpr nameEnv typeEnv valueExpr expectedFieldType
                                                            match typedExpr with
                                                            | Hir.Expr.ExprError _ as errExpr -> Result.Error errExpr
                                                            | _ -> Result.Ok (typedArgs @ [ typedExpr ]))
                                                (Result.Ok [])

                                        match typedArgsResult with
                                        | Result.Error exprErr -> exprErr
                                        | Result.Ok typedArgs ->
                                            match unifyOrError nameEnv typeEnv tid enumType enumInitExpr.span with
                                            | Result.Error exprErr -> exprErr
                                            | Result.Ok _ -> lowerEnumCaseConstructor enumRootDef enumDef caseDef enumType typedArgs enumInitExpr.span
                                    | _ ->
                                        Hir.Expr.ExprError(sprintf "Enum case '%s' has inconsistent payload metadata" enumInitExpr.caseName, tid, enumInitExpr.span)
        | :? Ast.Expr.Id as idExpr ->
            match nameEnv.resolveVar idExpr.name with
            | [sid] ->
                match nameEnv.resolveSym sid with
                | Some symInfo ->
                    match symInfo.kind with
                    | SymbolKind.External(ExternalBinding.NativeMethodGroup _)
                    | SymbolKind.External(ExternalBinding.ConstructorGroup _) ->
                        Hir.Expr.Id(sid, tid, idExpr.span)
                    | SymbolKind.BuiltinFn _ ->
                        let instantiated = instantiateTypeScheme typeEnv symInfo.typ
                        match unifyOrError nameEnv typeEnv tid instantiated idExpr.span with
                        | Result.Ok _ -> Hir.Expr.Id(sid, tid, idExpr.span)
                        | Result.Error exprErr -> exprErr
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
                    match nameEnv.scope.ResolveType(idExpr.name) |> Option.bind tryGetTypeSidFromResolvedType with
                    | Some sid ->
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
                | Hir.Expr.MemberAccess (Hir.Member.NativeMethodGroup methodInfos, instanceOpt, _, _) ->
                    let matchedMethods = methodInfos |> List.choose (NativeInterop.closeGenericMethod genericArgTypes)
                    match matchedMethods with
                    | [closedMethodInfo] -> buildGenericMemberExpr instanceOpt closedMethodInfo
                    | [] -> Hir.Expr.ExprError(sprintf "No generic overload of '%s' matched the given type arguments" (methodInfos |> List.tryHead |> Option.map (fun m -> m.Name) |> Option.defaultValue "?"), tid, genericApplyExpr.span)
                    | _ -> Hir.Expr.ExprError(sprintf "Ambiguous generic overloads matched for '%s'" (methodInfos |> List.tryHead |> Option.map (fun m -> m.Name) |> Option.defaultValue "?"), tid, genericApplyExpr.span)
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
            // struct コンストラクター呼び出し検出:
            //   `{ f = v, ... } TypeName.` を `TypeName { f = v, ... }` 相当として扱う。
            // 関数が型名識別子で、唯一の引数が RecordLit のとき適用される。
            let structCtorOpt =
                match applyExpr.func, applyExpr.args with
                | (:? Ast.Expr.Id as typeId), [ :? Ast.Expr.RecordLit as recordLit ] ->
                    match Map.tryFind typeId.name nameEnv.dataTypeDefs with
                    | Some dataTypeDef when dataTypeDef.enumInfo.IsNone -> Some (typeId.name, dataTypeDef, recordLit)
                    | _ -> None
                | _ -> None

            match structCtorOpt with
            | Some (typeName, dataTypeDef, recordLit) ->
                let span = applyExpr.span
                let fieldMap = dataTypeDef.fields |> List.map (fun fieldDef -> fieldDef.name, fieldDef) |> Map.ofList
                let initFields =
                    recordLit.fields
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
                    Hir.Expr.ExprError(sprintf "Duplicate field initializer '%s'" duplicatedName, tid, span)
                | None ->
                    let unknownFieldName =
                        initFields
                        |> List.tryPick (fun field ->
                            if Map.containsKey field.name fieldMap then None else Some field.name)
                    match unknownFieldName with
                    | Some unknownName ->
                        Hir.Expr.ExprError(sprintf "Unknown field '%s' for struct type '%s'" unknownName typeName, tid, span)
                    | None ->
                        let providedFieldNames = initFields |> List.map (fun field -> field.name) |> Set.ofList
                        let missingField =
                            dataTypeDef.fields
                            |> List.tryFind (fun fieldDef -> not (Set.contains fieldDef.name providedFieldNames))
                        match missingField with
                        | Some missing ->
                            Hir.Expr.ExprError(sprintf "Missing required field '%s' for struct type '%s'" missing.name typeName, tid, span)
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
                                                Result.Error(Hir.Expr.ExprError(sprintf "Missing required field '%s' for struct type '%s'" fieldDef.name typeName, tid, span))
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
                                match unifyOrError nameEnv typeEnv tid dataType span with
                                | Result.Error exprErr -> exprErr
                                | Result.Ok _ ->
                                    Hir.Expr.Call(
                                        Hir.Callable.DataConstructor(dataTypeDef.typeSid, dataTypeDef.fields |> List.map (fun fieldDef -> fieldDef.sid)),
                                        None,
                                        typedArgs,
                                        dataType,
                                        span)
            | None ->

            // 列挙型ケースコンストラクター呼び出し検出:
            //   arg1 ... argN Type'Case. を Type'Case { field1 = arg1, ..., fieldN = argN } として扱う。
            // レシーバーが型名識別子で、メンバー名が enum case であり、
            // 引数数がフィールド数と一致するときに適用される。
            let enumCaseCtorOpt =
                match applyExpr.func with
                | :? Ast.Expr.MemberAccess as ma ->
                    match ma.receiver with
                    | :? Ast.Expr.Id as receiverId when receiverId.name <> "base" ->
                        match nameEnv.scope.ResolveType(receiverId.name) |> Option.bind tryGetTypeSidFromResolvedType with
                        | Some typeSid ->
                            match tryFindTypeDefBySid nameEnv typeSid with
                            | Some typeDef ->
                                typeDef.enumInfo
                                |> Option.bind (fun enumDef ->
                                    enumDef.cases
                                    |> List.tryFind (fun c -> c.name = ma.memberName)
                                    |> Option.filter (fun c -> c.fields.Length = applyExpr.args.Length)
                                    |> Option.map (fun c -> typeDef, enumDef, c))
                            | None -> None
                        | _ -> None
                    | _ -> None
                | _ -> None

            match enumCaseCtorOpt with
            | Some (typeDef, enumDef, caseDef) ->
                // ペイロードあり/なし enum case コンストラクター: 引数列をフィールドへ順に対応させる。
                let rootType = instantiateEnumRootType typeEnv typeDef tid
                let typeVarSubst = buildTypeVarSubst typeEnv typeDef rootType
                let typedArgsResult =
                    List.zip caseDef.fields applyExpr.args
                    |> List.fold
                        (fun acc (fieldDef, argExpr) ->
                            match acc with
                            | Result.Error e -> Result.Error e
                            | Result.Ok typedArgs ->
                                let expectedFieldType = substituteTypeVars typeVarSubst fieldDef.typ
                                let typedExpr = analyzeExpr nameEnv typeEnv argExpr expectedFieldType
                                match typedExpr with
                                | Hir.Expr.ExprError _ -> Result.Error typedExpr
                                | _ -> Result.Ok (typedArgs @ [ typedExpr ]))
                        (Result.Ok [])
                match typedArgsResult with
                | Result.Error errExpr -> errExpr
                | Result.Ok typedArgs ->
                    match unifyOrError nameEnv typeEnv tid rootType applyExpr.span with
                    | Result.Error exprErr -> exprErr
                    | Result.Ok _ -> lowerEnumCaseConstructor typeDef enumDef caseDef rootType typedArgs applyExpr.span
            | None ->

            // 通常の関数呼び出しパス。
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
                // `receiver'method` 形式の impl instance 呼び出しは、
                // 先頭の receiver を暗黙 this として注入して解決する。
                // それ以外（.NET instance 呼び出しなど）は従来どおり
                // callInstance に receiver を保持し、引数列には含めない。
                let effectiveCallInstance, allArgs =
                    match resolvedCallable, callInstance with
                    | Hir.Callable.Fn sid, Some instance ->
                        match nameEnv.resolveSym sid with
                        | Some symInfo ->
                            match typeEnv.resolveType symInfo.typ with
                            | TypeId.Fn(expectedArgs, _) when expectedArgs.Length = analyzedArgs.Length + 1 ->
                                // receiver が expectedThis のサブタイプ（union バリアント <: union ルート等）でも
                                // 暗黙 this 注入を許可する。canUnify は別 SID の Name 同士を非互換とするため、
                                // 名義サブタイプ判定 isSubtype を併用する。
                                let isThisCompatible (expectedThis: TypeId) =
                                    typeEnv.canUnify instance.typ expectedThis
                                    || (match typeEnv.resolveType instance.typ, typeEnv.resolveType expectedThis with
                                        | TypeId.Name actualSid, TypeId.Name expectedSid -> nameEnv.isSubtype actualSid expectedSid
                                        | _ -> false)
                                match expectedArgs with
                                | expectedThis :: _ when isThisCompatible expectedThis ->
                                    None, instance :: analyzedArgs
                                | _ -> callInstance, analyzedArgs
                            | _ -> callInstance, analyzedArgs
                        | None -> callInstance, analyzedArgs
                    | _ -> callInstance, analyzedArgs

                // 拡張メソッドがインスタンスレシーバー付きで呼ばれているか判定する。
                // 拡張メソッドの GetParameters() には this パラメータが含まれるが、
                // ユーザーが明示的に渡す引数ではないため論理引数から除外する必要がある。
                let isExtMethodWithInstance (methodInfo: MethodInfo) =
                    methodInfo.IsStatic
                    && methodInfo.IsDefined(typeof<System.Runtime.CompilerServices.ExtensionAttribute>, false)
                    && callInstance.IsSome

                // 拡張メソッドの this を除いた論理パラメータ配列を返す。
                // 通常のインスタンスメソッドやスタティックメソッドはそのまま返す。
                let logicalParams (methodInfo: MethodInfo) : ParameterInfo array =
                    let parameters = methodInfo.GetParameters()
                    if isExtMethodWithInstance methodInfo then Array.skip 1 parameters
                    else parameters

                let suppliedParameterCount (_methodInfo: MethodInfo) =
                    allArgs.Length

                // 呼び出し時の実引数に対応する論理パラメータ型列を返す。
                // 拡張メソッドの場合は this パラメータを除いた残りのパラメータと照合する。
                let suppliedParameterTypes (methodInfo: MethodInfo) : TypeId list =
                    let parameterTypes =
                        logicalParams methodInfo
                        |> Array.toList
                        |> List.map (fun p -> TypeId.fromSystemType p.ParameterType)

                    let suppliedCount = min parameterTypes.Length allArgs.Length
                    parameterTypes |> List.take suppliedCount

                let canApplyWithOptionalDefaults (methodInfo: MethodInfo) =
                    let parameters = logicalParams methodInfo
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
                                    match def.baseType with
                                    | Some (TypeId.Name bs) -> isNameSubtypeOfNative bs expectedSysType (vis.Add sid.id)
                                    // `impl X as DotNetBase`: X の直接 .NET 基底型との互換チェック。
                                    | Some (TypeId.Native baseSysType) -> expectedSysType.IsAssignableFrom(baseSysType)
                                    | _ -> false
                                | None -> false
                    match resolvedExpected, resolvedActual with
                    | _ when resolvedActual = resolvedExpected -> true
                    | TypeId.Name expectedSid, TypeId.Name actualSid ->
                        expectedSid = actualSid || nameEnv.isSubtype actualSid expectedSid
                    | TypeId.Native expectedSysType, TypeId.Name actualSid ->
                        isNameSubtypeOfNative actualSid expectedSysType Set.empty
                    | TypeId.Native expectedSysType, TypeId.Native actualSysType ->
                        expectedSysType.IsAssignableFrom(actualSysType)
                    // Atla プリミティブ型（String, Bool, Int, Float など）を .NET ランタイム型へ変換してサブタイプチェックを行う。
                    // 例: isSubtypeCompatible TypeId.String (TypeId.Native typeof<System.Object>) → true。
                    | TypeId.Native expectedSysType, _ ->
                        match TypeId.tryToRuntimeSystemType resolvedActual with
                        | Some actualSysType -> expectedSysType.IsAssignableFrom(actualSysType)
                        | None -> typeEnv.canUnify actualType expectedType
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
                            | [] -> None
                            | candidates ->
                                // パラメータ型では区別できない場合、宣言型の派生階層でタイブレークする。
                                // より派生した宣言型を持つメソッドを優先する。
                                // 例: AvaloniaList<T>.Add(T) vs ICollection<T>.Add(T) → 前者を選択。
                                let isMostDerived (m: MethodInfo) =
                                    let mDeclType = m.DeclaringType
                                    not (obj.ReferenceEquals(mDeclType, null))
                                    && candidates |> List.forall (fun other ->
                                        obj.ReferenceEquals(other, m)
                                        || obj.ReferenceEquals(other.DeclaringType, null)
                                        || other.DeclaringType.IsAssignableFrom(mDeclType))
                                let mostDerived = candidates |> List.filter isMostDerived
                                match mostDerived with
                                | [methodInfo] -> Some methodInfo
                                | _ -> None

                let resolvedCall =
                    match resolvedCallable with
                    | Hir.Callable.NativeMethodGroup methods ->
                        let typeCompatible =
                            methods
                            |> List.filter (fun methodInfo ->
                                let suppliedCount = suppliedParameterCount methodInfo
                                // 拡張メソッドの this パラメータを除いた論理パラメータ数で照合する。
                                let parameterCount = (logicalParams methodInfo).Length
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
                        // 拡張メソッドの this を除いた論理パラメータ数でアリティ照合する。
                        if (logicalParams methodInfo).Length = suppliedParameterCount methodInfo || canApplyWithOptionalDefaults methodInfo then
                            Some (resolvedCallable, TypeId.fromSystemType methodInfo.ReturnType)
                        else
                            None
                    | Hir.Callable.NativeBaseMethod methodInfo ->
                        // base 呼び出し: 親クラスのインスタンスメソッドなので拡張メソッドは想定しない。
                        if methodInfo.GetParameters().Length = allArgs.Length || canApplyWithOptionalDefaults methodInfo then
                            Some (resolvedCallable, TypeId.fromSystemType methodInfo.ReturnType)
                        else
                            None
                    | Hir.Callable.NativeBaseMethodGroup methods ->
                        let typeCompatible =
                            methods
                            |> List.filter (fun methodInfo ->
                                let parameterCount = methodInfo.GetParameters().Length
                                (parameterCount = allArgs.Length || (parameterCount > allArgs.Length && canApplyWithOptionalDefaults methodInfo))
                                && methodMatchesTypes methodInfo)
                        match tryPickBestMethod typeCompatible with
                        | Some methodInfo -> Some (Hir.Callable.NativeBaseMethod methodInfo, TypeId.fromSystemType methodInfo.ReturnType)
                        | None -> None
                    | Hir.Callable.Fn sid ->
                        match nameEnv.resolveSym sid with
                        | Some symInfo ->
                            match typeEnv.resolveType symInfo.typ with
                            | TypeId.Fn(expectedArgs, expectedRet) when expectedArgs.Length = allArgs.Length ->
                                let argsMatch =
                                    List.zip allArgs expectedArgs
                                    |> List.forall (fun (actualArg, expectedArg) ->
                                        typeEnv.canUnify actualArg.typ expectedArg
                                        // 名義サブタイプ（union バリアント <: union ルート等）も許容する。
                                        || (match typeEnv.resolveType actualArg.typ, typeEnv.resolveType expectedArg with
                                            | TypeId.Name actualSid, TypeId.Name expectedSid -> nameEnv.isSubtype actualSid expectedSid
                                            | _ -> false))
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
                    | Hir.Callable.BuiltinArray ->
                        Some (Hir.Callable.BuiltinArray, typeEnv.resolveType callRetType)
                    | Hir.Callable.BuiltinList ->
                        Some (Hir.Callable.BuiltinList, typeEnv.resolveType callRetType)
                    | Hir.Callable.BuiltinRange ->
                        Some (Hir.Callable.BuiltinRange, typeEnv.resolveType callRetType)
                    | Hir.Callable.BuiltinConvert targetTid ->
                        // 変換組込関数: 引数1個・戻り型は変換先の数値型。
                        if allArgs.Length = 1 then Some (resolvedCallable, targetTid)
                        else None
                    | _ -> Some (resolvedCallable, typeEnv.resolveType callRetType)

                match resolvedCall with
                | Some (callableExpr, callRetType) ->
                    let callArgs =
                        match callableExpr with
                        | Hir.Callable.NativeMethod methodInfo when (logicalParams methodInfo).Length > suppliedParameterCount methodInfo ->
                            // 拡張メソッドの this を除いた論理パラメータからデフォルト引数を補完する。
                            let parameters = logicalParams methodInfo
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
                    // 拡張メソッドの場合は GetParameters()[0] が this パラメータなので 1 オフセットする。
                    let specializedCallArgs =
                        match callableExpr with
                        | Hir.Callable.NativeMethod mi ->
                            let mParams = mi.GetParameters()
                            let paramOffset = if isExtMethodWithInstance mi then 1 else 0
                            callArgs |> List.mapi (fun i arg ->
                                let paramIdx = i + paramOffset
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
                    | Result.Ok _ -> Hir.Expr.Call(callableExpr, effectiveCallInstance, specializedCallArgs, callRetType, applyExpr.span)
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
                match tryFindTypeDefBySid nameEnv receiverTypeSid with
                | Some typeDef ->
                    match typeDef.enumInfo |> Option.bind (fun enumDef -> enumDef.cases |> List.tryFind (fun caseDef -> caseDef.name = memberAccessExpr.memberName) |> Option.map (fun caseDef -> enumDef, caseDef)) with
                    | Some (enumDef, caseDef) when caseDef.fields.IsEmpty ->
                        let rootType = instantiateEnumRootType typeEnv typeDef tid
                        Result.Ok(lowerEnumCaseConstructor typeDef enumDef caseDef rootType [] memberAccessExpr.span)
                    | Some (_, caseDef) ->
                        Result.Error(sprintf "Enum case '%s' requires a payload initializer" caseDef.name)
                    | None ->
                    // union バリアントの値構築（`Union'Variant` ブレースなし）。
                    // object バリアント（自身フィールドなし）の単集合値、または全フィールドが
                    // object 初期値で供給される場合に 0 引数構築へ lower する。
                    match typeDef.unionInfo |> Option.bind (fun unionDef -> unionDef.variants |> List.tryFind (fun v -> v.name = memberAccessExpr.memberName)) with
                    | Some variantInfo ->
                        let qualifiedName = sprintf "%s'%s" receiverTypeName variantInfo.name
                        match nameEnv.dataTypeDefs |> Map.tryFind qualifiedName with
                        | None -> Result.Error(sprintf "Union variant type '%s' is not defined" qualifiedName)
                        | Some variantDef ->
                            let allFieldDefs = variantDef.fields @ typeDef.fields
                            // object バリアントの初期値マップ（フィールド名 → AST 式）。
                            let initMap = variantInfo.objectFieldInits |> Option.defaultValue [] |> Map.ofList
                            // 全フィールドが初期値で供給されるか検査する。
                            let missing = allFieldDefs |> List.tryFind (fun fd -> not (Map.containsKey fd.name initMap))
                            match missing with
                            | Some fd ->
                                Result.Error(sprintf "Union variant '%s' requires a field initializer for '%s'" variantInfo.name fd.name)
                            | None ->
                                let variantType = TypeId.Name variantDef.typeSid
                                let typedArgsResult =
                                    allFieldDefs
                                    |> List.fold (fun acc fd ->
                                        match acc with
                                        | Result.Error _ -> acc
                                        | Result.Ok args ->
                                            match Map.tryFind fd.name initMap with
                                            | None -> Result.Error(sprintf "Missing field initializer '%s'" fd.name)
                                            | Some valueAst ->
                                                let typed = analyzeExpr nameEnv typeEnv valueAst fd.typ
                                                match typed with
                                                | Hir.Expr.ExprError(msg, _, _) -> Result.Error msg
                                                | _ -> Result.Ok(args @ [ typed ])) (Result.Ok [])
                                match typedArgsResult with
                                | Result.Error msg -> Result.Error msg
                                | Result.Ok typedArgs ->
                                    match unifyOrError nameEnv typeEnv tid variantType memberAccessExpr.span with
                                    | Result.Error e -> (match e with Hir.Expr.ExprError(m, _, _) -> Result.Error m | _ -> Result.Error "type mismatch")
                                    | Result.Ok _ ->
                                        let allFieldSids = allFieldDefs |> List.map (fun fd -> fd.sid)
                                        Result.Ok(Hir.Expr.Call(Hir.Callable.DataConstructor(variantDef.typeSid, allFieldSids), None, typedArgs, variantType, memberAccessExpr.span))
                    | None ->
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
                | None ->
                    match nameEnv.resolveSym receiverTypeSid with
                    | Some symInfo ->
                        match symInfo.kind with
                        | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) -> resolveMemberFromSystemTypeResult sysType
                        | SymbolKind.External(ExternalBinding.SystemTypeRef _) -> Result.Error (unresolvedImportedSystemTypeMessage receiverTypeName memberAccessExpr.span)
                        | _ -> Result.Error (sprintf "Type '%s' has no static member '%s'" receiverTypeName memberAccessExpr.memberName)
                    | None -> Result.Error (sprintf "Undefined type symbol '%s'" receiverTypeName)

            /// Resolve `base'member` using current `this` and the base type fixed by impl metadata.
            /// `base` is valid only inside instance impl methods.
            let resolveMemberFromBaseReceiver () : Result<Hir.Expr, string> =
                let selfSymbolIdOpt = nameEnv.resolveVar "self" |> List.tryHead
                match selfSymbolIdOpt with
                | None ->
                    Result.Error("Keyword 'base' can only be used inside an instance impl method")
                | Some selfSymbolId ->
                    let resolvedSelfType = nameEnv.resolveSymType selfSymbolId |> typeEnv.resolveType
                    let resolvedSelfExpr = Hir.Expr.Id(selfSymbolId, resolvedSelfType, memberAccessExpr.receiver.span)
                    match resolvedSelfType with
                    | TypeId.Name selfTypeSid ->
                        let thisTypeDefOpt =
                            nameEnv.dataTypeDefs
                            |> Map.toSeq
                            |> Seq.tryPick (fun (_, def) ->
                                if def.typeSid.id = selfTypeSid.id then Some def else None)
                        match thisTypeDefOpt with
                        | None ->
                            Result.Error(sprintf "Type '%s' does not support 'base' access" (formatTypeForDisplay nameEnv typeEnv resolvedSelfType))
                        | Some thisTypeDef ->
                            match thisTypeDef.baseType with
                            | None ->
                                Result.Error(sprintf "Type '%s' has no base type for 'base' access" (formatTypeForDisplay nameEnv typeEnv resolvedSelfType))
                            | Some (TypeId.Name baseSid) ->
                                tryResolveDataInstanceMember nameEnv typeEnv tid baseSid memberAccessExpr.memberName memberAccessExpr.span resolvedSelfExpr
                            | Some (TypeId.Native baseSysType) ->
                                let memberInfos =
                                    baseSysType.GetMembers(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
                                    |> Seq.filter (fun m ->
                                        if m.Name <> memberAccessExpr.memberName then
                                            false
                                        else
                                            match m with
                                            | :? MethodInfo as methodInfo ->
                                                methodInfo.IsPublic || methodInfo.IsFamily || methodInfo.IsFamilyOrAssembly || methodInfo.IsFamilyAndAssembly
                                            | :? FieldInfo as fieldInfo ->
                                                fieldInfo.IsPublic || fieldInfo.IsFamily || fieldInfo.IsFamilyOrAssembly || fieldInfo.IsFamilyAndAssembly
                                            | :? PropertyInfo as propertyInfo ->
                                                let accessorOpt =
                                                    if isNull propertyInfo.GetMethod then
                                                        if isNull propertyInfo.SetMethod then None
                                                        else Some propertyInfo.SetMethod
                                                    else Some propertyInfo.GetMethod
                                                match accessorOpt with
                                                | Some accessor ->
                                                    accessor.IsPublic || accessor.IsFamily || accessor.IsFamilyOrAssembly || accessor.IsFamilyAndAssembly
                                                | None -> false
                                            | _ -> false)
                                    |> Seq.toList
                                match NativeInterop.resolveNativeMember typeEnv memberInfos tid with
                                | [memberInfo, resolvedTid] ->
                                    match memberInfo with
                                    | :? MethodInfo as methodInfo -> Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeBaseMethod methodInfo, Some resolvedSelfExpr, resolvedTid, memberAccessExpr.span))
                                    | :? FieldInfo as fieldInfo -> Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, Some resolvedSelfExpr, resolvedTid, memberAccessExpr.span))
                                    | :? PropertyInfo as propertyInfo -> Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeBaseProperty propertyInfo, Some resolvedSelfExpr, resolvedTid, memberAccessExpr.span))
                                    | _ -> Result.Error(sprintf "Unsupported member type for '%s'" memberAccessExpr.memberName)
                                | [] -> Result.Error(sprintf "Undefined member '%s' for type '%s'" memberAccessExpr.memberName baseSysType.FullName)
                                | members ->
                                    let methodInfos =
                                        members
                                        |> List.choose (fun (memberInfo, _) ->
                                            match memberInfo with
                                            | :? MethodInfo as methodInfo -> Some methodInfo
                                            | _ -> None)
                                    if methodInfos.Length = members.Length then
                                        Result.Ok(Hir.Expr.MemberAccess(Hir.Member.NativeBaseMethodGroup methodInfos, Some resolvedSelfExpr, tid, memberAccessExpr.span))
                                    else
                                        Result.Error(sprintf "Ambiguous member '%s' for type '%s'" memberAccessExpr.memberName baseSysType.FullName)
                            | Some unsupportedBaseType ->
                                Result.Error(sprintf "Type '%s' does not support 'base' access" (formatTypeForDisplay nameEnv typeEnv unsupportedBaseType))
                    | _ ->
                        Result.Error(sprintf "Type '%s' does not support 'base' access" (formatTypeForDisplay nameEnv typeEnv resolvedSelfType))

            let resolvedMemberResult =
                match memberAccessExpr.receiver with
                | :? Ast.Expr.Id as receiverId ->
                    if receiverId.name = "base" then
                        resolveMemberFromBaseReceiver ()
                    else
                        match nameEnv.tryResolveImportedModuleMember receiverId.name memberAccessExpr.memberName with
                        | Some moduleMember ->
                            Result.Ok(Hir.Expr.Id(moduleMember.symbolId, moduleMember.typ, memberAccessExpr.span))
                        | None ->
                            match nameEnv.scope.ResolveType(receiverId.name) with
                            | Some resolvedType ->
                                match tryGetTypeSidFromResolvedType resolvedType with
                                | Some sid ->
                                    resolveMemberFromTypeName sid receiverId.name
                                // Float, Int, Bool, String などのビルトイン型を静的メンバーアクセスのレシーバとして使う場合、
                                // 対応する .NET ランタイム型へ変換して静的メンバーを探索する。
                                // 例: `Float'Parse` → `System.Double.Parse`
                                | None ->
                                    match TypeId.tryToRuntimeSystemType resolvedType with
                                    | Some sysType -> resolveMemberFromSystemTypeResult sysType
                                    | None -> Result.Error (sprintf "Builtin type '%s' cannot be mapped to a .NET runtime type for static member access" receiverId.name)
                            | None ->
                                let receiver = analyzeExpr nameEnv typeEnv memberAccessExpr.receiver (typeEnv.freshMeta())
                                let receiverType = typeEnv.resolveType receiver.typ
                                let tryResolveNativeInstance (systemType: System.Type) =
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
                                            if isNull convertMethod then
                                                Result.Error (sprintf "Undefined member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                                            else
                                                Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod convertMethod, Some receiver, TypeId.Fn([TypeId.fromSystemType systemType], TypeId.String), memberAccessExpr.span))
                                        | [] ->
                                            // Fall back to generic method definitions (to be closed via GenericApply)
                                            let genericMethods =
                                                memberInfos
                                                |> List.choose (fun m -> match m with :? MethodInfo as mi when mi.IsGenericMethodDefinition -> Some mi | _ -> None)
                                            if not genericMethods.IsEmpty then
                                                Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethodGroup genericMethods, Some receiver, tid, memberAccessExpr.span))
                                            else
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

                                match tryGetTypeSidFromResolvedType receiverType with
                                | Some receiverTypeSid ->
                                    match tryResolveDataInstanceMember nameEnv typeEnv tid receiverTypeSid memberAccessExpr.memberName memberAccessExpr.span receiver with
                                    | Result.Ok resolvedMember ->
                                        Result.Ok resolvedMember
                                    | Result.Error _ ->
                                        match NativeInterop.resolveRuntimeSystemType nameEnv typeEnv receiverType with
                                        | Some systemType -> tryResolveNativeInstance systemType
                                        | None ->
                                            match resolveTyp nameEnv typeEnv receiver.typ with
                                            | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                                            | None -> Result.Error (sprintf "Undefined type '%s'" (formatTypeForDisplay nameEnv typeEnv receiver.typ))
                                | None ->
                                    match NativeInterop.resolveRuntimeSystemType nameEnv typeEnv receiverType with
                                    | Some systemType -> tryResolveNativeInstance systemType
                                    | None ->
                                        match resolveTyp nameEnv typeEnv receiver.typ with
                                        | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                                        | None -> Result.Error (sprintf "Undefined type '%s'" (formatTypeForDisplay nameEnv typeEnv receiver.typ))
                | _ ->
                    let receiver = analyzeExpr nameEnv typeEnv memberAccessExpr.receiver (typeEnv.freshMeta())
                    let receiverType = typeEnv.resolveType receiver.typ
                    // TypeId.tryToRuntimeSystemType は TypeId.Name を解決できないため、
                    // resolveRuntimeSystemType を使いインポート .NET 型（TypeId.Name sid）もインスタンスメンバー探索へ通す。
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
                                if isNull convertMethod then
                                    Result.Error (sprintf "Undefined member '%s' for type '%s'" memberAccessExpr.memberName systemType.FullName)
                                else
                                    Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod convertMethod, Some receiver, TypeId.Fn([TypeId.fromSystemType systemType], TypeId.String), memberAccessExpr.span))
                            | [] ->
                                // Fall back to generic method definitions (to be closed via GenericApply)
                                let genericMethods =
                                    memberInfos
                                    |> List.choose (fun m -> match m with :? MethodInfo as mi when mi.IsGenericMethodDefinition -> Some mi | _ -> None)
                                if not genericMethods.IsEmpty then
                                    Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethodGroup genericMethods, Some receiver, tid, memberAccessExpr.span))
                                else
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
                        match typeEnv.resolveType receiver.typ |> tryGetTypeSidFromResolvedType with
                        | Some receiverTypeSid ->
                            match tryResolveDataInstanceMember nameEnv typeEnv tid receiverTypeSid memberAccessExpr.memberName memberAccessExpr.span receiver with
                            | Result.Ok resolvedMember -> Result.Ok resolvedMember
                            | Result.Error _ ->
                                match resolveTyp nameEnv typeEnv receiver.typ with
                                | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                                | None -> Result.Error (sprintf "Undefined type '%s'" (formatTypeForDisplay nameEnv typeEnv receiver.typ))
                        | None ->
                            match resolveTyp nameEnv typeEnv receiver.typ with
                            | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                            | None -> Result.Error (sprintf "Undefined type '%s'" (formatTypeForDisplay nameEnv typeEnv receiver.typ))

            // メンバーアクセスの解決結果を HIR 式に変換し、期待型 tid と解決後の型を単一化する。
            // これにより `let x = receiver'member` のような束縛で x の型が具体型に確定する。
            resultToExpr tid memberAccessExpr.span resolvedMemberResult
            |> unifyAndReturn nameEnv typeEnv tid memberAccessExpr.span
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
                // ビルトイン型（Float, Int など）を StaticAccess レシーバとして使う場合、
                // 対応する .NET ランタイム型へ変換して静的メンバーを探索する。
                | Some builtinType ->
                    match TypeId.tryToRuntimeSystemType builtinType with
                    | Some sysType ->
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
                    | None -> Result.Error (sprintf "Builtin type '%s' cannot be mapped to a .NET runtime type for static member access" staticAccessExpr.typeName)
                | None -> Result.Error (sprintf "Undefined type '%s'" staticAccessExpr.typeName)

            // 静的アクセスの解決結果を HIR 式に変換し、期待型 tid と解決後の型を単一化する。
            resultToExpr tid staticAccessExpr.span resolvedStaticResult
            |> unifyAndReturn nameEnv typeEnv tid staticAccessExpr.span
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
        | :? Ast.Expr.Match as matchExpr ->
            let analyzedScrutinee = analyzeExpr nameEnv typeEnv matchExpr.scrutinee (typeEnv.freshMeta ())
            match tryGetRootTypeSid typeEnv analyzedScrutinee.typ with
            | Some scrutineeTypeSid ->
                match tryFindTypeDefBySid nameEnv scrutineeTypeSid with
                | None ->
                    Hir.Expr.ExprError(sprintf "Type '%s' is not an enum" (formatTypeForDisplay nameEnv typeEnv analyzedScrutinee.typ), tid, matchExpr.span)
                | Some scrutineeTypeDef ->
                    match scrutineeTypeDef.enumInfo with
                    | None ->
                        match scrutineeTypeDef.unionInfo with
                        | None ->
                            Hir.Expr.ExprError(sprintf "Type '%s' is not an enum or union" (formatTypeForDisplay nameEnv typeEnv analyzedScrutinee.typ), tid, matchExpr.span)
                        | Some unionDef ->
                            // union の match: 各 arm を isinst 型テストの if 連鎖へ lower する。
                            let unionName =
                                nameEnv.resolveSym scrutineeTypeDef.typeSid
                                |> Option.map (fun symInfo -> symInfo.name)
                                |> Option.defaultValue ""
                            let matchNameEnv = nameEnv.sub()
                            let scrutineeSid = matchNameEnv.declareLocal "__match_scrutinee" analyzedScrutinee.typ
                            let scrutineeRef = Hir.Expr.Id(scrutineeSid, analyzedScrutinee.typ, matchExpr.scrutinee.span)

                            // バリアント名 → (variantDef, variantInfo) の索引を作る。
                            // バリアント DataTypeDef は修飾名 `Union'Variant` で dataTypeDefs に登録されている。
                            let variantByName =
                                unionDef.variants
                                |> List.choose (fun variantInfo ->
                                    let qualifiedName = sprintf "%s'%s" unionName variantInfo.name
                                    nameEnv.dataTypeDefs
                                    |> Map.tryFind qualifiedName
                                    |> Option.map (fun variantDef -> variantInfo.name, (variantDef, variantInfo)))
                                |> Map.ofList

                            let analyzeUnionArm (arm: Ast.MatchArm) =
                                match arm with
                                | :? Ast.MatchArm.Arm as matchArm ->
                                    match matchArm.pattern with
                                    | :? Ast.Pattern.Enum as variantPattern ->
                                        if variantPattern.typeName <> unionName then
                                            Result.Error(Hir.Expr.ExprError(sprintf "Pattern type '%s' does not match union type '%s'" variantPattern.typeName unionName, tid, matchArm.span))
                                        else
                                            match variantByName |> Map.tryFind variantPattern.caseName with
                                            | None ->
                                                Result.Error(Hir.Expr.ExprError(sprintf "Unknown union variant '%s' for type '%s'" variantPattern.caseName unionName, tid, matchArm.span))
                                            | Some (variantDef, variantInfo) ->
                                                // フィールド束縛を構築する。`{ name, .. }` の各 name は
                                                // バリアント自身のフィールド、または union 共有フィールドのいずれか。
                                                let namedFieldNames =
                                                    variantPattern.fields
                                                    |> List.choose (fun field ->
                                                        match field with
                                                        | :? Ast.PatternField.Named as namedField -> Some namedField.name
                                                        | _ -> None)
                                                // フィールド名 → (定義型 SID, fieldDef) を own → union 共有の順に解決する。
                                                let resolveField (fieldName: string) =
                                                    variantDef.fields
                                                    |> List.tryFind (fun fd -> fd.name = fieldName)
                                                    |> Option.map (fun fd -> variantDef.typeSid, fd)
                                                    |> Option.orElseWith (fun () ->
                                                        scrutineeTypeDef.fields
                                                        |> List.tryFind (fun fd -> fd.name = fieldName)
                                                        |> Option.map (fun fd -> scrutineeTypeDef.typeSid, fd))
                                                let unknownField =
                                                    namedFieldNames |> List.tryFind (fun n -> (resolveField n).IsNone)
                                                match unknownField with
                                                | Some unknownName ->
                                                    Result.Error(Hir.Expr.ExprError(sprintf "Unknown pattern field '%s' for union variant '%s'" unknownName variantPattern.caseName, tid, matchArm.span))
                                                | None ->
                                                    let armNameEnv = matchNameEnv.sub()
                                                    let variantType = TypeId.Name variantDef.typeSid
                                                    // scrutinee をバリアント型へダウンキャストした式（own フィールドアクセス用）。
                                                    let castExpr = Hir.Expr.Cast(scrutineeRef, variantType, matchArm.span)
                                                    let bindingStmts =
                                                        namedFieldNames
                                                        |> List.choose (fun fieldName ->
                                                            resolveField fieldName
                                                            |> Option.map (fun (ownerSid, fieldDef) ->
                                                                let sid = armNameEnv.declareLocal fieldName fieldDef.typ
                                                                let valueExpr =
                                                                    Hir.Expr.MemberAccess(Hir.Member.DataField(ownerSid, fieldDef.sid), Some castExpr, fieldDef.typ, matchArm.span)
                                                                Hir.Stmt.Let(sid, false, valueExpr, matchArm.span)))
                                                    let bodyExpr = analyzeExpr armNameEnv typeEnv matchArm.body tid
                                                    let thenExpr =
                                                        if bindingStmts.IsEmpty then bodyExpr
                                                        else Hir.Expr.Block(bindingStmts, bodyExpr, bodyExpr.typ, matchArm.span)
                                                    Result.Ok(variantInfo.name, variantDef, thenExpr)
                                    | _ ->
                                        Result.Error(Hir.Expr.ExprError("Unsupported match pattern", tid, matchArm.span))
                                | _ ->
                                    Result.Error(Hir.Expr.ExprError("Unsupported match arm", tid, arm.span))

                            let unionArmResults = matchExpr.arms |> List.map analyzeUnionArm
                            match unionArmResults |> List.tryPick (function | Result.Error err -> Some err | _ -> None) with
                            | Some errExpr -> errExpr
                            | None ->
                                let analyzedArms = unionArmResults |> List.choose (function | Result.Ok value -> Some value | _ -> None)
                                let duplicateVariant =
                                    analyzedArms
                                    |> List.fold
                                        (fun (seen, dup) (variantName, _, _) ->
                                            match dup with
                                            | Some _ -> seen, dup
                                            | None when Set.contains variantName seen -> seen, Some variantName
                                            | None -> Set.add variantName seen, None)
                                        (Set.empty, None)
                                    |> snd
                                match duplicateVariant with
                                | Some variantName ->
                                    Hir.Expr.ExprError(sprintf "Duplicate match arm for union variant '%s'" variantName, tid, matchExpr.span)
                                | None ->
                                    // 網羅性チェック: sealed union のみ。extendable union は省略する。
                                    let coveredVariants = analyzedArms |> List.map (fun (n, _, _) -> n) |> Set.ofList
                                    let missingVariant =
                                        if unionDef.isExtendable then None
                                        else unionDef.variants |> List.tryFind (fun v -> not (Set.contains v.name coveredVariants))
                                    match missingVariant with
                                    | Some v ->
                                        Hir.Expr.ExprError(sprintf "Non-exhaustive match: missing union variant '%s'" v.name, tid, matchExpr.span)
                                    | None when analyzedArms.IsEmpty ->
                                        Hir.Expr.ExprError("Match expression has no arms", tid, matchExpr.span)
                                    | None ->
                                        // isinst による if 連鎖。最後の arm を末端 else とする。
                                        let lastThen = analyzedArms |> List.last |> (fun (_, _, e) -> e)
                                        let ifChain =
                                            analyzedArms
                                            |> List.rev
                                            |> List.fold
                                                (fun elseExpr (_, variantDef, thenExpr) ->
                                                    let condExpr = Hir.Expr.TypeTest(scrutineeRef, TypeId.Name variantDef.typeSid, matchExpr.span)
                                                    Hir.Expr.If(condExpr, thenExpr, elseExpr, tid, matchExpr.span))
                                                lastThen
                                        Hir.Expr.Block([ Hir.Stmt.Let(scrutineeSid, false, analyzedScrutinee, matchExpr.scrutinee.span) ], ifChain, tid, matchExpr.span)
                    | Some enumDef ->
                        let matchNameEnv = nameEnv.sub()
                        let scrutineeSid = matchNameEnv.declareLocal "__match_scrutinee" analyzedScrutinee.typ
                        let scrutineeRef = Hir.Expr.Id(scrutineeSid, analyzedScrutinee.typ, matchExpr.scrutinee.span)

                        let analyzeArm (arm: Ast.MatchArm) =
                            match arm with
                            | :? Ast.MatchArm.Arm as matchArm ->
                                match matchArm.pattern with
                                | :? Ast.Pattern.Enum as enumPattern ->
                                    // パターン型名とスクルティニー型を SymbolId で比較する。
                                    // atlalib からインポートした型は symInfo.name が "module.Type" の
                                    // 形式（修飾名）になるため、名前文字列での比較では一致しない。
                                    let patternTypeSidOpt =
                                        nameEnv.dataTypeDefs
                                        |> Map.tryFind enumPattern.typeName
                                        |> Option.map (fun def -> def.typeSid)
                                    let typesMismatch =
                                        match patternTypeSidOpt with
                                        | Some patternTypeSid -> patternTypeSid.id <> scrutineeTypeDef.typeSid.id
                                        | None ->
                                            let scrutineeTypeName =
                                                nameEnv.resolveSym scrutineeTypeDef.typeSid
                                                |> Option.map (fun symInfo -> symInfo.name)
                                                |> Option.defaultValue enumPattern.typeName
                                            enumPattern.typeName <> scrutineeTypeName
                                    if typesMismatch then
                                        let scrutineeTypeName =
                                            nameEnv.resolveSym scrutineeTypeDef.typeSid
                                            |> Option.map (fun symInfo -> symInfo.name)
                                            |> Option.defaultValue enumPattern.typeName
                                        Result.Error(Hir.Expr.ExprError(sprintf "Pattern type '%s' does not match enum type '%s'" enumPattern.typeName scrutineeTypeName, tid, matchArm.span))
                                    else
                                        match enumDef.cases |> List.tryFind (fun caseDef -> caseDef.name = enumPattern.caseName) with
                                        | None ->
                                            Result.Error(Hir.Expr.ExprError(sprintf "Unknown enum case '%s' for type '%s'" enumPattern.caseName enumPattern.typeName, tid, matchArm.span))
                                        | Some caseDef ->
                                            let positionalFields =
                                                enumPattern.fields
                                                |> List.choose (fun field ->
                                                    match field with
                                                    | :? Ast.PatternField.Positional as pf -> Some pf
                                                    | _ -> None)

                                            let duplicateFieldName =
                                                enumPattern.fields
                                                |> List.choose (fun field ->
                                                    match field with
                                                    | :? Ast.PatternField.Named as namedField -> Some namedField.name
                                                    | _ -> None)
                                                |> List.fold
                                                    (fun (seen, dup) fieldName ->
                                                        match dup with
                                                        | Some _ -> seen, dup
                                                        | None when Set.contains fieldName seen -> seen, Some fieldName
                                                        | None -> Set.add fieldName seen, None)
                                                    (Set.empty, None)
                                                |> snd

                                            match duplicateFieldName with
                                            | Some duplicatedName ->
                                                Result.Error(Hir.Expr.ExprError(sprintf "Duplicate pattern field '%s'" duplicatedName, tid, matchArm.span))
                                            | None ->
                                                let caseFieldMap = caseDef.fields |> List.map (fun fieldDef -> fieldDef.name, fieldDef) |> Map.ofList
                                                let patternFieldNames =
                                                    enumPattern.fields
                                                    |> List.choose (fun field ->
                                                        match field with
                                                        | :? Ast.PatternField.Named as namedField -> Some namedField.name
                                                        | _ -> None)
                                                let unknownFieldName =
                                                    patternFieldNames
                                                    |> List.tryFind (fun fieldName -> not (Map.containsKey fieldName caseFieldMap))

                                                match unknownFieldName with
                                                | Some unknownField ->
                                                    Result.Error(Hir.Expr.ExprError(sprintf "Unknown pattern field '%s' for enum case '%s'" unknownField caseDef.name, tid, matchArm.span))
                                                | None ->
                                                    // Named フィールドで未カバーかつ positional バインディングでも未カバーのフィールドを検出する。
                                                    // i 番目の positional バインディングはケースの i 番目のフィールドをカバーする。
                                                    let missingField =
                                                        if enumPattern.hasRest then
                                                            None
                                                        else
                                                            caseDef.fields
                                                            |> List.mapi (fun i fieldDef -> i, fieldDef)
                                                            |> List.tryFind (fun (i, fieldDef) ->
                                                                not (patternFieldNames |> List.contains fieldDef.name)
                                                                && i >= positionalFields.Length)
                                                            |> Option.map snd
                                                    match missingField with
                                                    | Some fieldDef ->
                                                        Result.Error(Hir.Expr.ExprError(sprintf "Missing pattern field '%s' for enum case '%s'" fieldDef.name caseDef.name, tid, matchArm.span))
                                                    | None ->
                                                        let armNameEnv = matchNameEnv.sub()
                                                        // スクラッチニー型が App(Name enumSid, concreteArgs) のとき、
                                                        // enum の型パラメータを具体型へ置換してパターン束縛型を確定する。
                                                        let scrutineeRootType = instantiateEnumRootType typeEnv scrutineeTypeDef analyzedScrutinee.typ
                                                        let typeVarSubst = buildTypeVarSubst typeEnv scrutineeTypeDef scrutineeRootType
                                                        let bindingStmts =
                                                            match caseDef.payloadTypeSid, caseDef.payloadFieldSid with
                                                            | Some payloadTypeSid, Some payloadFieldSid ->
                                                                let payloadExpr =
                                                                    Hir.Expr.MemberAccess(Hir.Member.DataField(scrutineeTypeDef.typeSid, payloadFieldSid), Some scrutineeRef, TypeId.Name payloadTypeSid, matchArm.span)
                                                                // Named フィールドのバインディング
                                                                let namedBindings =
                                                                    patternFieldNames
                                                                    |> List.choose (fun fieldName ->
                                                                        caseFieldMap
                                                                        |> Map.tryFind fieldName
                                                                        |> Option.map (fun fieldDef ->
                                                                            let boundType = substituteTypeVars typeVarSubst fieldDef.typ
                                                                            let sid = armNameEnv.declareLocal fieldName boundType
                                                                            let valueExpr =
                                                                                Hir.Expr.MemberAccess(Hir.Member.DataField(payloadTypeSid, fieldDef.sid), Some payloadExpr, boundType, matchArm.span)
                                                                            Hir.Stmt.Let(sid, false, valueExpr, matchArm.span)))
                                                                // Positional フィールドのバインディング:
                                                                // i 番目の Positional はケースの i 番目のフィールドを varName に束縛する。
                                                                let positionalBindings =
                                                                    positionalFields
                                                                    |> List.mapi (fun i pf ->
                                                                        if pf.varName = "_" then
                                                                            None  // "_" はワイルドカード；フィールドアクセスを生成しない
                                                                        else
                                                                        caseDef.fields
                                                                        |> List.tryItem i
                                                                        |> Option.map (fun fieldDef ->
                                                                            let boundType = substituteTypeVars typeVarSubst fieldDef.typ
                                                                            let sid = armNameEnv.declareLocal pf.varName boundType
                                                                            let valueExpr =
                                                                                Hir.Expr.MemberAccess(Hir.Member.DataField(payloadTypeSid, fieldDef.sid), Some payloadExpr, boundType, matchArm.span)
                                                                            Hir.Stmt.Let(sid, false, valueExpr, matchArm.span)))
                                                                    |> List.choose id
                                                                namedBindings @ positionalBindings
                                                            | _ -> []

                                                        let bodyExpr = analyzeExpr armNameEnv typeEnv matchArm.body tid
                                                        let thenExpr =
                                                            if bindingStmts.IsEmpty then
                                                                bodyExpr
                                                            else
                                                                Hir.Expr.Block(bindingStmts, bodyExpr, bodyExpr.typ, matchArm.span)
                                                        Result.Ok(caseDef, thenExpr)
                                | _ ->
                                    Result.Error(Hir.Expr.ExprError("Unsupported match pattern", tid, matchArm.span))
                            | _ ->
                                Result.Error(Hir.Expr.ExprError("Unsupported match arm", tid, arm.span))

                        let armResults =
                            matchExpr.arms
                            |> List.map analyzeArm

                        match armResults |> List.tryPick (function | Result.Error err -> Some err | _ -> None) with
                        | Some errExpr -> errExpr
                        | None ->
                            let analyzedArms = armResults |> List.choose (function | Result.Ok value -> Some value | _ -> None)
                            let duplicateCase =
                                analyzedArms
                                |> List.fold
                                    (fun (seen, dup) (caseDef, _) ->
                                        match dup with
                                        | Some _ -> seen, dup
                                        | None when Set.contains caseDef.name seen -> seen, Some caseDef.name
                                        | None -> Set.add caseDef.name seen, None)
                                    (Set.empty, None)
                                |> snd
                            match duplicateCase with
                            | Some caseName ->
                                Hir.Expr.ExprError(sprintf "Duplicate match arm for enum case '%s'" caseName, tid, matchExpr.span)
                            | None ->
                                let coveredCases = analyzedArms |> List.map (fun (caseDef, _) -> caseDef.name) |> Set.ofList
                                let missingCase =
                                    enumDef.cases
                                    |> List.tryFind (fun caseDef -> not (Set.contains caseDef.name coveredCases))
                                match missingCase with
                                | Some caseDef ->
                                    Hir.Expr.ExprError(sprintf "Non-exhaustive match: missing enum case '%s'" caseDef.name, tid, matchExpr.span)
                                | None ->
                                    let tagAccess caseTagSpan =
                                        Hir.Expr.MemberAccess(Hir.Member.DataField(scrutineeTypeDef.typeSid, enumDef.hiddenTagField.sid), Some scrutineeRef, enumDef.hiddenTagField.typ, caseTagSpan)

                                    let ifChain =
                                        analyzedArms
                                        |> List.rev
                                        |> List.fold
                                            (fun elseExpr (caseDef, thenExpr) ->
                                                let condExpr =
                                                    Hir.Expr.Call(
                                                        Hir.Callable.BuiltinOperator Builtins.Operators.OpEq,
                                                        None,
                                                        [ tagAccess matchExpr.span
                                                          Hir.Expr.Int(caseDef.tag, matchExpr.span) ],
                                                        TypeId.Bool,
                                                        matchExpr.span)
                                                Hir.Expr.If(condExpr, thenExpr, elseExpr, tid, matchExpr.span))
                                            (analyzedArms |> List.last |> snd)
                                    Hir.Expr.Block([ Hir.Stmt.Let(scrutineeSid, false, analyzedScrutinee, matchExpr.scrutinee.span) ], ifChain, tid, matchExpr.span)
            | None ->
                Hir.Expr.ExprError(sprintf "Type '%s' is not an enum" (formatTypeForDisplay nameEnv typeEnv analyzedScrutinee.typ), tid, matchExpr.span)
        | :? Ast.Expr.Lambda as lambdaExpr ->
            // ラムダ本体専用のスコープを作成し、引数束縛を外側と分離する。
            // Lambda 境界では `isInsideAsyncFn` を false にリセットする（ラムダは async 不可）。
            let lambdaNameEnv = nameEnv.subLambda()

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
                |> List.fold (fun (seen: Set<string>, dup: string option) arg ->
                    match dup with
                    | Some _ -> seen, dup
                    | None ->
                        match arg with
                        | :? Ast.FnArg.Named as namedArg ->
                            if seen.Contains namedArg.name then
                                seen, Some namedArg.name
                            else
                                seen.Add namedArg.name, None
                        | :? Ast.FnArg.Inferred as inferredArg ->
                            if seen.Contains inferredArg.name then
                                seen, Some inferredArg.name
                            else
                                seen.Add inferredArg.name, None
                        | :? Ast.FnArg.Unit -> seen, None
                        | _ -> seen, None) (Set.empty, None)
                |> snd

            match duplicateArgName with
            | Some dupName ->
                Hir.Expr.ExprError(sprintf "Duplicate lambda parameter '%s'" dupName, tid, lambdaExpr.span)
            | None ->
                // 引数を宣言順に Arg シンボルとして登録し、HIR 引数ノードを構築する。
                let hirArgs =
                    lambdaExpr.args
                    |> List.mapi (fun index arg ->
                        match arg with
                        | :? Ast.FnArg.Named as namedArg ->
                            let argType = expectedArgTypes.[index]
                            let resolvedArgType = lambdaNameEnv.resolveTypeExpr namedArg.typeExpr
                            match typeEnv.unifyTypes argType resolvedArgType with
                            | Result.Ok _ ->
                                let argSid = lambdaNameEnv.declareArg namedArg.name argType
                                Hir.Arg(argSid, namedArg.name, argType, arg.span)
                            | Result.Error _ ->
                                let argSid = lambdaNameEnv.declareArg namedArg.name (TypeId.Error "Type mismatch")
                                Hir.Arg(argSid, namedArg.name, TypeId.Error "Type mismatch", arg.span)
                        | :? Ast.FnArg.Inferred as inferredArg ->
                            let argType = expectedArgTypes.[index]
                            let argSid = lambdaNameEnv.declareArg inferredArg.name argType
                            Hir.Arg(argSid, inferredArg.name, argType, arg.span)
                        | :? Ast.FnArg.Unit ->
                            // Unit引数の場合はスキップ（0引数関数として扱う）
                            let dummySid = lambdaNameEnv.symbolTable.NextId()
                            Hir.Arg(dummySid, "()", TypeId.Unit, arg.span)
                        | _ ->
                            let dummySid = lambdaNameEnv.symbolTable.NextId()
                            Hir.Arg(dummySid, "?", TypeId.Error "Unknown arg type", arg.span))

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
                    // 期待型が具体的な .NET デリゲート型（Native）の場合、Lambda の型をその型で確定する。
                    // TypeId.Fn(...)のままにすると、Layout フェーズで Action<,> などの汎用デリゲートへ変換され、
                    // EventHandler<T> を要求するイベントへの代入で InvalidCastException が発生する。
                    let finalLambdaType =
                        match typeEnv.resolveType tid with
                        | TypeId.Native t when TypeId.isDelegateType t -> TypeId.Native t
                        | _ -> resolvedLambdaType
                    Hir.Expr.Lambda(resolvedArgs, resolvedRetType, analyzedBody, finalLambdaType, lambdaExpr.span)
                | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Await as awaitExpr ->
            // `await` は `async fn` 本体内でのみ使用可能。それ以外の文脈ではエラーを返す。
            if not nameEnv.isInsideAsyncFn then
                errorExpr tid awaitExpr.span "'await' can only be used inside an 'async fn' body"
            else
                // operand の型は推論が必要。`freshMeta` を与えて自由に解析させ、結果型から判定する。
                let operandTid = typeEnv.freshMeta()
                let analyzedOperand = analyzeExpr nameEnv typeEnv awaitExpr.operand operandTid
                match tryUnwrapTaskType nameEnv typeEnv analyzedOperand.typ with
                | Some resultType ->
                    match unifyOrError nameEnv typeEnv tid resultType awaitExpr.span with
                    | Result.Ok _ ->
                        Hir.Expr.Await(analyzedOperand, typeEnv.resolveType resultType, awaitExpr.span)
                    | Result.Error exprErr -> exprErr
                | None ->
                    let actualTypeStr = formatTypeForDisplay nameEnv typeEnv analyzedOperand.typ
                    errorExpr tid awaitExpr.span
                        (sprintf "'await' expects Task or Task<T>, got '%s'" actualTypeStr)
        | :? Ast.Expr.TypeAscription as ascExpr ->
            // 型注釈は推論を注釈型へ固定する制約のみ。キャストはしない。
            // 注釈型を期待型 tid と単一化し、内側式をその型を期待して解析する。
            let annotTid = nameEnv.resolveTypeExpr ascExpr.typeExpr
            match unifyOrError nameEnv typeEnv tid annotTid ascExpr.span with
            | Result.Ok _ -> analyzeExpr nameEnv typeEnv ascExpr.expr annotTid
            | Result.Error exprErr -> exprErr
        | _ -> errorExpr tid expr.span "Unsupported expression type"

    and private analyzeStmt (nameEnv: NameEnv) (typeEnv: TypeEnv) (stmt: Ast.Stmt) : Hir.Stmt =
        match stmt with
        | :? Ast.Stmt.Error as errorStmt ->
            // Parser 由来の文エラーは ErrorStmt として後段へ渡す。
            Hir.Stmt.ErrorStmt(errorStmt.message, errorStmt.span)
        | :? Ast.Stmt.Let as letStmt ->
            let tid =
                match letStmt.typeAnnotation with
                | Some te -> nameEnv.resolveTypeExpr te
                | None -> typeEnv.freshMeta ()
            let rhs = analyzeExpr nameEnv typeEnv letStmt.value tid
            let sid = nameEnv.declareLocal letStmt.name tid
            Hir.Stmt.Let(sid, false, rhs, letStmt.span)
        | :? Ast.Stmt.Var as varStmt ->
            let tid =
                match varStmt.typeAnnotation with
                | Some te -> nameEnv.resolveTypeExpr te
                | None -> typeEnv.freshMeta ()
            let rhs = analyzeExpr nameEnv typeEnv varStmt.value tid
            let sid = nameEnv.declareLocal varStmt.name tid
            Hir.Stmt.Let(sid, true, rhs, varStmt.span)
        | :? Ast.Stmt.CompoundAssign as compoundAssignStmt ->
            let analyzeAsVariableCompound sid targetType =
                let lhsExpr = Hir.Expr.Id(sid, targetType, compoundAssignStmt.target.span)
                let rhs = analyzeExpr nameEnv typeEnv compoundAssignStmt.value targetType
                let opName =
                    match compoundAssignStmt.op with
                    | Ast.Stmt.CompoundAssignOp.Add -> "+"
                    | Ast.Stmt.CompoundAssignOp.Sub -> "-"
                    | Ast.Stmt.CompoundAssignOp.Mul -> "*"
                    | Ast.Stmt.CompoundAssignOp.Div -> "/"
                let opExpr =
                    Hir.Expr.Call(
                        Hir.Callable.BuiltinOperator(match compoundAssignStmt.op with | Ast.Stmt.CompoundAssignOp.Add -> Builtins.Operators.OpAdd | Ast.Stmt.CompoundAssignOp.Sub -> Builtins.Operators.OpSub | Ast.Stmt.CompoundAssignOp.Mul -> Builtins.Operators.OpMul | Ast.Stmt.CompoundAssignOp.Div -> Builtins.Operators.OpDiv),
                        None,
                        [ lhsExpr; rhs ],
                        targetType,
                        compoundAssignStmt.span)
                match unifyOrError nameEnv typeEnv targetType opExpr.typ compoundAssignStmt.span with
                | Result.Ok _ -> Hir.Stmt.Assign(sid, opExpr, compoundAssignStmt.span)
                | Result.Error exprErr -> Hir.Stmt.ExprStmt(exprErr, compoundAssignStmt.span)

            match compoundAssignStmt.target with
            | :? Ast.Expr.Id as idTarget ->
                match nameEnv.resolveVar idTarget.name with
                | [ sid ] -> analyzeAsVariableCompound sid (nameEnv.resolveSymType sid)
                | [] ->
                    let message = sprintf "Undefined variable '%s'" idTarget.name
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, compoundAssignStmt.span), compoundAssignStmt.span)
                | _ ->
                    let message = sprintf "Ambiguous variable '%s'" idTarget.name
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, compoundAssignStmt.span), compoundAssignStmt.span)
            | :? Ast.Expr.MemberAccess as memberTarget ->
                let analyzedTarget = analyzeExpr nameEnv typeEnv memberTarget (typeEnv.freshMeta ())
                match analyzedTarget with
                | Hir.Expr.MemberAccess (Hir.Member.DataField (typeSid, fieldSid), Some instanceExpr, fieldTid, _) ->
                    // ユーザー定義データ型フィールドへの複合代入を `field = field op rhs` 相当に lower する。
                    match tryFindImmutableFieldName nameEnv typeSid fieldSid with
                    | Some fieldName ->
                        let message = sprintf "Cannot assign to immutable field '%s' (declared as val)" fieldName
                        Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, compoundAssignStmt.span), compoundAssignStmt.span)
                    | None ->
                        let rhs = analyzeExpr nameEnv typeEnv compoundAssignStmt.value fieldTid
                        let opExpr =
                            Hir.Expr.Call(
                                Hir.Callable.BuiltinOperator(match compoundAssignStmt.op with | Ast.Stmt.CompoundAssignOp.Add -> Builtins.Operators.OpAdd | Ast.Stmt.CompoundAssignOp.Sub -> Builtins.Operators.OpSub | Ast.Stmt.CompoundAssignOp.Mul -> Builtins.Operators.OpMul | Ast.Stmt.CompoundAssignOp.Div -> Builtins.Operators.OpDiv),
                                None,
                                [ analyzedTarget; rhs ],
                                fieldTid,
                                compoundAssignStmt.span)
                        Hir.Stmt.StoreField(instanceExpr, typeSid, fieldSid, opExpr, compoundAssignStmt.span)
                | Hir.Expr.MemberAccess (Hir.Member.NativeField fieldInfo, Some fieldReceiver, fieldTid, _) ->
                    // ネイティブ（.NET）フィールドへの複合代入を `field = field op rhs` 相当に lower する。
                    // 値型（struct）フィールドは Layout でアドレス経由の read-modify-write に下る。
                    let rhs = analyzeExpr nameEnv typeEnv compoundAssignStmt.value fieldTid
                    let opExpr =
                        Hir.Expr.Call(
                            Hir.Callable.BuiltinOperator(match compoundAssignStmt.op with | Ast.Stmt.CompoundAssignOp.Add -> Builtins.Operators.OpAdd | Ast.Stmt.CompoundAssignOp.Sub -> Builtins.Operators.OpSub | Ast.Stmt.CompoundAssignOp.Mul -> Builtins.Operators.OpMul | Ast.Stmt.CompoundAssignOp.Div -> Builtins.Operators.OpDiv),
                            None,
                            [ analyzedTarget; rhs ],
                            fieldTid,
                            compoundAssignStmt.span)
                    Hir.Stmt.StoreNativeField(fieldReceiver, fieldInfo, opExpr, compoundAssignStmt.span)
                | _ ->
                    let receiverExpr = analyzeExpr nameEnv typeEnv memberTarget.receiver (typeEnv.freshMeta ())
                    match NativeInterop.resolveRuntimeSystemType nameEnv typeEnv receiverExpr.typ with
                    | None ->
                        let message = "Event compound assignment requires a native receiver type"
                        Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, compoundAssignStmt.span), compoundAssignStmt.span)
                    | Some receiverType ->
                        let flags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic
                        match receiverType.GetEvent(memberTarget.memberName, flags) with
                        | null ->
                            // イベントでない場合、プロパティへの複合代入を試みる（例: string プロパティの += 連結）。
                            match receiverType.GetProperty(memberTarget.memberName, flags) with
                            | null ->
                                let message = sprintf "Member '%s' does not support compound assignment" memberTarget.memberName
                                Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, compoundAssignStmt.span), compoundAssignStmt.span)
                            | propInfo ->
                                let getter = propInfo.GetGetMethod(true)
                                let setter = propInfo.GetSetMethod(true)
                                if isNull getter || isNull setter then
                                    let message = sprintf "Property '%s' must have both getter and setter for compound assignment" propInfo.Name
                                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, compoundAssignStmt.span), compoundAssignStmt.span)
                                else
                                    let propTid = TypeId.fromSystemType propInfo.PropertyType
                                    let rhs = analyzeExpr nameEnv typeEnv compoundAssignStmt.value propTid
                                    // 現在の値を getter で取得し、rhs と合成して setter で書き戻す。
                                    let currentVal =
                                        Hir.Expr.Call(Hir.Callable.NativeMethod getter, Some receiverExpr, [], propTid, compoundAssignStmt.span)
                                    // += の場合: String.Concat で連結する（String プロパティを想定）。
                                    let newVal =
                                        match compoundAssignStmt.op with
                                        | Ast.Stmt.CompoundAssignOp.Add ->
                                            let concatMethod = typeof<string>.GetMethod("Concat", [| typeof<string>; typeof<string> |])
                                            if not (isNull concatMethod) && propInfo.PropertyType = typeof<string> then
                                                Hir.Expr.Call(Hir.Callable.NativeMethod concatMethod, None, [ currentVal; rhs ], propTid, compoundAssignStmt.span)
                                            else
                                                Hir.Expr.ExprError(sprintf "Unsupported += on property type %s" propInfo.PropertyType.Name, TypeId.Error "unsupported", compoundAssignStmt.span)
                                        | Ast.Stmt.CompoundAssignOp.Sub ->
                                            Hir.Expr.ExprError("Compound -= on properties is not supported", TypeId.Error "unsupported", compoundAssignStmt.span)
                                        | Ast.Stmt.CompoundAssignOp.Mul ->
                                           Hir.Expr.ExprError("Compound *= on properties is not supported", TypeId.Error "unsupported", compoundAssignStmt.span)
                                        | Ast.Stmt.CompoundAssignOp.Div ->
                                           Hir.Expr.ExprError("Compound /= on properties is not supported", TypeId.Error "unsupported", compoundAssignStmt.span)
                                    let callExpr =
                                        Hir.Expr.Call(Hir.Callable.NativeMethod setter, Some receiverExpr, [ newVal ], TypeId.Unit, compoundAssignStmt.span)
                                    Hir.Stmt.ExprStmt(callExpr, compoundAssignStmt.span)
                        | eventInfo ->
                            let accessorOpt =
                                match compoundAssignStmt.op with
                                | Ast.Stmt.CompoundAssignOp.Add -> Option.ofObj (eventInfo.GetAddMethod(true))
                                | Ast.Stmt.CompoundAssignOp.Sub -> Option.ofObj (eventInfo.GetRemoveMethod(true))
                                | Ast.Stmt.CompoundAssignOp.Mul | Ast.Stmt.CompoundAssignOp.Div -> None

                            match accessorOpt with
                            | None ->
                                let message = sprintf "Event '%s' does not have a required accessor" eventInfo.Name
                                Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, compoundAssignStmt.span), compoundAssignStmt.span)
                            | Some accessor ->
                                let handlerType = TypeId.fromSystemType eventInfo.EventHandlerType
                                let rhs = analyzeExpr nameEnv typeEnv compoundAssignStmt.value handlerType
                                let callExpr =
                                    Hir.Expr.Call(
                                        Hir.Callable.NativeMethod accessor,
                                        Some receiverExpr,
                                        [ rhs ],
                                        TypeId.Unit,
                                        compoundAssignStmt.span)
                                Hir.Stmt.ExprStmt(callExpr, compoundAssignStmt.span)
            | _ ->
                let message = "Invalid assignment target"
                Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, compoundAssignStmt.span), compoundAssignStmt.span)

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
                | Hir.Expr.MemberAccess (Hir.Member.NativeField fieldInfo, Some fieldReceiver, fieldTid, _) ->
                    // ネイティブ（.NET）フィールドへの代入を StoreNativeField に lower する。
                    // 値型（struct）フィールドは Layout でアドレス経由の書き込みに下る。
                    let rhs = analyzeExpr nameEnv typeEnv assignStmt.value fieldTid
                    Hir.Stmt.StoreNativeField(fieldReceiver, fieldInfo, rhs, assignStmt.span)
                | Hir.Expr.MemberAccess (Hir.Member.NativeField fieldInfo, None, _, _) ->
                    let message = sprintf "Field assignment requires an instance receiver for member '%s'" fieldInfo.Name
                    Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                | Hir.Expr.MemberAccess (Hir.Member.DataField (typeSid, fieldSid), Some instanceExpr, fieldTid, _) ->
                    // データ型フィールドへの代入を StoreField に lower する。
                    match tryFindImmutableFieldName nameEnv typeSid fieldSid with
                    | Some fieldName ->
                        let message = sprintf "Cannot assign to immutable field '%s' (declared as val)" fieldName
                        Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                    | None ->
                        let rhs = analyzeExpr nameEnv typeEnv assignStmt.value fieldTid
                        Hir.Stmt.StoreField(instanceExpr, typeSid, fieldSid, rhs, assignStmt.span)
                | Hir.Expr.MemberAccess (Hir.Member.DataField _, None, _, _) ->
                    let message = "Field assignment requires an instance receiver"
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
            | :? Ast.Expr.IndexAccess as indexTarget ->
                // 添字代入 `receiver[index] = value` をインデクサ setter 呼び出しへ lower する。
                // List<T> → set_Item(index, value)、1次元配列 → SetValue(value, index)。
                let receiver = analyzeExpr nameEnv typeEnv indexTarget.receiver (typeEnv.freshMeta ())
                let indexExpr = analyzeExpr nameEnv typeEnv indexTarget.index TypeId.Int
                let resolvedReceiverType = typeEnv.resolveType receiver.typ
                match NativeInterop.resolveRuntimeSystemType nameEnv typeEnv resolvedReceiverType with
                | Some systemType ->
                    match NativeInterop.tryResolveIndexerSetterMethod systemType with
                    | Some setMethod ->
                        let ps = setMethod.GetParameters()
                        // SetValue(value, index) は value が第1引数。set_Item(index, value) は value が第2引数。
                        let isArraySetter = (setMethod.Name = "SetValue")
                        let valueParamType = if isArraySetter then ps.[0].ParameterType else ps.[1].ParameterType
                        let rhs = analyzeExpr nameEnv typeEnv assignStmt.value (TypeId.fromSystemType valueParamType)
                        let callArgs = if isArraySetter then [ receiver; rhs; indexExpr ] else [ receiver; indexExpr; rhs ]
                        let callExpr =
                            Hir.Expr.Call(Hir.Callable.NativeMethod setMethod, None, callArgs, TypeId.Unit, assignStmt.span)
                        Hir.Stmt.ExprStmt(callExpr, assignStmt.span)
                    | None ->
                        let message = sprintf "Type '%s' does not support index assignment" systemType.FullName
                        Hir.Stmt.ExprStmt(Hir.Expr.ExprError(message, TypeId.Error message, assignStmt.span), assignStmt.span)
                | None ->
                    let message = sprintf "Type '%s' does not support indexing" (formatTypeForDisplay nameEnv typeEnv resolvedReceiverType)
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
        | :? Ast.Stmt.If as ifStmt ->
            let rec analyzeIfStmtBranches (branches: Ast.IfBranch list) : Hir.Stmt list =
                match branches with
                | [] -> []
                | branch :: rest ->
                    match branch with
                    | :? Ast.IfBranch.Then as thenBranch ->
                        let cond = analyzeExpr nameEnv typeEnv thenBranch.cond TypeId.Bool
                        let thenStmts =
                            match thenBranch.body with
                            | :? Ast.Expr.Block as block -> block.stmts |> List.map (analyzeStmt nameEnv typeEnv)
                            | body -> [analyzeStmt nameEnv typeEnv (Ast.Stmt.ExprStmt(body, body.span) :> Ast.Stmt)]
                        [Hir.Stmt.If(cond, thenStmts, analyzeIfStmtBranches rest, thenBranch.span)]
                    | :? Ast.IfBranch.Else as elseBranch ->
                        match elseBranch.body with
                        | :? Ast.Expr.Block as block -> block.stmts |> List.map (analyzeStmt nameEnv typeEnv)
                        | body -> [analyzeStmt nameEnv typeEnv (Ast.Stmt.ExprStmt(body, body.span) :> Ast.Stmt)]
                    | _ -> []
            match analyzeIfStmtBranches ifStmt.branches with
            | [stmt] -> stmt
            | stmts -> Hir.Stmt.ErrorStmt(sprintf "Unexpected if stmt branch count: %d" stmts.Length, ifStmt.span)
        | :? Ast.Stmt.While as whileStmt ->
            let cond = analyzeExpr nameEnv typeEnv whileStmt.cond TypeId.Bool
            let body = whileStmt.body |> List.map (analyzeStmt nameEnv typeEnv)
            Hir.Stmt.While(cond, body, whileStmt.span)
        | :? Ast.Stmt.Break as breakStmt ->
            Hir.Stmt.Break(breakStmt.span)
        | :? Ast.Stmt.Continue as continueStmt ->
            Hir.Stmt.Continue(continueStmt.span)
        | :? Ast.Stmt.Return as returnStmt ->
            let analyzedExpr = analyzeExpr nameEnv typeEnv returnStmt.expr (typeEnv.freshMeta ())
            Hir.Stmt.Return(analyzedExpr, returnStmt.span)
        | :? Ast.Stmt.LetElse as s ->
            analyzeLetVarElseStmt nameEnv typeEnv s.pattern s.value s.elseBranch false s.span
        | :? Ast.Stmt.VarElse as s ->
            analyzeLetVarElseStmt nameEnv typeEnv s.pattern s.value s.elseBranch true s.span
        | _ -> Hir.Stmt.ErrorStmt("Unsupported statement type", stmt.span)

    and private analyzeLetVarElseStmt (nameEnv: NameEnv) (typeEnv: TypeEnv) (pattern: Ast.Pattern) (value: Ast.Expr) (elseBranch: Ast.Stmt list) (isMutable: bool) (span: Atla.Core.Data.Span) : Hir.Stmt =
        let analyzedRHS = analyzeExpr nameEnv typeEnv value (typeEnv.freshMeta ())
        match pattern with
        | :? Ast.Pattern.Enum as enumPattern ->
            match tryGetRootTypeSid typeEnv analyzedRHS.typ with
            | None ->
                Hir.Stmt.ErrorStmt(sprintf "Type '%s' is not an enum" (formatTypeForDisplay nameEnv typeEnv analyzedRHS.typ), span)
            | Some scrutineeTypeSid ->
                match tryFindTypeDefBySid nameEnv scrutineeTypeSid with
                | None ->
                    Hir.Stmt.ErrorStmt(sprintf "Type '%s' is not defined" (formatTypeForDisplay nameEnv typeEnv analyzedRHS.typ), span)
                | Some scrutineeTypeDef ->
                    match scrutineeTypeDef.enumInfo with
                    | None ->
                        Hir.Stmt.ErrorStmt(sprintf "Type '%s' is not an enum" (formatTypeForDisplay nameEnv typeEnv analyzedRHS.typ), span)
                    | Some enumDef ->
                        match enumDef.cases |> List.tryFind (fun c -> c.name = enumPattern.caseName) with
                        | None ->
                            Hir.Stmt.ErrorStmt(sprintf "Unknown enum case '%s' for type '%s'" enumPattern.caseName enumPattern.typeName, span)
                        | Some caseDef ->
                            let scrutineeSubEnv = nameEnv.sub()
                            let scrutineeSid = scrutineeSubEnv.declareLocal "__le_scrutinee" analyzedRHS.typ
                            let scrutineeRef = Hir.Expr.Id(scrutineeSid, analyzedRHS.typ, value.span)

                            let scrutineeRootType = instantiateEnumRootType typeEnv scrutineeTypeDef analyzedRHS.typ
                            let typeVarSubst = buildTypeVarSubst typeEnv scrutineeTypeDef scrutineeRootType

                            // フィールドバインディング（外側の nameEnv に宣言してスコープ外からも参照可能）
                            let bindingStmts =
                                match caseDef.payloadTypeSid, caseDef.payloadFieldSid with
                                | Some payloadTypeSid, Some payloadFieldSid ->
                                    let payloadExpr =
                                        Hir.Expr.MemberAccess(Hir.Member.DataField(scrutineeTypeDef.typeSid, payloadFieldSid), Some scrutineeRef, TypeId.Name payloadTypeSid, span)
                                    let positionalFields =
                                        enumPattern.fields
                                        |> List.choose (fun f -> match f with | :? Ast.PatternField.Positional as pf -> Some pf | _ -> None)
                                    let namedBindings =
                                        enumPattern.fields
                                        |> List.choose (fun f ->
                                            match f with
                                            | :? Ast.PatternField.Named as nf ->
                                                caseDef.fields
                                                |> List.tryFind (fun fd -> fd.name = nf.name)
                                                |> Option.map (fun fieldDef ->
                                                    let boundType = substituteTypeVars typeVarSubst fieldDef.typ
                                                    let sid = nameEnv.declareLocal nf.name boundType
                                                    let valueExpr = Hir.Expr.MemberAccess(Hir.Member.DataField(payloadTypeSid, fieldDef.sid), Some payloadExpr, boundType, span)
                                                    Hir.Stmt.Let(sid, isMutable, valueExpr, span))
                                            | _ -> None)
                                    let positionalBindings =
                                        positionalFields
                                        |> List.mapi (fun i pf ->
                                            if pf.varName = "_" then None
                                            else
                                                caseDef.fields
                                                |> List.tryItem i
                                                |> Option.map (fun fieldDef ->
                                                    let boundType = substituteTypeVars typeVarSubst fieldDef.typ
                                                    let sid = nameEnv.declareLocal pf.varName boundType
                                                    let valueExpr = Hir.Expr.MemberAccess(Hir.Member.DataField(payloadTypeSid, fieldDef.sid), Some payloadExpr, boundType, span)
                                                    Hir.Stmt.Let(sid, isMutable, valueExpr, span)))
                                        |> List.choose id
                                    namedBindings @ positionalBindings
                                | _ -> []

                            let tagAccessExpr =
                                Hir.Expr.MemberAccess(Hir.Member.DataField(scrutineeTypeDef.typeSid, enumDef.hiddenTagField.sid), Some scrutineeRef, enumDef.hiddenTagField.typ, span)
                            let tagCheckExpr =
                                Hir.Expr.Call(Hir.Callable.BuiltinOperator Builtins.Operators.OpEq, None,
                                    [ tagAccessExpr; Hir.Expr.Int(caseDef.tag, span) ], TypeId.Bool, span)

                            // 条件式: RHS を一時変数に束縛しつつタグを確認
                            let condExpr =
                                Hir.Expr.Block([ Hir.Stmt.Let(scrutineeSid, false, analyzedRHS, span) ], tagCheckExpr, TypeId.Bool, span)

                            let elseBodyStmts = elseBranch |> List.map (analyzeStmt nameEnv typeEnv)

                            // else ブランチは必ず発散（return/break/continue で終わる）こと
                            let diverges =
                                match List.tryLast elseBodyStmts with
                                | Some(Hir.Stmt.Return _) | Some(Hir.Stmt.Break _) | Some(Hir.Stmt.Continue _) -> true
                                | _ -> false
                            let finalElseBody =
                                if not diverges then
                                    elseBodyStmts @ [ Hir.Stmt.ErrorStmt("'else' branch of 'let-else' must end with 'return', 'break', or 'continue'", span) ]
                                else
                                    elseBodyStmts

                            Hir.Stmt.If(condExpr, bindingStmts, finalElseBody, span)
        | _ ->
            Hir.Stmt.ErrorStmt("let-else requires an enum pattern (e.g. 'Type'Case x')", span)

    let analyzeMethodCoreWithOverride (nameEnv: NameEnv) (typeEnv: TypeEnv) (sid: SymbolId) (fnDecl: Ast.Decl.Fn) (overrideTarget: MethodInfo option) : Hir.Method =
        // `async fn` の場合は本体を async 文脈で解析する。戻り値注釈は「本体が返す内側の型」
        // とみなし、シグネチャ型（Hir.Method.typ）では暗黙に Task で包む（Unit→Task, T→Task<T>）。
        // 本体は包む前の内側の型（effectiveBodyRet）に対して型検査する。
        let bodyNameEnv =
            if fnDecl.isAsync then nameEnv.subAsync() else nameEnv.sub()
        let declaredInner = nameEnv.resolveTypeExpr fnDecl.ret

        let effectiveBodyRet : TypeId = declaredInner

        let retType : TypeId =
            if fnDecl.isAsync then TypeId.wrapInTask declaredInner else declaredInner

        let rawArgTypes =
            let nonUnitArgCount =
                fnDecl.args
                |> List.filter (fun arg -> not (arg :? Ast.FnArg.Unit))
                |> List.length
            match nameEnv.resolveSymType sid |> typeEnv.resolveType with
            | TypeId.Fn(symbolArgTypes, _) when symbolArgTypes.Length = nonUnitArgCount ->
                symbolArgTypes
            | _ ->
                fnDecl.args
                |> List.map bodyNameEnv.resolveArgType
                |> List.filter (fun t -> t <> TypeId.Unit)
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
                | :? Ast.FnArg.Inferred as inferredArg ->
                    let argType = argTypes.[index]
                    let sid = bodyNameEnv.declareArg inferredArg.name argType
                    Some (sid, argType)
                | :? Ast.FnArg.Unit -> None
                | _ -> None)
            |> List.choose id

        let tid = TypeId.Fn(argTypes, retType)
        let body = analyzeExpr bodyNameEnv typeEnv fnDecl.body effectiveBodyRet

        Hir.Method(sid, argSids, body, tid, overrideTarget, fnDecl.isAsync, fnDecl.span)

    let analyzeMethodCore (nameEnv: NameEnv) (typeEnv: TypeEnv) (sid: SymbolId) (fnDecl: Ast.Decl.Fn) : Hir.Method =
        analyzeMethodCoreWithOverride nameEnv typeEnv sid fnDecl None

    let analyzeMethod (nameEnv: NameEnv) (typeEnv: TypeEnv) (fnDecl: Ast.Decl.Fn) : Hir.Method =
        let argTypes =
            fnDecl.args
            |> List.map nameEnv.resolveArgType
            |> (fun raw ->
                match fnDecl.args, raw with
                | [ (:? Ast.FnArg.Unit) ], [ TypeId.Unit ] -> []
                | _ -> raw)
        let retType =
            let declared = nameEnv.resolveTypeExpr fnDecl.ret
            if fnDecl.isAsync then TypeId.wrapInTask declared else declared
        let sid = nameEnv.declareLocal fnDecl.name (TypeId.Fn(argTypes, retType))
        analyzeMethodCore nameEnv typeEnv sid fnDecl
