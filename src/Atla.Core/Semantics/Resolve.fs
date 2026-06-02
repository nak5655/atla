namespace Atla.Core.Semantics

open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open System.Runtime.Loader

module Resolve =
    type ResolvedDataDecl =
        { typeSid: SymbolId
          typeParams: string list
          decl: Ast.Decl.Data }


    /// 解決済み union 宣言。variantSids は修飾なしバリアント名 → バリアント型 SymbolId のマップ
    /// （union 本体内バリアントと extendable union への外部バリアントの両方を含む）。
    /// externalVariants は union 本体外で宣言された追加バリアント（struct/object）の AST。
    /// qualifiedName はネスト union を含む完全修飾名（例: トップは "Color"、ネストは "Color'HueColor"）。
    /// baseUnionSid はネスト union の親 union 型 SID（トップレベルでは None）。
    type ResolvedUnionDecl =
        { typeSid: SymbolId
          typeParams: string list
          qualifiedName: string
          baseUnionSid: SymbolId option
          variantSids: Map<string, SymbolId>
          externalVariants: Ast.Decl list
          decl: Ast.Decl.Union }

    type ResolvedRoleDecl =
        { typeSid: SymbolId
          decl: Ast.Decl.Role }

    type ResolvedModule =
        { moduleName: string
          moduleScope: Scope
          importedModules: Map<string, string>
          importedTypeAliases: Map<string, string>
          fnDecls: Ast.Decl.Fn list
          dataDecls: ResolvedDataDecl list
          unionDecls: ResolvedUnionDecl list
          roleDecls: ResolvedRoleDecl list
          implDecls: (SymbolId * TypeId option * string option * Ast.Decl.Impl) list }

    let private tryResolveSystemType (classPath: string) : System.Type option =
        match System.Type.GetType(classPath) with
        | null ->
            let candidateAssemblies =
                seq {
                    yield! System.AppDomain.CurrentDomain.GetAssemblies()

                    for context in AssemblyLoadContext.All do
                        yield! context.Assemblies
                }
                |> Seq.distinct

            candidateAssemblies
            |> Seq.tryPick (fun asm ->
                match asm.GetType(classPath, false) with
                | null -> None
                | t -> Some t)
        | t -> Some t

    let private declareSystemType (symbolTable: SymbolTable) (scope: Scope) (classPath: string) : SymbolId =
        let sid = symbolTable.NextId()
        let name = Array.last (classPath.Split('.'))
        let resolvedType = tryResolveSystemType classPath |> Option.toObj
        let kind = SymbolKind.External(ExternalBinding.SystemTypeRef resolvedType)
        let symInfo = { name = name; typ = TypeId.Name sid; kind = kind }
        symbolTable.Add(sid, symInfo)
        scope.DeclareType(name, TypeId.Name sid)

        // import した .NET 型は値コンテキストで ctor 呼び出しできるよう、同名の変数シンボルとして ctor グループも公開する。
        if not (obj.ReferenceEquals(resolvedType, null)) then
            let ctorInfos =
                resolvedType.GetConstructors(System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Instance)
                |> Array.toList

            if not ctorInfos.IsEmpty then
                let ctorSid = symbolTable.NextId()
                let ctorType = TypeId.Fn([], TypeId.fromSystemType resolvedType)
                let ctorKind = SymbolKind.External(ExternalBinding.ConstructorGroup ctorInfos)
                symbolTable.Add(ctorSid, { name = name; typ = ctorType; kind = ctorKind })
                scope.DeclareVar(name, ctorSid)

        sid

    // import 宣言を処理し、型・変数スコープへ登録する。
    // 対象の .NET 型がロード済みアセンブリ内に見つからない場合は Warning 診断を返す。
    let private resolveImport
        (symbolTable: SymbolTable)
        (scope: Scope)
        (sourceModuleNames: Set<string>)
        (sourceTypeFullNames: Set<string>)
        (dependencyModuleNames: Set<string>)
        (dependencyTypeFullNames: Set<string>)
        (importedModules: ResizeArray<string * string>)
        (importedTypeAliases: ResizeArray<string * string>)
        (importedAtlaNominalTypeSids: ResizeArray<SymbolId>)
        (importDecl: Ast.Decl.Import)
        : Diagnostic list =
        let classPath = String.concat "." importDecl.path
        let shortName = Array.last (classPath.Split('.'))
        let resolveAtlaImport (layerName: string) (moduleNames: Set<string>) (typeNames: Set<string>) =
            let hasModule = moduleNames.Contains(classPath)
            let hasType = typeNames.Contains(classPath)

            match hasModule, hasType with
            | true, true ->
                [ Diagnostic.Error(
                      $"import '{classPath}' is ambiguous in dependency layer '{layerName}': both module and type were found",
                      importDecl.span) ]
            | true, false ->
                importedModules.Add(shortName, classPath)
                []
            | false, true ->
                importedTypeAliases.Add(shortName, classPath)
                // atlalib 型インポートはスコープにプレースホルダー SID を登録し、
                // 同モジュール内の `impl` 宣言が Resolve フェーズで型を参照できるようにする。
                // Analyze フェーズで正式な typeSid へ上書きされる。
                let sid = symbolTable.NextId()
                symbolTable.Add(sid, { name = classPath; typ = TypeId.Name sid; kind = SymbolKind.Local() })
                scope.DeclareType(shortName, TypeId.Name sid)
                importedAtlaNominalTypeSids.Add(sid)
                []
            | false, false ->
                []

        let sourceDiagnostics = resolveAtlaImport "source" sourceModuleNames sourceTypeFullNames

        if sourceDiagnostics.IsEmpty && (sourceModuleNames.Contains(classPath) || sourceTypeFullNames.Contains(classPath)) then
            []
        else
            let dependencyDiagnostics = resolveAtlaImport "dependency" dependencyModuleNames dependencyTypeFullNames
            if dependencyDiagnostics.IsEmpty && (dependencyModuleNames.Contains(classPath) || dependencyTypeFullNames.Contains(classPath)) then
                []
            elif not dependencyDiagnostics.IsEmpty then
                dependencyDiagnostics
            elif not sourceDiagnostics.IsEmpty then
                sourceDiagnostics
            else
                // TODO: 今はSystem.Typeのみをサポートしているが、将来的にはユーザー定義型やモジュールもサポートする必要がある
                match scope.ResolveType(shortName) with
                | Some _ -> []
                | None ->
                    let sid = declareSystemType symbolTable scope classPath

                    // declareSystemType が型を解決できたか確認する。
                    // 解決できなかった場合（sysType が null）は依存関係の設定不備を示す Warning を返す。
                    match symbolTable.Get(sid) with
                    | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) } when isNull sysType ->
                        [ Diagnostic.Warning(
                              sprintf
                                  "import '%s': type could not be resolved from loaded assemblies. Ensure the dependency providing this type is listed in atla.yaml and has been restored."
                                  classPath,
                              importDecl.span) ]
                    | _ -> []

    let resolveModuleWithImports
        (
            symbolTable: SymbolTable,
            moduleName: string,
            moduleAst: Ast.Module,
            sourceModuleNames: Set<string>,
            sourceTypeFullNames: Set<string>,
            dependencyModuleNames: Set<string>,
            dependencyTypeFullNames: Set<string>
        )
        : PhaseResult<ResolvedModule> =
        let moduleScope = Scope(None)
        moduleScope.DeclareType("Unit", TypeId.Unit)
        moduleScope.DeclareType("Bool", TypeId.Bool)
        moduleScope.DeclareType("Int", TypeId.Int)
        moduleScope.DeclareType("Float", TypeId.Float)
        moduleScope.DeclareType("Double", TypeId.Double)
        moduleScope.DeclareType("String", TypeId.String)
        declareSystemType symbolTable moduleScope "System.Int32" |> ignore

        // List は型位置を AnalyzeEnv.resolveTypeExpr の特別扱いで、値位置（空構築 `List.`）を
        // 組込関数 List（SymbolInfo.builtinFunctions）で解決する。ここでの明示登録は不要。

        symbolTable.BuiltinOperators
        |> List.iter (fun (name, sid) -> moduleScope.DeclareVar(name, sid))

        symbolTable.BuiltinFunctions
        |> List.iter (fun (name, sid) -> moduleScope.DeclareVar(name, sid))

        let fnDecls = ResizeArray<Ast.Decl.Fn>()
        let dataDecls = ResizeArray<ResolvedDataDecl>()
        let unionDecls = ResizeArray<ResolvedUnionDecl>()
        // 完全修飾 union 名 → (ルート型 SID, 親 union SID option, union 宣言 AST)。
        // ネスト union も含めて第一パスで再帰登録する。
        let unionRootsByName = System.Collections.Generic.Dictionary<string, SymbolId * SymbolId option * Ast.Decl.Union>()
        // 完全修飾 union 名 → 直下バリアントの単純名 → バリアント型 SID。本体内・外部の両バリアントを蓄積する。
        let unionVariantSids = System.Collections.Generic.Dictionary<string, System.Collections.Generic.Dictionary<string, SymbolId>>()
        // 完全修飾 union 名 → 外部バリアント宣言（struct/object）の蓄積。
        let unionExternalVariants = System.Collections.Generic.Dictionary<string, ResizeArray<Ast.Decl>>()
        // 単純 union 名 → 完全修飾名のリスト。外部バリアントが単純名で親 union を参照する際の解決に使う。
        let unionQualifiedBySimple = System.Collections.Generic.Dictionary<string, ResizeArray<string>>()
        let roleDecls = ResizeArray<ResolvedRoleDecl>()
        let implDecls = ResizeArray<SymbolId * TypeId option * string option * Ast.Decl.Impl>()
        let importedModules = ResizeArray<string * string>()
        let importedTypeAliases = ResizeArray<string * string>()
        // atlalib 型インポート時に登録したプレースホルダー SID のリスト。
        // `impl ImportedType` の isNominalType チェックでこれらを許容するために使う。
        let importedAtlaNominalTypeSids = ResizeArray<SymbolId>()
        let diagnostics = ResizeArray<Diagnostic>()

        // union を再帰的に登録する。qualifiedPrefix は親までの修飾名（トップでは空文字）。
        // baseUnionSidOpt はネスト union の親 union 型 SID（トップでは None）。
        // ルート型と各直下バリアント（struct/object/ネスト union）を完全修飾名で型スコープへ登録し、
        // ネスト union は再帰的に処理する。
        let rec registerUnion (qualifiedPrefix: string) (baseUnionSidOpt: SymbolId option) (unionDecl: Ast.Decl.Union) =
            let qualifiedName =
                if qualifiedPrefix = "" then unionDecl.name
                else sprintf "%s'%s" qualifiedPrefix unionDecl.name
            let alreadyDefined =
                if qualifiedPrefix = "" then moduleScope.ResolveType(unionDecl.name) |> Option.isSome
                else unionRootsByName.ContainsKey(qualifiedName)
            if alreadyDefined then
                diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' is already defined" qualifiedName, unionDecl.span))
            else
                let typeSid = symbolTable.NextId()
                symbolTable.Add(typeSid, { name = qualifiedName; typ = TypeId.Name typeSid; kind = SymbolKind.Local() })
                moduleScope.DeclareType(qualifiedName, TypeId.Name typeSid)

                let variantSids = System.Collections.Generic.Dictionary<string, SymbolId>()
                for variant in unionDecl.variants do
                    match variant with
                    | :? Ast.Decl.Data as structVariant ->
                        let q = sprintf "%s'%s" qualifiedName structVariant.name
                        let vSid = symbolTable.NextId()
                        symbolTable.Add(vSid, { name = q; typ = TypeId.Name vSid; kind = SymbolKind.Local() })
                        moduleScope.DeclareType(q, TypeId.Name vSid)
                        variantSids.[structVariant.name] <- vSid
                    | :? Ast.Decl.Object as objVariant ->
                        let q = sprintf "%s'%s" qualifiedName objVariant.name
                        let vSid = symbolTable.NextId()
                        symbolTable.Add(vSid, { name = q; typ = TypeId.Name vSid; kind = SymbolKind.Local() })
                        moduleScope.DeclareType(q, TypeId.Name vSid)
                        variantSids.[objVariant.name] <- vSid
                    | :? Ast.Decl.Union as nestedUnion ->
                        // ネスト union は再帰登録する。バリアントとしての SID は登録後に取得する。
                        registerUnion qualifiedName (Some typeSid) nestedUnion
                        let nestedQ = sprintf "%s'%s" qualifiedName nestedUnion.name
                        match unionRootsByName.TryGetValue(nestedQ) with
                        | true, (nestedSid, _, _) -> variantSids.[nestedUnion.name] <- nestedSid
                        | false, _ -> ()
                    | _ -> ()

                unionRootsByName.[qualifiedName] <- (typeSid, baseUnionSidOpt, unionDecl)
                unionVariantSids.[qualifiedName] <- variantSids
                unionExternalVariants.[qualifiedName] <- ResizeArray<Ast.Decl>()
                match unionQualifiedBySimple.TryGetValue(unionDecl.name) with
                | true, lst -> lst.Add(qualifiedName)
                | false, _ ->
                    let lst = ResizeArray<string>()
                    lst.Add(qualifiedName)
                    unionQualifiedBySimple.[unionDecl.name] <- lst

        // data / enum / role 型名を先に登録し、同一モジュール内で相互参照可能にする。
        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Data as dataDecl when dataDecl.baseUnionName.IsNone ->
                match moduleScope.ResolveType(dataDecl.name) with
                | Some _ ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' is already defined" dataDecl.name, dataDecl.span))
                | None ->
                    let typeSid = symbolTable.NextId()
                    symbolTable.Add(typeSid, { name = dataDecl.name; typ = TypeId.Name typeSid; kind = SymbolKind.Local() })
                    moduleScope.DeclareType(dataDecl.name, TypeId.Name typeSid)
                    dataDecls.Add({ typeSid = typeSid; typeParams = dataDecl.typeParams; decl = dataDecl })
            // `struct V: Union` / `object V: Union` 形式の外部バリアントは第一パスでは登録しない。
            // union ルートの登録後に処理する必要があるため、後続の専用パスで扱う。
            | :? Ast.Decl.Data -> ()
            | :? Ast.Decl.Object -> ()
            | :? Ast.Decl.Union as unionDecl ->
                registerUnion "" None unionDecl
            | :? Ast.Decl.Role as roleDecl ->
                // role 型を型スコープへ事前登録し、同一モジュール内での型注釈で参照可能にする。
                match moduleScope.ResolveType(roleDecl.name) with
                | Some _ ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' is already defined" roleDecl.name, roleDecl.span))
                | None ->
                    let typeSid = symbolTable.NextId()
                    symbolTable.Add(typeSid, { name = roleDecl.name; typ = TypeId.Name typeSid; kind = SymbolKind.Local() })
                    moduleScope.DeclareType(roleDecl.name, TypeId.Name typeSid)
                    roleDecls.Add({ typeSid = typeSid; decl = roleDecl })
            | _ -> ()

        // 外部バリアントパス: union 本体外で宣言された `struct V: Union` / `object V: Union` を処理する。
        // - 対象 union が存在し、かつ extendable であることを検証する（sealed への外部追加はエラー）。
        // - バリアント型を修飾名 `Union'V` で登録し、unionExternalVariants へ蓄積する。
        let registerExternalVariant (unionRef: string) (variantName: string) (span: Atla.Core.Data.Span) (variantDecl: Ast.Decl) =
            // 親 union 参照は完全修飾名（"Color'HueColor"）か単純名（"Color"）のいずれか。
            // 完全修飾で見つからない場合、単純名から一意な完全修飾名へ解決する。
            let resolvedUnionName =
                if unionRootsByName.ContainsKey(unionRef) then Some unionRef
                else
                    match unionQualifiedBySimple.TryGetValue(unionRef) with
                    | true, lst when lst.Count = 1 -> Some lst.[0]
                    | true, lst when lst.Count > 1 ->
                        diagnostics.Add(Diagnostic.Error(sprintf "Ambiguous union '%s' for variant '%s'; use a fully-qualified name" unionRef variantName, span))
                        None
                    | _ -> None
            match resolvedUnionName with
            | None ->
                diagnostics.Add(Diagnostic.Error(sprintf "Undefined union '%s' for variant '%s'" unionRef variantName, span))
            | Some unionName ->
                let (_, _, unionAst) = unionRootsByName.[unionName]
                if not unionAst.isExtendable then
                    diagnostics.Add(Diagnostic.Error(sprintf "Cannot add external variant '%s' to sealed union '%s'; mark the union 'extendable'" variantName unionName, span))
                else
                    let variantSids = unionVariantSids.[unionName]
                    if variantSids.ContainsKey(variantName) then
                        diagnostics.Add(Diagnostic.Error(sprintf "Variant '%s' is already defined for union '%s'" variantName unionName, span))
                    else
                        let qualifiedName = sprintf "%s'%s" unionName variantName
                        let variantSid = symbolTable.NextId()
                        symbolTable.Add(variantSid, { name = qualifiedName; typ = TypeId.Name variantSid; kind = SymbolKind.Local() })
                        moduleScope.DeclareType(qualifiedName, TypeId.Name variantSid)
                        variantSids.[variantName] <- variantSid
                        unionExternalVariants.[unionName].Add(variantDecl)

        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Data as structVariant ->
                match structVariant.baseUnionName with
                | Some unionName -> registerExternalVariant unionName structVariant.name structVariant.span decl
                | None -> ()
            | :? Ast.Decl.Object as objVariant ->
                registerExternalVariant objVariant.baseUnionName objVariant.name objVariant.span decl
            | _ -> ()

        // union 解決結果を確定する（本体内 + 外部バリアントの SID マップと外部バリアント AST を含む）。
        // 修飾名の深さ（'区切りセグメント数）昇順で確定することで、親 union が常にネスト union より
        // 先に並ぶ。これにより後段の型生成（Gen.CreateType）で基底型が派生型より先に確定し、
        // ネスト union の継承（Hsv -> HueColor -> Color）が正しく構築される。
        let orderedUnionNames =
            unionRootsByName.Keys
            |> Seq.sortBy (fun q -> q.Split('\'').Length)
            |> Seq.toList
        for qualifiedName in orderedUnionNames do
            let (typeSid, baseUnionSidOpt, unionAst) = unionRootsByName.[qualifiedName]
            let variantSids = unionVariantSids.[qualifiedName] |> Seq.map (fun e -> e.Key, e.Value) |> Map.ofSeq
            let externalVariants = unionExternalVariants.[qualifiedName] |> List.ofSeq
            unionDecls.Add({ typeSid = typeSid; typeParams = unionAst.typeParams; qualifiedName = qualifiedName; baseUnionSid = baseUnionSidOpt; variantSids = variantSids; externalVariants = externalVariants; decl = unionAst })

        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Import as importDecl ->
                let importDiagnostics =
                    resolveImport
                        symbolTable
                        moduleScope
                        sourceModuleNames
                        sourceTypeFullNames
                        dependencyModuleNames
                        dependencyTypeFullNames
                        importedModules
                        importedTypeAliases
                        importedAtlaNominalTypeSids
                        importDecl
                diagnostics.AddRange(importDiagnostics)
            | :? Ast.Decl.Data ->
                ()
            | :? Ast.Decl.Union ->
                // union 宣言は第一パスで型名・バリアント名を登録済みのため、ここでは何もしない。
                ()
            | :? Ast.Decl.Object ->
                // 外部 object バリアント宣言（union 本体外）は Phase 2 で対応する。
                // Phase 1 では union 本体内の object のみを扱い、ここでは何もしない。
                ()
            | :? Ast.Decl.Impl as implDecl ->
                let implTargetTypeName = implDecl.forTypeName |> Option.defaultValue implDecl.typeName
                let typeResolution = moduleScope.ResolveType(implTargetTypeName)
                match typeResolution with
                | Some (TypeId.Name typeSid) ->
                    let isNominalType =
                        dataDecls
                        |> Seq.exists (fun dataDecl -> dataDecl.typeSid.id = typeSid.id)
                        || unionDecls
                        |> Seq.exists (fun unionDecl -> unionDecl.typeSid.id = typeSid.id)
                        // atlalib インポート型（プレースホルダー SID）も `impl` の対象として許容する。
                        || importedAtlaNominalTypeSids
                        |> Seq.exists (fun importedSid -> importedSid.id = typeSid.id)
                    if not isNominalType then
                        diagnostics.Add(Diagnostic.Error(sprintf "impl target '%s' must be a data or enum type in this module" implTargetTypeName, implDecl.span))
                    else
                        // `impl A as DotNetClass` の `as` 句を解決し .NET 基底型を返す。
                        // `impl B for A` の `for` 句は「A が B のサブタイプ」を意味するため、
                        // `B` を基底型として解決して保持する。
                        let resolvedBaseTypeOpt =
                            match implDecl.asTypeName, implDecl.forTypeName with
                            | Some asTypeName, _ ->
                                // `impl A as B` 形式: B は import 済みの .NET クラスでなければならない。
                                match moduleScope.ResolveType(asTypeName) with
                                | Some (TypeId.Name asSid) ->
                                    match symbolTable.Get(asSid) with
                                    | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) } when not (obj.ReferenceEquals(sysType, null)) ->
                                        if sysType.IsInterface then
                                            diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' as '%s': cannot inherit from interface type '%s'" implDecl.typeName asTypeName sysType.FullName, implDecl.span))
                                            None
                                        elif sysType.IsSealed then
                                            diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' as '%s': cannot inherit from sealed class '%s'" implDecl.typeName asTypeName sysType.FullName, implDecl.span))
                                            None
                                        elif not sysType.IsClass then
                                            diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' as '%s': type '%s' is not a class" implDecl.typeName asTypeName sysType.FullName, implDecl.span))
                                            None
                                        else
                                            Some (TypeId.Native sysType)
                                    | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef _) } ->
                                        // sysType が null（解決失敗）のケース。
                                        diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' as '%s': the .NET type could not be resolved. Ensure it is imported and the dependency is restored." implDecl.typeName asTypeName, implDecl.span))
                                        None
                                    | _ ->
                                        diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' as '%s': '%s' is not an imported .NET type" implDecl.typeName asTypeName asTypeName, implDecl.span))
                                        None
                                | Some _ ->
                                    diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' as '%s': '%s' must be an imported .NET type" implDecl.typeName asTypeName asTypeName, implDecl.span))
                                    None
                                | None ->
                                    diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' as '%s': type '%s' is not defined. Use 'import' to import it." implDecl.typeName asTypeName asTypeName, implDecl.span))
                                    None
                            | None, Some _ ->
                                // `impl B for A` 形式: A は Atla の data 型、B は imported .NET インターフェイスのみ許可する。
                                match moduleScope.ResolveType(implDecl.typeName) with
                                | Some (TypeId.Name baseTypeSid) ->
                                    match symbolTable.Get(baseTypeSid) with
                                    | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) } when not (obj.ReferenceEquals(sysType, null)) ->
                                        if sysType.IsInterface then
                                            Some (TypeId.Native sysType)
                                        else
                                            diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' for '%s': '%s' must be a .NET interface type" implDecl.typeName implTargetTypeName sysType.FullName, implDecl.span))
                                            None
                                    | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef _) } ->
                                        diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' for '%s': the .NET type could not be resolved. Ensure it is imported and the dependency is restored." implDecl.typeName implTargetTypeName, implDecl.span))
                                        None
                                    | _ ->
                                        Some (TypeId.Name baseTypeSid)
                                | Some (TypeId.Native sysType) when not (obj.ReferenceEquals(sysType, null)) ->
                                    if sysType.IsInterface then
                                        Some (TypeId.Native sysType)
                                    else
                                        diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' for '%s': '%s' must be a .NET interface type" implDecl.typeName implTargetTypeName sysType.FullName, implDecl.span))
                                        None
                                | Some (TypeId.Native _) ->
                                    diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' for '%s': the .NET type could not be resolved. Ensure it is imported and the dependency is restored." implDecl.typeName implTargetTypeName, implDecl.span))
                                    None
                                | Some _ ->
                                    diagnostics.Add(Diagnostic.Error(sprintf "Unsupported impl base type '%s'" implDecl.typeName, implDecl.span))
                                    None
                                | None ->
                                    diagnostics.Add(Diagnostic.Error(sprintf "Undefined impl base type '%s'" implDecl.typeName, implDecl.span))
                                    None
                            | None, None -> None

                        if implDecl.byFieldName.IsSome && implDecl.forTypeName.IsNone && implDecl.asTypeName.IsNone then
                            diagnostics.Add(Diagnostic.Error("'impl ... by ...' requires an explicit 'for' base type", implDecl.span))

                        let byFieldNameOpt =
                            match implDecl.byFieldName with
                            | None -> None
                            | Some byFieldName ->
                                let hasField =
                                    dataDecls
                                    |> Seq.tryFind (fun dataDecl -> dataDecl.typeSid.id = typeSid.id)
                                    |> Option.map (fun dataDecl ->
                                        dataDecl.decl.items
                                        |> List.exists (fun dataItem ->
                                            match dataItem with
                                            | :? Ast.DataItem.Field as fieldDecl -> fieldDecl.name = byFieldName
                                            | _ -> false))
                                    |> Option.defaultValue false

                                if hasField then
                                    Some byFieldName
                                else
                                    diagnostics.Add(Diagnostic.Error(sprintf "Delegate field '%s' is not defined in data '%s'" byFieldName implDecl.typeName, implDecl.span))
                                    None

                        if implDecl.methods.IsEmpty && byFieldNameOpt.IsNone then
                            diagnostics.Add(Diagnostic.Error(sprintf "impl '%s' must contain at least one method" implDecl.typeName, implDecl.span))

                        let duplicateMethodName =
                            implDecl.methods
                            |> List.fold
                                (fun (seen, dup) methodDecl ->
                                    match dup with
                                    | Some _ -> seen, dup
                                    | None when Set.contains methodDecl.name seen -> seen, Some methodDecl.name
                                    | None -> Set.add methodDecl.name seen, None)
                                (Set.empty, None)
                            |> snd
                        match duplicateMethodName with
                        | Some methodName ->
                            diagnostics.Add(Diagnostic.Error(sprintf "Duplicate method '%s' in impl '%s'" methodName implDecl.typeName, implDecl.span))
                        | None -> ()

                        // `override` 修飾子は `impl A as B` 形式（asTypeName が解決済み）でのみ許可する。
                        // - `impl A` / `impl B for A` 内の override はエラー。
                        // - `impl A as B` でも as の解決に失敗（resolvedBaseTypeOpt = None）した場合はエラー（基底クラスが無いため）。
                        match implDecl.asTypeName, resolvedBaseTypeOpt with
                        | Some _, Some (TypeId.Native _) -> ()
                        | _ ->
                            for methodDecl in implDecl.methods do
                                if methodDecl.isOverride then
                                    diagnostics.Add(
                                        Diagnostic.Error(
                                            sprintf "'override' keyword is only allowed in 'impl ... as ...' blocks; method '%s' in impl '%s'"
                                                methodDecl.name implDecl.typeName,
                                            methodDecl.span))

                        // impl ブロックの個数制約:
                        // - `impl T`（for なし）は型ごとに 1 つ
                        // - `impl T as DotNetClass` は（T, DotNetClass）ごとに 1 つ
                        // - `impl T for Role` は (T, Role) ごとに 1 つ
                        let hasResolvableRoleKey =
                            match implDecl.forTypeName, resolvedBaseTypeOpt with
                            | Some _, None -> false
                            | _ -> true

                        let targetTypeSid =
                            match implDecl.forTypeName with
                            | Some subtypeTypeName ->
                                match moduleScope.ResolveType(subtypeTypeName) with
                                | Some (TypeId.Name sid) -> sid
                                | _ -> typeSid
                            | None -> typeSid

                        let hasExistingImpl =
                            if not hasResolvableRoleKey then
                                false
                            else
                                implDecls
                                |> Seq.exists (fun (sid, existingBaseTypeOpt, _, _) ->
                                    sid.id = targetTypeSid.id
                                    && existingBaseTypeOpt = resolvedBaseTypeOpt)

                        if hasExistingImpl then
                            match implDecl.asTypeName, implDecl.forTypeName with
                            | Some asName, _ ->
                                diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' already has an impl block for .NET base type '%s'" implDecl.typeName asName, implDecl.span))
                            | _, Some roleName ->
                                let targetTypeName = implDecl.forTypeName |> Option.defaultValue implDecl.typeName
                                diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' already has an impl block for role '%s'" targetTypeName roleName, implDecl.span))
                            | None, None ->
                                diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' already has a default impl block" implDecl.typeName, implDecl.span))
                        elif hasResolvableRoleKey then
                            implDecls.Add(targetTypeSid, resolvedBaseTypeOpt, byFieldNameOpt, implDecl)
                | Some _ ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Unsupported impl target '%s'" implDecl.typeName, implDecl.span))
                | None ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Undefined impl target type '%s'" implDecl.typeName, implDecl.span))
            | :? Ast.Decl.Fn as fnDecl ->
                fnDecls.Add(fnDecl)
            | :? Ast.Decl.Role ->
                // role 宣言は第一パスで登録済みのため、ここでは何もしない。
                ()
            | _ -> diagnostics.Add(Diagnostic.Error("Unsupported declaration type in module", decl.span))

        // data 型の継承関係（impl B for A）に循環がないことを検証する。
        // `TypeId.Native`（.NET 継承）は Atla 型チェーンに含まれないため除外する。
        let implBaseMap =
            implDecls
            |> Seq.choose (fun (typeSid, baseTypeOpt, _, _) ->
                match baseTypeOpt with
                | Some (TypeId.Name baseSid) -> Some (typeSid, baseSid)
                | _ -> None)
            |> Map.ofSeq

        let hasInheritanceCycle (startSid: SymbolId) : bool =
            let rec loop (visited: Set<int>) (currentSid: SymbolId) =
                if visited |> Set.contains currentSid.id then
                    true
                else
                    match implBaseMap |> Map.tryFind currentSid with
                    | Some nextSid -> loop (visited |> Set.add currentSid.id) nextSid
                    | None -> false
            loop Set.empty startSid

        for (typeSid, _, _, implDecl) in implDecls do
            if hasInheritanceCycle typeSid then
                diagnostics.Add(Diagnostic.Error(sprintf "Cyclic subtype relation detected for '%s'" implDecl.typeName, implDecl.span))

        // Warning 診断があっても解析は続行する。Error 診断がある場合のみ失敗とする。
        let allDiagnostics = Seq.toList diagnostics
        if allDiagnostics |> List.exists (fun d -> d.isError) then
            PhaseResult.failed allDiagnostics
        else
            PhaseResult.succeeded
                { moduleName = moduleName
                  moduleScope = moduleScope
                  importedModules = importedModules |> Seq.distinct |> Map.ofSeq
                  importedTypeAliases = importedTypeAliases |> Seq.distinct |> Map.ofSeq
                  fnDecls = Seq.toList fnDecls
                  dataDecls = Seq.toList dataDecls
                  unionDecls = Seq.toList unionDecls
                  roleDecls = Seq.toList roleDecls
                  implDecls = Seq.toList implDecls }
                allDiagnostics

    /// 既存呼び出し向け互換 API。Atla モジュール import 判定は行わない。
    let resolveModule (symbolTable: SymbolTable, moduleName: string, moduleAst: Ast.Module) : PhaseResult<ResolvedModule> =
        resolveModuleWithImports (symbolTable, moduleName, moduleAst, Set.empty, Set.empty, Set.empty, Set.empty)
