namespace Atla.Core.Semantics

open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open System.Runtime.Loader

module Resolve =
    type ResolvedDataDecl =
        { typeSid: SymbolId
          typeParams: string list
          decl: Ast.Decl.Data }

    type ResolvedEnumDecl =
        { typeSid: SymbolId
          typeParams: string list
          decl: Ast.Decl.Enum }

    /// 解決済み union 宣言。variantSids は修飾なしバリアント名 → バリアント型 SymbolId のマップ。
    type ResolvedUnionDecl =
        { typeSid: SymbolId
          typeParams: string list
          variantSids: Map<string, SymbolId>
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
          enumDecls: ResolvedEnumDecl list
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
        let enumDecls = ResizeArray<ResolvedEnumDecl>()
        let unionDecls = ResizeArray<ResolvedUnionDecl>()
        let roleDecls = ResizeArray<ResolvedRoleDecl>()
        let implDecls = ResizeArray<SymbolId * TypeId option * string option * Ast.Decl.Impl>()
        let importedModules = ResizeArray<string * string>()
        let importedTypeAliases = ResizeArray<string * string>()
        let diagnostics = ResizeArray<Diagnostic>()

        // data / enum / role 型名を先に登録し、同一モジュール内で相互参照可能にする。
        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Data as dataDecl ->
                match moduleScope.ResolveType(dataDecl.name) with
                | Some _ ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' is already defined" dataDecl.name, dataDecl.span))
                | None ->
                    let typeSid = symbolTable.NextId()
                    symbolTable.Add(typeSid, { name = dataDecl.name; typ = TypeId.Name typeSid; kind = SymbolKind.Local() })
                    moduleScope.DeclareType(dataDecl.name, TypeId.Name typeSid)
                    dataDecls.Add({ typeSid = typeSid; typeParams = dataDecl.typeParams; decl = dataDecl })
            | :? Ast.Decl.Enum as enumDecl ->
                match moduleScope.ResolveType(enumDecl.name) with
                | Some _ ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' is already defined" enumDecl.name, enumDecl.span))
                | None ->
                    let typeSid = symbolTable.NextId()
                    symbolTable.Add(typeSid, { name = enumDecl.name; typ = TypeId.Name typeSid; kind = SymbolKind.Local() })
                    moduleScope.DeclareType(enumDecl.name, TypeId.Name typeSid)
                    enumDecls.Add({ typeSid = typeSid; typeParams = enumDecl.typeParams; decl = enumDecl })
            | :? Ast.Decl.Union as unionDecl ->
                match moduleScope.ResolveType(unionDecl.name) with
                | Some _ ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' is already defined" unionDecl.name, unionDecl.span))
                | None ->
                    // union ルート型を型スコープへ登録する。
                    let typeSid = symbolTable.NextId()
                    symbolTable.Add(typeSid, { name = unionDecl.name; typ = TypeId.Name typeSid; kind = SymbolKind.Local() })
                    moduleScope.DeclareType(unionDecl.name, TypeId.Name typeSid)

                    // 各バリアントの型を修飾名（`Union'Variant`）で登録する。
                    // Phase 1 は単層のみ対応し、ネスト union（Ast.Decl.Union）はバリアントとしてまだ扱わない。
                    let variantSids =
                        unionDecl.variants
                        |> List.choose (fun variant ->
                            let variantNameOpt =
                                match variant with
                                | :? Ast.Decl.Data as structVariant -> Some structVariant.name
                                | :? Ast.Decl.Object as objVariant -> Some objVariant.name
                                | _ -> None
                            variantNameOpt
                            |> Option.map (fun variantName ->
                                let qualifiedName = sprintf "%s'%s" unionDecl.name variantName
                                let variantSid = symbolTable.NextId()
                                symbolTable.Add(variantSid, { name = qualifiedName; typ = TypeId.Name variantSid; kind = SymbolKind.Local() })
                                moduleScope.DeclareType(qualifiedName, TypeId.Name variantSid)
                                variantName, variantSid))
                        |> Map.ofList

                    unionDecls.Add({ typeSid = typeSid; typeParams = unionDecl.typeParams; variantSids = variantSids; decl = unionDecl })
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
                        importDecl
                diagnostics.AddRange(importDiagnostics)
            | :? Ast.Decl.Data ->
                ()
            | :? Ast.Decl.Enum ->
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
                        || enumDecls
                        |> Seq.exists (fun enumDecl -> enumDecl.typeSid.id = typeSid.id)
                        || unionDecls
                        |> Seq.exists (fun unionDecl -> unionDecl.typeSid.id = typeSid.id)
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
                  enumDecls = Seq.toList enumDecls
                  unionDecls = Seq.toList unionDecls
                  roleDecls = Seq.toList roleDecls
                  implDecls = Seq.toList implDecls }
                allDiagnostics

    /// 既存呼び出し向け互換 API。Atla モジュール import 判定は行わない。
    let resolveModule (symbolTable: SymbolTable, moduleName: string, moduleAst: Ast.Module) : PhaseResult<ResolvedModule> =
        resolveModuleWithImports (symbolTable, moduleName, moduleAst, Set.empty, Set.empty, Set.empty, Set.empty)
