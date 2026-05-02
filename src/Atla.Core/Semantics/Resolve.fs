namespace Atla.Core.Semantics

open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open System.Runtime.Loader

module Resolve =
    type ResolvedDataDecl =
        { typeSid: SymbolId
          decl: Ast.Decl.Data }

    type ResolvedModule =
        { moduleName: string
          moduleScope: Scope
          importedModules: Map<string, string>
          importedTypeAliases: Map<string, string>
          fnDecls: Ast.Decl.Fn list
          dataDecls: ResolvedDataDecl list
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
        (availableModuleNames: Set<string>)
        (availableTypeFullNames: Set<string>)
        (importedModules: ResizeArray<string * string>)
        (importedTypeAliases: ResizeArray<string * string>)
        (importDecl: Ast.Decl.Import)
        : Diagnostic list =
        let classPath = String.concat "." importDecl.path
        let shortName = Array.last (classPath.Split('.'))
        if availableModuleNames.Contains(classPath) then
            importedModules.Add(shortName, classPath)
            []
        elif availableTypeFullNames.Contains(classPath) then
            // 型 import: Analyze で実体 data 宣言へ接続するため、ここでは alias 情報のみ保持する。
            importedTypeAliases.Add(shortName, classPath)
            []
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
        (symbolTable: SymbolTable, moduleName: string, moduleAst: Ast.Module, availableModuleNames: Set<string>, availableTypeFullNames: Set<string>)
        : PhaseResult<ResolvedModule> =
        let moduleScope = Scope(None)
        moduleScope.DeclareType("Unit", TypeId.Unit)
        moduleScope.DeclareType("Bool", TypeId.Bool)
        moduleScope.DeclareType("Int", TypeId.Int)
        moduleScope.DeclareType("Float", TypeId.Float)
        moduleScope.DeclareType("String", TypeId.String)
        declareSystemType symbolTable moduleScope "System.Int32" |> ignore

        symbolTable.BuiltinOperators
        |> List.iter (fun (name, sid) -> moduleScope.DeclareVar(name, sid))

        let fnDecls = ResizeArray<Ast.Decl.Fn>()
        let dataDecls = ResizeArray<ResolvedDataDecl>()
        let implDecls = ResizeArray<SymbolId * TypeId option * string option * Ast.Decl.Impl>()
        let importedModules = ResizeArray<string * string>()
        let importedTypeAliases = ResizeArray<string * string>()
        let diagnostics = ResizeArray<Diagnostic>()

        // data 型名を先に登録し、同一モジュール内で相互参照可能にする。
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
                    dataDecls.Add({ typeSid = typeSid; decl = dataDecl })
            | _ -> ()

        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Import as importDecl ->
                let importDiagnostics =
                    resolveImport symbolTable moduleScope availableModuleNames availableTypeFullNames importedModules importedTypeAliases importDecl
                diagnostics.AddRange(importDiagnostics)
            | :? Ast.Decl.Data ->
                ()
            | :? Ast.Decl.Impl as implDecl ->
                let typeResolution = moduleScope.ResolveType(implDecl.typeName)
                match typeResolution with
                | Some (TypeId.Name typeSid) ->
                    let isDataType =
                        dataDecls
                        |> Seq.exists (fun dataDecl -> dataDecl.typeSid.id = typeSid.id)
                    if not isDataType then
                        diagnostics.Add(Diagnostic.Error(sprintf "impl target '%s' must be a data type in this module" implDecl.typeName, implDecl.span))
                    else
                        // `impl A as DotNetClass` の `as` 句を解決し .NET 基底型を返す。
                        // `impl B for A` の `for` 句を解決し Atla 基底型を返す。
                        // 結果は TypeId option として統一して保持する。
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
                            | None, Some forTypeName ->
                                // `impl B for A` 形式: A は Atla の data 型でなければならない。
                                match moduleScope.ResolveType(forTypeName) with
                                | Some (TypeId.Name baseTypeSid) -> Some (TypeId.Name baseTypeSid)
                                | Some _ ->
                                    diagnostics.Add(Diagnostic.Error(sprintf "Unsupported impl base type '%s'" forTypeName, implDecl.span))
                                    None
                                | None ->
                                    diagnostics.Add(Diagnostic.Error(sprintf "Undefined impl base type '%s'" forTypeName, implDecl.span))
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

                        // impl ブロックの個数制約:
                        // - `impl T`（for なし）は型ごとに 1 つ
                        // - `impl T as DotNetClass` は（T, DotNetClass）ごとに 1 つ
                        // - `impl T for Role` は (T, Role) ごとに 1 つ
                        let hasResolvableRoleKey =
                            match implDecl.forTypeName, resolvedBaseTypeOpt with
                            | Some _, None -> false
                            | _ -> true

                        let hasExistingImpl =
                            if not hasResolvableRoleKey then
                                false
                            else
                                implDecls
                                |> Seq.exists (fun (sid, existingBaseTypeOpt, _, _) ->
                                    sid.id = typeSid.id
                                    && existingBaseTypeOpt = resolvedBaseTypeOpt)

                        if hasExistingImpl then
                            match implDecl.asTypeName, implDecl.forTypeName with
                            | Some asName, _ ->
                                diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' already has an impl block for .NET base type '%s'" implDecl.typeName asName, implDecl.span))
                            | _, Some roleName ->
                                diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' already has an impl block for role '%s'" implDecl.typeName roleName, implDecl.span))
                            | None, None ->
                                diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' already has a default impl block" implDecl.typeName, implDecl.span))
                        elif hasResolvableRoleKey then
                            implDecls.Add(typeSid, resolvedBaseTypeOpt, byFieldNameOpt, implDecl)
                | Some _ ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Unsupported impl target '%s'" implDecl.typeName, implDecl.span))
                | None ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Undefined impl target type '%s'" implDecl.typeName, implDecl.span))
            | :? Ast.Decl.Fn as fnDecl ->
                fnDecls.Add(fnDecl)
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
                  implDecls = Seq.toList implDecls }
                allDiagnostics

    /// 既存呼び出し向け互換 API。Atla モジュール import 判定は行わない。
    let resolveModule (symbolTable: SymbolTable, moduleName: string, moduleAst: Ast.Module) : PhaseResult<ResolvedModule> =
        resolveModuleWithImports (symbolTable, moduleName, moduleAst, Set.empty, Set.empty)
