namespace Atla.Compiler

open System
open System.IO
open System.IO.Compression
open System.Reflection
open System.Runtime.Loader
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Atla.Core.Data
open Atla.Core.Semantics.Data
open Atla.Core.Semantics.Data.AnalyzeEnv

module AtlaLib =
    let formatVersion = "1.0"
    let languageAbi = "atla-abi-1"
    let symbolSchemaVersion = "1.0"

    type ImportedDependencyIndex =
        { moduleNames: Set<string>
          typeFullNames: Set<string>
          moduleExports: Map<string, Map<string, ModuleExport>>
          typeDefsByFullName: Map<string, DataTypeDef>
          diagnostics: Diagnostic list }

    type ResolvedRuntimeAssets =
        { packageName: string
          packageVersion: string
          assemblyPath: string
          runtimeLoadPaths: string list
          nativeRuntimePaths: string list }

    type private PredeclaredType =
        { moduleName: string
          typeName: string
          fullName: string
          element: JsonElement
          kind: string
          typeSid: SymbolId
          systemType: Type option }

    type private LoadedArchive =
        { metadata: JsonDocument
          publicApi: JsonDocument }

    /// 文字列のメジャーバージョン部分を取得する。
    let private tryGetMajorVersion (version: string) : int option =
        match version.Split('.', StringSplitOptions.RemoveEmptyEntries) |> Array.tryHead with
        | Some head ->
            match Int32.TryParse(head) with
            | true, major -> Some major
            | _ -> None
        | None -> None

    /// バージョン文字列が同じメジャーバージョンかを検証する。
    let private isCompatibleMajorVersion (expected: string) (actual: string) : bool =
        match tryGetMajorVersion expected, tryGetMajorVersion actual with
        | Some expectedMajor, Some actualMajor -> expectedMajor = actualMajor
        | _ -> String.Equals(expected, actual, StringComparison.Ordinal)

    /// バイト列の SHA-256 ハッシュを16進小文字文字列へ変換する。
    let private computeSha256Hex (bytes: byte[]) : string =
        use sha = SHA256.Create()
        sha.ComputeHash(bytes)
        |> Array.map (fun b -> b.ToString("x2"))
        |> String.concat ""

    /// Zip エントリの内容をすべて読み込む。
    let private readEntryBytes (entry: ZipArchiveEntry) : byte[] =
        use stream = entry.Open()
        use buffer = new MemoryStream()
        stream.CopyTo(buffer)
        buffer.ToArray()

    /// `.atlalib` 抽出用の決定的なテンポラリディレクトリ名を生成する。
    let private buildExtractionRoot (atlaLibPath: string) : string =
        let fingerprint =
            let lastWrite = File.GetLastWriteTimeUtc(atlaLibPath).Ticks
            Encoding.UTF8.GetBytes($"{Path.GetFullPath(atlaLibPath)}:{lastWrite}")
            |> computeSha256Hex
        Path.Join(Path.GetTempPath(), $"atla-atlalib-{fingerprint}")

    /// ロード済みアセンブリ群から完全修飾名で .NET 型を検索する。
    let private tryResolveLoadedType (fullName: string) : Type option =
        let candidates =
            let lastDot = fullName.LastIndexOf('.')
            if lastDot > 0 then [ fullName; fullName.Substring(lastDot + 1) ] else [ fullName ]

        let tryFindInAssemblies (typeName: string) =
            match Type.GetType(typeName, false) with
            | null ->
                seq {
                    yield! AppDomain.CurrentDomain.GetAssemblies()

                    for context in AssemblyLoadContext.All do
                        yield! context.Assemblies
                }
                |> Seq.distinct
                |> Seq.tryPick (fun asm ->
                    match asm.GetType(typeName, false) with
                    | null -> None
                    | resolved -> Some resolved)
            | resolved ->
                Some resolved

        candidates
        |> List.tryPick tryFindInAssemblies
        |> Option.toObj
        |> function
        | null ->
            None
        | resolved -> Some resolved

    /// JSON オブジェクトの必須文字列プロパティを取得する。
    let private tryGetRequiredString (fieldPath: string) (element: JsonElement) : Result<string, Diagnostic list> =
        match element.ValueKind with
        | JsonValueKind.String ->
            let value = element.GetString()
            if String.IsNullOrWhiteSpace value then
                Result.Error [ Diagnostic.Error($"`{fieldPath}` must not be empty", Span.Empty) ]
            else
                Ok value
        | _ ->
            Result.Error [ Diagnostic.Error($"`{fieldPath}` must be a string", Span.Empty) ]

    /// JSON オブジェクトの必須プロパティを取得する。
    let private tryGetRequiredProperty (fieldPath: string) (name: string) (element: JsonElement) : Result<JsonElement, Diagnostic list> =
        let mutable value = Unchecked.defaultof<JsonElement>
        if element.TryGetProperty(name, &value) then
            Ok value
        else
            Result.Error [ Diagnostic.Error($"missing required field `{fieldPath}.{name}`", Span.Empty) ]

    /// 依存 `.atlalib` の最小メタデータ・整合性を検証し、必要 JSON を返す。
    let private tryOpenArchive (atlaLibPath: string) : Result<LoadedArchive, Diagnostic list> =
        try
            use archive = ZipFile.OpenRead(atlaLibPath)

            let requiredEntries =
                [ "atlalib.json"
                  "symbols/public.api.json"
                  "hashes/sha256sums.txt" ]

            let missingEntries =
                requiredEntries
                |> List.filter (fun entryPath -> isNull (archive.GetEntry(entryPath)))

            if not missingEntries.IsEmpty then
                let entriesText = String.Join(", ", missingEntries)
                Result.Error [ Diagnostic.Error($"dependency `.atlalib` is missing required entries: {entriesText}", Span.Empty) ]
            else
                let hashEntry = archive.GetEntry("hashes/sha256sums.txt")
                let expectedHashes =
                    readEntryBytes hashEntry
                    |> Encoding.UTF8.GetString
                    |> fun text -> text.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.choose (fun line ->
                        let trimmed = line.Trim()
                        let parts = trimmed.Split("  ", 2, StringSplitOptions.None)
                        if parts.Length = 2 then Some(parts.[1], parts.[0]) else None)
                    |> Map.ofArray

                let hashDiagnostics =
                    requiredEntries
                    |> List.filter (fun entryPath -> entryPath <> "hashes/sha256sums.txt")
                    |> List.choose (fun entryPath ->
                        match archive.GetEntry(entryPath) with
                        | null -> None
                        | entry ->
                            let bytes = readEntryBytes entry
                            let actualHash = computeSha256Hex bytes
                            match expectedHashes.TryFind entryPath with
                            | Some expectedHash when String.Equals(expectedHash, actualHash, StringComparison.OrdinalIgnoreCase) -> None
                            | Some _ -> Some(Diagnostic.Error($"dependency `.atlalib` hash mismatch: {entryPath}", Span.Empty))
                            | None -> Some(Diagnostic.Error($"dependency `.atlalib` hash entry is missing: {entryPath}", Span.Empty)))

                if not hashDiagnostics.IsEmpty then
                    Result.Error hashDiagnostics
                else
                    let metadataBytes = readEntryBytes (archive.GetEntry("atlalib.json"))
                    let metadata = JsonDocument.Parse(metadataBytes)
                    let metadataRoot = metadata.RootElement

                    let compatDiagnostics =
                        let diagnostics = ResizeArray<Diagnostic>()

                        let validateVersion fieldPath expected actual =
                            if not (isCompatibleMajorVersion expected actual) then
                                diagnostics.Add(Diagnostic.Error($"dependency `.atlalib` has incompatible `{fieldPath}`: `{actual}`", Span.Empty))

                        match tryGetRequiredProperty "atlalib" "formatVersion" metadataRoot with
                        | Ok value ->
                            match tryGetRequiredString "atlalib.formatVersion" value with
                            | Ok actual -> validateVersion "formatVersion" formatVersion actual
                            | Result.Error errs -> diagnostics.AddRange(errs)
                        | Result.Error errs -> diagnostics.AddRange(errs)

                        match tryGetRequiredProperty "atlalib" "compat" metadataRoot with
                        | Ok compatNode ->
                            match tryGetRequiredProperty "atlalib.compat" "languageAbi" compatNode with
                            | Ok value ->
                                match tryGetRequiredString "atlalib.compat.languageAbi" value with
                                | Ok actual when actual = languageAbi -> ()
                                | Ok actual ->
                                    diagnostics.Add(Diagnostic.Error($"dependency `.atlalib` has incompatible `languageAbi`: `{actual}`", Span.Empty))
                                | Result.Error errs -> diagnostics.AddRange(errs)
                            | Result.Error errs -> diagnostics.AddRange(errs)

                            match tryGetRequiredProperty "atlalib.compat" "symbolSchemaVersion" compatNode with
                            | Ok value ->
                                match tryGetRequiredString "atlalib.compat.symbolSchemaVersion" value with
                                | Ok actual -> validateVersion "symbolSchemaVersion" symbolSchemaVersion actual
                                | Result.Error errs -> diagnostics.AddRange(errs)
                            | Result.Error errs -> diagnostics.AddRange(errs)
                        | Result.Error errs -> diagnostics.AddRange(errs)

                        diagnostics |> Seq.toList

                    if not compatDiagnostics.IsEmpty then
                        metadata.Dispose()
                        Result.Error compatDiagnostics
                    else
                        let publicApi = JsonDocument.Parse(readEntryBytes (archive.GetEntry("symbols/public.api.json")))
                        Ok { metadata = metadata; publicApi = publicApi }
        with ex ->
            Result.Error [ Diagnostic.Error($"failed to open `.atlalib` dependency `{atlaLibPath}`: {ex.Message}", Span.Empty) ]

    /// JSON 型ノードを TypeId へ変換する。
    let private parseTypeNode (predeclaredTypes: Map<string, SymbolId>) (fieldPath: string) (element: JsonElement) : Result<TypeId, Diagnostic list> =
        let rec loop (path: string) (node: JsonElement) =
            let mutable implicitArgsNode = Unchecked.defaultof<JsonElement>
            let mutable implicitReturnNode = Unchecked.defaultof<JsonElement>
            let mutable explicitKindNode = Unchecked.defaultof<JsonElement>
            if node.ValueKind = JsonValueKind.Object
               && not (node.TryGetProperty("kind", &explicitKindNode))
               && node.TryGetProperty("args", &implicitArgsNode)
               && node.TryGetProperty("return", &implicitReturnNode) then
                let argResults =
                    implicitArgsNode.EnumerateArray()
                    |> Seq.mapi (fun index argNode -> loop $"{path}.args[{index}]" argNode)
                    |> Seq.toList
                let argDiagnostics =
                    argResults |> List.collect (function | Ok _ -> [] | Result.Error diagnostics -> diagnostics)
                match loop $"{path}.return" implicitReturnNode with
                | Result.Error diagnostics -> Result.Error(argDiagnostics @ diagnostics)
                | Ok retType when List.isEmpty argDiagnostics ->
                    Ok(TypeId.Fn(argResults |> List.choose Result.toOption, retType))
                | Ok _ ->
                    Result.Error argDiagnostics
            else
                let kindResult =
                    match tryGetRequiredProperty path "kind" node with
                    | Ok kindNode -> tryGetRequiredString $"{path}.kind" kindNode
                    | Result.Error diagnostics -> Result.Error diagnostics

                match kindResult with
                | Result.Error diagnostics -> Result.Error diagnostics
                | Ok kind ->
                    match kind with
                    | "builtin" ->
                        match tryGetRequiredProperty path "name" node with
                        | Ok nameNode ->
                            match tryGetRequiredString $"{path}.name" nameNode with
                            | Ok "Unit" -> Ok TypeId.Unit
                            | Ok "Bool" -> Ok TypeId.Bool
                            | Ok "Int" -> Ok TypeId.Int
                            | Ok "Float" -> Ok TypeId.Float
                            | Ok "String" -> Ok TypeId.String
                            | Ok name -> Result.Error [ Diagnostic.Error($"unsupported builtin type `{name}` in `{path}`", Span.Empty) ]
                            | Result.Error diagnostics -> Result.Error diagnostics
                        | Result.Error diagnostics -> Result.Error diagnostics
                    | "nativeType" ->
                        match tryGetRequiredProperty path "fullName" node with
                        | Ok fullNameNode ->
                            match tryGetRequiredString $"{path}.fullName" fullNameNode with
                            | Ok fullName ->
                                match tryResolveLoadedType fullName with
                                | Some systemType -> Ok(TypeId.Native systemType)
                                | None -> Result.Error [ Diagnostic.Error($"native type `{fullName}` could not be resolved while importing `.atlalib`", Span.Empty) ]
                            | Result.Error diagnostics -> Result.Error diagnostics
                        | Result.Error diagnostics -> Result.Error diagnostics
                    | "packageType" ->
                        match tryGetRequiredProperty path "module" node, tryGetRequiredProperty path "name" node with
                        | Ok moduleNode, Ok nameNode ->
                            match tryGetRequiredString $"{path}.module" moduleNode, tryGetRequiredString $"{path}.name" nameNode with
                            | Ok moduleName, Ok typeName ->
                                let fullName = $"{moduleName}.{typeName}"
                                match predeclaredTypes.TryFind fullName with
                                | Some sid -> Ok(TypeId.Name sid)
                                | None -> Result.Error [ Diagnostic.Error($"package type `{fullName}` is not exported by the loaded `.atlalib` dependencies", Span.Empty) ]
                            | Result.Error diagnostics, Ok _
                            | Ok _, Result.Error diagnostics -> Result.Error diagnostics
                            | Result.Error left, Result.Error right -> Result.Error(left @ right)
                        | Result.Error diagnostics, Ok _
                        | Ok _, Result.Error diagnostics -> Result.Error diagnostics
                        | Result.Error left, Result.Error right -> Result.Error(left @ right)
                    | "function" ->
                        match tryGetRequiredProperty path "args" node, tryGetRequiredProperty path "return" node with
                        | Ok argsNode, Ok retNode when argsNode.ValueKind = JsonValueKind.Array ->
                            let argResults =
                                argsNode.EnumerateArray()
                                |> Seq.mapi (fun index argNode -> loop $"{path}.args[{index}]" argNode)
                                |> Seq.toList
                            let argDiagnostics =
                                argResults |> List.collect (function | Ok _ -> [] | Result.Error diagnostics -> diagnostics)
                            match loop $"{path}.return" retNode with
                            | Result.Error diagnostics -> Result.Error(argDiagnostics @ diagnostics)
                            | Ok retType when List.isEmpty argDiagnostics ->
                                Ok(TypeId.Fn(argResults |> List.choose Result.toOption, retType))
                            | Ok _ ->
                                Result.Error argDiagnostics
                        | Ok _, Ok _ -> Result.Error [ Diagnostic.Error($"`{path}.args` must be an array", Span.Empty) ]
                        | Result.Error diagnostics, Ok _
                        | Ok _, Result.Error diagnostics -> Result.Error diagnostics
                        | Result.Error left, Result.Error right -> Result.Error(left @ right)
                    | "apply" ->
                        match tryGetRequiredProperty path "head" node, tryGetRequiredProperty path "args" node with
                        | Ok headNode, Ok argsNode when argsNode.ValueKind = JsonValueKind.Array ->
                            match loop $"{path}.head" headNode with
                            | Result.Error diagnostics -> Result.Error diagnostics
                            | Ok headType ->
                                let argResults =
                                    argsNode.EnumerateArray()
                                    |> Seq.mapi (fun index argNode -> loop $"{path}.args[{index}]" argNode)
                                    |> Seq.toList
                                let argDiagnostics =
                                    argResults |> List.collect (function | Ok _ -> [] | Result.Error diagnostics -> diagnostics)
                                if List.isEmpty argDiagnostics then
                                    Ok(TypeId.App(headType, argResults |> List.choose Result.toOption))
                                else
                                    Result.Error argDiagnostics
                        | Ok _, Ok _ -> Result.Error [ Diagnostic.Error($"`{path}.args` must be an array", Span.Empty) ]
                        | Result.Error diagnostics, Ok _
                        | Ok _, Result.Error diagnostics -> Result.Error diagnostics
                        | Result.Error left, Result.Error right -> Result.Error(left @ right)
                    | "typeVar" ->
                        match tryGetRequiredProperty path "name" node with
                        | Ok nameNode ->
                            match tryGetRequiredString $"{path}.name" nameNode with
                            | Ok name -> Ok(TypeId.TypeVar name)
                            | Result.Error diagnostics -> Result.Error diagnostics
                        | Result.Error diagnostics -> Result.Error diagnostics
                    | _ ->
                        Result.Error [ Diagnostic.Error($"unsupported type node kind `{kind}` in `{path}`", Span.Empty) ]

        loop fieldPath element

    /// シグネチャ JSON（args/return）を TypeId.Fn へ変換する。
    let private parseSignatureNode (predeclaredTypes: Map<string, SymbolId>) (fieldPath: string) (element: JsonElement) : Result<TypeId, Diagnostic list> =
        let argsNode = element.GetProperty("args")
        let retNode = element.GetProperty("return")
        let argResults =
            argsNode.EnumerateArray()
            |> Seq.mapi (fun index argNode -> parseTypeNode predeclaredTypes $"{fieldPath}.args[{index}].type" (argNode.GetProperty("type")))
            |> Seq.toList
        let argDiagnostics =
            argResults |> List.collect (function | Ok _ -> [] | Result.Error diagnostics -> diagnostics)
        match parseTypeNode predeclaredTypes $"{fieldPath}.return" retNode with
        | Result.Error diagnostics -> Result.Error(argDiagnostics @ diagnostics)
        | Ok retType when List.isEmpty argDiagnostics ->
            Ok(TypeId.Fn(argResults |> List.choose Result.toOption, retType))
        | Ok _ ->
            Result.Error argDiagnostics

    /// field シンボル名から所有型とフィールド名を分解する。
    let private splitOwnerAndMemberName (fullName: string) : string * string =
        let lastDot = fullName.LastIndexOf('.')
        if lastDot <= 0 then "", fullName else fullName.Substring(0, lastDot), fullName.Substring(lastDot + 1)

    /// import 用インデックスを dependencies から構築する。
    let loadDependencyIndex (symbolTable: SymbolTable) (dependencySources: string list) : ImportedDependencyIndex =
        let loadedArchives =
            dependencySources
            |> List.choose (fun dependencySource ->
                if dependencySource.EndsWith(".atlalib", StringComparison.OrdinalIgnoreCase) && File.Exists(dependencySource) then
                    Some(dependencySource, tryOpenArchive dependencySource)
                else
                    None)

        let archiveDiagnostics =
            loadedArchives
            |> List.collect (fun (_, result) ->
                match result with
                | Ok _ -> []
                | Result.Error diagnostics -> diagnostics)

        let openedArchives =
            loadedArchives
            |> List.choose (fun (path, result) ->
                match result with
                | Ok archive -> Some(path, archive)
                | Result.Error _ -> None)

        let folder (moduleExportsAcc, typeDefsAcc, moduleNamesAcc, typeNamesAcc, diagnosticsAcc) (_path, archive: LoadedArchive) =
            // パッケージ名を atlalib.json から取得する（修飾モジュール名生成に使用）。
            let packageName =
                let mutable packageNode = Unchecked.defaultof<JsonElement>
                let mutable nameNode = Unchecked.defaultof<JsonElement>
                if archive.metadata.RootElement.TryGetProperty("package", &packageNode)
                   && packageNode.TryGetProperty("name", &nameNode)
                   && nameNode.ValueKind = JsonValueKind.String then
                    nameNode.GetString()
                else
                    ""

            let root = archive.publicApi.RootElement
            let schemaDiagnostics =
                match tryGetRequiredProperty "public.api" "schemaVersion" root with
                | Ok schemaNode ->
                    match tryGetRequiredString "public.api.schemaVersion" schemaNode with
                    | Ok actual when isCompatibleMajorVersion symbolSchemaVersion actual -> []
                    | Ok actual -> [ Diagnostic.Error($"dependency `.atlalib` has incompatible `public.api.json` schema version: `{actual}`", Span.Empty) ]
                    | Result.Error diagnostics -> diagnostics
                | Result.Error diagnostics ->
                    diagnostics

            if not schemaDiagnostics.IsEmpty then
                moduleExportsAcc, typeDefsAcc, moduleNamesAcc, typeNamesAcc, diagnosticsAcc @ schemaDiagnostics
            else
                let mutable modulesNode = Unchecked.defaultof<JsonElement>
                if not (root.TryGetProperty("modules", &modulesNode) && modulesNode.ValueKind = JsonValueKind.Array) then
                    moduleExportsAcc, typeDefsAcc, moduleNamesAcc, typeNamesAcc, diagnosticsAcc @ [ Diagnostic.Error("dependency `.atlalib` public API must contain a `modules` array", Span.Empty) ]
                else
                    let moduleArray = modulesNode.EnumerateArray() |> Seq.toList

                    let predeclaredTypes =
                        moduleArray
                        |> List.collect (fun moduleNode ->
                            let moduleName = moduleNode.GetProperty("name").GetString()
                            let typesNode = moduleNode.GetProperty("exports").GetProperty("types")
                            typesNode.EnumerateArray()
                            |> Seq.collect (fun typeNode ->
                                let typeName = typeNode.GetProperty("name").GetString()
                                let fullName = $"{moduleName}.{typeName}"
                                let typeSid = symbolTable.NextId()
                                let systemType = tryResolveLoadedType fullName
                                symbolTable.Add(typeSid, { name = fullName; typ = TypeId.Name typeSid; kind = SymbolKind.External(ExternalBinding.SystemTypeRef(systemType |> Option.toObj)) })
                                let mainEntry =
                                    fullName,
                                    { moduleName = moduleName
                                      typeName = typeName
                                      fullName = fullName
                                      element = typeNode
                                      kind = typeNode.GetProperty("kind").GetString()
                                      typeSid = typeSid
                                      systemType = systemType }
                                // enum 型の場合、ペイロード型（内部実装型）も先行登録する。
                                // 隠しフィールドの型がペイロード型への packageType 参照として
                                // エクスポートされるため、フィールド型解析前に predeclaredTypes に
                                // 含まれていなければ "not exported" エラーになる。
                                let payloadEntries =
                                    if typeNode.GetProperty("kind").GetString() = "enum" then
                                        let mutable casesNode = Unchecked.defaultof<JsonElement>
                                        if typeNode.TryGetProperty("cases", &casesNode) && casesNode.ValueKind = JsonValueKind.Array then
                                            casesNode.EnumerateArray()
                                            |> Seq.choose (fun caseNode ->
                                                let mutable payloadTypeNode = Unchecked.defaultof<JsonElement>
                                                if caseNode.TryGetProperty("payloadTypeName", &payloadTypeNode) && payloadTypeNode.ValueKind = JsonValueKind.String then
                                                    let payloadTypeName = payloadTypeNode.GetString()
                                                    let payloadFullName = $"{moduleName}.{payloadTypeName}"
                                                    let payloadSid = symbolTable.NextId()
                                                    // payloadFullName の最終セグメント分割では CIL 型名（例: "Opt.__enum_payload_Some_type"）に
                                                    // 到達できないため、payloadTypeName でも直接検索する。
                                                    let payloadSystemType =
                                                        tryResolveLoadedType payloadFullName
                                                        |> Option.orElseWith (fun () -> tryResolveLoadedType payloadTypeName)
                                                    symbolTable.Add(payloadSid, { name = payloadFullName; typ = TypeId.Name payloadSid; kind = SymbolKind.External(ExternalBinding.SystemTypeRef(payloadSystemType |> Option.toObj)) })
                                                    Some (payloadFullName, { moduleName = moduleName; typeName = payloadTypeName; fullName = payloadFullName; element = Unchecked.defaultof<JsonElement>; kind = "data"; typeSid = payloadSid; systemType = payloadSystemType })
                                                else
                                                    None)
                                            |> Seq.toList
                                        else []
                                    else []
                                mainEntry :: payloadEntries)
                            |> Seq.toList)
                        |> Map.ofList

                    let parseModule (moduleNode: JsonElement) =
                        let moduleName = moduleNode.GetProperty("name").GetString()
                        let exportsNode = moduleNode.GetProperty("exports")
                        let valuesNode = exportsNode.GetProperty("values")
                        let typesNode = exportsNode.GetProperty("types")
                        let globalsType = tryResolveLoadedType $"{moduleName}.Globals"

                        let valueExports, valueDiagnostics =
                            valuesNode.EnumerateArray()
                            |> Seq.fold
                                (fun (exports, diagnostics) valueNode ->
                                    let name = valueNode.GetProperty("name").GetString()
                                    let signatureNode = valueNode.GetProperty("signature")
                                    match parseSignatureNode (predeclaredTypes |> Map.map (fun _ value -> value.typeSid)) $"module `{moduleName}` value `{name}` signature" signatureNode with
                                    | Result.Error errs -> exports, diagnostics @ errs
                                    | Ok signatureType ->
                                        let methodInfos =
                                            match globalsType with
                                            | Some globals ->
                                                globals.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                                                |> Array.filter (fun methodInfo -> methodInfo.Name = name)
                                                |> Array.toList
                                            | None -> []
                                        let sid = symbolTable.NextId()
                                        symbolTable.Add(sid, { name = $"{moduleName}.Globals.{name}"; typ = signatureType; kind = SymbolKind.External(ExternalBinding.NativeMethodGroup methodInfos) })
                                        Map.add name { symbolId = sid; typ = signatureType } exports,
                                        if methodInfos.IsEmpty then diagnostics @ [ Diagnostic.Warning($"imported function `{moduleName}.{name}` could not be resolved from loaded assemblies", Span.Empty) ] else diagnostics)
                                (Map.empty, [])

                        let typeDefs, typeDiagnostics =
                            typesNode.EnumerateArray()
                            |> Seq.fold
                                (fun (defs, diagnostics) typeNode ->
                                    let typeName = typeNode.GetProperty("name").GetString()
                                    let fullTypeName = $"{moduleName}.{typeName}"
                                    let predeclared = predeclaredTypes[fullTypeName]
                                    let systemType = predeclared.systemType |> Option.toObj
                                    let typeParamNames =
                                        typeNode.GetProperty("typeParameters").EnumerateArray()
                                        |> Seq.map (fun item -> item.GetString())
                                        |> Seq.toList

                                    let fieldDefs, hiddenFieldDefs, fieldDiagnostics =
                                        typeNode.GetProperty("fields").EnumerateArray()
                                        |> Seq.fold
                                            (fun (publicFields, hiddenFields, fieldErrs) fieldNode ->
                                                let fieldName = fieldNode.GetProperty("name").GetString()
                                                let isHidden =
                                                    let mutable hiddenNode = Unchecked.defaultof<JsonElement>
                                                    fieldNode.TryGetProperty("isHidden", &hiddenNode)
                                                    && hiddenNode.ValueKind = JsonValueKind.True
                                                match parseTypeNode (predeclaredTypes |> Map.map (fun _ value -> value.typeSid)) $"field `{fullTypeName}.{fieldName}`" (fieldNode.GetProperty("type")) with
                                                | Result.Error errs ->
                                                    publicFields, hiddenFields, fieldErrs @ errs
                                                | Ok fieldType ->
                                                    let fieldSid = symbolTable.NextId()
                                                    let reflectedField =
                                                        if isNull systemType then None
                                                        else
                                                            systemType.GetField(fieldName, BindingFlags.Public ||| BindingFlags.Instance)
                                                            |> Option.ofObj
                                                    let symbolName = $"{fullTypeName}.{fieldName}"
                                                    let fieldKind =
                                                        reflectedField
                                                        |> Option.map (fun fieldInfo -> SymbolKind.External(ExternalBinding.SystemFieldRef fieldInfo))
                                                        |> Option.defaultValue (SymbolKind.Local())
                                                    symbolTable.Add(fieldSid, { name = symbolName; typ = fieldType; kind = fieldKind })
                                                    let fieldDef =
                                                        { name = fieldName
                                                          sid = fieldSid
                                                          typ = fieldType
                                                          span = Span.Empty }
                                                    if isHidden then
                                                        publicFields, hiddenFields @ [ fieldDef ], fieldErrs
                                                    else
                                                        publicFields @ [ fieldDef ], hiddenFields, fieldErrs)
                                            ([], [], [])

                                    let methodMap, methodDiagnostics =
                                        typeNode.GetProperty("methods").EnumerateArray()
                                        |> Seq.fold
                                            (fun (methodAcc, diagAcc) methodNode ->
                                                let methodName = methodNode.GetProperty("name").GetString()
                                                let isStatic =
                                                    let mutable staticNode = Unchecked.defaultof<JsonElement>
                                                    methodNode.TryGetProperty("isStatic", &staticNode)
                                                    && staticNode.ValueKind = JsonValueKind.True
                                                match parseSignatureNode (predeclaredTypes |> Map.map (fun _ value -> value.typeSid)) $"method `{fullTypeName}.{methodName}` signature" (methodNode.GetProperty("signature")) with
                                                | Result.Error errs -> methodAcc, diagAcc @ errs
                                                | Ok methodType ->
                                                    let methodSid = symbolTable.NextId()
                                                    let reflectedMethods =
                                                        if String.Equals(predeclared.kind, "role", StringComparison.Ordinal) then
                                                            if isNull systemType then
                                                                []
                                                            else
                                                                let flags =
                                                                    BindingFlags.Public
                                                                    ||| (if isStatic then BindingFlags.Static else BindingFlags.Instance)
                                                                systemType.GetMethods(flags)
                                                                |> Array.filter (fun methodInfo -> methodInfo.Name = methodName)
                                                                |> Array.toList
                                                        elif isStatic then
                                                            // 静的 impl メソッドは Globals 型の static メソッドとしてコンパイルされる。
                                                            match globalsType with
                                                            | Some globals ->
                                                                globals.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                                                                |> Array.filter (fun methodInfo -> methodInfo.Name = $"{typeName}.{methodName}")
                                                                |> Array.toList
                                                            | None ->
                                                                []
                                                        else
                                                            // インスタンス impl メソッドは型自体のインスタンスメソッドとしてコンパイルされる。
                                                            if isNull systemType then
                                                                []
                                                            else
                                                                systemType.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
                                                                |> Array.filter (fun methodInfo -> methodInfo.Name = methodName)
                                                                |> Array.toList
                                                    symbolTable.Add(methodSid, { name = $"{fullTypeName}.{methodName}"; typ = methodType; kind = SymbolKind.External(ExternalBinding.NativeMethodGroup reflectedMethods) })
                                                    Map.add methodName (methodSid, methodType, isStatic) methodAcc,
                                                    if reflectedMethods.IsEmpty then
                                                        diagAcc @ [ Diagnostic.Warning($"imported method `{fullTypeName}.{methodName}` could not be resolved from loaded assemblies", Span.Empty) ]
                                                    else
                                                        diagAcc)
                                            (Map.empty, [])

                                    let baseTypeOpt, baseTypeDiagnostics =
                                        let mutable baseTypeNode = Unchecked.defaultof<JsonElement>
                                        if typeNode.TryGetProperty("baseType", &baseTypeNode) && baseTypeNode.ValueKind <> JsonValueKind.Null then
                                            match parseTypeNode (predeclaredTypes |> Map.map (fun _ value -> value.typeSid)) $"type `{fullTypeName}` baseType" baseTypeNode with
                                            | Ok baseType -> Some baseType, []
                                            | Result.Error errs -> None, errs
                                        else
                                            None, []

                                    let delegatedByFieldName =
                                        let mutable delegatedNode = Unchecked.defaultof<JsonElement>
                                        if typeNode.TryGetProperty("delegatedByFieldName", &delegatedNode) && delegatedNode.ValueKind = JsonValueKind.String then
                                            delegatedNode.GetString() |> Option.ofObj
                                        else
                                            None

                                    let enumInfo, enumDiagnostics =
                                        if String.Equals(predeclared.kind, "enum", StringComparison.Ordinal) then
                                            let casesNode = typeNode.GetProperty("cases")
                                            let tagFieldOpt = hiddenFieldDefs |> List.tryFind (fun fieldDef -> fieldDef.name = "__enum_tag")
                                            match tagFieldOpt with
                                            | None ->
                                                None, [ Diagnostic.Error($"enum `{fullTypeName}` is missing hidden tag metadata", Span.Empty) ]
                                            | Some tagField ->
                                                let caseDefs, caseErrs =
                                                    casesNode.EnumerateArray()
                                                    |> Seq.fold
                                                        (fun (caseAcc, diagAcc) caseNode ->
                                                            let caseName = caseNode.GetProperty("name").GetString()
                                                            let tag = caseNode.GetProperty("tag").GetInt32()
                                                            let payloadTypeSidOpt =
                                                                let mutable payloadTypeNode = Unchecked.defaultof<JsonElement>
                                                                if caseNode.TryGetProperty("payloadTypeName", &payloadTypeNode) && payloadTypeNode.ValueKind = JsonValueKind.String then
                                                                    let payloadTypeName = payloadTypeNode.GetString()
                                                                    let payloadFullName = $"{moduleName}.{payloadTypeName}"
                                                                    // 先行登録済みの SID を再利用して重複登録を防ぐ。
                                                                    match predeclaredTypes.TryFind payloadFullName with
                                                                    | Some predeclaredPayload -> Some predeclaredPayload.typeSid
                                                                    | None ->
                                                                        // payloadFullName の最終セグメントでは CIL 型名に到達できないため payloadTypeName も試す。
                                                                        let payloadSystemType =
                                                                            tryResolveLoadedType payloadFullName
                                                                            |> Option.orElseWith (fun () -> tryResolveLoadedType payloadTypeName)
                                                                            |> Option.toObj
                                                                        let payloadSid = symbolTable.NextId()
                                                                        symbolTable.Add(payloadSid, { name = payloadFullName; typ = TypeId.Name payloadSid; kind = SymbolKind.External(ExternalBinding.SystemTypeRef payloadSystemType) })
                                                                        Some payloadSid
                                                                else
                                                                    None
                                                            let payloadSlotSidOpt =
                                                                let payloadFieldName = $"__enum_payload_{caseName}"
                                                                hiddenFieldDefs
                                                                |> List.tryFind (fun fieldDef -> fieldDef.name = payloadFieldName)
                                                                |> Option.map (fun fieldDef -> fieldDef.sid)
                                                            let payloadFields =
                                                                let mutable payloadFieldsNode = Unchecked.defaultof<JsonElement>
                                                                if caseNode.TryGetProperty("payloadFields", &payloadFieldsNode) && payloadFieldsNode.ValueKind = JsonValueKind.Array then
                                                                    payloadFieldsNode.EnumerateArray()
                                                                    |> Seq.choose (fun payloadFieldNode ->
                                                                        let fieldName = payloadFieldNode.GetProperty("name").GetString()
                                                                        let payloadTypeName = $"__enum_payload_{caseName}_type"
                                                                        match parseTypeNode (predeclaredTypes |> Map.map (fun _ value -> value.typeSid)) $"enum payload `{fullTypeName}.{caseName}.{fieldName}`" (payloadFieldNode.GetProperty("type")) with
                                                                        | Result.Error _ -> None
                                                                        | Ok fieldType ->
                                                                            let fieldSid = symbolTable.NextId()
                                                                            let reflectedField =
                                                                                match payloadTypeSidOpt with
                                                                                | Some payloadSid ->
                                                                                    match symbolTable.Get(payloadSid) with
                                                                                    | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef payloadSystemType) } when not (isNull payloadSystemType) ->
                                                                                        payloadSystemType.GetField(fieldName, BindingFlags.Public ||| BindingFlags.Instance) |> Option.ofObj
                                                                                    | _ -> None
                                                                                | None -> None
                                                                            let fieldKind =
                                                                                reflectedField
                                                                                |> Option.map (fun fieldInfo -> SymbolKind.External(ExternalBinding.SystemFieldRef fieldInfo))
                                                                                |> Option.defaultValue (SymbolKind.Local())
                                                                            symbolTable.Add(fieldSid, { name = $"{moduleName}.{payloadTypeName}.{fieldName}"; typ = fieldType; kind = fieldKind })
                                                                            Some
                                                                                { name = fieldName
                                                                                  sid = fieldSid
                                                                                  typ = fieldType
                                                                                  span = Span.Empty })
                                                                    |> Seq.toList
                                                                else
                                                                    []
                                                            let caseDef =
                                                                { name = caseName
                                                                  tag = tag
                                                                  payloadTypeSid = payloadTypeSidOpt
                                                                  payloadFieldSid = payloadSlotSidOpt
                                                                  fields = payloadFields
                                                                  span = Span.Empty }
                                                            caseAcc @ [ caseDef ], diagAcc)
                                                        ([], [])
                                                Some { hiddenTagField = tagField; cases = caseDefs }, caseErrs
                                        else
                                            None, []

                                    let allDiagnostics =
                                        fieldDiagnostics @ methodDiagnostics @ baseTypeDiagnostics @ enumDiagnostics

                                    let dataTypeDef =
                                        { typeSid = predeclared.typeSid
                                          baseType = baseTypeOpt
                                          delegatedByFieldName = delegatedByFieldName
                                          typeParams = typeParamNames
                                          fields = fieldDefs
                                          hiddenFields = hiddenFieldDefs
                                          enumInfo = enumInfo
                                          methods = methodMap }

                                    Map.add fullTypeName dataTypeDef defs, diagnostics @ allDiagnostics)
                                (Map.empty, [])

                        let moduleExportMap =
                            let typeExports =
                                typeDefs
                                |> Map.toList
                                |> List.filter (fun (fullName, _) -> fullName.StartsWith(moduleName + ".", StringComparison.Ordinal))
                                |> List.map (fun (fullName, typeDef) ->
                                    let typeName = fullName.Substring(moduleName.Length + 1)
                                    $"type:{typeName}", { symbolId = typeDef.typeSid; typ = TypeId.Name typeDef.typeSid })
                                |> Map.ofList

                            let fieldExports =
                                typeDefs
                                |> Map.toList
                                |> List.filter (fun (fullName, _) -> fullName.StartsWith(moduleName + ".", StringComparison.Ordinal))
                                |> List.collect (fun (fullName, typeDef) ->
                                    let typeName = fullName.Substring(moduleName.Length + 1)
                                    (typeDef.fields @ typeDef.hiddenFields)
                                    |> List.map (fun fieldDef -> $"field:{typeName}.{fieldDef.name}", { symbolId = fieldDef.sid; typ = fieldDef.typ }))
                                |> Map.ofList

                            let implBaseExports =
                                typeDefs
                                |> Map.toList
                                |> List.choose (fun (fullName, typeDef) ->
                                    match typeDef.baseType with
                                    | Some baseType when fullName.StartsWith(moduleName + ".", StringComparison.Ordinal) ->
                                        let typeName = fullName.Substring(moduleName.Length + 1)
                                        Some($"implBase:{typeName}", { symbolId = typeDef.typeSid; typ = baseType })
                                    | _ -> None)
                                |> Map.ofList

                            [ valueExports; typeExports; fieldExports; implBaseExports ]
                            |> List.fold (fun state exports -> Map.fold (fun acc key value -> Map.add key value acc) state exports) Map.empty

                        moduleName,
                        moduleExportMap,
                        typeDefs,
                        valueDiagnostics @ typeDiagnostics

                    let parsedModules = moduleArray |> List.map parseModule
                    let parsedDiagnostics = parsedModules |> List.collect (fun (_, _, _, diagnostics) -> diagnostics)
                    let parsedModuleExports =
                        parsedModules
                        |> List.fold (fun acc (moduleName, exports, _, _) -> Map.add moduleName exports acc) moduleExportsAcc
                    let parsedTypeDefs =
                        parsedModules
                        |> List.fold (fun acc (_, _, typeDefs, _) -> Map.fold (fun state key value -> Map.add key value state) acc typeDefs) typeDefsAcc
                    let parsedModuleNames =
                        parsedModules |> List.fold (fun acc (moduleName, _, _, _) -> Set.add moduleName acc) moduleNamesAcc
                    let parsedTypeNames =
                        parsedModules
                        |> List.fold (fun acc (_, _, typeDefs, _) -> typeDefs |> Map.keys |> Set.ofSeq |> Set.union acc) typeNamesAcc

                    // public import による再エクスポート（"reexports" 配列）を処理する。
                    // 参照先モジュールのエクスポートと型定義をこのモジュールにマージする（SID は共有）。
                    let moduleExportsWithReexports, typeDefsWithReexports, typeNamesWithReexports =
                        let reexportFolder (exAcc, tdAcc, tnAcc) (moduleNode: JsonElement) =
                            let moduleName = moduleNode.GetProperty("name").GetString()
                            let mutable reexportsNode = Unchecked.defaultof<JsonElement>
                            if moduleNode.TryGetProperty("reexports", &reexportsNode) && reexportsNode.ValueKind = JsonValueKind.Array then
                                reexportsNode.EnumerateArray()
                                |> Seq.fold (fun (exAcc2: Map<string, Map<string, ModuleExport>>, tdAcc2: Map<string, DataTypeDef>, tnAcc2: Set<string>) (refNode: JsonElement) ->
                                    let referencedModuleName = refNode.GetString()
                                    let additionalExports =
                                        match exAcc2.TryFind referencedModuleName with
                                        | None -> Map.empty
                                        | Some refExports -> refExports
                                    let exAcc3 =
                                        match exAcc2.TryFind moduleName with
                                        | None -> exAcc2
                                        | Some existing ->
                                            let merged = Map.fold (fun acc k v -> Map.add k v acc) existing additionalExports
                                            Map.add moduleName merged exAcc2
                                    let reexportedTypeDefs =
                                        tdAcc2
                                        |> Map.toList
                                        |> List.filter (fun (fullName, _) -> fullName.StartsWith(referencedModuleName + "."))
                                        |> List.map (fun (origFullName, typeDef) ->
                                            let typeName = origFullName.Substring(referencedModuleName.Length + 1)
                                            $"{moduleName}.{typeName}", typeDef)
                                    let tdAcc3 = List.fold (fun acc (k, v) -> Map.add k v acc) tdAcc2 reexportedTypeDefs
                                    let tnAcc3 = List.fold (fun acc (k, _) -> Set.add k acc) tnAcc2 reexportedTypeDefs
                                    exAcc3, tdAcc3, tnAcc3)
                                    (exAcc, tdAcc, tnAcc)
                            else
                                exAcc, tdAcc, tnAcc
                        List.fold reexportFolder (parsedModuleExports, parsedTypeDefs, parsedTypeNames) moduleArray

                    // パッケージ名で修飾したモジュール名エントリを追加する（例: "Std.lib" を "lib" の別名として登録）。
                    // これにより、ユーザーは "import lib'Type" でインポートでき、
                    // Prelude ルックアップは "Std.lib" で一意に検索できる。
                    let finalExports, finalTypeDefs, finalModuleNames, finalTypeNames =
                        if String.IsNullOrEmpty packageName then
                            moduleExportsWithReexports, typeDefsWithReexports, parsedModuleNames, typeNamesWithReexports
                        else
                            let qualifyFolder (exAcc: Map<string, Map<string, ModuleExport>>, tdAcc: Map<string, DataTypeDef>, mnAcc: Set<string>, tnAcc: Set<string>) (moduleName: string, _: Map<string, ModuleExport>, _: Map<string, DataTypeDef>, _: Diagnostic list) =
                                let qualifiedModuleName = $"{packageName}.{moduleName}"
                                let qualifiedExports =
                                    match exAcc.TryFind moduleName with
                                    | None -> Map.empty
                                    | Some exports -> exports
                                let exAcc2 = Map.add qualifiedModuleName qualifiedExports exAcc
                                let qualifiedTypeDefs =
                                    tdAcc
                                    |> Map.toList
                                    |> List.filter (fun (fullName, _) -> fullName.StartsWith(moduleName + "."))
                                    |> List.map (fun (origFullName, typeDef) ->
                                        let typeName = origFullName.Substring(moduleName.Length + 1)
                                        $"{qualifiedModuleName}.{typeName}", typeDef)
                                let tdAcc2 = List.fold (fun acc (k, v) -> Map.add k v acc) tdAcc qualifiedTypeDefs
                                let mnAcc2 = Set.add qualifiedModuleName mnAcc
                                let tnAcc2 = List.fold (fun acc (k, _) -> Set.add k acc) tnAcc qualifiedTypeDefs
                                exAcc2, tdAcc2, mnAcc2, tnAcc2
                            List.fold qualifyFolder (moduleExportsWithReexports, typeDefsWithReexports, parsedModuleNames, typeNamesWithReexports) parsedModules

                    finalExports, finalTypeDefs, finalModuleNames, finalTypeNames, diagnosticsAcc @ parsedDiagnostics

        let moduleExports, typeDefsByFullName, moduleNames, typeFullNames, diagnostics =
            openedArchives
            |> List.fold folder (Map.empty, Map.empty, Set.empty, Set.empty, archiveDiagnostics)

        for (_, archive) in openedArchives do
            archive.publicApi.Dispose()
            archive.metadata.Dispose()

        { moduleNames = moduleNames
          typeFullNames = typeFullNames
          moduleExports = moduleExports
          typeDefsByFullName = typeDefsByFullName
          diagnostics = diagnostics }

    /// `.atlalib` の埋め込み assembly と lock 記載 runtime/native asset を解決する。
    let resolveRuntimeAssets (atlaLibPath: string) : Result<ResolvedRuntimeAssets, Diagnostic list> =
        match tryOpenArchive atlaLibPath with
        | Result.Error diagnostics -> Result.Error diagnostics
        | Ok archive ->
            try
                use zip = ZipFile.OpenRead(atlaLibPath)
                let metadataRoot = archive.metadata.RootElement
                let assemblyEntryPath =
                    metadataRoot.GetProperty("artifacts").GetProperty("assembly").GetString()
                let dependencyLockPath =
                    metadataRoot.GetProperty("artifacts").GetProperty("dependencyLock").GetString()
                let packageName = metadataRoot.GetProperty("package").GetProperty("name").GetString()
                let packageVersion = metadataRoot.GetProperty("package").GetProperty("version").GetString()
                let extractionRoot = buildExtractionRoot atlaLibPath
                Directory.CreateDirectory(extractionRoot) |> ignore

                let assemblyTargetPath =
                    let fileName = Path.GetFileName(assemblyEntryPath)
                    Path.Join(extractionRoot, fileName)

                let assemblyEntry = zip.GetEntry(assemblyEntryPath)
                if isNull assemblyEntry then
                    Result.Error [ Diagnostic.Error($"dependency `.atlalib` is missing assembly entry `{assemblyEntryPath}`", Span.Empty) ]
                else
                    let assemblyBytes = readEntryBytes assemblyEntry
                    File.WriteAllBytes(assemblyTargetPath, assemblyBytes)

                    let runtimeLoadPaths = ResizeArray<string>()
                    runtimeLoadPaths.Add(assemblyTargetPath)
                    let nativeRuntimePaths = ResizeArray<string>()

                    let dependencyLockEntry = zip.GetEntry(dependencyLockPath)
                    if not (isNull dependencyLockEntry) then
                        use lockDoc = JsonDocument.Parse(readEntryBytes dependencyLockEntry)
                        let dependenciesNode = lockDoc.RootElement.GetProperty("dependencies")

                        for dependencyNode in dependenciesNode.EnumerateArray() do
                            let addAssetPath (pathsNode: JsonElement) (target: ResizeArray<string>) =
                                for assetNode in pathsNode.EnumerateArray() do
                                    let assetPath = assetNode.[0].GetString()
                                    let expectedHash = assetNode.[1].GetString()
                                    if File.Exists(assetPath) then
                                        let actualHash = $"sha256:{computeSha256Hex (File.ReadAllBytes(assetPath))}"
                                        if String.Equals(actualHash, expectedHash, StringComparison.OrdinalIgnoreCase) then
                                            target.Add(assetPath)

                            addAssetPath (dependencyNode.GetProperty("runtimeAssets")) runtimeLoadPaths
                            addAssetPath (dependencyNode.GetProperty("nativeAssets")) nativeRuntimePaths

                    archive.publicApi.Dispose()
                    archive.metadata.Dispose()
                    Ok
                        { packageName = packageName
                          packageVersion = packageVersion
                          assemblyPath = assemblyTargetPath
                          runtimeLoadPaths = runtimeLoadPaths |> Seq.distinct |> Seq.sort |> Seq.toList
                          nativeRuntimePaths = nativeRuntimePaths |> Seq.distinct |> Seq.sort |> Seq.toList }
            with ex ->
                archive.publicApi.Dispose()
                archive.metadata.Dispose()
                Result.Error [ Diagnostic.Error($"failed to resolve `.atlalib` runtime assets `{atlaLibPath}`: {ex.Message}", Span.Empty) ]
