namespace Atla.Build

open System
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open Atla.Core.Data
open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open Atla.Compiler
open NuGet.Versioning
open YamlDotNet.Core
open YamlDotNet.RepresentationModel

type BuildRequest =
    { projectRoot: string }

type BuildPackageType =
    | Exe
    | Lib
    | Dll

type BuildPlan =
    { projectName: string
      projectVersion: string
      projectRoot: string
      packageType: BuildPackageType
      dependencies: Compiler.ResolvedDependency list }

type BuildResult =
    { succeeded: bool
      plan: BuildPlan option
      diagnostics: Diagnostic list }

module BuildSystem =
    (* Manifest ファイル関連の固定設定と、共通ユーティリティ。 *)
    let private manifestFileName = "atla.yaml"
    let private targetFrameworkMoniker = ".NETCoreApp,Version=v10.0"
    let private atlaLibFormatVersion = AtlaLib.formatVersion
    let private atlaLanguageAbi = AtlaLib.languageAbi
    let private atlaLibSymbolSchemaVersion = AtlaLib.symbolSchemaVersion
    let private atlaCompilerVersion = "0.1.0"

    /// Build 入力パスを絶対パスへ正規化する。
    let private normalizePath (path: string) : string =
        Path.GetFullPath(path)

    /// パスを .deps.json 向けに正規化する（`/` 区切り）。
    let private normalizeDepsPath (path: string) : string =
        path.Replace(Path.DirectorySeparatorChar, '/').Replace(Path.AltDirectorySeparatorChar, '/')

    /// runtimeLoadPaths のコピー先パスを返す（copyDependencies と同一規則）。
    let private getManagedDestinationPath (outDir: string) (srcPath: string) : string =
        Path.Join(outDir, Path.GetFileName(srcPath))

    /// nativeRuntimePaths のコピー先パスを返す（copyDependencies と同一規則）。
    let private getNativeDestinationPath (outDir: string) (dep: Compiler.ResolvedDependency) (srcPath: string) : string =
        // `.atlalib` のような単一ファイル依存は dep.source がディレクトリではないため、
        // ネイティブアセットもフラット配置へフォールバックする。
        if String.IsNullOrWhiteSpace dep.source || File.Exists(dep.source) then
            Path.Join(outDir, Path.GetFileName(srcPath))
        else
            let normalizedSource = Path.GetFullPath(dep.source)
            let normalizedSrc = Path.GetFullPath(srcPath)
            Path.Join(outDir, Path.GetRelativePath(normalizedSource, normalizedSrc))

    /// 依存アセットの出力先相対パスを計算する。
    let private collectDependencyAssetPaths
        (outDir: string)
        (dep: Compiler.ResolvedDependency)
        : string list * string list =
        let runtimePaths =
            dep.runtimeLoadPaths
            |> List.map (getManagedDestinationPath outDir)
            |> List.map (fun dstPath -> Path.GetRelativePath(outDir, dstPath))
            |> List.map normalizeDepsPath
            |> List.distinct
            |> List.sort

        let nativePaths =
            dep.nativeRuntimePaths
            |> List.map (getNativeDestinationPath outDir dep)
            |> List.map (fun dstPath -> Path.GetRelativePath(outDir, dstPath))
            |> List.map normalizeDepsPath
            |> List.distinct
            |> List.sort

        runtimePaths, nativePaths

    /// `asmName.deps.json` を outDir 配下へ書き出す。
    let writeDepsFile
        (projectName: string)
        (projectVersion: string)
        (asmName: string)
        (dependencies: Compiler.ResolvedDependency list)
        (outDir: string)
        : Result<string, Diagnostic list> =
        try
            let appLibraryName = $"{projectName}/{projectVersion}"

            let runtimeTargetNode = JsonObject()
            runtimeTargetNode.Add("name", JsonValue.Create(targetFrameworkMoniker))
            runtimeTargetNode.Add("signature", JsonValue.Create(""))

            let appRuntimeNode = JsonObject()
            appRuntimeNode.Add($"{asmName}.dll", JsonObject())
            let appTargetNode = JsonObject()
            appTargetNode.Add("runtime", appRuntimeNode)

            let dependencyTargetNodes =
                dependencies
                |> List.sortBy (fun dep -> dep.name.ToLowerInvariant())
                |> List.choose (fun dep ->
                    let runtimePaths, nativePaths = collectDependencyAssetPaths outDir dep

                    let node = JsonObject()

                    if not (List.isEmpty runtimePaths) then
                        let runtimeNode = JsonObject()
                        runtimePaths |> List.iter (fun path -> runtimeNode.Add(path, JsonObject()))
                        node.Add("runtime", runtimeNode)

                    if not (List.isEmpty nativePaths) then
                        let nativeNode = JsonObject()
                        nativePaths |> List.iter (fun path -> nativeNode.Add(path, JsonObject()))
                        node.Add("native", nativeNode)

                    if node.Count = 0 then
                        None
                    else
                        Some($"{dep.name}/{dep.version}", node))

            let targetNode = JsonObject()
            targetNode.Add(appLibraryName, appTargetNode)
            dependencyTargetNodes |> List.iter (fun (name, node) -> targetNode.Add(name, node))

            let targetsNode = JsonObject()
            targetsNode.Add(targetFrameworkMoniker, targetNode)

            let librariesNode = JsonObject()
            let appLibraryNode = JsonObject()
            appLibraryNode.Add("type", JsonValue.Create("project"))
            appLibraryNode.Add("serviceable", JsonValue.Create(false))
            appLibraryNode.Add("sha512", JsonValue.Create(""))
            librariesNode.Add(appLibraryName, appLibraryNode)

            dependencies
            |> List.sortBy (fun dep -> dep.name.ToLowerInvariant())
            |> List.iter (fun dep ->
                let packageLibraryNode = JsonObject()
                packageLibraryNode.Add("type", JsonValue.Create("package"))
                packageLibraryNode.Add("serviceable", JsonValue.Create(true))
                packageLibraryNode.Add("sha512", JsonValue.Create(""))
                librariesNode.Add($"{dep.name}/{dep.version}", packageLibraryNode))

            let rootNode = JsonObject()
            rootNode.Add("runtimeTarget", runtimeTargetNode)
            rootNode.Add("compilationOptions", JsonObject())
            rootNode.Add("targets", targetsNode)
            rootNode.Add("libraries", librariesNode)

            let depsPath = Path.Join(outDir, $"{asmName}.deps.json")
            let options = JsonSerializerOptions(WriteIndented = true)
            File.WriteAllText(depsPath, rootNode.ToJsonString(options))
            Ok depsPath
        with ex ->
            Result.Error [ Diagnostic.Error($"Failed to write `{asmName}.deps.json`: {ex.Message}", Span.Empty) ]

    /// エラーメッセージを Diagnostic.Error へ変換する。
    let private error (message: string) : Diagnostic =
        Diagnostic.Error(message, Span.Empty)

    /// 診断付き失敗 BuildResult を構築する。
    let private failed (diagnostics: Diagnostic list) : BuildResult =
        { succeeded = false
          plan = None
          diagnostics = diagnostics }

    /// 診断なし成功 BuildResult を構築する。
    let private succeeded (plan: BuildPlan) : BuildResult =
        { succeeded = true
          plan = Some plan
          diagnostics = [] }

    /// YAML マッピングから指定キーの値ノードを取得する。
    let private tryFindMappingValue (mapping: YamlMappingNode) (key: string) : YamlNode option =
        mapping.Children
        |> Seq.tryPick (fun pair ->
            match pair.Key with
            | :? YamlScalarNode as scalar when scalar.Value = key -> Some pair.Value
            | _ -> None)

    /// Scalar ノードを空白不可の文字列として読み取る。
    let private tryGetRequiredScalarString (fieldPath: string) (node: YamlNode) : Result<string, Diagnostic list> =
        match node with
        | :? YamlScalarNode as scalar when not (String.IsNullOrWhiteSpace scalar.Value) ->
            Ok scalar.Value
        | :? YamlScalarNode ->
            Result.Error [ error $"`{fieldPath}` must not be empty" ]
        | _ ->
            Result.Error [ error $"`{fieldPath}` must be a string" ]

    /// [package] 相当ノードの必須項目を検証する。
    let private tryGetRequiredPackageField (package: YamlMappingNode) (fieldName: string) : Result<string, Diagnostic list> =
        match tryFindMappingValue package fieldName with
        | Some valueNode -> tryGetRequiredScalarString $"package.{fieldName}" valueNode
        | None -> Result.Error [ error $"missing required field `package.{fieldName}`" ]

    /// `package.type` を解釈する。未指定時は `exe` とする。
    let private tryParsePackageType (package: YamlMappingNode) : Result<Resolver.PackageType, Diagnostic list> =
        match tryFindMappingValue package "type" with
        | None -> Ok Resolver.PackageType.Exe
        | Some valueNode ->
            match tryGetRequiredScalarString "package.type" valueNode with
            | Ok value ->
                match value.Trim().ToLowerInvariant() with
                | "lib" -> Ok Resolver.PackageType.Lib
                | "exe" -> Ok Resolver.PackageType.Exe
                | "dll" -> Ok Resolver.PackageType.Dll
                | _ -> Result.Error [ error "`package.type` must be one of `lib`, `exe`, `dll`" ]
            | Result.Error diagnostics -> Result.Error diagnostics

    /// 複数の診断結果を左から順に連結して返す。
    let private collectErrors (errorResults: Diagnostic list list) : Diagnostic list =
        errorResults |> List.collect id

    /// Result から失敗診断のみを取り出す。
    let private errorsOf (result: Result<'T, Diagnostic list>) : Diagnostic list =
        match result with
        | Ok _ -> []
        | Result.Error diagnostics -> diagnostics

    /// dependencies の単一エントリを Resolver 用仕様へ変換する。
    let private parseDependencyEntry (dependencyName: string) (valueNode: YamlNode) : Result<Resolver.DependencySpec, Diagnostic list> =
        match valueNode with
        | :? YamlScalarNode as pathNode when not (String.IsNullOrWhiteSpace pathNode.Value) ->
            Ok(Resolver.PathDependency(dependencyName, pathNode.Value))
        | :? YamlScalarNode ->
            Result.Error [ error $"`dependencies.{dependencyName}` path must not be empty" ]
        | :? YamlMappingNode as inlineMapping ->
            let pathResult =
                match tryFindMappingValue inlineMapping "path" with
                | Some pathNode ->
                    match tryGetRequiredScalarString $"dependencies.{dependencyName}.path" pathNode with
                    | Ok pathValue -> Ok(Some pathValue)
                    | Result.Error diagnostics -> Result.Error diagnostics
                | None -> Ok None

            let versionResult =
                match tryFindMappingValue inlineMapping "version" with
                | Some versionNode ->
                    match tryGetRequiredScalarString $"dependencies.{dependencyName}.version" versionNode with
                    | Ok versionValue -> Ok(Some versionValue)
                    | Result.Error diagnostics -> Result.Error diagnostics
                | None -> Ok None

            match pathResult, versionResult with
            | Result.Error pathErrors, Ok _ -> Result.Error pathErrors
            | Ok _, Result.Error versionErrors -> Result.Error versionErrors
            | Result.Error pathErrors, Result.Error versionErrors ->
                Result.Error(pathErrors @ versionErrors)
            | Ok(Some _), Ok(Some _) ->
                Result.Error [ error $"`dependencies.{dependencyName}` cannot specify both `path` and `version`" ]
            | Ok(Some pathValue), Ok None ->
                Ok(Resolver.PathDependency(dependencyName, pathValue))
            | Ok None, Ok(Some versionValue) ->
                (* ユーザー指定バージョンは「その値以上」の開区間 range として解釈する。
                   nuspec 由来の推移依存と同じ VersionRange 型で統一することで、
                   MVS（Minimum Version Selection）ロジックが範囲比較を一貫して扱える。 *)
                try
                    let parsedVersion = NuGetVersion.Parse(versionValue)
                    Ok(Resolver.NuGetDependency(dependencyName, VersionRange(parsedVersion, true)))
                with _ ->
                    Result.Error [ error $"`dependencies.{dependencyName}.version` is not a valid NuGet version: `{versionValue}`" ]
            | Ok None, Ok None ->
                Result.Error [ error $"`dependencies.{dependencyName}` must define either `path` or `version`" ]
        | _ ->
            Result.Error [ error $"`dependencies.{dependencyName}` must be a string path or mapping with `path` / `version`" ]

    /// [dependencies] マッピングを deterministic な順序で Resolver 用仕様へ変換する。
    let private parseDependencies (root: YamlMappingNode) : Result<Resolver.DependencySpec list, Diagnostic list> =
        match tryFindMappingValue root "dependencies" with
        | None -> Ok []
        | Some (:? YamlMappingNode as dependenciesMapping) ->
            let orderedEntries =
                dependenciesMapping.Children
                |> Seq.map (fun pair -> pair.Key, pair.Value)
                |> Seq.sortBy (fun (keyNode, _) -> keyNode.ToString())
                |> Seq.toList

            let folder (dependencies, diagnostics) ((keyNode: YamlNode), (valueNode: YamlNode)) =
                match keyNode with
                | :? YamlScalarNode as keyScalar when not (String.IsNullOrWhiteSpace keyScalar.Value) ->
                    match parseDependencyEntry keyScalar.Value valueNode with
                    | Ok dependency -> (dependency :: dependencies, diagnostics)
                    | Result.Error errs -> (dependencies, diagnostics @ errs)
                | _ ->
                    let keyDiagnostic = error "`dependencies` keys must be non-empty strings"
                    (dependencies, diagnostics @ [ keyDiagnostic ])

            let (deps, diagnostics) = orderedEntries |> List.fold folder ([], [])

            if List.isEmpty diagnostics then
                Ok(List.rev deps)
            else
                Result.Error diagnostics
        | Some _ ->
            Result.Error [ error "`dependencies` must be a mapping" ]

    /// YAML テキストを root mapping へデシリアライズする。
    let private parseYamlRootMapping (manifestText: string) : Result<YamlMappingNode, Diagnostic list> =
        try
            let yaml = YamlStream()
            use reader = new StringReader(manifestText)
            yaml.Load(reader)

            if yaml.Documents.Count = 0 then
                Result.Error [ error "atla.yaml parse error: yaml document is empty" ]
            else
                match yaml.Documents[0].RootNode with
                | :? YamlMappingNode as rootMapping -> Ok rootMapping
                | _ -> Result.Error [ error "`atla.yaml` root node must be a mapping" ]
        with
        | :? YamlException as yamlEx ->
            Result.Error [ error $"atla.yaml parse error: {yamlEx.Message}" ]

    (* atla.yaml 全体を読み取り、package/dependencies を検証済み Manifest に変換する。 *)
    let private parseManifest (manifestPath: string) : Result<Resolver.Manifest, Diagnostic list> =
        if not (File.Exists manifestPath) then
            Result.Error [ error $"atla.yaml not found: {manifestPath}" ]
        else
            match parseYamlRootMapping (File.ReadAllText(manifestPath)) with
            | Result.Error diagnostics -> Result.Error diagnostics
            | Ok root ->
                match tryFindMappingValue root "package" with
                | Some (:? YamlMappingNode as packageMapping) ->
                    let nameResult = tryGetRequiredPackageField packageMapping "name"
                    let versionResult = tryGetRequiredPackageField packageMapping "version"
                    let packageTypeResult = tryParsePackageType packageMapping
                    let dependenciesResult = parseDependencies root
                    let diagnostics =
                        collectErrors [ errorsOf nameResult; errorsOf versionResult; errorsOf packageTypeResult; errorsOf dependenciesResult ]

                    match nameResult, versionResult, packageTypeResult, dependenciesResult with
                    | Ok packageName, Ok packageVersion, Ok packageType, Ok dependencies when List.isEmpty diagnostics ->
                        Ok {
                            name = packageName
                            version = packageVersion
                            packageType = packageType
                            dependencies = dependencies
                        }
                    | _ ->
                        Result.Error diagnostics
                | Some _ ->
                    Result.Error [ error "`package` must be a mapping" ]
                | None ->
                    Result.Error [ error "missing required mapping `package`" ]

    /// ソースが宛先より新しい（または宛先が存在しない）場合のみファイルをコピーする。
    /// 宛先の親ディレクトリが存在しない場合は自動的に作成する。
    /// ソースファイルが存在しない場合は Diagnostic.Error を返す。
    /// コピーした場合は Some destPath を、スキップした場合は None を返す。
    let private copyIfNewer (srcPath: string) (dstPath: string) : Result<string option, Diagnostic list> =
        try
            if not (File.Exists srcPath) then
                Result.Error [ error $"dependency DLL not found: `{srcPath}`" ]
            elif not (File.Exists dstPath) || File.GetLastWriteTimeUtc(srcPath) > File.GetLastWriteTimeUtc(dstPath) then
                Directory.CreateDirectory(Path.GetDirectoryName(dstPath)) |> ignore
                File.Copy(srcPath, dstPath, overwrite = true)
                Ok(Some dstPath)
            else
                Ok None
        with ex ->
            Result.Error [ error $"failed to copy `{srcPath}` to `{dstPath}`: {ex.Message}" ]

    /// 依存 DLL およびネイティブランタイムファイルを outDir へコピーする。
    /// runtimeLoadPaths はフラットに outDir へコピーし、
    /// nativeRuntimePaths は dep.source からの相対パスを保持して outDir 配下の
    /// runtimes/<rid>/native/ 階層を再現する。
    /// ソースが宛先より新しい場合（または宛先が存在しない場合）のみコピーを実行する。
    /// outDir は呼び出し前に存在していなければならない。
    /// コピーしたファイルのパスリストを返す。エラーは全ファイル分まとめて返す。
    let copyDependencies (dependencies: Compiler.ResolvedDependency list) (outDir: string) : Result<string list, Diagnostic list> =
        (* runtimeLoadPaths はフラットに outDir へコピーする。 *)
        let managedResults =
            dependencies
            |> List.collect (fun dep -> dep.runtimeLoadPaths)
            |> List.map (fun srcPath ->
                let dstPath = Path.Join(outDir, Path.GetFileName(srcPath))
                copyIfNewer srcPath dstPath)

        (* nativeRuntimePaths は dep.source からの相対パスを維持して outDir 配下に配置する。
           dep.source が空または空白のみの場合はフラットコピーにフォールバックする。
           パスの相対計算前に両パスを絶対パスへ正規化し、OS 依存の区切り文字や相対パス混入を防ぐ。 *)
        let nativeResults =
            dependencies
            |> List.collect (fun dep ->
                dep.nativeRuntimePaths
                |> List.map (fun srcPath ->
                    let dstPath =
                        if String.IsNullOrWhiteSpace dep.source || File.Exists(dep.source) then
                            Path.Join(outDir, Path.GetFileName(srcPath))
                        else
                            let normalizedSource = Path.GetFullPath(dep.source)
                            let normalizedSrc = Path.GetFullPath(srcPath)
                            Path.Join(outDir, Path.GetRelativePath(normalizedSource, normalizedSrc))
                    copyIfNewer srcPath dstPath))

        let results = managedResults @ nativeResults

        (* エラーを全件収集し、ひとつでもあれば失敗を返す。 *)
        let errors =
            results
            |> List.collect (fun r ->
                match r with
                | Result.Error diagnostics -> diagnostics
                | Ok _ -> [])

        if List.isEmpty errors then
            let copied =
                results
                |> List.choose (fun r ->
                    match r with
                    | Ok(Some path) -> Some path
                    | _ -> None)

            Ok copied
        else
            Result.Error errors

    /// `Resolver.PackageType` を公開 Build 用の型へ変換する。
    let private toBuildPackageType (packageType: Resolver.PackageType) : BuildPackageType =
        match packageType with
        | Resolver.PackageType.Exe -> BuildPackageType.Exe
        | Resolver.PackageType.Lib -> BuildPackageType.Lib
        | Resolver.PackageType.Dll -> BuildPackageType.Dll

    (* BuildRequest から最小の空 BuildPlan を組み立てる補助API。 *)
    let createEmptyPlan (request: BuildRequest) : BuildPlan =
        { projectName = ""
          projectVersion = ""
          projectRoot = request.projectRoot
          packageType = BuildPackageType.Exe
          dependencies = [] }

    (* Build エントリポイント:
       1) manifest を解析し
       2) Resolver で依存解決を実行し
       3) BuildPlan を返す。 *)
    let buildProject (request: BuildRequest) : BuildResult =
        let projectRoot = normalizePath request.projectRoot
        let manifestPath = Path.Join(projectRoot, manifestFileName)

        match parseManifest manifestPath with
        | Result.Error diagnostics -> failed diagnostics
        | Ok manifest ->
            match Resolver.resolveDependencies manifestFileName parseManifest projectRoot manifest with
            | Result.Error diagnostics -> failed diagnostics
            | Ok dependencies ->
                succeeded {
                    projectName = manifest.name
                    projectVersion = manifest.version
                    projectRoot = projectRoot
                    packageType = toBuildPackageType manifest.packageType
                    dependencies = dependencies
                }
    /// 文字列を UTF-8 バイト列へ変換する。
    let private toUtf8Bytes (value: string) : byte[] =
        Encoding.UTF8.GetBytes(value)

    /// バイト列の SHA-256 ハッシュを16進小文字文字列で返す。
    let private computeSha256Hex (bytes: byte[]) : string =
        use sha = SHA256.Create()
        let hashBytes = sha.ComputeHash(bytes)
        hashBytes
        |> Array.map (fun b -> b.ToString("x2"))
        |> String.concat ""

    /// 指定ファイルの SHA-256 ハッシュを計算する。
    let private computeFileSha256Hex (path: string) : Result<string, Diagnostic list> =
        try
            if not (File.Exists(path)) then
                Result.Error [ Diagnostic.Error($"file not found for hashing: `{path}`", Span.Empty) ]
            else
                let bytes = File.ReadAllBytes(path)
                Ok(computeSha256Hex bytes)
        with ex ->
            Result.Error [ Diagnostic.Error($"failed to hash `{path}`: {ex.Message}", Span.Empty) ]

    /// `A.B.c` 形式のシンボル名から末尾セグメントのみを取り出す。
    let private lastNameSegment (value: string) : string =
        let lastDot = value.LastIndexOf('.')
        if lastDot < 0 then value else value.Substring(lastDot + 1)

    /// SymbolId を安定した名前文字列へ変換する（未登録時は `Symbol#<id>` を返す）。
    let private symbolNameOrFallback (symbolTable: SymbolTable) (sid: SymbolId) : string =
        match symbolTable.Get(sid) with
        | Some symbolInfo -> symbolInfo.name
        | None -> $"Symbol#{sid.id}"

    /// TypeId を `.atlalib` 用の構造化 JSON ノードへ変換する。
    let rec private typeIdToApiNode (typeOwners: Map<int, string * string>) (symbolTable: SymbolTable) (tid: TypeId) : JsonNode =
        match tid with
        | TypeId.Unit -> JsonSerializer.SerializeToNode({| kind = "builtin"; name = "Unit" |})
        | TypeId.Bool -> JsonSerializer.SerializeToNode({| kind = "builtin"; name = "Bool" |})
        | TypeId.Int -> JsonSerializer.SerializeToNode({| kind = "builtin"; name = "Int" |})
        | TypeId.Float -> JsonSerializer.SerializeToNode({| kind = "builtin"; name = "Float" |})
        | TypeId.String -> JsonSerializer.SerializeToNode({| kind = "builtin"; name = "String" |})
        | TypeId.Native systemType ->
            JsonSerializer.SerializeToNode(
                {| kind = "nativeType"
                   fullName =
                       systemType.FullName
                       |> Option.ofObj
                       |> Option.defaultValue systemType.Name |})
        | TypeId.TypeVar name -> JsonSerializer.SerializeToNode({| kind = "typeVar"; name = name |})
        | TypeId.Name sid ->
            let node = JsonObject()
            node.Add("kind", JsonValue.Create("packageType"))
            match typeOwners.TryFind sid.id with
            | Some (moduleName, typeName) ->
                node.Add("module", JsonValue.Create(moduleName))
                node.Add("name", JsonValue.Create(typeName))
            | None ->
                node.Add("module", JsonValue.Create(""))
                node.Add("name", JsonValue.Create(symbolNameOrFallback symbolTable sid))
            node
        | TypeId.Fn(args, ret) ->
            let node = JsonObject()
            node.Add("kind", JsonValue.Create("function"))
            let argsNode = JsonArray()
            args |> List.iter (fun argType -> argsNode.Add(typeIdToApiNode typeOwners symbolTable argType))
            node.Add("args", argsNode)
            node.Add("return", typeIdToApiNode typeOwners symbolTable ret)
            node
        | TypeId.App(head, args) ->
            let node = JsonObject()
            node.Add("kind", JsonValue.Create("apply"))
            node.Add("head", typeIdToApiNode typeOwners symbolTable head)
            let argsNode = JsonArray()
            args |> List.iter (fun argType -> argsNode.Add(typeIdToApiNode typeOwners symbolTable argType))
            node.Add("args", argsNode)
            node
        | TypeId.Meta _ ->
            JsonSerializer.SerializeToNode({| kind = "builtin"; name = "Unit" |})
        | TypeId.Error message ->
            JsonSerializer.SerializeToNode({| kind = "nativeType"; fullName = $"error:{message}" |})
        | TypeId.VarargFn _ ->
            JsonSerializer.SerializeToNode({| kind = "builtin"; name = "Unit" |})
        | TypeId.ByRef inner ->
            let node = JsonObject()
            node.Add("kind", JsonValue.Create("byref"))
            node.Add("inner", typeIdToApiNode typeOwners symbolTable inner)
            node

    /// Export ID を仕様どおりに構築する。
    let private buildExportId (kind: string) (segments: string list) : string =
        String.concat ":" (kind :: segments)

    /// field export に hidden 予約接頭辞を適用する。
    let private buildFieldExportId (moduleName: string) (typeName: string) (fieldName: string) (isHidden: bool) : string =
        let exportFieldName = if isHidden then $"__hidden__{fieldName}" else fieldName
        buildExportId "field" [ moduleName; typeName; exportFieldName ]

    /// 関数シグネチャ JSON を構築する。
    let private createSignatureNode
        (typeOwners: Map<int, string * string>)
        (symbolTable: SymbolTable)
        (argDefs: (string * TypeId) list)
        (retType: TypeId)
        : JsonObject =
        let signatureNode = JsonObject()
        let argsNode = JsonArray()
        argDefs
        |> List.iter (fun (argName, argType) ->
            let argNode = JsonObject()
            argNode.Add("name", JsonValue.Create(argName))
            argNode.Add("type", typeIdToApiNode typeOwners symbolTable argType)
            argsNode.Add(argNode))
        signatureNode.Add("args", argsNode)
        signatureNode.Add("return", typeIdToApiNode typeOwners symbolTable retType)
        signatureNode

    /// `self` レシーバーを持つメソッドかを判定する。
    let private isInstanceMethod (methodDecl: Ast.Decl.Fn) : bool =
        match methodDecl.args with
        | (:? Ast.FnArg.Inferred as inferredArg) :: _ -> inferredArg.name = "self"
        | _ -> false

    /// モジュール別 HIR からトップレベル型所有者マップを構築する。
    let private collectTypeOwners (hirModules: Hir.Module list) (symbolTable: SymbolTable) : Map<int, string * string> =
        hirModules
        |> List.collect (fun hirModule ->
            hirModule.types
            |> List.map (fun hirType ->
                let typeName = symbolNameOrFallback symbolTable hirType.sym
                hirType.sym.id, (hirModule.name, typeName)))
        |> Map.ofList

    /// モジュール別 HIR + AST から public.api.json ノードを構築する。
    let private createPublicApiNode
        (packageName: string)
        (hirModules: Hir.Module list)
        (moduleAsts: Map<string, Ast.Module>)
        (symbolTable: SymbolTable)
        : JsonObject =
        let typeOwners = collectTypeOwners hirModules symbolTable
        let modulesNode = JsonArray()

        hirModules
        |> List.sortBy (fun hirModule -> hirModule.name)
        |> List.iter (fun hirModule ->
            let moduleNode = JsonObject()
            moduleNode.Add("name", JsonValue.Create(hirModule.name))
            let exportsNode = JsonObject()
            let valuesNode = JsonArray()
            let typesNode = JsonArray()
            let moduleAst = moduleAsts[hirModule.name]

            moduleAst.decls
            |> List.choose (fun decl ->
                match decl with
                | :? Ast.Decl.Fn as fnDecl -> Some fnDecl
                | _ -> None)
            |> List.sortBy (fun fnDecl -> fnDecl.name)
            |> List.iter (fun fnDecl ->
                let hirMethod =
                    hirModule.methods
                    |> List.find (fun methodInfo -> symbolNameOrFallback symbolTable methodInfo.sym = fnDecl.name)
                let valueNode = JsonObject()
                valueNode.Add("name", JsonValue.Create(fnDecl.name))
                valueNode.Add("exportId", JsonValue.Create(buildExportId "value" [ hirModule.name; fnDecl.name ]))
                valueNode.Add("kind", JsonValue.Create("function"))
                let argDefs =
                    hirMethod.args
                    |> List.map (fun (argSid, argType) -> lastNameSegment (symbolNameOrFallback symbolTable argSid), argType)
                valueNode.Add(
                    "signature",
                    createSignatureNode typeOwners symbolTable argDefs (match hirMethod.typ with | TypeId.Fn(_, ret) -> ret | _ -> hirMethod.typ))
                valuesNode.Add(valueNode))

            let hirTypesByName =
                hirModule.types
                |> List.map (fun hirType -> symbolNameOrFallback symbolTable hirType.sym, hirType)
                |> Map.ofList

            // インスタンス impl メソッドは hirType.methods へ移動したため、
            // モジュールメソッドと型インスタンスメソッドを統合して検索するヘルパーを定義する。
            let allHirMethods =
                hirModule.methods
                @ (hirModule.types
                   |> List.filter (fun t -> not t.isInterface)
                   |> List.collect (fun t -> t.methods))

            moduleAst.decls
            |> List.choose (fun decl ->
                match decl with
                | :? Ast.Decl.Data as dataDecl -> Some(dataDecl.name, "data", dataDecl.typeParams, Some dataDecl, None, None)
                | :? Ast.Decl.Enum as enumDecl -> Some(enumDecl.name, "enum", enumDecl.typeParams, None, Some enumDecl, None)
                | :? Ast.Decl.Role as roleDecl -> Some(roleDecl.name, "role", [], None, None, Some roleDecl)
                | _ -> None)
            |> List.sortBy (fun (typeName, _, _, _, _, _) -> typeName)
            |> List.iter (fun (typeName, kind, typeParams, dataDeclOpt, enumDeclOpt, roleDeclOpt) ->
                let hirType = hirTypesByName[typeName]
                let typeNode = JsonObject()
                typeNode.Add("name", JsonValue.Create(typeName))
                typeNode.Add("exportId", JsonValue.Create(buildExportId "type" [ hirModule.name; typeName ]))
                typeNode.Add("kind", JsonValue.Create(kind))
                typeNode.Add("typeParameters", JsonSerializer.SerializeToNode(typeParams))

                let fieldsNode = JsonArray()
                let methodsNode = JsonArray()

                let addFieldNode (fieldSid: SymbolId) (fieldType: TypeId) (isHidden: bool) =
                    let fieldName = lastNameSegment (symbolNameOrFallback symbolTable fieldSid)
                    let fieldNode = JsonObject()
                    fieldNode.Add("name", JsonValue.Create(fieldName))
                    fieldNode.Add("exportId", JsonValue.Create(buildFieldExportId hirModule.name typeName fieldName isHidden))
                    fieldNode.Add("type", typeIdToApiNode typeOwners symbolTable fieldType)
                    fieldNode.Add("isHidden", JsonValue.Create(isHidden))
                    fieldsNode.Add(fieldNode)

                let addMethodNode (methodDecl: Ast.Decl.Fn) (methodSym: SymbolId) =
                    let methodInfo =
                        allHirMethods
                        |> List.find (fun methodInfo -> methodInfo.sym.id = methodSym.id)
                    let isStatic = not (isInstanceMethod methodDecl)
                    let methodNode = JsonObject()
                    methodNode.Add("name", JsonValue.Create(methodDecl.name))
                    methodNode.Add(
                        "exportId",
                        JsonValue.Create(buildExportId "method" [ hirModule.name; typeName; methodDecl.name; if isStatic then "static" else "instance" ]))
                    methodNode.Add("isStatic", JsonValue.Create(isStatic))
                    let argDefs =
                        methodInfo.args
                        |> List.map (fun (argSid, argType) -> lastNameSegment (symbolNameOrFallback symbolTable argSid), argType)
                    methodNode.Add(
                        "signature",
                        createSignatureNode typeOwners symbolTable argDefs (match methodInfo.typ with | TypeId.Fn(_, ret) -> ret | _ -> methodInfo.typ))
                    methodsNode.Add(methodNode)

                match dataDeclOpt with
                | Some dataDecl ->
                    hirType.fields
                    |> List.sortBy (fun field -> lastNameSegment (symbolNameOrFallback symbolTable field.sym))
                    |> List.iter (fun field -> addFieldNode field.sym field.typ false)
                    moduleAst.decls
                    |> List.choose (fun decl ->
                        match decl with
                        | :? Ast.Decl.Impl as implDecl when (implDecl.forTypeName |> Option.defaultValue implDecl.typeName) = typeName ->
                            Some implDecl
                        | _ -> None)
                    |> List.collect (fun implDecl -> implDecl.methods)
                    |> List.sortBy (fun methodDecl -> methodDecl.name)
                    |> List.iter (fun methodDecl ->
                        let methodSym =
                            allHirMethods
                            |> List.find (fun methodInfo -> symbolNameOrFallback symbolTable methodInfo.sym = $"{typeName}.{methodDecl.name}")
                            |> fun methodInfo -> methodInfo.sym
                        addMethodNode methodDecl methodSym)
                | None -> ()

                match roleDeclOpt with
                | Some roleDecl ->
                    roleDecl.methods
                    |> List.sortBy (fun methodDecl -> methodDecl.name)
                    |> List.iter (fun methodDecl ->
                        let methodSym =
                            hirType.methods
                            |> List.find (fun methodInfo -> symbolNameOrFallback symbolTable methodInfo.sym = $"{typeName}.{methodDecl.name}")
                            |> fun methodInfo -> methodInfo.sym
                        addMethodNode (Ast.Decl.Fn(methodDecl.name, methodDecl.args, methodDecl.ret, Ast.Expr.Unit(Span.Empty), false, false, methodDecl.span)) methodSym)
                | None -> ()

                match enumDeclOpt with
                | Some enumDecl ->
                    hirType.fields
                    |> List.iter (fun field -> addFieldNode field.sym field.typ true)
                    let casesNode = JsonArray()
                    enumDecl.cases
                    |> List.mapi (fun tag caseDecl -> tag, caseDecl)
                    |> List.iter (fun (tag, caseDecl) ->
                        match caseDecl with
                        | :? Ast.EnumCase.Case as enumCase ->
                            let caseNode = JsonObject()
                            caseNode.Add("name", JsonValue.Create(enumCase.name))
                            caseNode.Add("exportId", JsonValue.Create(buildExportId "enumCase" [ hirModule.name; typeName; enumCase.name ]))
                            caseNode.Add("tag", JsonValue.Create(tag))
                            let payloadTypeName = $"{typeName}.__enum_payload_{enumCase.name}_type"
                            let payloadTypeOpt = hirTypesByName |> Map.tryFind payloadTypeName
                            let payloadFieldsNode = JsonArray()
                            enumCase.fields
                            |> List.iteri (fun index fieldDecl ->
                                let payloadFieldNode = JsonObject()
                                payloadFieldNode.Add("name", JsonValue.Create(fieldDecl.name))
                                let payloadFieldType =
                                    match payloadTypeOpt with
                                    | Some payloadType when index < payloadType.fields.Length ->
                                        payloadType.fields[index].typ
                                    | _ ->
                                        TypeId.Unit
                                payloadFieldNode.Add("type", typeIdToApiNode typeOwners symbolTable payloadFieldType)
                                payloadFieldsNode.Add(payloadFieldNode))
                            if not enumCase.fields.IsEmpty then
                                caseNode.Add("payloadTypeName", JsonValue.Create(payloadTypeName))
                            caseNode.Add("payloadFields", payloadFieldsNode)
                            casesNode.Add(caseNode)
                        | _ -> ())
                    typeNode.Add("cases", casesNode)
                | None -> ()

                typeNode.Add("fields", fieldsNode)
                typeNode.Add("methods", methodsNode)
                match hirType.baseType with
                | Some baseType -> typeNode.Add("baseType", typeIdToApiNode typeOwners symbolTable baseType)
                | None -> ()
                moduleAst.decls
                |> List.choose (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as implDecl when (implDecl.forTypeName |> Option.defaultValue implDecl.typeName) = typeName && implDecl.byFieldName.IsSome ->
                        implDecl.byFieldName
                    | _ -> None)
                |> List.tryHead
                |> Option.iter (fun delegatedFieldName -> typeNode.Add("delegatedByFieldName", JsonValue.Create(delegatedFieldName)))
                typesNode.Add(typeNode))

            exportsNode.Add("values", valuesNode)
            exportsNode.Add("types", typesNode)
            moduleNode.Add("exports", exportsNode)

            // public import 宣言による再エクスポートを記録する（非修飾モジュール名で記録）。
            let reexportsNode = JsonArray()
            moduleAst.decls
            |> List.iter (fun decl ->
                match decl with
                | :? Ast.Decl.Import as importDecl when importDecl.isPublic ->
                    let importedModuleName = String.concat "." importDecl.path
                    reexportsNode.Add(JsonValue.Create(importedModuleName))
                | _ -> ())
            if reexportsNode.Count > 0 then
                moduleNode.Add("reexports", reexportsNode)

            modulesNode.Add(moduleNode))

        let rootNode = JsonObject()
        rootNode.Add("schemaVersion", JsonValue.Create(atlaLibSymbolSchemaVersion))
        rootNode.Add("modules", modulesNode)
        rootNode

    /// 依存ごとの lock 情報ノードを構築する。
    let private createDependencyLockNode (dependencies: Compiler.ResolvedDependency list) : Result<JsonObject, Diagnostic list> =
        let appendAssetHash (acc: Result<(string * string) list, Diagnostic list>) (assetPath: string) =
            match acc with
            | Result.Error diagnostics -> Result.Error diagnostics
            | Ok hashes ->
                match computeFileSha256Hex assetPath with
                | Result.Error diagnostics -> Result.Error diagnostics
                | Ok hash ->
                    Ok((normalizeDepsPath assetPath, $"sha256:{hash}") :: hashes)

        let buildOneDependency (dependency: Compiler.ResolvedDependency) : Result<JsonObject, Diagnostic list> =
            let runtimeAssetResult =
                dependency.runtimeLoadPaths
                |> List.sort
                |> List.fold appendAssetHash (Ok [])

            let nativeAssetResult =
                dependency.nativeRuntimePaths
                |> List.sort
                |> List.fold appendAssetHash (Ok [])

            match runtimeAssetResult, nativeAssetResult with
            | Result.Error runtimeDiagnostics, Result.Error nativeDiagnostics ->
                Result.Error(runtimeDiagnostics @ nativeDiagnostics)
            | Result.Error diagnostics, _
            | _, Result.Error diagnostics ->
                Result.Error diagnostics
            | Ok runtimeAssets, Ok nativeAssets ->
                let orderedRuntimeAssets = runtimeAssets |> List.rev
                let orderedNativeAssets = nativeAssets |> List.rev
                // contentHash は `sha256:<asset-hash>|...` の連結をさらに SHA-256 した安定表現とする。
                // SHA-256 文字列配列を JSON として直列化し、そのバイト列をハッシュして contentHash を作る。
                let contentHashSeed =
                    (orderedRuntimeAssets @ orderedNativeAssets)
                    |> List.map snd
                    |> JsonSerializer.Serialize
                let dependencyNode = JsonObject()
                dependencyNode.Add("id", JsonValue.Create(dependency.name))
                dependencyNode.Add("version", JsonValue.Create(dependency.version))
                dependencyNode.Add("source", JsonValue.Create(dependency.source))
                dependencyNode.Add("contentHash", JsonValue.Create($"sha256:{computeSha256Hex (toUtf8Bytes contentHashSeed)}"))
                dependencyNode.Add("runtimeAssets", JsonSerializer.SerializeToNode(orderedRuntimeAssets))
                dependencyNode.Add("nativeAssets", JsonSerializer.SerializeToNode(orderedNativeAssets))
                Ok dependencyNode

        let orderedDependencies =
            dependencies
            |> List.sortBy (fun dependency -> dependency.name.ToLowerInvariant())

        let folder (state: Result<JsonArray, Diagnostic list>) (dependency: Compiler.ResolvedDependency) =
            match state with
            | Result.Error diagnostics -> Result.Error diagnostics
            | Ok dependenciesNode ->
                match buildOneDependency dependency with
                | Result.Error diagnostics -> Result.Error diagnostics
                | Ok dependencyNode ->
                    dependenciesNode.Add(dependencyNode)
                    Ok dependenciesNode

        match orderedDependencies |> List.fold folder (Ok(JsonArray())) with
        | Result.Error diagnostics -> Result.Error diagnostics
        | Ok dependenciesNode ->
            let rootNode = JsonObject()
            rootNode.Add("dependencies", dependenciesNode)
            Ok rootNode

    /// in-memory エントリ群を .atlalib (ZIP) として書き出す。
    let private writeAtlaLibZip (atlaLibPath: string) (entries: (string * byte[]) list) : Result<unit, Diagnostic list> =
        try
            if File.Exists(atlaLibPath) then
                File.Delete(atlaLibPath)

            use fileStream = File.Open(atlaLibPath, FileMode.CreateNew, FileAccess.Write, FileShare.None)
            use archive = new ZipArchive(fileStream, ZipArchiveMode.Create, leaveOpen = false)
            entries
            |> List.sortBy fst
            |> List.iter (fun (entryPath, entryBytes) ->
                let entry = archive.CreateEntry(entryPath, CompressionLevel.Optimal)
                use stream = entry.Open()
                stream.Write(entryBytes, 0, entryBytes.Length))

            Ok()
        with ex ->
            Result.Error [ Diagnostic.Error($"Failed to write `.atlalib`: {ex.Message}", Span.Empty) ]

    /// `compileModules` の成果物を .atlalib コンテナとして出力する。
    let createAtlaLib
        (projectName: string)
        (projectVersion: string)
        (asmName: string)
        (compileOutDir: string)
        (outDir: string)
        (dependencies: Compiler.ResolvedDependency list)
        (compileResult: Compiler.CompileResult)
        : Result<string, Diagnostic list> =
        try
            let assemblyPath = Path.Join(compileOutDir, $"{asmName}.dll")
            let pdbPath = Path.Join(compileOutDir, $"{asmName}.pdb")
            let atlaLibPath = Path.Join(outDir, $"{projectName}.atlalib")

            if not (File.Exists(assemblyPath)) then
                Result.Error [ Diagnostic.Error($"assembly not found for .atlalib packaging: `{assemblyPath}`", Span.Empty) ]
            else
                match compileResult.hirModules, compileResult.moduleAsts, compileResult.symbolTable with
                | None, _, _
                | _, None, _
                | _, _, None ->
                    Result.Error [ Diagnostic.Error("public API extraction requires successful semantic analysis result", Span.Empty) ]
                | Some hirModules, Some moduleAsts, Some symbolTable ->
                    match createDependencyLockNode dependencies with
                    | Result.Error diagnostics -> Result.Error diagnostics
                    | Ok lockNode ->
                        let jsonOptions = JsonSerializerOptions(WriteIndented = true)
                        let publicApiNode = createPublicApiNode projectName hirModules moduleAsts symbolTable

                        let entryBytes = ResizeArray<string * byte[]>()
                        entryBytes.Add("assemblies/" + $"{asmName}.dll", File.ReadAllBytes(assemblyPath))

                        if File.Exists(pdbPath) then
                            entryBytes.Add("assemblies/" + $"{asmName}.pdb", File.ReadAllBytes(pdbPath))

                        let publicApiBytes = publicApiNode.ToJsonString(jsonOptions) |> toUtf8Bytes
                        entryBytes.Add("symbols/public.api.json", publicApiBytes)

                        let lockBytes = lockNode.ToJsonString(jsonOptions) |> toUtf8Bytes
                        entryBytes.Add("deps/manifest.lock.json", lockBytes)

                        let atlaLibMetadataNode = JsonObject()
                        atlaLibMetadataNode.Add("formatVersion", JsonValue.Create(atlaLibFormatVersion))
                        atlaLibMetadataNode.Add("package", JsonSerializer.SerializeToNode({| name = projectName; version = projectVersion |}))
                        atlaLibMetadataNode.Add(
                            "compiler",
                            JsonSerializer.SerializeToNode(
                                {| name = "atla"
                                   version = atlaCompilerVersion
                                   targetFramework = "net10.0" |}))
                        atlaLibMetadataNode.Add(
                            "artifacts",
                            JsonSerializer.SerializeToNode(
                                {| assembly = $"assemblies/{asmName}.dll"
                                   publicApi = "symbols/public.api.json"
                                   dependencyLock = "deps/manifest.lock.json" |}))
                        atlaLibMetadataNode.Add(
                            "compat",
                            JsonSerializer.SerializeToNode(
                                {| languageAbi = atlaLanguageAbi
                                   symbolSchemaVersion = atlaLibSymbolSchemaVersion |}))

                        let metadataBytes = atlaLibMetadataNode.ToJsonString(jsonOptions) |> toUtf8Bytes
                        entryBytes.Add("atlalib.json", metadataBytes)

                        let hashLines =
                            entryBytes
                            |> Seq.sortBy fst
                            // `sha256sum` 互換で `<sha256><space><space><path>` 形式を採用する。
                            |> Seq.map (fun (path, bytes) -> $"{computeSha256Hex bytes}  {path}")
                            |> String.concat Environment.NewLine
                        let hashBytes = toUtf8Bytes hashLines
                        entryBytes.Add("hashes/sha256sums.txt", hashBytes)

                        match writeAtlaLibZip atlaLibPath (entryBytes |> Seq.toList) with
                        | Result.Error diagnostics -> Result.Error diagnostics
                        | Ok () -> Ok atlaLibPath
        with ex ->
            Result.Error [ Diagnostic.Error($"Failed to package `.atlalib`: {ex.Message}", Span.Empty) ]
