namespace Atla.Build

open System
open System.IO
open Atla.Core.Data
open Atla.Core.Semantics.Data
open Atla.Compiler
open YamlDotNet.Core
open YamlDotNet.RepresentationModel

type BuildRequest =
    { projectRoot: string }

type BuildPlan =
    { projectName: string
      projectVersion: string
      projectRoot: string
      dependencies: Compiler.ResolvedDependency list }

type BuildResult =
    { succeeded: bool
      plan: BuildPlan option
      diagnostics: Diagnostic list }

module BuildSystem =
    (* Manifest ファイル関連の固定設定と、共通ユーティリティ。 *)
    let private manifestFileName = "atla.yaml"

    /// Build 入力パスを絶対パスへ正規化する。
    let private normalizePath (path: string) : string =
        Path.GetFullPath(path)

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
                Ok(Resolver.NuGetDependency(dependencyName, versionValue))
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
                    match
                        tryGetRequiredPackageField packageMapping "name",
                        tryGetRequiredPackageField packageMapping "version",
                        parseDependencies root
                    with
                    | Ok packageName, Ok packageVersion, Ok dependencies ->
                        Ok {
                            name = packageName
                            version = packageVersion
                            dependencies = dependencies
                        }
                    | Result.Error nameErrors, Ok _, Ok _ -> Result.Error nameErrors
                    | Ok _, Result.Error versionErrors, Ok _ -> Result.Error versionErrors
                    | Ok _, Ok _, Result.Error dependencyErrors -> Result.Error dependencyErrors
                    | Result.Error nameErrors, Result.Error versionErrors, Ok _ -> Result.Error(nameErrors @ versionErrors)
                    | Result.Error nameErrors, Ok _, Result.Error dependencyErrors -> Result.Error(nameErrors @ dependencyErrors)
                    | Ok _, Result.Error versionErrors, Result.Error dependencyErrors ->
                        Result.Error(versionErrors @ dependencyErrors)
                    | Result.Error nameErrors, Result.Error versionErrors, Result.Error dependencyErrors ->
                        Result.Error(nameErrors @ versionErrors @ dependencyErrors)
                | Some _ ->
                    Result.Error [ error "`package` must be a mapping" ]
                | None ->
                    Result.Error [ error "missing required mapping `package`" ]

    (* BuildRequest から最小の空 BuildPlan を組み立てる補助API。 *)
    let createEmptyPlan (request: BuildRequest) : BuildPlan =
        { projectName = ""
          projectVersion = ""
          projectRoot = request.projectRoot
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
                    dependencies = dependencies
                }
