namespace Atla.Build

open System
open System.IO
open System.Collections.Generic
open Atla.Core.Data
open Atla.Core.Semantics.Data
open Atla.Compiler
open Tomlyn
open Tomlyn.Model
open Tomlyn.Serialization

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
    let private manifestFileName = "atla.toml"

    let private normalizePath (path: string) : string =
        Path.GetFullPath(path)

    let private error (message: string) : Diagnostic =
        Diagnostic.Error(message, Span.Empty)

    let private failed (diagnostics: Diagnostic list) : BuildResult =
        { succeeded = false
          plan = None
          diagnostics = diagnostics }

    let private succeeded (plan: BuildPlan) : BuildResult =
        { succeeded = true
          plan = Some plan
          diagnostics = [] }

    (* [package] テーブルの必須文字列項目を検証する。 *)
    let private tryGetRequiredString (table: TomlTable) (fieldName: string) : Result<string, Diagnostic list> =
        match table.TryGetValue(fieldName) with
        | true, (:? string as value) when not (String.IsNullOrWhiteSpace value) ->
            Ok value
        | true, (:? string) ->
            Result.Error [ error $"`package.{fieldName}` must not be empty" ]
        | true, _ ->
            Result.Error [ error $"`package.{fieldName}` must be a string" ]
        | false, _ ->
            Result.Error [ error $"missing required field `package.{fieldName}`" ]

    (* [dependencies] テーブルを deterministic な順序で解釈し、Resolver 用の依存仕様へ変換する。 *)
    let private parseDependencies (root: TomlTable) : Result<Resolver.DependencySpec list, Diagnostic list> =
        match root.TryGetValue("dependencies") with
        | false, _ -> Ok []
        | true, (:? TomlTable as dependenciesTable) ->
            let parseEntry (entry: KeyValuePair<string, obj>) : Result<Resolver.DependencySpec, Diagnostic list> =
                match entry.Value with
                | :? string as pathValue when not (String.IsNullOrWhiteSpace pathValue) ->
                    Ok(Resolver.PathDependency(entry.Key, pathValue))
                | :? string ->
                    Result.Error [ error $"`dependencies.{entry.Key}` path must not be empty" ]
                | :? TomlTable as inlineTable ->
                    let pathResult =
                        match inlineTable.TryGetValue("path") with
                        | true, (:? string as pathValue) when not (String.IsNullOrWhiteSpace pathValue) -> Ok(Some pathValue)
                        | true, (:? string) ->
                            Result.Error [ error $"`dependencies.{entry.Key}.path` must not be empty" ]
                        | true, _ ->
                            Result.Error [ error $"`dependencies.{entry.Key}.path` must be a string" ]
                        | false, _ -> Ok None

                    let versionResult =
                        match inlineTable.TryGetValue("version") with
                        | true, (:? string as version) when not (String.IsNullOrWhiteSpace version) -> Ok(Some version)
                        | true, (:? string) ->
                            Result.Error [ error $"`dependencies.{entry.Key}.version` must not be empty" ]
                        | true, _ ->
                            Result.Error [ error $"`dependencies.{entry.Key}.version` must be a string" ]
                        | false, _ -> Ok None

                    match pathResult, versionResult with
                    | Result.Error pathErrors, Ok _ -> Result.Error pathErrors
                    | Ok _, Result.Error versionErrors -> Result.Error versionErrors
                    | Result.Error pathErrors, Result.Error versionErrors ->
                        Result.Error(pathErrors @ versionErrors)
                    | Ok(Some _), Ok(Some _) ->
                        Result.Error [ error $"`dependencies.{entry.Key}` cannot specify both `path` and `version`" ]
                    | Ok(Some pathValue), Ok None ->
                        Ok(Resolver.PathDependency(entry.Key, pathValue))
                    | Ok None, Ok(Some version) ->
                        Ok(Resolver.NuGetDependency(entry.Key, version))
                    | Ok None, Ok None ->
                        Result.Error [ error $"`dependencies.{entry.Key}` must define either `path` or `version`" ]
                | _ ->
                    Result.Error [ error $"`dependencies.{entry.Key}` must be a string path or table with `path` / `version`" ]

            let orderedEntries =
                dependenciesTable
                |> Seq.sortBy (fun pair -> pair.Key)
                |> Seq.toList

            let folder (dependencies, diagnostics) entry =
                match parseEntry entry with
                | Ok dependency -> (dependency :: dependencies, diagnostics)
                | Result.Error errs -> (dependencies, diagnostics @ errs)

            let (deps, diagnostics) = orderedEntries |> List.fold folder ([], [])

            if List.isEmpty diagnostics then
                Ok(List.rev deps)
            else
                Result.Error diagnostics
        | true, _ ->
            Result.Error [ error "`dependencies` must be a table" ]

    (* atla.toml 全体を読み取り、package/dependencies を検証済み Manifest に変換する。 *)
    let private parseManifest (manifestPath: string) : Result<Resolver.Manifest, Diagnostic list> =
        if not (File.Exists manifestPath) then
            Result.Error [ error $"atla.toml not found: {manifestPath}" ]
        else
            try
                let root = TomlSerializer.Deserialize<TomlTable>(File.ReadAllText(manifestPath))

                match root.TryGetValue("package") with
                | true, (:? TomlTable as packageTable) ->
                    match tryGetRequiredString packageTable "name", tryGetRequiredString packageTable "version", parseDependencies root with
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
                | true, _ ->
                    Result.Error [ error "`package` must be a table" ]
                | false, _ ->
                    Result.Error [ error "missing required table `[package]`" ]
            with
            | :? TomlException as tomlEx when not (isNull tomlEx.Diagnostics) && tomlEx.Diagnostics.Count > 0 ->
                tomlEx.Diagnostics
                |> Seq.map (fun diag -> error $"atla.toml parse error: {diag.Message}")
                |> Seq.toList
                |> Result.Error
            | :? TomlException as tomlEx ->
                Result.Error [ error $"atla.toml parse error: {tomlEx.Message}" ]

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
