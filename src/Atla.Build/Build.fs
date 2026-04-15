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
    let private manifestFileName = "atla.toml"

    type private DependencySpec =
        { name: string
          relativePath: string }

    type private Manifest =
        { name: string
          version: string
          dependencies: DependencySpec list }

    type private ResolveState =
        { stack: string list
          visitedByPath: Map<string, Compiler.ResolvedDependency>
          pathByName: Map<string, string>
          diagnostics: Diagnostic list }

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

    let private parseDependencies (root: TomlTable) : Result<DependencySpec list, Diagnostic list> =
        match root.TryGetValue("dependencies") with
        | false, _ -> Ok []
        | true, (:? TomlTable as dependenciesTable) ->
            let parseEntry (entry: KeyValuePair<string, obj>) : Result<DependencySpec, Diagnostic list> =
                match entry.Value with
                | :? string as pathValue when not (String.IsNullOrWhiteSpace pathValue) ->
                    Ok { name = entry.Key; relativePath = pathValue }
                | :? string ->
                    Result.Error [ error $"`dependencies.{entry.Key}` path must not be empty" ]
                | :? TomlTable as inlineTable ->
                    match inlineTable.TryGetValue("path") with
                    | true, (:? string as pathValue) when not (String.IsNullOrWhiteSpace pathValue) ->
                        Ok { name = entry.Key; relativePath = pathValue }
                    | true, (:? string) ->
                        Result.Error [ error $"`dependencies.{entry.Key}.path` must not be empty" ]
                    | true, _ ->
                        Result.Error [ error $"`dependencies.{entry.Key}.path` must be a string" ]
                    | false, _ ->
                        Result.Error [ error $"missing required field `dependencies.{entry.Key}.path`" ]
                | _ ->
                    Result.Error [ error $"`dependencies.{entry.Key}` must be a string path or table with `path`" ]

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

    let private parseManifest (manifestPath: string) : Result<Manifest, Diagnostic list> =
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

    let createEmptyPlan (request: BuildRequest) : BuildPlan =
        { projectName = ""
          projectVersion = ""
          projectRoot = request.projectRoot
          dependencies = [] }

    let private resolveDependencies (projectRoot: string) (manifest: Manifest) : Result<Compiler.ResolvedDependency list, Diagnostic list> =
        let rec visitDependency (state: ResolveState) (ownerRoot: string) (dependency: DependencySpec) : ResolveState =
            let dependencyRoot = normalizePath (Path.Join(ownerRoot, dependency.relativePath))
            let manifestPath = Path.Join(dependencyRoot, manifestFileName)

            if List.contains dependencyRoot state.stack then
                let cyclePath = state.stack @ [ dependencyRoot ]
                let cycleDescription = cyclePath |> List.map Path.GetFileName |> String.concat " -> "
                { state with diagnostics = state.diagnostics @ [ error $"cyclic dependency detected: {cycleDescription}" ] }
            elif not (Directory.Exists dependencyRoot) then
                { state with diagnostics = state.diagnostics @ [ error $"dependency path not found: {dependency.name} -> {dependencyRoot}" ] }
            elif state.visitedByPath.ContainsKey(dependencyRoot) then
                state
            else
                match parseManifest manifestPath with
                | Result.Error diagnostics ->
                    let wrapped =
                        diagnostics
                        |> List.map (fun diagnostic -> error $"dependency `{dependency.name}`: {diagnostic.message}")

                    { state with diagnostics = state.diagnostics @ wrapped }
                | Ok dependencyManifest ->
                    let dependencyNameKey = dependencyManifest.name.ToLowerInvariant()

                    match state.pathByName.TryFind(dependencyNameKey) with
                    | Some existingPath when not (String.Equals(existingPath, dependencyRoot, StringComparison.Ordinal)) ->
                        { state with diagnostics = state.diagnostics @ [ error $"duplicate dependency name `{dependencyManifest.name}` resolved from `{existingPath}` and `{dependencyRoot}`" ] }
                    | _ ->
                        let resolved : Compiler.ResolvedDependency =
                            { name = dependencyManifest.name
                              version = dependencyManifest.version
                              source = dependencyRoot }

                        let enteredState =
                            { state with
                                stack = state.stack @ [ dependencyRoot ]
                                visitedByPath = state.visitedByPath.Add(dependencyRoot, resolved)
                                pathByName = state.pathByName.Add(dependencyNameKey, dependencyRoot) }

                        let nestedState =
                            dependencyManifest.dependencies
                            |> List.fold (fun currentState child -> visitDependency currentState dependencyRoot child) enteredState

                        { nestedState with stack = state.stack }

        let initialState =
            { stack = [ normalizePath projectRoot ]
              visitedByPath = Map.empty
              pathByName = Map.empty
              diagnostics = [] }

        let finalState = manifest.dependencies |> List.fold (fun state dependency -> visitDependency state projectRoot dependency) initialState

        if List.isEmpty finalState.diagnostics then
            finalState.visitedByPath
            |> Map.toList
            |> List.map snd
            |> List.sortBy (fun dep -> dep.name)
            |> Ok
        else
            Result.Error finalState.diagnostics

    let buildProject (request: BuildRequest) : BuildResult =
        let projectRoot = normalizePath request.projectRoot
        let manifestPath = Path.Join(projectRoot, manifestFileName)

        match parseManifest manifestPath with
        | Result.Error diagnostics -> failed diagnostics
        | Ok manifest ->
            match resolveDependencies projectRoot manifest with
            | Result.Error diagnostics -> failed diagnostics
            | Ok dependencies ->
                succeeded {
                    projectName = manifest.name
                    projectVersion = manifest.version
                    projectRoot = projectRoot
                    dependencies = dependencies
                }
