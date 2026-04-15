namespace Atla.Build

open System
open System.IO
open System.Globalization
open Atla.Core.Data
open Atla.Core.Semantics.Data
open Atla.Compiler

module internal Resolver =
    type DependencySpec =
        | PathDependency of name: string * relativePath: string
        | NuGetDependency of packageId: string * version: string

    type Manifest =
        { name: string
          version: string
          dependencies: DependencySpec list }

    type private ResolveState =
        { stack: string list
          visitedByPath: Map<string, Compiler.ResolvedDependency>
          sourceByName: Map<string, string>
          resolvedByName: Map<string, Compiler.ResolvedDependency>
          diagnostics: Diagnostic list }

    let private normalizePath (path: string) : string =
        Path.GetFullPath(path)

    let private error (message: string) : Diagnostic =
        Diagnostic.Error(message, Span.Empty)

    let private getNuGetPackagesRoot () : string =
        match Environment.GetEnvironmentVariable("NUGET_PACKAGES") with
        | value when not (String.IsNullOrWhiteSpace value) -> normalizePath value
        | _ ->
            let homeDir = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
            normalizePath (Path.Join(homeDir, ".nuget", "packages"))

    let private toNuGetPathSegment (value: string) : string =
        value.ToLower(CultureInfo.InvariantCulture)

    let private tryResolveNuGetDependency (packageId: string) (version: string) : Result<Compiler.ResolvedDependency, Diagnostic list> =
        let packagesRoot = getNuGetPackagesRoot ()
        let packagePath =
            Path.Join(packagesRoot, toNuGetPathSegment packageId, toNuGetPathSegment version)
            |> normalizePath

        if Directory.Exists packagePath then
            Ok {
                name = packageId
                version = version
                source = packagePath
            }
        else
            Result.Error [
                error
                    $"nuget package not found in cache: {packageId} {version} (expected: {packagePath}). Set NUGET_PACKAGES or run restore beforehand."
            ]

    let resolveDependencies
        (manifestFileName: string)
        (parseManifest: string -> Result<Manifest, Diagnostic list>)
        (projectRoot: string)
        (manifest: Manifest)
        : Result<Compiler.ResolvedDependency list, Diagnostic list> =
        let rec visitDependency (state: ResolveState) (ownerRoot: string) (dependency: DependencySpec) : ResolveState =
            match dependency with
            | NuGetDependency(packageId, version) ->
                let dependencyNameKey = packageId.ToLowerInvariant()

                match tryResolveNuGetDependency packageId version with
                | Result.Error diagnostics ->
                    { state with diagnostics = state.diagnostics @ diagnostics }
                | Ok resolved ->
                    match state.sourceByName.TryFind(dependencyNameKey) with
                    | Some existingSource when not (String.Equals(existingSource, resolved.source, StringComparison.Ordinal)) ->
                        { state with diagnostics = state.diagnostics @ [ error $"duplicate dependency name `{packageId}` resolved from `{existingSource}` and `{resolved.source}`" ] }
                    | _ ->
                        { state with
                            sourceByName = state.sourceByName.Add(dependencyNameKey, resolved.source)
                            resolvedByName = state.resolvedByName.Add(dependencyNameKey, resolved) }
            | PathDependency(name, relativePath) ->
                let dependencyRoot = normalizePath (Path.Join(ownerRoot, relativePath))
                let manifestPath = Path.Join(dependencyRoot, manifestFileName)

                if List.contains dependencyRoot state.stack then
                    let cyclePath = state.stack @ [ dependencyRoot ]
                    let cycleDescription = cyclePath |> List.map Path.GetFileName |> String.concat " -> "
                    { state with diagnostics = state.diagnostics @ [ error $"cyclic dependency detected: {cycleDescription}" ] }
                elif not (Directory.Exists dependencyRoot) then
                    { state with diagnostics = state.diagnostics @ [ error $"dependency path not found: {name} -> {dependencyRoot}" ] }
                elif state.visitedByPath.ContainsKey(dependencyRoot) then
                    state
                else
                    match parseManifest manifestPath with
                    | Result.Error diagnostics ->
                        let wrapped =
                            diagnostics
                            |> List.map (fun diagnostic -> error $"dependency `{name}`: {diagnostic.message}")

                        { state with diagnostics = state.diagnostics @ wrapped }
                    | Ok dependencyManifest ->
                        let dependencyNameKey = dependencyManifest.name.ToLowerInvariant()

                        match state.sourceByName.TryFind(dependencyNameKey) with
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
                                    sourceByName = state.sourceByName.Add(dependencyNameKey, dependencyRoot)
                                    resolvedByName = state.resolvedByName.Add(dependencyNameKey, resolved) }

                            let nestedState =
                                dependencyManifest.dependencies
                                |> List.fold (fun currentState child -> visitDependency currentState dependencyRoot child) enteredState

                            { nestedState with stack = state.stack }

        let initialState =
            { stack = [ normalizePath projectRoot ]
              visitedByPath = Map.empty
              sourceByName = Map.empty
              resolvedByName = Map.empty
              diagnostics = [] }

        let finalState = manifest.dependencies |> List.fold (fun state dependency -> visitDependency state projectRoot dependency) initialState

        if List.isEmpty finalState.diagnostics then
            finalState.resolvedByName
            |> Map.toList
            |> List.map snd
            |> List.sortBy (fun dep -> dep.name)
            |> Ok
        else
            Result.Error finalState.diagnostics
