namespace Atla.Build

open System
open System.Diagnostics
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

    let private isAutoRestoreEnabled () : bool =
        match Environment.GetEnvironmentVariable("ATLA_BUILD_ENABLE_NUGET_RESTORE") with
        | null -> false
        | value ->
            let normalized = value.Trim().ToLowerInvariant()
            normalized = "1" || normalized = "true" || normalized = "yes"

    let private tryRunRestore (packagesRoot: string) (packageId: string) (version: string) : Result<unit, string> =
        let tempRoot = Path.Join(Path.GetTempPath(), $"atla-build-nuget-restore-{Guid.NewGuid():N}")
        Directory.CreateDirectory(tempRoot) |> ignore

        let projectPath = Path.Join(tempRoot, "restore.csproj")

        let projectContent =
            $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <RestorePackagesWithLockFile>false</RestorePackagesWithLockFile>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="{packageId}" Version="{version}" />
  </ItemGroup>
</Project>"""

        File.WriteAllText(projectPath, projectContent)

        let startInfo = ProcessStartInfo()
        startInfo.FileName <- "dotnet"
        startInfo.Arguments <- $"restore \"{projectPath}\" --nologo --verbosity quiet"
        startInfo.WorkingDirectory <- tempRoot
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.UseShellExecute <- false
        startInfo.CreateNoWindow <- true
        startInfo.Environment["NUGET_PACKAGES"] <- packagesRoot

        use proc = new Process()
        proc.StartInfo <- startInfo

        let started = proc.Start()
        if not started then
            Result.Error "failed to start `dotnet restore` process."
        else
            let stdOut = proc.StandardOutput.ReadToEnd()
            let stdErr = proc.StandardError.ReadToEnd()
            proc.WaitForExit()

            if proc.ExitCode = 0 then
                Ok()
            else
                let details = if String.IsNullOrWhiteSpace stdErr then stdOut else stdErr
                Result.Error(details.Trim())

    let private toNuGetPackagePath (packagesRoot: string) (packageId: string) (version: string) : string =
        let packagePath =
            Path.Join(packagesRoot, toNuGetPathSegment packageId, toNuGetPathSegment version)
            |> normalizePath
        packagePath

    let private tryResolveNuGetDependency (packageId: string) (version: string) : Result<Compiler.ResolvedDependency, Diagnostic list> =
        let packagesRoot = getNuGetPackagesRoot ()
        let packagePath = toNuGetPackagePath packagesRoot packageId version

        let resolved : Compiler.ResolvedDependency =
            { name = packageId
              version = version
              source = packagePath }

        if Directory.Exists packagePath then
            Ok resolved
        elif isAutoRestoreEnabled () then
            match tryRunRestore packagesRoot packageId version with
            | Ok () when Directory.Exists packagePath ->
                Ok resolved
            | Ok () ->
                Result.Error [
                    error
                        $"nuget package restore completed but package was not found: {packageId} {version} (expected: {packagePath})."
                ]
            | Result.Error restoreError ->
                Result.Error [
                    error
                        $"nuget package restore failed: {packageId} {version}. {restoreError}"
                ]
        else
            Result.Error [
                error
                    $"nuget package not found in cache: {packageId} {version} (expected: {packagePath}). Set NUGET_PACKAGES or run restore beforehand. To enable automatic restore, set ATLA_BUILD_ENABLE_NUGET_RESTORE=1."
            ]

    let resolveDependencies
        (manifestFileName: string)
        (parseManifest: string -> Result<Manifest, Diagnostic list>)
        (projectRoot: string)
        (manifest: Manifest)
        : Result<Compiler.ResolvedDependency list, Diagnostic list> =
        let mergeResolvedDependency (state: ResolveState) (resolved: Compiler.ResolvedDependency) : ResolveState =
            let dependencyNameKey = resolved.name.ToLowerInvariant()

            match state.resolvedByName.TryFind(dependencyNameKey) with
            | None ->
                { state with resolvedByName = state.resolvedByName.Add(dependencyNameKey, resolved) }
            | Some existing when not (String.Equals(existing.version, resolved.version, StringComparison.Ordinal)) ->
                { state with
                    diagnostics =
                        state.diagnostics
                        @ [ error $"dependency version conflict `{resolved.name}`: `{existing.version}` vs `{resolved.version}`" ] }
            | Some existing when not (String.Equals(existing.source, resolved.source, StringComparison.Ordinal)) ->
                { state with
                    diagnostics =
                        state.diagnostics
                        @ [ error $"duplicate dependency name `{resolved.name}` resolved from `{existing.source}` and `{resolved.source}`" ] }
            | Some _ ->
                state

        let rec visitDependency (state: ResolveState) (ownerRoot: string) (dependency: DependencySpec) : ResolveState =
            match dependency with
            | NuGetDependency(packageId, version) ->
                match tryResolveNuGetDependency packageId version with
                | Result.Error diagnostics ->
                    { state with diagnostics = state.diagnostics @ diagnostics }
                | Ok resolved -> mergeResolvedDependency state resolved
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
                        let resolved : Compiler.ResolvedDependency =
                            { name = dependencyManifest.name
                              version = dependencyManifest.version
                              source = dependencyRoot }

                        let enteredState =
                            let mergedState = mergeResolvedDependency state resolved

                            { mergedState with
                                stack = state.stack @ [ dependencyRoot ]
                                visitedByPath = state.visitedByPath.Add(dependencyRoot, resolved) }

                        let nestedState =
                            dependencyManifest.dependencies
                            |> List.fold (fun currentState child -> visitDependency currentState dependencyRoot child) enteredState

                        { nestedState with stack = state.stack }

        let initialState =
            { stack = [ normalizePath projectRoot ]
              visitedByPath = Map.empty
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
