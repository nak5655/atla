namespace Atla.Build

open System
open System.Diagnostics
open System.IO
open System.Globalization
open Atla.Core.Data
open Atla.Core.Semantics.Data
open Atla.Compiler

module internal Resolver =
    (* Manifest で受け入れる依存指定の正規化表現。 *)
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

    (* NuGet キャッシュの探索ルートを決定する:
       NUGET_PACKAGES 優先、未指定時は ~/.nuget/packages。 *)
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

    (* ローカル一時 csproj を生成して `dotnet restore` を起動し、
       指定 package/version のキャッシュ展開を試行する。 *)
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

    (* 参照DLL探索で許可する TFM を優先順で保持する。 *)
    let private tfmPriority: string list =
        [ "net10.0"; "net9.0"; "net8.0"; "netstandard2.1"; "netstandard2.0" ]

    (* 指定ディレクトリ直下から、優先順位に従って採用する TFM ディレクトリを決める。 *)
    let private trySelectTfmDirectory (root: string) : string option =
        if not (Directory.Exists root) then
            None
        else
            let children =
                Directory.GetDirectories(root)
                |> Array.map (fun dir -> Path.GetFileName(dir).ToLowerInvariant(), normalizePath dir)
                |> Map.ofArray

            tfmPriority
            |> List.tryPick (fun tfm -> children.TryFind tfm)

    (* TFM ディレクトリ配下の DLL を simple name 単位に検証し、決定的順序で返す。 *)
    let private tryCollectDllsFromDirectory (dependencyName: string) (probeRoot: string) (tfmDir: string) : Result<string list, Diagnostic list> =
        let dlls =
            Directory.GetFiles(tfmDir, "*.dll", SearchOption.AllDirectories)
            |> Array.map normalizePath
            |> Array.sort
            |> Array.toList

        let duplicateSimpleNames =
            dlls
            |> List.groupBy (fun path -> Path.GetFileNameWithoutExtension(path).ToLowerInvariant())
            |> List.choose (fun (simpleName, items) ->
                if List.length items > 1 then
                    Some simpleName
                else
                    None)
            |> List.sort

        let missingFiles = dlls |> List.filter (fun path -> not (File.Exists path))
        let missingFilesText = String.Join(", ", missingFiles)
        let duplicateSimpleNamesText = String.Join(", ", duplicateSimpleNames)

        if not (List.isEmpty missingFiles) then
            Result.Error [
                error
                    $"dependency `{dependencyName}` has broken reference path(s) under `{probeRoot}` (tfm `{Path.GetFileName(tfmDir)}`): {missingFilesText}"
            ]
        elif not (List.isEmpty duplicateSimpleNames) then
            Result.Error [
                error
                    $"dependency `{dependencyName}` has ambiguous reference assemblies under `{probeRoot}` (tfm `{Path.GetFileName(tfmDir)}`): {duplicateSimpleNamesText}"
            ]
        else
            Ok dlls

    (* 依存ルート配下から参照DLL候補を収集する。
       フェーズ2仕様に従い ref/ を優先し、次に lib/ を探索して、優先TFMのDLL群を採用する。 *)
    let private tryCollectReferenceAssemblyPaths (dependencyName: string) (dependencyRoot: string) : Result<string list, Diagnostic list> =
        let normalizedRoot = normalizePath dependencyRoot
        let refRoot = Path.Join(normalizedRoot, "ref")
        let libRoot = Path.Join(normalizedRoot, "lib")

        let tryCollectFromRoot (probeRoot: string) : Result<string list, Diagnostic list> option =
            match trySelectTfmDirectory probeRoot with
            | None -> None
            | Some tfmDir ->
                match tryCollectDllsFromDirectory dependencyName probeRoot tfmDir with
                | Ok dlls when List.isEmpty dlls ->
                    Some(Result.Error [ error $"dependency `{dependencyName}` has no dll candidates under `{probeRoot}` (tfm `{Path.GetFileName(tfmDir)}`)" ])
                | Ok dlls ->
                    Some(Ok dlls)
                | Result.Error diagnostics ->
                    Some(Result.Error diagnostics)

        match tryCollectFromRoot refRoot, tryCollectFromRoot libRoot with
        | Some(Ok dlls), _ -> Ok dlls
        | Some(Result.Error diagnostics), _ -> Result.Error diagnostics
        | None, Some(Ok dlls) -> Ok dlls
        | None, Some(Result.Error diagnostics) -> Result.Error diagnostics
        | None, None ->
            let tfmListText = String.Join(", ", tfmPriority)
            Result.Error [
                error
                    $"dependency `{dependencyName}` has no supported reference assemblies. expected under `ref/<tfm>` or `lib/<tfm>` where tfm is one of [{tfmListText}]. root: {normalizedRoot}"
            ]

    (* NuGet 依存の解決本体:
       1) キャッシュ直解決
       2) 必要に応じて自動 restore
       3) 診断付き失敗 *)
    let private tryResolveNuGetDependency (packageId: string) (version: string) : Result<Compiler.ResolvedDependency, Diagnostic list> =
        let packagesRoot = getNuGetPackagesRoot ()
        let packagePath = toNuGetPackagePath packagesRoot packageId version

        if Directory.Exists packagePath then
            match tryCollectReferenceAssemblyPaths packageId packagePath with
            | Ok referenceAssemblyPaths ->
                Ok
                    { name = packageId
                      version = version
                      source = packagePath
                      referenceAssemblyPaths = referenceAssemblyPaths }
            | Result.Error diagnostics ->
                Result.Error diagnostics
        elif isAutoRestoreEnabled () then
            match tryRunRestore packagesRoot packageId version with
            | Ok () when Directory.Exists packagePath ->
                match tryCollectReferenceAssemblyPaths packageId packagePath with
                | Ok referenceAssemblyPaths ->
                    Ok
                        { name = packageId
                          version = version
                          source = packagePath
                          referenceAssemblyPaths = referenceAssemblyPaths }
                | Result.Error diagnostics ->
                    Result.Error diagnostics
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
        (* 依存名単位での一意化と競合診断を行う。 *)
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

        (* 依存木 DFS:
           - path 依存は再帰的に manifest を読む
           - nuget 依存はローカルキャッシュを解決する
           - stack で循環参照を検出する *)
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
                        match tryCollectReferenceAssemblyPaths dependencyManifest.name dependencyRoot with
                        | Result.Error diagnostics ->
                            { state with diagnostics = state.diagnostics @ diagnostics }
                        | Ok referenceAssemblyPaths ->
                            let resolved : Compiler.ResolvedDependency =
                                { name = dependencyManifest.name
                                  version = dependencyManifest.version
                                  source = dependencyRoot
                                  referenceAssemblyPaths = referenceAssemblyPaths }

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

        (* top-level dependencies を順序付きで走査し、結果を名前順で正規化して返す。 *)
        let finalState = manifest.dependencies |> List.fold (fun state dependency -> visitDependency state projectRoot dependency) initialState

        if List.isEmpty finalState.diagnostics then
            finalState.resolvedByName
            |> Map.toList
            |> List.map snd
            |> List.sortBy (fun dep -> dep.name)
            |> Ok
        else
            Result.Error finalState.diagnostics
