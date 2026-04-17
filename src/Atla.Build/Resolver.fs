namespace Atla.Build

open System
open System.IO
open System.Globalization
open System.IO.Compression
open Atla.Core.Data
open Atla.Core.Semantics.Data
open Atla.Compiler
open NuGet.Common
open NuGet.Configuration
open NuGet.Packaging.Core
open NuGet.Protocol
open NuGet.Protocol.Core.Types
open NuGet.Versioning

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

    (* NuGet.Client の Task 実行を同期化し、F# の純関数型パイプラインへ接続する。 *)
    let private runTask (task: System.Threading.Tasks.Task<'T>) : 'T =
        task.GetAwaiter().GetResult()

    (* NuGet.Config を読み取り、利用可能な package source 一覧を決定する。 *)
    let private getSourceRepositories () : SourceRepository list =
        let settings = Settings.LoadDefaultSettings(null)
        let sourceProvider = PackageSourceProvider(settings)
        let repositories =
            SourceRepositoryProvider(sourceProvider, Repository.Provider.GetCoreV3())
                .GetRepositories()
            |> Seq.toList

        if List.isEmpty repositories then
            [ SourceRepository(PackageSource("https://api.nuget.org/v3/index.json"), Repository.Provider.GetCoreV3()) ]
        else
            repositories

    (* 展開前の既存ディレクトリを安全に掃除し、抽出先を決定的に初期化する。 *)
    let private resetDirectory (path: string) : unit =
        if Directory.Exists path then
            Directory.Delete(path, recursive = true)
        Directory.CreateDirectory(path) |> ignore

    (* 取得済み nupkg を global-packages 互換レイアウトへ展開する。 *)
    let private extractNupkgToPackagePath (nupkgPath: string) (packagePath: string) : Result<unit, string> =
        try
            let parent = Directory.GetParent(packagePath).FullName
            Directory.CreateDirectory(parent) |> ignore
            resetDirectory packagePath
            ZipFile.ExtractToDirectory(nupkgPath, packagePath, overwriteFiles = true)
            Ok()
        with ex ->
            Result.Error($"failed to extract downloaded package: {ex.Message}")

    (* NuGet source を順次探索し、package/version の nupkg を取得する。 *)
    let rec private tryDownloadFromSources
        (sources: SourceRepository list)
        (packageId: string)
        (version: NuGetVersion)
        (destinationNupkgPath: string)
        : Result<unit, string> =
        match sources with
        | [] ->
            Result.Error($"package `{packageId}` `{version}` was not found in configured sources.")
        | source :: rest ->
            try
                let resource = runTask (source.GetResourceAsync<FindPackageByIdResource>())
                use cache = new SourceCacheContext()
                use destination = File.Create(destinationNupkgPath)

                let copied =
                    runTask (
                        resource.CopyNupkgToStreamAsync(
                            packageId,
                            version,
                            destination,
                            cache,
                            NullLogger.Instance,
                            System.Threading.CancellationToken.None
                        )
                    )

                if copied then
                    Ok()
                else
                    tryDownloadFromSources rest packageId version destinationNupkgPath
            with ex ->
                let tailResult = tryDownloadFromSources rest packageId version destinationNupkgPath
                match tailResult with
                | Ok () -> Ok ()
                | Result.Error nextError ->
                    Result.Error($"{source.PackageSource.Source}: {ex.Message}; {nextError}")

    (* NuGet.Client API を使い、指定 package/version を packagesRoot 配下へ展開する。 *)
    let private tryRunRestore (packagesRoot: string) (packageId: string) (version: string) : Result<unit, string> =
        try
            let parsedVersion = NuGetVersion.Parse(version)
            let packagePath =
                Path.Join(packagesRoot, toNuGetPathSegment packageId, toNuGetPathSegment version)
                |> normalizePath
            let tempRoot = Path.Join(Path.GetTempPath(), $"atla-build-nuget-download-{Guid.NewGuid():N}")
            Directory.CreateDirectory(tempRoot) |> ignore
            let nupkgPath = Path.Join(tempRoot, $"{toNuGetPathSegment packageId}.{toNuGetPathSegment version}.nupkg")
            let sources = getSourceRepositories ()

            match tryDownloadFromSources sources packageId parsedVersion nupkgPath with
            | Result.Error message ->
                Result.Error message
            | Ok () ->
                match extractNupkgToPackagePath nupkgPath packagePath with
                | Ok () -> Ok ()
                | Result.Error message -> Result.Error message
        with ex ->
            Result.Error($"failed to run NuGet.Client restore: {ex.Message}")

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
       2) キャッシュ不在時は自動 restore
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
        else
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
