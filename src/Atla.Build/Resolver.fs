namespace Atla.Build

open System
open System.IO
open System.Globalization
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open Atla.Core.Data
open Atla.Core.Semantics.Data
open Atla.Compiler
open NuGet.Common
open NuGet.Configuration
open NuGet.Packaging
open NuGet.Packaging.Core
open NuGet.Protocol
open NuGet.Protocol.Core.Types
open NuGet.Versioning

module internal Resolver =
    type PackageType =
        | Lib
        | Exe
        | Dll

    (* Manifest で受け入れる依存指定の正規化表現。
       NuGetDependency は NuGet.Versioning の VersionRange で依存範囲を表現する。
       - manifest 指定: VersionRange(NuGetVersion.Parse(ver), true) → [ver, ∞)
       - nuspec 指定: pkg.VersionRange をそのまま保持し、範囲情報（上限等）を失わない。 *)
    type DependencySpec =
        | PathDependency of name: string * relativePath: string
        | NuGetDependency of packageId: string * versionRange: VersionRange

    type Manifest =
        { name: string
          version: string
          packageType: PackageType
          dependencies: DependencySpec list }

    type private ResolveState =
        { stack: string list
          visitedByPath: Map<string, Compiler.ResolvedDependency>
          resolvedByName: Map<string, Compiler.ResolvedDependency>
          (* MVS (Minimum Version Selection) で選択済みの具体バージョン。
             packageId 小文字 → 現在選択されている NuGetVersion。
             nuspec 由来の推移依存でバージョン範囲の比較・アップグレード判定に使用する。 *)
          selectedVersions: Map<string, NuGetVersion>
          (* 各パッケージに対してこれまでに観測した全バージョン範囲制約のリスト。
             アップグレード時に候補バージョンが全制約を充足するかを検証するために使用する。 *)
          seenRanges: Map<string, VersionRange list>
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
    let private buildDeterministicTempRootName (packageId: string) (version: string) : string =
        (* package/version をハッシュ化し、OS依存しない固定長のテンポラリ名へ正規化する。 *)
        let payload = $"{toNuGetPathSegment packageId}/{toNuGetPathSegment version}"
        let bytes = Encoding.UTF8.GetBytes(payload)
        let hash = SHA256.HashData(bytes)
        let hex = Convert.ToHexString(hash).ToLowerInvariant()
        $"atla-build-nuget-download-{hex}"

    let private tryRunRestore (packagesRoot: string) (packageId: string) (version: string) : Result<unit, string> =
        try
            let parsedVersion = NuGetVersion.Parse(version)
            let packagePath =
                Path.Join(packagesRoot, toNuGetPathSegment packageId, toNuGetPathSegment version)
                |> normalizePath
            let tempRootName = buildDeterministicTempRootName packageId (parsedVersion.ToNormalizedString())
            let tempRoot = Path.Join(Path.GetTempPath(), tempRootName)
            resetDirectory tempRoot
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
        [ "net10.0"; "net9.0"; "net8.0"; "netstandard2.1"; "netstandard2.0"; "netstandard1.1"; "net48"; "net472"; "net471"; "net462"; "net461" ]

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
            Directory.GetFiles(tfmDir, "*.dll", SearchOption.TopDirectoryOnly)
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

    (* 探索ルート優先順に従って DLL 群を収集する共通実装。
       routeLabel は診断時に compile/runtime のどちらを解決中か識別するために使う。 *)
    let private tryCollectAssemblyPathsByPriority
        (routeLabel: string)
        (dependencyName: string)
        (dependencyRoot: string)
        (preferredRoots: string list)
        : Result<string list, Diagnostic list> =
        let normalizedRoot = normalizePath dependencyRoot
        let refRoot = Path.Join(normalizedRoot, "ref")
        let libRoot = Path.Join(normalizedRoot, "lib")

        let rootLookup =
            Map.ofList [
                "ref", refRoot
                "lib", libRoot
            ]

        let tryCollectFromRoot (probeRoot: string) : Result<string list, Diagnostic list> option =
            match trySelectTfmDirectory probeRoot with
            | None -> None
            | Some tfmDir ->
                match tryCollectDllsFromDirectory dependencyName probeRoot tfmDir with
                (* TFM ディレクトリが存在するが DLL が 0 件の場合（`_._` プレースホルダのみ等）は
                   "マネージドアセットなし・ネイティブのみ提供" パッケージとして Ok [] を返す。
                   エラーとしないことで runtimes/<rid>/native/ のみを持つパッケージが解決できる。 *)
                | Ok dlls ->
                    Some(Ok dlls)
                | Result.Error diagnostics ->
                    Some(Result.Error diagnostics)

        let resolvedCandidates =
            preferredRoots
            |> List.choose (fun rootKey ->
                match rootLookup.TryFind(rootKey) with
                | Some probeRoot ->
                    Some(rootKey, probeRoot, tryCollectFromRoot probeRoot)
                | None ->
                    None)

        let rec pickCandidate (candidates: (string * string * Result<string list, Diagnostic list> option) list) =
            match candidates with
            | [] ->
                let tfmListText = String.Join(", ", tfmPriority)
                Result.Error [
                    error
                        $"dependency `{dependencyName}` has no supported {routeLabel} assemblies. expected under `ref/<tfm>` or `lib/<tfm>` where tfm is one of [{tfmListText}]. root: {normalizedRoot}"
                ]
            | (_, _, Some(Ok dlls)) :: _ ->
                Ok dlls
            | (_, _, Some(Result.Error diagnostics)) :: _ ->
                Result.Error diagnostics
            | _ :: rest ->
                pickCandidate rest

        pickCandidate resolvedCandidates

    (* compile 時に使用する参照 DLL 群を収集する（ref 優先）。 *)
    let private tryCollectCompileReferenceAssemblyPaths (dependencyName: string) (dependencyRoot: string) : Result<string list, Diagnostic list> =
        tryCollectAssemblyPathsByPriority "compile reference" dependencyName dependencyRoot [ "ref"; "lib" ]

    (* runtime ロード時に使用する DLL 群を収集する（lib 優先、無ければ ref へフォールバック）。 *)
    let private tryCollectRuntimeLoadAssemblyPaths (dependencyName: string) (dependencyRoot: string) : Result<string list, Diagnostic list> =
        tryCollectAssemblyPathsByPriority "runtime load" dependencyName dependencyRoot [ "lib"; "ref" ]

    (* 依存ルート配下の runtimes/*/native/ からすべてのプラットフォームのネイティブファイルを収集する。
       runtimes ディレクトリが存在しない場合は空リストを返す（エラーとはしない）。
       すべての RID サブディレクトリを対象とし、決定的順序で返す。 *)
    let private collectNativeRuntimePaths (dependencyRoot: string) : string list =
        let runtimesRoot = Path.Join(dependencyRoot, "runtimes")

        if not (Directory.Exists runtimesRoot) then
            []
        else
            Directory.GetDirectories(runtimesRoot)
            |> Array.sort
            |> Array.collect (fun ridDir ->
                let nativeDir = Path.Join(ridDir, "native")

                if Directory.Exists nativeDir then
                    Directory.GetFiles(nativeDir, "*", SearchOption.AllDirectories)
                    |> Array.map normalizePath
                    |> Array.sort
                else
                    [||])
            |> Array.toList

    (* 依存ルート配下から compile/runtime/native の全経路を収集する。
       lib/ と ref/ の両方が存在せず、native ランタイムファイルのみ持つパッケージ
       （例: Avalonia.Win32 等の native-only NuGet パッケージ）は
       Ok([], [], nativeFiles) として成功扱いにする。 *)
    let private tryCollectDependencyAssemblyPaths
        (dependencyName: string)
        (dependencyRoot: string)
        : Result<string list * string list * string list, Diagnostic list> =
        let nativeRuntimePaths = collectNativeRuntimePaths dependencyRoot

        match
            tryCollectCompileReferenceAssemblyPaths dependencyName dependencyRoot,
            tryCollectRuntimeLoadAssemblyPaths dependencyName dependencyRoot
        with
        | Ok compileReferencePaths, Ok runtimeLoadPaths ->
            Ok(compileReferencePaths, runtimeLoadPaths, nativeRuntimePaths)
        | Result.Error _, _ | _, Result.Error _ when not (List.isEmpty nativeRuntimePaths) ->
            (* マネージドアセットが存在しないが native ランタイムファイルが存在する場合は
               native-only パッケージとして成功扱いにする。 *)
            Ok([], [], nativeRuntimePaths)
        | Result.Error diagnostics, Ok _ ->
            Result.Error diagnostics
        | Ok _, Result.Error diagnostics ->
            Result.Error diagnostics
        | Result.Error compileDiagnostics, Result.Error runtimeDiagnostics ->
            Result.Error(compileDiagnostics @ runtimeDiagnostics)

    (* NuGet パッケージの nuspec から推移的依存パッケージ仕様を読み取る。
       tfmPriority に従って最適な依存グループを選択し、NuGetDependency リストを返す。
       パッケージディレクトリに nuspec ファイルが見つからない場合は Ok [] を返す。
       例外は Diagnostic.Error に変換して Result.Error で返す。 *)
    let private tryReadTransitiveNuGetDependencies (packageId: string) (packagePath: string) : Result<DependencySpec list, Diagnostic list> =
        (* パッケージディレクトリ内に nuspec ファイルが存在しない場合は推移的依存なしとして Ok [] を返す。 *)
        let nuspecFiles = Directory.GetFiles(packagePath, "*.nuspec")

        if Array.isEmpty nuspecFiles then
            Ok []
        else
            try
                use reader = new PackageFolderReader(packagePath)
                let depGroups = reader.NuspecReader.GetDependencyGroups() |> Seq.toList

                if List.isEmpty depGroups then
                    Ok []
                else
                    (* tfmPriority と同じ優先順位で最適な dependency group を選択する。 *)
                    let bestGroup =
                        tfmPriority
                        |> List.tryPick (fun tfm ->
                            depGroups
                            |> List.tryFind (fun group ->
                                try
                                    group.TargetFramework.GetShortFolderName().ToLowerInvariant() = tfm
                                with _ ->
                                    false))
                        |> Option.orElse (
                            depGroups
                            |> List.tryFind (fun group -> group.TargetFramework.IsAny || group.TargetFramework.IsAgnostic))
                        |> Option.orElse (List.tryHead depGroups)

                    match bestGroup with
                    | None -> Ok []
                    | Some group ->
                        let deps =
                            group.Packages
                            |> Seq.toList
                            |> List.choose (fun pkg ->
                                (* VersionRange をそのまま保持することで範囲情報（上限等）を失わない。
                                   MinVersion が null のエントリは解決対象外としてスキップする。 *)
                                if isNull pkg.VersionRange || isNull pkg.VersionRange.MinVersion then
                                    None
                                else
                                    Some(NuGetDependency(pkg.Id, pkg.VersionRange)))

                        Ok deps
            with ex ->
                Result.Error [ error $"failed to read nuspec for `{packageId}`: {ex.Message}" ]


    (* NuGet 依存の解決本体:
       1) キャッシュ直解決
       2) キャッシュ不在時は自動 restore
       3) 診断付き失敗 *)
    let private tryResolveNuGetDependency (packageId: string) (version: string) : Result<Compiler.ResolvedDependency, Diagnostic list> =
        let packagesRoot = getNuGetPackagesRoot ()
        let packagePath = toNuGetPackagePath packagesRoot packageId version

        if Directory.Exists packagePath then
            match tryCollectDependencyAssemblyPaths packageId packagePath with
            | Ok(compileReferencePaths, runtimeLoadPaths, nativeRuntimePaths) ->
                Ok
                    { name = packageId
                      version = version
                      source = packagePath
                      compileReferencePaths = compileReferencePaths
                      runtimeLoadPaths = runtimeLoadPaths
                      nativeRuntimePaths = nativeRuntimePaths }
            | Result.Error diagnostics ->
                Result.Error diagnostics
        else
            match tryRunRestore packagesRoot packageId version with
            | Ok () when Directory.Exists packagePath ->
                match tryCollectDependencyAssemblyPaths packageId packagePath with
                | Ok(compileReferencePaths, runtimeLoadPaths, nativeRuntimePaths) ->
                    Ok
                        { name = packageId
                          version = version
                          source = packagePath
                          compileReferencePaths = compileReferencePaths
                          runtimeLoadPaths = runtimeLoadPaths
                          nativeRuntimePaths = nativeRuntimePaths }
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
        (* 依存名単位での DLL パス登録を行う。バージョン選択は呼び出し元（visitDependency / visitTransitiveDependency）で
           完了済みのため、ここでは同一パッケージ名が異なるソース（異なるバージョン）から解決された場合のみ競合として診断する。
           path 依存と nuget 依存が同一パッケージ名で衝突する場合も、ソース不一致として捕捉される。 *)
        let mergeResolvedDependency (state: ResolveState) (resolved: Compiler.ResolvedDependency) : ResolveState =
            let dependencyNameKey = resolved.name.ToLowerInvariant()

            match state.resolvedByName.TryFind(dependencyNameKey) with
            | None ->
                { state with resolvedByName = state.resolvedByName.Add(dependencyNameKey, resolved) }
            | Some existing when not (String.Equals(existing.source, resolved.source, StringComparison.Ordinal)) ->
                (* ソースが異なる場合は常にバージョン競合である（同一ソースなら同一バージョン）。 *)
                { state with
                    diagnostics =
                        state.diagnostics
                        @ [ error $"dependency version conflict `{resolved.name}`: `{existing.version}` vs `{resolved.version}`" ] }
            | Some _ ->
                state

        (* 依存木 DFS:
           - path 依存は再帰的に manifest を読む
           - nuget 依存はローカルキャッシュを解決し、nuspec から推移的依存を再帰的に処理する
           - stack で循環参照を検出する
           - nuget 依存のバージョンは manifest で明示されるため、selectedVersions で厳密に一致を検査する *)
        let rec visitDependency (state: ResolveState) (ownerRoot: string) (dependency: DependencySpec) : ResolveState =
            match dependency with
            | NuGetDependency(packageId, range) ->
                let packageIdKey = packageId.ToLowerInvariant()

                match range.MinVersion with
                | null ->
                    { state with
                        diagnostics = state.diagnostics @ [ error $"dependency `{packageId}` version range has no minimum version" ] }
                | minVersion ->
                    let versionStr = minVersion.ToNormalizedString()

                    (* manifest 指定の NuGet 依存は明示バージョンとして扱う。
                       すでに別のバージョンが選択済みであれば競合エラーとする。 *)
                    match state.selectedVersions.TryFind(packageIdKey) with
                    | Some existing when not (existing.Equals(minVersion)) ->
                        { state with
                            diagnostics =
                                state.diagnostics
                                @ [ error $"dependency version conflict `{packageId}`: `{existing.ToNormalizedString()}` vs `{versionStr}`" ] }
                    | _ ->
                        (* 既に resolvedByName に登録済みの場合は推移的依存を再処理しない（無限ループ防止）。 *)
                        let alreadyResolved = state.resolvedByName.ContainsKey(packageIdKey)

                        match tryResolveNuGetDependency packageId versionStr with
                        | Result.Error diagnostics ->
                            { state with diagnostics = state.diagnostics @ diagnostics }
                        | Ok resolved ->
                            let mergedState = mergeResolvedDependency state resolved
                            let stateWithVersion =
                                { mergedState with selectedVersions = mergedState.selectedVersions.Add(packageIdKey, minVersion) }

                            if alreadyResolved then
                                stateWithVersion
                            else
                                (* nuspec から推移的依存を読み取り、再帰的に解決する。 *)
                                match tryReadTransitiveNuGetDependencies packageId resolved.source with
                                | Result.Error diagnostics ->
                                    { stateWithVersion with diagnostics = stateWithVersion.diagnostics @ diagnostics }
                                | Ok transitiveDeps ->
                                    transitiveDeps
                                    |> List.fold (fun currentState dep -> visitTransitiveDependency currentState ownerRoot dep) stateWithVersion
            | PathDependency(name, relativePath) ->
                let dependencyRoot = normalizePath (Path.Join(ownerRoot, relativePath))
                let manifestPath = Path.Join(dependencyRoot, manifestFileName)

                if List.contains dependencyRoot state.stack then
                    let cyclePath = state.stack @ [ dependencyRoot ]
                    let cycleDescription = cyclePath |> List.map Path.GetFileName |> String.concat " -> "
                    { state with diagnostics = state.diagnostics @ [ error $"cyclic dependency detected: {cycleDescription}" ] }
                elif File.Exists dependencyRoot && dependencyRoot.EndsWith(".atlalib", StringComparison.OrdinalIgnoreCase) then
                    match AtlaLib.resolveRuntimeAssets dependencyRoot with
                    | Result.Error diagnostics ->
                        { state with diagnostics = state.diagnostics @ diagnostics }
                    | Ok resolvedAssets ->
                        let resolved : Compiler.ResolvedDependency =
                            { name = resolvedAssets.packageName
                              version = resolvedAssets.packageVersion
                              source = dependencyRoot
                              compileReferencePaths = []
                              runtimeLoadPaths = resolvedAssets.runtimeLoadPaths
                              nativeRuntimePaths = resolvedAssets.nativeRuntimePaths }
                        mergeResolvedDependency state resolved
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
                        match tryCollectDependencyAssemblyPaths dependencyManifest.name dependencyRoot with
                        | Result.Error diagnostics ->
                            { state with diagnostics = state.diagnostics @ diagnostics }
                        | Ok(compileReferencePaths, runtimeLoadPaths, nativeRuntimePaths) ->
                            let resolved : Compiler.ResolvedDependency =
                                { name = dependencyManifest.name
                                  version = dependencyManifest.version
                                  source = dependencyRoot
                                  compileReferencePaths = compileReferencePaths
                                  runtimeLoadPaths = runtimeLoadPaths
                                  nativeRuntimePaths = nativeRuntimePaths }

                            let enteredState =
                                let mergedState = mergeResolvedDependency state resolved

                                { mergedState with
                                    stack = state.stack @ [ dependencyRoot ]
                                    visitedByPath = state.visitedByPath.Add(dependencyRoot, resolved) }

                            let nestedState =
                                dependencyManifest.dependencies
                                |> List.fold (fun currentState child -> visitDependency currentState dependencyRoot child) enteredState

                            { nestedState with stack = state.stack }

        (* 推移的 NuGet 依存の DFS（Minimum Version Selection）:
           nuspec 由来の依存は VersionRange で表現されており、単純なバージョン一致ではなく
           範囲の充足性で既存選択を評価する。以下の 3 ケースで処理を分岐する:
           1. 選択済みバージョンが range を満たす → 制約を記録してスキップ
           2. range.MinVersion が選択済みより高い → アップグレード候補を検証して採否を決定
              - 候補バージョンが seenRanges の全制約を満たす場合のみアップグレード（MVS）
              - いずれかの既存制約を違反する場合は真の競合として診断する
           3. 選択済みが range の上限制約を超えている → 真の競合エラー
           DLL を持たないパッケージ（ビルドツール・アナライザー等）の解決失敗はスキップする。 *)
        and visitTransitiveDependency (state: ResolveState) (ownerRoot: string) (dependency: DependencySpec) : ResolveState =
            match dependency with
            | PathDependency _ ->
                visitDependency state ownerRoot dependency
            | NuGetDependency(packageId, range) ->
                let packageIdKey = packageId.ToLowerInvariant()

                match range.MinVersion with
                | null ->
                    (* MinVersion のない range は解決対象外としてスキップする。 *)
                    state
                | minVersion ->
                    (* この range を seenRanges へ追加する。アップグレード時の全制約検証に使う。 *)
                    let prevRanges = state.seenRanges.TryFind(packageIdKey) |> Option.defaultValue []
                    let allRanges = range :: prevRanges
                    let stateWithRange = { state with seenRanges = state.seenRanges.Add(packageIdKey, allRanges) }

                    match stateWithRange.selectedVersions.TryFind(packageIdKey) with
                    | Some selected ->
                        if range.Satisfies(selected) then
                            (* ケース 1: 選択済みバージョンが要求 range を満たす。追加アクション不要。 *)
                            stateWithRange
                        elif minVersion.CompareTo(selected) > 0 then
                            (* ケース 2: より高い最小バージョンが要求されている。
                               候補バージョン（minVersion）が seenRanges 内の全既存制約を充足する場合のみアップグレードする。
                               いずれかの既存制約を違反する場合は、どのバージョンでも両立できない真の競合である。 *)
                            let versionStr = minVersion.ToNormalizedString()
                            let violatingRanges = prevRanges |> List.filter (fun r -> not (r.Satisfies(minVersion)))

                            if not (List.isEmpty violatingRanges) then
                                let violatingDesc = violatingRanges |> List.map (fun r -> $"`{r}`") |> String.concat ", "

                                { stateWithRange with
                                    diagnostics =
                                        stateWithRange.diagnostics
                                        @ [ error $"dependency version conflict `{packageId}`: `{versionStr}` does not satisfy {violatingDesc}" ] }
                            else
                                match tryResolveNuGetDependency packageId versionStr with
                                | Result.Error _ ->
                                    (* アップグレード先パッケージが解決できない場合はスキップする。 *)
                                    stateWithRange
                                | Ok resolved ->
                                    let upgraded =
                                        { stateWithRange with
                                            selectedVersions = stateWithRange.selectedVersions.Add(packageIdKey, minVersion)
                                            resolvedByName = stateWithRange.resolvedByName.Add(packageIdKey, resolved) }

                                    match tryReadTransitiveNuGetDependencies packageId resolved.source with
                                    | Result.Error _ -> upgraded
                                    | Ok transitiveDeps ->
                                        transitiveDeps
                                        |> List.fold (fun currentState dep -> visitTransitiveDependency currentState ownerRoot dep) upgraded
                        else
                            (* ケース 3: 選択済みが range の上限制約を超えている等、真の競合。 *)
                            let selectedStr = selected.ToNormalizedString()

                            { stateWithRange with
                                diagnostics =
                                    stateWithRange.diagnostics
                                    @ [ error $"dependency version conflict `{packageId}`: `{selectedStr}` does not satisfy `{range}`" ] }
                    | None ->
                        (* パッケージが未解決。path 依存等で既に resolvedByName に存在する場合はスキップする。 *)
                        if stateWithRange.resolvedByName.ContainsKey(packageIdKey) then
                            stateWithRange
                        else
                            (* 初回解決。range の最小バージョンを選択し DFS する。 *)
                            let versionStr = minVersion.ToNormalizedString()

                            match tryResolveNuGetDependency packageId versionStr with
                            | Result.Error _ ->
                                (* DLL を持たないパッケージ（ビルドツール・アナライザー等）はスキップする。 *)
                                stateWithRange
                            | Ok resolved ->
                                let state1 =
                                    { mergeResolvedDependency stateWithRange resolved with
                                        selectedVersions = stateWithRange.selectedVersions.Add(packageIdKey, minVersion) }

                                match tryReadTransitiveNuGetDependencies packageId resolved.source with
                                | Result.Error _ -> state1
                                | Ok transitiveDeps ->
                                    transitiveDeps
                                    |> List.fold (fun currentState dep -> visitTransitiveDependency currentState ownerRoot dep) state1

        let initialState =
            { stack = [ normalizePath projectRoot ]
              visitedByPath = Map.empty
              resolvedByName = Map.empty
              selectedVersions = Map.empty
              seenRanges = Map.empty
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
