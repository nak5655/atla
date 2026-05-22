namespace Atla.Compiler

open System
open System.Diagnostics
open System.IO
open System.Runtime.Loader
open System.Security.Cryptography
open System.Text
open Atla.Core.Data
open Atla.Core.Semantics.Data

module DependencyLoader =
    type DependencyLoadPolicy =
        | Direct
        | LocalCopyCache

    type DependencyLoadRequest =
        { dependencyName: string
          runtimeLoadPaths: string list
          loadPolicy: DependencyLoadPolicy }

    (* 依存DLLロード中に使う AssemblyLoadContext。
       既知DLLインデックスから simple name 解決を行い、依存連鎖のロードを補助する。 *)
    type private DependencyAssemblyLoadContext(assemblyIndex: Map<string, string>) =
        inherit AssemblyLoadContext($"atla-dependency-load-{Guid.NewGuid():N}", isCollectible = true)

        override this.Load(assemblyName) =
            let key = assemblyName.Name.ToLowerInvariant()

            match assemblyIndex.TryFind(key) with
            | Some assemblyPath when File.Exists assemblyPath ->
                try
                    this.LoadFromAssemblyPath(assemblyPath)
                with
                | _ ->
                    null
            | _ ->
                null

    type DependencyLoadResult =
        { succeeded: bool
          diagnostics: Diagnostic list
          loadContext: AssemblyLoadContext option }

    type LocalCopyCacheEntry =
        { sourcePath: string
          copyPath: string
          fingerprint: string }

    type private LocalCopyCacheState =
        { mutable initialized: bool
          entries: Collections.Generic.Dictionary<string, LocalCopyCacheEntry * DateTime>
          lockObj: obj
          baseRoot: string
          processRoot: string
          maxEntries: int }

    let private localCopyCache =
        let baseRoot = Path.Join(Path.GetTempPath(), "atla-dependency-loader-cache")
        let processRoot = Path.Join(baseRoot, $"pid-{Environment.ProcessId}")
        { initialized = false
          entries = Collections.Generic.Dictionary<string, LocalCopyCacheEntry * DateTime>()
          lockObj = obj()
          baseRoot = baseRoot
          processRoot = processRoot
          maxEntries = 256 }

    let private succeeded (context: AssemblyLoadContext option) (diagnostics: Diagnostic list) : DependencyLoadResult =
        { succeeded = true
          diagnostics = diagnostics
          loadContext = context }

    let private failed (diagnostics: Diagnostic list) : DependencyLoadResult =
        { succeeded = false
          diagnostics = diagnostics
          loadContext = None }

    let private computeSha256Hex (value: string) : string =
        let bytes = Encoding.UTF8.GetBytes(value)
        let hash = SHA256.HashData(bytes)
        Convert.ToHexString(hash).ToLowerInvariant()

    let private tryDeleteDirectoryRecursively (path: string) : unit =
        try
            if Directory.Exists path then
                Directory.Delete(path, recursive = true)
        with
        | :? IOException
        | :? UnauthorizedAccessException -> ()

    let private tryParsePidFromDirectoryName (name: string) : int option =
        if name.StartsWith("pid-", StringComparison.OrdinalIgnoreCase) then
            let pidText = name.Substring("pid-".Length)
            match Int32.TryParse(pidText) with
            | true, pid -> Some pid
            | _ -> None
        else
            None

    let private isProcessAlive (pid: int) : bool =
        try
            let p = Process.GetProcessById(pid)
            not p.HasExited
        with _ ->
            false

    let private ensureLocalCopyCacheInitialized () : unit =
        lock localCopyCache.lockObj (fun () ->
            if not localCopyCache.initialized then
                Directory.CreateDirectory(localCopyCache.baseRoot) |> ignore
                Directory.CreateDirectory(localCopyCache.processRoot) |> ignore

                for dir in Directory.GetDirectories(localCopyCache.baseRoot) do
                    let name = Path.GetFileName(dir)
                    match tryParsePidFromDirectoryName name with
                    | Some pid when pid <> Environment.ProcessId && not (isProcessAlive pid) ->
                        tryDeleteDirectoryRecursively dir
                    | _ -> ()

                localCopyCache.initialized <- true)

    let private evictLocalCopyCacheEntriesIfNeeded () : unit =
        let staleKeys =
            localCopyCache.entries
            |> Seq.choose (fun kvp ->
                let key = kvp.Key
                let entry, _ = kvp.Value
                if File.Exists(entry.copyPath) then None else Some key)
            |> Seq.toList

        for key in staleKeys do
            localCopyCache.entries.Remove(key) |> ignore

        if localCopyCache.entries.Count > localCopyCache.maxEntries then
            let removable =
                localCopyCache.entries
                |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
                |> Seq.sortBy (fun (_, (_, lastUsedUtc)) -> lastUsedUtc)
                |> Seq.toList

            let overflowCount = localCopyCache.entries.Count - localCopyCache.maxEntries
            removable
            |> Seq.truncate overflowCount
            |> Seq.iter (fun (key, (entry, _)) ->
                localCopyCache.entries.Remove(key) |> ignore
                tryDeleteDirectoryRecursively (Path.GetDirectoryName(entry.copyPath)))

    let private resolveLocalCopyPath (dependencyName: string) (sourcePath: string) : Result<string, Diagnostic> =
        ensureLocalCopyCacheInitialized ()
        let normalizedSourcePath = Path.GetFullPath(sourcePath)
        let sourceFileInfo = FileInfo(normalizedSourcePath)
        let fingerprint = $"{sourceFileInfo.LastWriteTimeUtc.Ticks}:{sourceFileInfo.Length}"
        let cacheKey = $"{normalizedSourcePath}|{fingerprint}"
        let nowUtc = DateTime.UtcNow

        lock localCopyCache.lockObj (fun () ->
            match localCopyCache.entries.TryGetValue(cacheKey) with
            | true, (entry, _) when File.Exists(entry.copyPath) ->
                localCopyCache.entries[cacheKey] <- (entry, nowUtc)
                Ok entry.copyPath
            | _ ->
                let targetDirName = computeSha256Hex cacheKey
                let targetDir = Path.Join(localCopyCache.processRoot, targetDirName)
                let targetPath = Path.Join(targetDir, Path.GetFileName(normalizedSourcePath))

                try
                    Directory.CreateDirectory(targetDir) |> ignore
                    File.Copy(normalizedSourcePath, targetPath, overwrite = true)
                    let entry =
                        { sourcePath = normalizedSourcePath
                          copyPath = targetPath
                          fingerprint = fingerprint }
                    localCopyCache.entries[cacheKey] <- (entry, nowUtc)
                    evictLocalCopyCacheEntriesIfNeeded ()
                    Ok targetPath
                with ex ->
                    Result.Error(
                        Diagnostic.Error(
                            $"dependency `{dependencyName}` failed to create local runtime copy: source `{normalizedSourcePath}` -> copy `{targetPath}` ({ex.Message})",
                            Span.Empty
                        )
                    ))

    (* 依存と runtime ロード対象DLLを決定的順序へ正規化する。 *)
    let private normalizeRuntimeLoadPaths (dependencies: DependencyLoadRequest list) : (string * string * DependencyLoadPolicy) list =
        dependencies
        |> List.sortBy (fun dep -> dep.dependencyName.ToLowerInvariant())
        |> List.collect (fun dep ->
            let dependencyName = dep.dependencyName
            let loadPolicy = dep.loadPolicy
            let runtimeLoadPaths = dep.runtimeLoadPaths
            runtimeLoadPaths
            |> List.sort
            |> List.map (fun path -> dependencyName, path, loadPolicy))

    (* ロード前のファイル存在チェックを行い、欠損を診断に変換する。 *)
    let private validateReferencePaths (normalizedRuntimeLoads: (string * string * DependencyLoadPolicy) list) : Diagnostic list =
        normalizedRuntimeLoads
        |> List.choose (fun (dependencyName, path, _) ->
            if String.IsNullOrWhiteSpace path then
                Some(Diagnostic.Error($"dependency `{dependencyName}` contains empty runtime load path", Span.Empty))
            else
                let normalizedPath = Path.GetFullPath(path)

                if File.Exists normalizedPath then
                    None
                else
                    Some(Diagnostic.Error($"dependency `{dependencyName}` runtime load assembly not found: {normalizedPath}", Span.Empty)))

    (* simple name 衝突を検出し、競合診断を返す。 *)
    let private detectSimpleNameConflicts (normalizedRuntimeLoads: (string * string) list) : Diagnostic list =
        normalizedRuntimeLoads
        |> List.groupBy (fun (_, path) -> Path.GetFileNameWithoutExtension(path).ToLowerInvariant())
        |> List.choose (fun (simpleName, entries) ->
            let distinctPaths =
                entries
                |> List.map snd
                |> List.map Path.GetFullPath
                |> List.distinct
                |> List.sort

            if List.length distinctPaths > 1 then
                let pathsText = String.Join(", ", distinctPaths)
                Some(
                    Diagnostic.Error(
                        $"runtime load assembly simple-name conflict `{simpleName}` detected across multiple paths: {pathsText}",
                        Span.Empty
                    )
                )
            else
                None)

    (* 例外を構造化診断へ変換する。 *)
    let private toLoadDiagnostic (dependencyName: string) (assemblyPath: string) (exn: exn) : Diagnostic =
        match exn with
        | :? BadImageFormatException ->
            Diagnostic.Error($"dependency `{dependencyName}` runtime load assembly is not a valid .NET assembly: {assemblyPath}", Span.Empty)
        | :? FileNotFoundException as fileNotFound ->
            let missingDetail =
                if String.IsNullOrWhiteSpace fileNotFound.FileName then
                    fileNotFound.Message
                else
                    fileNotFound.FileName

            Diagnostic.Warning(
                $"dependency `{dependencyName}` skipped missing chained dependency while loading `{assemblyPath}`: {missingDetail}",
                Span.Empty
            )
        | :? FileLoadException as fileLoad ->
            Diagnostic.Error(
                $"dependency `{dependencyName}` failed runtime load for assembly `{assemblyPath}` due to load error: {fileLoad.Message}",
                Span.Empty
            )
        | _ ->
            Diagnostic.Error($"dependency `{dependencyName}` failed runtime load for assembly `{assemblyPath}`: {exn.Message}", Span.Empty)

    (* 依存DLLを AssemblyLoadContext へロードし、意味解析直前に解決可能な状態を作る。 *)
    let private resolveLoadPaths (normalizedRuntimeLoads: (string * string * DependencyLoadPolicy) list) : Result<(string * string) list, Diagnostic list> =
        let folder (resolved, diagnostics) (dependencyName, path, loadPolicy) =
            let normalizedPath = Path.GetFullPath(path)
            match loadPolicy with
            | DependencyLoadPolicy.Direct ->
                ((dependencyName, normalizedPath) :: resolved, diagnostics)
            | DependencyLoadPolicy.LocalCopyCache ->
                match resolveLocalCopyPath dependencyName normalizedPath with
                | Ok copiedPath ->
                    ((dependencyName, copiedPath) :: resolved, diagnostics)
                | Result.Error diagnostic ->
                    (resolved, diagnostic :: diagnostics)

        let resolved, diagnostics = normalizedRuntimeLoads |> List.fold folder ([], [])
        if List.isEmpty diagnostics then
            Ok(List.rev resolved)
        else
            Result.Error(List.rev diagnostics)

    let loadDependenciesByRequest (dependencies: DependencyLoadRequest list) : DependencyLoadResult =
        let normalizedRuntimeLoads = normalizeRuntimeLoadPaths dependencies
        let pathDiagnostics = validateReferencePaths normalizedRuntimeLoads

        if not (List.isEmpty pathDiagnostics) then
            failed pathDiagnostics
        elif List.isEmpty normalizedRuntimeLoads then
            succeeded None []
        else
            match resolveLoadPaths normalizedRuntimeLoads with
            | Result.Error diagnostics ->
                failed diagnostics
            | Ok resolvedRuntimeLoads ->
                let conflictDiagnostics = detectSimpleNameConflicts resolvedRuntimeLoads

                if not (List.isEmpty conflictDiagnostics) then
                    failed conflictDiagnostics
                else
                    let assemblyIndex =
                        resolvedRuntimeLoads
                        |> List.map snd
                        |> List.distinct
                        |> List.map (fun path -> Path.GetFileNameWithoutExtension(path).ToLowerInvariant(), path)
                        |> Map.ofList

                    let context = new DependencyAssemblyLoadContext(assemblyIndex)

                    let folder (loadedPaths, diagnostics) (dependencyName, assemblyPath) =
                        try
                            let normalizedPath = Path.GetFullPath(assemblyPath)
                            context.LoadFromAssemblyPath(normalizedPath) |> ignore
                            (normalizedPath :: loadedPaths, diagnostics)
                        with ex ->
                            (loadedPaths, diagnostics @ [ toLoadDiagnostic dependencyName assemblyPath ex ])

                    let _, diagnostics = resolvedRuntimeLoads |> List.fold folder ([], [])
                    let hasErrors = diagnostics |> List.exists (fun diag -> diag.isError)

                    if hasErrors then
                        context.Unload()
                        failed diagnostics
                    else
                        succeeded (Some context) diagnostics

    let loadDependenciesWithPolicy (loadPolicy: DependencyLoadPolicy) (dependencies: (string * string list) list) : DependencyLoadResult =
        dependencies
        |> List.map (fun (dependencyName, runtimeLoadPaths) ->
            { dependencyName = dependencyName
              runtimeLoadPaths = runtimeLoadPaths
              loadPolicy = loadPolicy })
        |> loadDependenciesByRequest

    let loadDependencies (dependencies: (string * string list) list) : DependencyLoadResult =
        loadDependenciesWithPolicy DependencyLoadPolicy.Direct dependencies

    let getLocalCopyCacheEntries () : LocalCopyCacheEntry list =
        ensureLocalCopyCacheInitialized ()
        lock localCopyCache.lockObj (fun () ->
            localCopyCache.entries
            |> Seq.map (fun kvp -> fst kvp.Value)
            |> Seq.sortBy (fun entry -> entry.sourcePath)
            |> Seq.toList)

    let clearLocalCopyCache () : unit =
        ensureLocalCopyCacheInitialized ()
        lock localCopyCache.lockObj (fun () ->
            localCopyCache.entries
            |> Seq.map (fun kvp -> Path.GetDirectoryName((fst kvp.Value).copyPath))
            |> Seq.distinct
            |> Seq.iter tryDeleteDirectoryRecursively
            localCopyCache.entries.Clear()
            if Directory.Exists(localCopyCache.processRoot) then
                for dir in Directory.GetDirectories(localCopyCache.processRoot) do
                    tryDeleteDirectoryRecursively dir)

    (* ロードした依存を明示的に解放する。 *)
    let unloadDependencies (loadContext: AssemblyLoadContext option) : unit =
        match loadContext with
        | Some context ->
            context.Unload()
            GC.Collect()
            GC.WaitForPendingFinalizers()
        | None ->
            ()
