namespace Atla.Compiler

open System
open System.IO
open System.Runtime.Loader
open Atla.Core.Data
open Atla.Core.Semantics.Data

module DependencyLoader =
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

    let private succeeded (context: AssemblyLoadContext option) : DependencyLoadResult =
        { succeeded = true
          diagnostics = []
          loadContext = context }

    let private failed (diagnostics: Diagnostic list) : DependencyLoadResult =
        { succeeded = false
          diagnostics = diagnostics
          loadContext = None }

    (* 依存と参照DLLを決定的順序へ正規化する。 *)
    let private normalizeReferenceAssemblyPaths (dependencies: (string * string list) list) : (string * string) list =
        dependencies
        |> List.sortBy (fun (dependencyName, _) -> dependencyName.ToLowerInvariant())
        |> List.collect (fun (dependencyName, referenceAssemblyPaths) ->
            referenceAssemblyPaths
            |> List.sort
            |> List.map (fun path -> dependencyName, path))

    (* ロード前のファイル存在チェックを行い、欠損を診断に変換する。 *)
    let private validateReferencePaths (normalizedReferences: (string * string) list) : Diagnostic list =
        normalizedReferences
        |> List.choose (fun (dependencyName, path) ->
            if String.IsNullOrWhiteSpace path then
                Some(Diagnostic.Error($"dependency `{dependencyName}` contains empty reference assembly path", Span.Empty))
            else
                let normalizedPath = Path.GetFullPath(path)

                if File.Exists normalizedPath then
                    None
                else
                    Some(Diagnostic.Error($"dependency `{dependencyName}` reference assembly not found: {normalizedPath}", Span.Empty)))

    (* simple name 衝突を検出し、競合診断を返す。 *)
    let private detectSimpleNameConflicts (normalizedReferences: (string * string) list) : Diagnostic list =
        normalizedReferences
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
                        $"reference assembly simple-name conflict `{simpleName}` detected across multiple paths: {pathsText}",
                        Span.Empty
                    )
                )
            else
                None)

    (* 例外を構造化診断へ変換する。 *)
    let private toLoadDiagnostic (dependencyName: string) (assemblyPath: string) (exn: exn) : Diagnostic =
        match exn with
        | :? BadImageFormatException ->
            Diagnostic.Error($"dependency `{dependencyName}` reference assembly is not a valid .NET assembly: {assemblyPath}", Span.Empty)
        | :? FileNotFoundException as fileNotFound ->
            let missingDetail =
                if String.IsNullOrWhiteSpace fileNotFound.FileName then
                    fileNotFound.Message
                else
                    fileNotFound.FileName

            Diagnostic.Error(
                $"dependency `{dependencyName}` failed to load due to missing chained dependency while loading `{assemblyPath}`: {missingDetail}",
                Span.Empty
            )
        | :? FileLoadException as fileLoad ->
            Diagnostic.Error(
                $"dependency `{dependencyName}` failed to load assembly `{assemblyPath}` due to load error: {fileLoad.Message}",
                Span.Empty
            )
        | _ ->
            Diagnostic.Error($"dependency `{dependencyName}` failed to load assembly `{assemblyPath}`: {exn.Message}", Span.Empty)

    (* 依存DLLを AssemblyLoadContext へロードし、意味解析直前に解決可能な状態を作る。 *)
    let loadDependencies (dependencies: (string * string list) list) : DependencyLoadResult =
        let normalizedReferences = normalizeReferenceAssemblyPaths dependencies
        let pathDiagnostics = validateReferencePaths normalizedReferences
        let conflictDiagnostics = detectSimpleNameConflicts normalizedReferences

        if not (List.isEmpty pathDiagnostics) then
            failed pathDiagnostics
        elif not (List.isEmpty conflictDiagnostics) then
            failed conflictDiagnostics
        elif List.isEmpty normalizedReferences then
            succeeded None
        else
            let assemblyIndex =
                normalizedReferences
                |> List.map snd
                |> List.distinct
                |> List.map (fun path -> Path.GetFileNameWithoutExtension(path).ToLowerInvariant(), path)
                |> Map.ofList

            let context = new DependencyAssemblyLoadContext(assemblyIndex)

            let folder (loadedPaths, diagnostics) (dependencyName, assemblyPath) =
                if List.isEmpty diagnostics then
                    try
                        let normalizedPath = Path.GetFullPath(assemblyPath)
                        context.LoadFromAssemblyPath(normalizedPath) |> ignore
                        (normalizedPath :: loadedPaths, diagnostics)
                    with ex ->
                        (loadedPaths, diagnostics @ [ toLoadDiagnostic dependencyName assemblyPath ex ])
                else
                    (loadedPaths, diagnostics)

            let _, diagnostics = normalizedReferences |> List.fold folder ([], [])

            if List.isEmpty diagnostics then
                succeeded (Some context)
            else
                context.Unload()
                failed diagnostics

    (* ロードした依存を明示的に解放する。 *)
    let unloadDependencies (loadContext: AssemblyLoadContext option) : unit =
        match loadContext with
        | Some context ->
            context.Unload()
            GC.Collect()
            GC.WaitForPendingFinalizers()
        | None ->
            ()
