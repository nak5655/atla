module Atla.Console

open System
open System.IO
open Atla.Build
open Atla.Compiler

module Console =
    /// `atla build` サブコマンド向けオプション。
    type BuildOptions =
        { projectRoot: string
          outDir: string option
          asmName: string option }

    /// `atla install` サブコマンド向けオプション。
    type InstallOptions =
        { projectRoot: string
          atlaHome: string option }

    /// `buildProject` が成功した際の BuildPlan を取り出す中間結果。
    type BuildPreparation =
        { plan: BuildPlan }

    let private usage () =
        String.concat Environment.NewLine [
            "Usage: atla <command> [options]"
            ""
            "Commands:"
            "  build    Build an Atla project rooted at [projectRoot] (defaults to current directory)"
            "           atla build [projectRoot] [-o <outDir>] [--name <assemblyName>]"
            "  install  Build and install an Atla project to ~/.atla (or --atla-home)"
            "           atla install [projectRoot] [--atla-home <path>]"
        ]

    let private parseBuildArgs (args: string list) : Result<BuildOptions, string> =
        let rec loop (rest: string list) (projectRoot: string option) (outDir: string option) (asmName: string option) =
            match rest with
            | [] ->
                let resolvedProjectRoot =
                    match projectRoot with
                    | Some root -> root
                    | None -> Directory.GetCurrentDirectory()

                Ok {
                    projectRoot = resolvedProjectRoot
                    outDir = outDir
                    asmName = asmName
                }
            | "-o" :: value :: tail -> loop tail projectRoot (Some value) asmName
            | "--name" :: value :: tail -> loop tail projectRoot outDir (Some value)
            | flag :: [] when flag = "-o" || flag = "--name" ->
                Error $"{flag} requires a value"
            | value :: _ when value.StartsWith("-") ->
                Error $"unknown option: {value}"
            | value :: tail ->
                match projectRoot with
                | Some _ -> Error "only one projectRoot is supported"
                | None -> loop tail (Some value) outDir asmName

        loop args None None None

    /// install コマンド引数を解析する。
    let private parseInstallArgs (args: string list) : Result<InstallOptions, string> =
        let rec loop (rest: string list) (projectRoot: string option) (atlaHome: string option) =
            match rest with
            | [] ->
                let resolvedProjectRoot =
                    match projectRoot with
                    | Some root -> root
                    | None -> Directory.GetCurrentDirectory()

                Ok {
                    projectRoot = resolvedProjectRoot
                    atlaHome = atlaHome
                }
            | "--atla-home" :: value :: tail ->
                loop tail projectRoot (Some value)
            | "--atla-home" :: [] ->
                Error "--atla-home requires a value"
            | value :: _ when value.StartsWith("-") ->
                Error $"unknown option: {value}"
            | value :: tail ->
                match projectRoot with
                | Some _ -> Error "only one projectRoot is supported"
                | None -> loop tail (Some value) atlaHome

        loop args None None

    let private validateProjectRoot (projectRoot: string) : Result<string, string> =
        let normalizedProjectRoot = Path.GetFullPath(projectRoot)

        if not (Directory.Exists normalizedProjectRoot) then
            Error $"project root not found: {normalizedProjectRoot}"
        else
            Ok normalizedProjectRoot

    let private diagnosticPrefix (severity: Atla.Core.Semantics.Data.DiagnosticSeverity) : string =
        match severity with
        | Atla.Core.Semantics.Data.DiagnosticSeverity.Error -> "error"
        | Atla.Core.Semantics.Data.DiagnosticSeverity.Warning -> "warning"
        | Atla.Core.Semantics.Data.DiagnosticSeverity.Info -> "info"

    let private printDiagnostics (diagnostics: Atla.Core.Semantics.Data.Diagnostic list) : unit =
        diagnostics
        |> List.iter (fun diagnostic ->
            Console.Error.WriteLine($"{diagnosticPrefix diagnostic.severity}: {diagnostic.toDisplayText()}"))

    /// BuildSystem.buildProject を実行して BuildPlan を取り出す。
    let private prepareBuild (normalizedProjectRoot: string) : Result<BuildPreparation, int> =
        let buildResult = BuildSystem.buildProject { projectRoot = normalizedProjectRoot }
        printDiagnostics buildResult.diagnostics

        if not buildResult.succeeded then
            Error 1
        else
            match buildResult.plan with
            | None ->
                Console.Error.WriteLine("build plan was not produced")
                Error 1
            | Some plan ->
                Ok { plan = plan }

    /// Resolve the compile entry module name from package.type and discovered source modules.
    /// - exe: `main` is required
    /// - lib/dll: prefer `main` when present; otherwise use the first discovered module
    let private resolveEntryModuleName
        (projectRoot: string)
        (packageType: BuildPackageType)
        (modules: Compiler.ModuleSource list)
        : Result<string, string> =
        let findMainModule () =
            modules |> List.tryFind (fun modul -> modul.moduleName = "main")

        match packageType with
        | BuildPackageType.Exe ->
            match findMainModule () with
            | Some _ -> Ok "main"
            | None ->
                let candidatePath = Path.Join(projectRoot, "src", "main.atla")
                Error $"project entrypoint not found: {candidatePath}"
        | BuildPackageType.Lib
        | BuildPackageType.Dll ->
            match findMainModule () with
            | Some _ -> Ok "main"
            | None ->
                match modules with
                | firstModule :: _ -> Ok firstModule.moduleName
                | [] ->
                    let srcRoot = Path.Join(projectRoot, "src")
                    Error $"no source modules were found: {srcRoot}"

    /// Enumerate Atla source files under the project with their module names.
    let private collectModuleSources (projectRoot: string) : Result<Compiler.ModuleSource list, string> =
        let srcRoot = Path.Join(projectRoot, "src")
        if not (Directory.Exists srcRoot) then
            Error $"project source directory not found: {srcRoot}"
        else
            let files =
                Directory.GetFiles(srcRoot, "*.atla", SearchOption.AllDirectories)
                |> Array.sort
                |> Array.toList

            let toModuleName (path: string) =
                let relativePath = Path.GetRelativePath(srcRoot, path)
                let withoutExtension = Path.ChangeExtension(relativePath, null)
                withoutExtension.Replace(Path.DirectorySeparatorChar, '.').Replace(Path.AltDirectorySeparatorChar, '.')

            files
            |> List.map (fun path ->
                ({ moduleName = toModuleName path
                   source = File.ReadAllText(path) }: Compiler.ModuleSource))
            |> Ok

    /// package.type に応じて compile 出力先を決定する。
    let private resolveCompileOutDir (plan: BuildPlan) (outDir: string) : string * bool =
        match plan.packageType with
        | BuildPackageType.Lib ->
            let tempOutDir = Directory.CreateTempSubdirectory("atla-lib-build-").FullName
            tempOutDir, true
        | _ ->
            outDir, false

    /// ビルド前段（ソース収集・エントリ解決・compile 実行・一時ディレクトリ後始末）を共通実行し、成功時に後段コールバックを呼び出す。
    let private withCompiledProject
        (plan: BuildPlan)
        (outDir: string)
        (asmName: string)
        (onSucceeded: string -> Compiler.CompileResult -> int)
        : int =
        match collectModuleSources plan.projectRoot with
        | Error message ->
            Console.Error.WriteLine(message)
            1
        | Ok modules ->
            match resolveEntryModuleName plan.projectRoot plan.packageType modules with
            | Error message ->
                Console.Error.WriteLine(message)
                1
            | Ok entryModuleName ->
                Directory.CreateDirectory(outDir) |> ignore
                let compileOutDir, shouldCleanupCompileOutDir = resolveCompileOutDir plan outDir

                try
                    let compileResult =
                        Compiler.compileModules {
                            asmName = asmName
                            modules = modules
                            entryModuleName = entryModuleName
                            outDir = compileOutDir
                            dependencies = plan.dependencies
                        }

                    printDiagnostics compileResult.diagnostics

                    if compileResult.succeeded then
                        onSucceeded compileOutDir compileResult
                    else
                        1
                finally
                    if shouldCleanupCompileOutDir && Directory.Exists(compileOutDir) then
                        try
                            Directory.Delete(compileOutDir, recursive = true)
                        with
                        | :? IOException as ioEx ->
                            Console.Error.WriteLine($"warning: failed to clean temporary build directory `{compileOutDir}`: {ioEx.Message}")
                        | :? UnauthorizedAccessException as authEx ->
                            Console.Error.WriteLine($"warning: failed to clean temporary build directory `{compileOutDir}`: {authEx.Message}")

    /// `atla build` コマンドの後段処理を実行する。
    let private runBuildWithPlan (plan: BuildPlan) (options: BuildOptions) : int =
        let outDir =
            match options.outDir with
            | Some value -> value
            | None -> Path.Join(plan.projectRoot, "out")

        let asmName =
            match options.asmName with
            | Some value when not (String.IsNullOrWhiteSpace value) -> value
            | _ -> plan.projectName

        withCompiledProject plan outDir asmName (fun compileOutDir compileResult ->
            let dllPath = Path.Join(compileOutDir, asmName + ".dll")

            match plan.packageType with
            | BuildPackageType.Dll ->
                Console.WriteLine($"Generated: {dllPath}")
                0
            | BuildPackageType.Lib ->
                match BuildSystem.createAtlaLib plan.projectName plan.projectVersion asmName compileOutDir outDir plan.dependencies compileResult with
                | Result.Error diagnostics ->
                    printDiagnostics diagnostics
                    1
                | Ok atlaLibPath ->
                    Console.WriteLine($"Generated: {atlaLibPath}")
                    0
            | BuildPackageType.Exe ->
                Console.WriteLine($"Generated: {dllPath}")

                match BuildSystem.copyDependencies plan.dependencies outDir with
                | Result.Error diagnostics ->
                    printDiagnostics diagnostics
                    1
                | Ok copied ->
                    copied |> List.iter (fun path -> Console.WriteLine($"Copied: {path}"))
                    match BuildSystem.writeDepsFile plan.projectName plan.projectVersion asmName plan.dependencies outDir with
                    | Result.Error diagnostics ->
                        printDiagnostics diagnostics
                        1
                    | Ok depsPath ->
                        Console.WriteLine($"Generated: {depsPath}")
                        0)

    /// `atla install` コマンドの後段処理を実行する。
    let private runInstallWithPlan (plan: BuildPlan) (options: InstallOptions) : int =
        let atlaHome =
            match options.atlaHome with
            | Some value -> Path.GetFullPath(value)
            | None -> InstallSystem.resolveAtlaHome ()

        let installOutDir = Directory.CreateTempSubdirectory("atla-install-out-").FullName
        let asmName = plan.projectName

        try
            withCompiledProject plan installOutDir asmName (fun compileOutDir compileResult ->
                match plan.packageType with
                | BuildPackageType.Lib ->
                    match BuildSystem.createAtlaLib plan.projectName plan.projectVersion asmName compileOutDir installOutDir plan.dependencies compileResult with
                    | Result.Error diagnostics ->
                        printDiagnostics diagnostics
                        1
                    | Ok atlaLibPath ->
                        match InstallSystem.installLib atlaHome plan.projectName plan.projectVersion atlaLibPath with
                        | Result.Error diagnostics ->
                            printDiagnostics diagnostics
                            1
                        | Ok installedPath ->
                            Console.WriteLine($"Installed: {installedPath}")
                            0
                | BuildPackageType.Dll ->
                    match InstallSystem.installDll atlaHome plan asmName compileOutDir installOutDir compileResult with
                    | Result.Error diagnostics ->
                        printDiagnostics diagnostics
                        1
                    | Ok installedPath ->
                        Console.WriteLine($"Installed: {installedPath}")
                        0
                | BuildPackageType.Exe ->
                    let dllPath = Path.Join(compileOutDir, asmName + ".dll")
                    Console.WriteLine($"Generated: {dllPath}")

                    match BuildSystem.copyDependencies plan.dependencies compileOutDir with
                    | Result.Error diagnostics ->
                        printDiagnostics diagnostics
                        1
                    | Ok copied ->
                        copied |> List.iter (fun path -> Console.WriteLine($"Copied: {path}"))
                        match BuildSystem.writeDepsFile plan.projectName plan.projectVersion asmName plan.dependencies compileOutDir with
                        | Result.Error diagnostics ->
                            printDiagnostics diagnostics
                            1
                        | Ok depsPath ->
                            Console.WriteLine($"Generated: {depsPath}")
                            match InstallSystem.installExe atlaHome plan.projectName compileOutDir with
                            | Result.Error diagnostics ->
                                printDiagnostics diagnostics
                                1
                            | Ok installedPaths ->
                                installedPaths |> List.iter (fun path -> Console.WriteLine($"Installed: {path}"))
                                0)
        finally
            if Directory.Exists(installOutDir) then
                try
                    Directory.Delete(installOutDir, recursive = true)
                with
                | :? IOException as ioEx ->
                    Console.Error.WriteLine($"warning: failed to clean temporary install directory `{installOutDir}`: {ioEx.Message}")
                | :? UnauthorizedAccessException as authEx ->
                    Console.Error.WriteLine($"warning: failed to clean temporary install directory `{installOutDir}`: {authEx.Message}")

    let run (args: string array) : int =
        match args |> Array.toList with
        | [] ->
            Console.Error.WriteLine(usage())
            1
        | [ "-h" ]
        | [ "--help" ] ->
            Console.WriteLine(usage())
            0
        | "build" :: buildArgs ->
            match parseBuildArgs buildArgs with
            | Error message ->
                Console.Error.WriteLine(message)
                Console.Error.WriteLine(usage())
                1
            | Ok options ->
                match validateProjectRoot options.projectRoot with
                | Error message ->
                    Console.Error.WriteLine(message)
                    1
                | Ok normalizedProjectRoot ->
                    match prepareBuild normalizedProjectRoot with
                    | Error code -> code
                    | Ok preparation ->
                        runBuildWithPlan preparation.plan options
        | "install" :: installArgs ->
            match parseInstallArgs installArgs with
            | Error message ->
                Console.Error.WriteLine(message)
                Console.Error.WriteLine(usage())
                1
            | Ok options ->
                match validateProjectRoot options.projectRoot with
                | Error message ->
                    Console.Error.WriteLine(message)
                    1
                | Ok normalizedProjectRoot ->
                    match prepareBuild normalizedProjectRoot with
                    | Error code -> code
                    | Ok preparation ->
                        runInstallWithPlan preparation.plan options
        | command :: _ ->
            Console.Error.WriteLine($"unknown command: {command}")
            Console.Error.WriteLine(usage())
            1

[<EntryPoint>]
let main argv = Console.run argv
