module Atla.Console

open System
open System.IO
open Atla.Build
open Atla.Compiler

module Console =
    type BuildOptions =
        { projectRoot: string
          outDir: string option
          asmName: string option }

    let private usage () =
        String.concat Environment.NewLine [
            "Usage: atla build <projectRoot> [-o <outDir>] [--name <assemblyName>]"
            ""
            "Commands:"
            "  build    Build an Atla project rooted at <projectRoot>"
        ]

    let private parseBuildArgs (args: string list) : Result<BuildOptions, string> =
        let rec loop (rest: string list) (projectRoot: string option) (outDir: string option) (asmName: string option) =
            match rest with
            | [] ->
                match projectRoot with
                | None -> Error "build command requires <projectRoot>"
                | Some root ->
                    Ok {
                        projectRoot = root
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

    let private resolveMainPath (projectRoot: string) : Result<string, string> =
        let candidatePath = Path.Join(projectRoot, "src", "main.atla")

        if File.Exists candidatePath then
            Ok candidatePath
        else
            Error $"project entrypoint not found: {candidatePath}"

    /// プロジェクト配下の Atla ソースをモジュール名付きで列挙する。
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
                    let buildResult = BuildSystem.buildProject { projectRoot = normalizedProjectRoot }
                    printDiagnostics buildResult.diagnostics

                    if not buildResult.succeeded then
                        1
                    else
                        match buildResult.plan with
                        | None ->
                            Console.Error.WriteLine("build plan was not produced")
                            1
                        | Some plan ->
                            match resolveMainPath plan.projectRoot with
                            | Error message ->
                                Console.Error.WriteLine(message)
                                1
                            | Ok _ ->
                                match collectModuleSources plan.projectRoot with
                                | Error message ->
                                    Console.Error.WriteLine(message)
                                    1
                                | Ok modules ->
                                    let outDir =
                                        match options.outDir with
                                        | Some value -> value
                                        | None -> Path.Join(plan.projectRoot, "out")
                                    let asmName =
                                        match options.asmName with
                                        | Some value when not (String.IsNullOrWhiteSpace value) -> value
                                        | _ -> plan.projectName

                                    Directory.CreateDirectory(outDir) |> ignore

                                    let compileResult =
                                        Compiler.compileModules {
                                            asmName = asmName
                                            modules = modules
                                            entryModuleName = "main"
                                            outDir = outDir
                                            dependencies = plan.dependencies
                                        }

                                    printDiagnostics compileResult.diagnostics

                                    if compileResult.succeeded then
                                        let dllPath = Path.Join(outDir, asmName + ".dll")
                                        Console.WriteLine($"Generated: {dllPath}")

                                        match BuildSystem.copyDependencies plan.dependencies outDir with
                                        | Result.Error diagnostics ->
                                            printDiagnostics diagnostics
                                            1
                                        | Ok copied ->
                                            copied |> List.iter (fun path -> Console.WriteLine($"Copied: {path}"))
                                            0
                                    else
                                        1
        | command :: _ ->
            Console.Error.WriteLine($"unknown command: {command}")
            Console.Error.WriteLine(usage())
            1

[<EntryPoint>]
let main argv = Console.run argv
