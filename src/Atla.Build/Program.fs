module Atla.Build

open System
open System.IO
open Atla.Compiler

module Cli =
    type BuildOptions = {
        inputPath: string
        outDir: string
        asmName: string
    }

    let private usage () =
        String.concat Environment.NewLine [
            "Usage: atla build <input.atla> [-o <outDir>] [--name <assemblyName>]"
            ""
            "Commands:"
            "  build    Compile .atla source into a .dll"
        ]

    let private parseBuildArgs (args: string list) : Result<BuildOptions, string> =
        let rec loop (rest: string list) (inputPath: string option) (outDir: string option) (asmName: string option) =
            match rest with
            | [] ->
                match inputPath with
                | None -> Error "build command requires <input.atla>"
                | Some path ->
                    let fileName = Path.GetFileNameWithoutExtension(path)
                    let resolvedOutDir =
                        match outDir with
                        | Some value -> value
                        | None -> Path.Join(Directory.GetCurrentDirectory(), "out")
                    let resolvedAsmName =
                        match asmName with
                        | Some value when not (String.IsNullOrWhiteSpace value) -> value
                        | _ -> fileName
                    Ok {
                        inputPath = path
                        outDir = resolvedOutDir
                        asmName = resolvedAsmName
                    }
            | "-o" :: value :: tail -> loop tail inputPath (Some value) asmName
            | "--name" :: value :: tail -> loop tail inputPath outDir (Some value)
            | flag :: [] when flag = "-o" || flag = "--name" ->
                Error $"{flag} requires a value"
            | value :: tail when value.StartsWith("-") ->
                Error $"unknown option: {value}"
            | value :: tail ->
                match inputPath with
                | Some _ -> Error "only one input file is supported"
                | None -> loop tail (Some value) outDir asmName

        loop args None None None

    let private validateInputPath (path: string) : Result<unit, string> =
        if not (File.Exists path) then
            Error $"input file not found: {path}"
        elif not (String.Equals(Path.GetExtension(path), ".atla", StringComparison.OrdinalIgnoreCase)) then
            Error $"input file extension must be .atla: {path}"
        else
            Ok ()

    let private diagnosticPrefix (severity: Atla.Core.Semantics.Data.DiagnosticSeverity) : string =
        match severity with
        | Atla.Core.Semantics.Data.DiagnosticSeverity.Error -> "error"
        | Atla.Core.Semantics.Data.DiagnosticSeverity.Warning -> "warning"
        | Atla.Core.Semantics.Data.DiagnosticSeverity.Info -> "info"

    let private printDiagnostics (diagnostics: Atla.Core.Semantics.Data.Diagnostic list) : unit =
        diagnostics
        |> List.iter (fun diagnostic ->
            Console.Error.WriteLine($"{diagnosticPrefix diagnostic.severity}: {diagnostic.toDisplayText()}"))

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
                match validateInputPath options.inputPath with
                | Error message ->
                    Console.Error.WriteLine(message)
                    1
                | Ok () ->
                    let source = File.ReadAllText(options.inputPath)
                    Directory.CreateDirectory(options.outDir) |> ignore
                    let compileResult = Compiler.compile(options.asmName, source, options.outDir)
                    printDiagnostics compileResult.diagnostics
                    if compileResult.succeeded then
                        let dllPath = Path.Join(options.outDir, options.asmName + ".dll")
                        Console.WriteLine($"Generated: {dllPath}")
                        0
                    else
                        1
        | command :: _ ->
            Console.Error.WriteLine($"unknown command: {command}")
            Console.Error.WriteLine(usage())
            1

[<EntryPoint>]
let main argv = Cli.run argv
