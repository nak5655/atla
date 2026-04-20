namespace Atla.Compiler

open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data
open Atla.Core.Semantics
open Atla.Core.Semantics.Data
open Atla.Core.Lowering
open System.IO

module Compiler =
    type ResolvedDependency =
        { name: string
          version: string
          source: string
          compileReferencePaths: string list
          runtimeLoadPaths: string list
          /// ネイティブランタイム DLL のパスリスト（runtimes/<rid>/native/ 配下のファイル）。
          nativeRuntimePaths: string list }

    type CompileRequest =
        { asmName: string
          source: string
          outDir: string
          dependencies: ResolvedDependency list }

    type CompileResult =
        { succeeded: bool
          diagnostics: Diagnostic list }

        member this.hasErrors = this.diagnostics |> List.exists (fun diagnostic -> diagnostic.isError)

    let private failed (diagnostics: Diagnostic list) : CompileResult =
        { succeeded = false
          diagnostics = diagnostics }

    let private succeeded (diagnostics: Diagnostic list) : CompileResult =
        { succeeded = true
          diagnostics = diagnostics }

    let compile (request: CompileRequest) : CompileResult =
        // Lexing
        let input: Input<SourceChar> = StringInput request.source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            // Parsing
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) ->
                try
                    // Dependency loading
                    let dependencyInputs =
                        request.dependencies
                        |> List.map (fun dependency -> dependency.name, dependency.runtimeLoadPaths)

                    match DependencyLoader.loadDependencies dependencyInputs with
                    | { succeeded = false; diagnostics = dependencyDiagnostics } ->
                        failed dependencyDiagnostics
                    | { loadContext = dependencyLoadContext } ->
                        try
                            // Semantic Analysis
                            let symbolTable = SymbolTable()
                            let typeSubst = TypeSubst()
                            match Analyze.analyzeModule(symbolTable, typeSubst, "main", moduleAst) with
                            | { succeeded = false; diagnostics = diagnostics } ->
                                failed diagnostics
                            | { value = Some hir; diagnostics = analyzeDiagnostics } ->
                                // Lowering
                                match Layout.layoutAssembly(request.asmName, Hir.Assembly("hello", [ hir ])) with
                                | { succeeded = false; diagnostics = layoutDiagnostics } ->
                                    failed (analyzeDiagnostics @ layoutDiagnostics)
                                | { value = Some mir; diagnostics = layoutDiagnostics } ->
                                    // Code Generation
                                    let outPath = Path.Join(request.outDir, sprintf "%s.dll" request.asmName)
                                    match Gen.genAssembly(mir, outPath, symbolTable) with
                                    | { succeeded = false; diagnostics = genDiagnostics } ->
                                        failed (analyzeDiagnostics @ layoutDiagnostics @ genDiagnostics)
                                    | { diagnostics = genDiagnostics } ->
                                        succeeded (analyzeDiagnostics @ layoutDiagnostics @ genDiagnostics)
                                | _ ->
                                    failed (analyzeDiagnostics @ [ Diagnostic.Error("Lowering failed with unknown state", Span.Empty) ])
                            | _ ->
                                failed [ Diagnostic.Error("Semantic analysis failed with unknown state", Span.Empty) ]
                        finally
                            DependencyLoader.unloadDependencies dependencyLoadContext
                with ex ->
                    failed [ Diagnostic.Error($"Compilation failed: {ex.Message}", Span.Empty) ]
            | Failure (reason, span) ->
                failed [ Diagnostic.Error($"Parsing failed: {reason}", span) ]
        | Failure (reason, span) ->
            failed [ Diagnostic.Error($"Lexing failed: {reason}", span) ]
