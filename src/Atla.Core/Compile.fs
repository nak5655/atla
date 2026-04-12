namespace Atla.Compiler

open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data
open Atla.Core.Semantics
open Atla.Core.Semantics.Data
open Atla.Core.Lowering
open System.IO

module Compiler =
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

    let compile (asmName: string, source: string, outDir: string) : CompileResult =
        // Lexing
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            // Parsing
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) ->
                try
                    // Semantic Analysis
                    let symbolTable = SymbolTable()
                    let typeSubst = TypeSubst()
                    match Analyze.analyzeModule(symbolTable, typeSubst, "main", moduleAst) with
                    | { succeeded = false; diagnostics = diagnostics } ->
                        failed diagnostics
                    | { value = Some hir; diagnostics = analyzeDiagnostics } ->
                        // Lowering
                        match Layout.layoutAssembly(asmName, Hir.Assembly("hello", [ hir ])) with
                        | { succeeded = false; diagnostics = layoutDiagnostics } ->
                            failed (analyzeDiagnostics @ layoutDiagnostics)
                        | { value = Some mir; diagnostics = layoutDiagnostics } ->
                            // Code Generation
                            let outPath = Path.Join(outDir, sprintf "%s.dll" asmName)
                            match Gen.genAssembly(mir, outPath) with
                            | { succeeded = false; diagnostics = genDiagnostics } ->
                                failed (analyzeDiagnostics @ layoutDiagnostics @ genDiagnostics)
                            | { diagnostics = genDiagnostics } ->
                                succeeded (analyzeDiagnostics @ layoutDiagnostics @ genDiagnostics)
                        | _ ->
                            failed (analyzeDiagnostics @ [ Diagnostic.Error("Lowering failed with unknown state", Span.Empty) ])
                    | _ ->
                        failed [ Diagnostic.Error("Semantic analysis failed with unknown state", Span.Empty) ]
                with ex ->
                    failed [ Diagnostic.Error($"Compilation failed: {ex.Message}", Span.Empty) ]
            | Failure (reason, span) ->
                failed [ Diagnostic.Error($"Parsing failed: {reason}", span) ]
        | Failure (reason, span) ->
            failed [ Diagnostic.Error($"Lexing failed: {reason}", span) ]
