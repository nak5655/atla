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
                    | Result.Ok hir ->
                        // Lowering
                        let mir = Layout.layoutAssembly(asmName, Hir.Assembly ("hello", [hir]))
                        // Code Generation
                        Gen.genAssembly(mir, Path.Join(outDir, sprintf "%s.dll" asmName))
                        succeeded []
                    | Result.Error diagnostics ->
                        failed diagnostics
                with ex ->
                    failed [ Diagnostic.Error($"Compilation failed: {ex.Message}", Span.Empty) ]
            | Failure (reason, span) ->
                failed [ Diagnostic.Error($"Parsing failed: {reason}", span) ]
        | Failure (reason, span) ->
            failed [ Diagnostic.Error($"Lexing failed: {reason}", span) ]
