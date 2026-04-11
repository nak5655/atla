namespace Atla.Compiler

open Atla.Compiler.Core.Data
open Atla.Compiler.Core.Syntax
open Atla.Compiler.Core.Syntax.Data
open Atla.Compiler.Core.Semantics
open Atla.Compiler.Core.Semantics.Data
open Atla.Compiler.Core.Lowering
open System.IO

module Compiler =
    let compile (asmName: string, source: string, outDir: string) : Result<unit, string> =
        // Lexing
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            // Parsing
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) ->
                // Semantic Analysis
                let symbolTable = SymbolTable()
                let typeSubst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, typeSubst, "main", moduleAst) with
                | Result.Ok hir ->
                    // Lowering
                    let mir = Layout.layoutAssembly(asmName, Hir.Assembly ("hello", [hir]))
                    // Code Generation
                    Gen.genAssembly(mir, Path.Join(outDir, sprintf "%s.dll" asmName))
                    Ok ()
                | Result.Error diagnostics ->
                    let message =
                        diagnostics
                        |> List.map (fun err -> err.toString())
                        |> String.concat "; "
                    Result.Error $"Semantic analysis failed: {message}"
            | Failure (reason, span) ->
                Result.Error $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}"
        | Failure (reason, span) ->
            Result.Error $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}"
