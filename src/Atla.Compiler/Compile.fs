namespace Atla.Compiler

open Atla.Compiler.Data
open Atla.Compiler.Syntax
open Atla.Compiler.Syntax.Data
open Atla.Compiler.Semantics
open Atla.Compiler.Semantics.Data
open Atla.Compiler.Lowering
open System.IO

module Compiler =
    let compile (asmName: string, source: string, outDir: string) : Result<unit, string> =
        // Lexing
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            // Parsing
            match Parser.fileModule() tokenInput tokens.Head.span.left with
            | Success (moduleAst, _) ->
                // Semantic Analysis
                let symbolTable = SymbolTable()
                let typeSubst = TypeSubst()
                let hir = Analyze.analyzeModule(symbolTable, typeSubst, "main", moduleAst)
                // Lowering
                let mir = Layout.layoutAssembly(asmName, Hir.Assembly ("hello", [hir]))
                // Code Generation
                Gen.genAssembly(mir, Path.Join(outDir, sprintf "%s.dll" asmName))
                Ok ()
            | Failure (reason, span) ->
                Result.Error $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}"
        | Failure (reason, span) ->
            Result.Error $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}"
