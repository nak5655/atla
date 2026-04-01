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
                let hir = Analyze.analyzeModule(symbolTable, "main", moduleAst)
                // Typing
                Typing.typingModule hir
                // Lowering
                let mir = Layout.layoutAssembly(asmName, Hir.Assembly ("hello", [hir]))
                // Code Generation
                let gen = Gen()
                gen.GenAssembly(mir, Path.Join(outDir, sprintf "%s.dll" asmName))
                Ok ()
            | Failure (reason, span) ->
                Result.Error $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}"
        | Failure (reason, span) ->
            Result.Error $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}"