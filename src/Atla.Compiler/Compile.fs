namespace Atla.Compiler

open Atla.Compiler.Ast
open Atla.Compiler.Types
open Atla.Compiler.Parsing
open Atla.Compiler.Hir
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
                // Desugaring
                let hir = Semant.analyzeModule("main", moduleAst)
                // Typing
                let globalScope = Scope.GlobalScope ()
                Typing.typingModule globalScope hir
                // Lowering
                let mir = Layout.layoutAssembly(asmName, Hir.Assembly [hir])
                // Code Generation
                let gen = Gen()
                gen.GenAssembly(mir, Path.Join(outDir, sprintf "%s.dll" asmName))
                Ok ()
            | Failure (reason, span) ->
                Result.Error $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}"
        | Failure (reason, span) ->
            Result.Error $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}"