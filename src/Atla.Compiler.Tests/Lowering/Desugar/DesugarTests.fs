namespace Atla.Compiler.Tests.Lowering

open Xunit
open Atla.Compiler.Data
open Atla.Compiler.Syntax
open Atla.Compiler.Syntax.Data
open Atla.Compiler.Semantics
open Atla.Compiler.Semantics.Data
open Atla.Compiler.Lowering

module DesugarTests =
    [<Fact>]
    let ``semantic analysis and lowering handle do block`` () =
        let program = """
fn main (): Int = do
    let value = 1
    value
"""

        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                let hirModule = Analyze.analyzeModule(symbolTable, subst, "main", moduleAst)
                let mirAsm = Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))

                Assert.Single(mirAsm.modules) |> ignore
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
