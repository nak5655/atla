namespace Atla.Compiler.Tests.Ast

open System
open Xunit
open Atla.Compiler.Parsing
open Atla.Compiler.Types

module LexerTests =
    [<Fact>]
    let ``hello`` () =
        let program = """let greeting = " hello !";var  a=-1-0.2 """
        let input: Input<SourceChar> = StringInput program
        let result = Lexer.tokenize input Position.Zero
        Assert.NotNull(Lexer.keywords)
