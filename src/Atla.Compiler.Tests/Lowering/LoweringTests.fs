namespace Atla.Compiler.Tests.Lowering

open System
open Xunit
open Atla.Compiler


module LoweringTests =
    [<Fact>]
    let ``hello`` () =
        let program = """
import System.Console

let main = do
    Console.WriteLine "Hello, World!"
    a
"""
        let res = Compiler.compile("HelloWorld", program, "files")
        Assert.True(res.IsOk)
