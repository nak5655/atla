namespace Atla.Compiler.Tests.Lowering

open System
open System.IO
open Xunit
open Atla.Compiler
open Atla.Compiler.Data
open Atla.Compiler.Syntax
open Atla.Compiler.Syntax.Data

module LoweringTests =
    [<Fact>]
    let ``hello`` () =
        let program = """
import System.Console

fn main: () = do
    Console.WriteLine "Hello, World!"
"""

        let outDir = "files"
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile("HelloWorld", program.Trim(), outDir)
        Assert.True(res.IsOk)

    [<Fact>]
    let fibonacci () =
        let program = """
import System.Int32
import System.Console

fn fibonacci (n: Int): Int = if 
    | n == 0 => 0
    | n == 1 => 1
    | n == 2 => 1
    | else => fibonacci (n - 2) + fibonacci (n - 1)

fn main: () = do
    let n = Int32.Parse (Console.ReadLine ())
    Console.WriteLine (fibonacci n)
"""

        Assert.Contains("fn fibonacci (n: Int): Int = if ", program)
        Assert.Contains("| else => fibonacci (n - 2) + fibonacci (n - 1)", program)
        Assert.Contains("Console.WriteLine (fibonacci n)", program)
