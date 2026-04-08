namespace Atla.Compiler.Tests.Lowering

open System
open System.IO
open Xunit
open Atla.Compiler

module LoweringTests =
    [<Fact>]
    let ``compile succeeds for minimal main`` () =
        let program = "fn main (): Int = 1"
        let outDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile("HelloWorld", program, outDir)
        Assert.True(res.IsOk)

    [<Fact>]
    let ``compile succeeds for if expression`` () =
        let program = "fn main (): Int = if | 1 == 1 => 1 | else => 0"
        let outDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile("IfExpr", program, outDir)
        Assert.True(res.IsOk)
