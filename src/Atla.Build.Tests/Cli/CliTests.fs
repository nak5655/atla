namespace Atla.Build.Cli.Tests

open System
open System.IO
open Xunit
open Atla.Build

module CliTests =
    [<Fact>]
    let ``help should return zero`` () =
        let code = Cli.run [| "--help" |]
        Assert.Equal(0, code)

    [<Fact>]
    let ``no args should return one`` () =
        let code = Cli.run [||]
        Assert.Equal(1, code)

    [<Fact>]
    let ``build should reject non atla extension`` () =
        let tempDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(tempDir) |> ignore
        let sourcePath = Path.Join(tempDir, "main.txt")
        File.WriteAllText(sourcePath, "fn main: () = ()")

        let code = Cli.run [| "build"; sourcePath |]
        Assert.Equal(1, code)

    [<Fact>]
    let ``build should emit dll for valid input`` () =
        let tempDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(tempDir) |> ignore

        let sourcePath = Path.Join(tempDir, "hello.atla")
        File.WriteAllText(sourcePath, """
import System.Console

fn main: () = do
    Console.WriteLine "Hello, World!"
""".Trim())

        let outDir = Path.Join(tempDir, "artifacts")
        let code = Cli.run [| "build"; sourcePath; "-o"; outDir; "--name"; "HelloCli" |]

        Assert.Equal(0, code)
        Assert.True(File.Exists(Path.Join(outDir, "HelloCli.dll")))
