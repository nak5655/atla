namespace Atla.Console.Build.Tests

open System
open System.IO
open Xunit
open Atla.Console

module BuildTests =
    [<Fact>]
    let ``help should return zero`` () =
        let code = Console.run [| "--help" |]
        Assert.Equal(0, code)

    [<Fact>]
    let ``no args should return one`` () =
        let code = Console.run [||]
        Assert.Equal(1, code)

    [<Fact>]
    let ``build should reject non atla extension`` () =
        let tempDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(tempDir) |> ignore
        let sourcePath = Path.Join(tempDir, "main.txt")
        File.WriteAllText(sourcePath, "fn main: () = ()")

        let code = Console.run [| "build"; sourcePath |]
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
        let code = Console.run [| "build"; sourcePath; "-o"; outDir; "--name"; "HelloConsole" |]

        Assert.Equal(0, code)
        Assert.True(File.Exists(Path.Join(outDir, "HelloConsole.dll")))
