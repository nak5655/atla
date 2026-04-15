namespace Atla.Console.Build.Tests

open System
open System.IO
open Xunit
open Atla.Console

module BuildTests =
    let private createTempProjectDir () =
        let projectRoot = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(Path.Join(projectRoot, "src")) |> ignore
        projectRoot

    let private writeManifest (projectRoot: string) (name: string) =
        File.WriteAllText(
            Path.Join(projectRoot, "atla.toml"),
            $"""
[package]
name = "{name}"
version = "0.1.0"
""".Trim())

    [<Fact>]
    let ``help should return zero`` () =
        let code = Console.run [| "--help" |]
        Assert.Equal(0, code)

    [<Fact>]
    let ``no args should return one`` () =
        let code = Console.run [||]
        Assert.Equal(1, code)

    [<Fact>]
    let ``build should fail when project root does not exist`` () =
        let projectRoot = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

        let code = Console.run [| "build"; projectRoot |]
        Assert.Equal(1, code)

    [<Fact>]
    let ``build should fail when src main is missing`` () =
        let projectRoot = createTempProjectDir ()
        writeManifest projectRoot "hello"

        let code = Console.run [| "build"; projectRoot |]
        Assert.Equal(1, code)

    [<Fact>]
    let ``build should emit dll for valid project root`` () =
        let projectRoot = createTempProjectDir ()
        writeManifest projectRoot "hello"

        File.WriteAllText(
            Path.Join(projectRoot, "src", "main.atla"),
            """
import System.Console

fn main: () = do
    Console.WriteLine "Hello, World!"
""".Trim())

        let outDir = Path.Join(projectRoot, "artifacts")
        let code = Console.run [| "build"; projectRoot; "-o"; outDir; "--name"; "HelloConsole" |]

        Assert.Equal(0, code)
        Assert.True(File.Exists(Path.Join(outDir, "HelloConsole.dll")))
