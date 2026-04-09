namespace Atla.Compiler.Tests.Lowering

open System
open System.Diagnostics
open System.IO
open Xunit
open Atla.Compiler

module LoweringTests =
    [<Fact>]
    let ``hello`` () =
        let program = """
import System.Console

fn main: () = do
    Console.WriteLine "Hello, World!"
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile("HelloWorld", program.Trim(), outDir)
        Assert.True(res.IsOk)

        let dllPath = Path.Join(outDir, "HelloWorld.dll")
        Assert.True(File.Exists dllPath)

        let psi =
            ProcessStartInfo(
                FileName = "dotnet",
                Arguments = dllPath,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            )

        use proc = Process.Start(psi)
        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()

        Assert.Equal(0, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        Assert.Equal("Hello, World!", stdout.Trim())

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

        let outDir = "files"
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile("Fibonacci", program, outDir)
        Assert.True(res.IsOk)
