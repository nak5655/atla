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
    let ``fizzbuzz program compiles`` () =
        let program = """
import System.Array
import System.Console
import System.Linq.Enumerable

fn fizzbuzz (n: Int): () =
    for i in (Enumerable.Range 1 n).GetEnumerator()
        Console.WriteLine if
            | i % 15 == 0 => "FizzBuzz"
            | i % 5 == 0 => "Buzz"
            | i % 3 == 0 => "Fizz"
            | else => i.ToString()

fn main: () = do
    let n = Int32.Parse (Console.ReadLine ())
    fizzbuzz n
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile("FizzBuzz", program.Trim(), outDir)
        Assert.True(res.IsOk)

        let dllPath = Path.Join(outDir, "FizzBuzz.dll")
        Assert.True(File.Exists dllPath)

        let psi =
            ProcessStartInfo(
                FileName = "dotnet",
                Arguments = dllPath,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            )

        use proc = Process.Start(psi)
        proc.StandardInput.WriteLine("15")
        proc.StandardInput.Close()

        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()

        Assert.Equal(0, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        let expected =
            [ "1"; "2"; "Fizz"; "4"; "Buzz"; "Fizz"; "7"; "8"; "Fizz"; "Buzz"; "11"; "Fizz"; "13"; "14"; "FizzBuzz" ]
            |> String.concat Environment.NewLine
        Assert.Equal(expected, stdout.Trim())

    [<Fact>]
    let ``fibonacci sum program compiles`` () =
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

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile("Fibonacci", program.Trim(), outDir)
        Assert.True(res.IsOk)

        let dllPath = Path.Join(outDir, "Fibonacci.dll")
        Assert.True(File.Exists dllPath)

        let psi =
            ProcessStartInfo(
                FileName = "dotnet",
                Arguments = dllPath,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            )

        use proc = Process.Start(psi)
        proc.StandardInput.WriteLine("10")
        proc.StandardInput.Close()

        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()

        Assert.Equal(0, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        Assert.Equal("55", stdout.Trim())
