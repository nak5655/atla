namespace Atla.Core.Tests.Lowering

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

        let res = Compiler.compile { asmName = "HelloWorld"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

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
    let ``nullary function call with unit argument syntax compiles and runs`` () =
        let program = """
import System.Console

fn greet (): () = Console.WriteLine "hello!"

fn main: () = greet ()
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile { asmName = "NullaryCall"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

        let dllPath = Path.Join(outDir, "NullaryCall.dll")
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
        Assert.Equal("hello!", stdout.Trim())

    [<Fact>]
    let ``fizzbuzz program compiles`` () =
        let program = """
import System.Array
import System.Console
import System.Linq.Enumerable
fn fizzbuzz (n: Int): () =
    for i in Enumerable.Range 1 n
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

        let res = Compiler.compile { asmName = "FizzBuzz"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

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

        let res = Compiler.compile { asmName = "Fibonacci"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

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

    [<Fact>]
    let ``main int return should become process exit code`` () =
        let program = """
fn main: Int = 7
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile { asmName = "ExitCodeProgram"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

        let dllPath = Path.Join(outDir, "ExitCodeProgram.dll")
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

        Assert.Equal(7, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        Assert.True(String.IsNullOrWhiteSpace stdout, stdout)

    [<Fact>]
    let ``string split with optional argument style compiles`` () =
        let program = """
import System.Int32
import System.Console

fn main: () = do
    var n_x = Console.ReadLine ()
    n_x = n_x.Split " "
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile { asmName = "SplitOptionalArg"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

        let dllPath = Path.Join(outDir, "SplitOptionalArg.dll")
        Assert.True(File.Exists dllPath)

    [<Fact>]
    let ``array index access reads second element`` () =
        let program = """
import System.Console

fn main: () = do
    let a = Console.ReadLine ()
    Console.WriteLine a[1]
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile { asmName = "ArrayIndexAccess"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

        let dllPath = Path.Join(outDir, "ArrayIndexAccess.dll")
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
        proc.StandardInput.WriteLine("12")
        proc.StandardInput.Close()

        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()

        Assert.Equal(0, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        Assert.Equal("2", stdout.Trim())

    [<Fact>]
    let ``array index access on split result reads first token`` () =
        let program = """
import System.Console

fn main: () = do
    let a = (Console.ReadLine ()).Split " "
    Console.WriteLine a[0]
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile { asmName = "ArrayIndexAccessSplit"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

        let dllPath = Path.Join(outDir, "ArrayIndexAccessSplit.dll")
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
        proc.StandardInput.WriteLine("12 34")
        proc.StandardInput.Close()

        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()

        Assert.Equal(0, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        Assert.Equal("12", stdout.Trim())

    [<Fact>]
    let ``range with array length and index access prints all tokens`` () =
        let program = """
import System.Int32
import System.Console
import System.Linq.Enumerable

fn main: () = do
    let a = (Console.ReadLine ()).Split " "
    for i in Enumerable.Range 0 a.Length
        Console.WriteLine a[i]
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = Compiler.compile { asmName = "RangeArrayLengthLoop"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

        let dllPath = Path.Join(outDir, "RangeArrayLengthLoop.dll")
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
        proc.StandardInput.WriteLine("10 20 30")
        proc.StandardInput.Close()

        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()

        Assert.Equal(0, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        Assert.Equal("10\n20\n30".Replace("\n", Environment.NewLine), stdout.Trim())

    [<Fact>]
    let ``compile result should include diagnostics list on success`` () =
        let program = """
fn main: Int = 0
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let result = Compiler.compile { asmName = "CompileResultSuccess"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(result.succeeded)
        Assert.Empty(result.diagnostics)

    [<Fact>]
    let ``compile result should include error diagnostics on failure`` () =
        let program = """
fn main: Int =
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let result = Compiler.compile { asmName = "CompileResultFailure"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.False(result.succeeded)
        Assert.NotEmpty(result.diagnostics)
        Assert.Contains(result.diagnostics, fun diagnostic -> diagnostic.isError)

    [<Fact>]
    let ``compile should fail when dependency reference assembly path is missing`` () =
        let program = """
fn main: Int = 0
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore
        let missingPath = Path.Join(outDir, "missing-dependency.dll")

        let dependency: Compiler.ResolvedDependency =
            { name = "missing-dependency"
              version = "1.0.0"
              source = outDir
              referenceAssemblyPaths = [ missingPath ] }

        let result =
            Compiler.compile
                { asmName = "MissingDependencyProgram"
                  source = program.Trim()
                  outDir = outDir
                  dependencies = [ dependency ] }

        Assert.False(result.succeeded)
        Assert.Contains(result.diagnostics, fun diagnostic -> diagnostic.message.Contains("reference assembly not found"))

    [<Fact>]
    let ``compile should fail when dependency reference assembly is invalid format`` () =
        let program = """
fn main: Int = 0
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore
        let invalidDllPath = Path.Join(outDir, "invalid-dependency.dll")
        File.WriteAllText(invalidDllPath, "not a valid managed assembly")

        let dependency: Compiler.ResolvedDependency =
            { name = "invalid-dependency"
              version = "1.0.0"
              source = outDir
              referenceAssemblyPaths = [ invalidDllPath ] }

        let result =
            Compiler.compile
                { asmName = "InvalidDependencyProgram"
                  source = program.Trim()
                  outDir = outDir
                  dependencies = [ dependency ] }

        Assert.False(result.succeeded)
        Assert.Contains(result.diagnostics, fun diagnostic -> diagnostic.message.Contains("not a valid .NET assembly"))

    [<Fact>]
    let ``compile should resolve imported system type after dependency assembly load`` () =
        let runtimeDir = Path.GetDirectoryName(typeof<string>.Assembly.Location)
        let jsonAssemblyPath = Path.Join(runtimeDir, "System.Text.Json.dll")
        Assert.True(File.Exists(jsonAssemblyPath), $"missing runtime assembly for test: {jsonAssemblyPath}")

        let program = """
import System.Text.Json.JsonNamingPolicy

fn main: () = do
    var policy = JsonNamingPolicy.CamelCase
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let dependency: Compiler.ResolvedDependency =
            { name = "System.Text.Json"
              version = "runtime"
              source = runtimeDir
              referenceAssemblyPaths = [ jsonAssemblyPath ] }

        let result =
            Compiler.compile
                { asmName = "JsonImportWithDependency"
                  source = program.Trim()
                  outDir = outDir
                  dependencies = [ dependency ] }

        Assert.True(result.succeeded, String.concat Environment.NewLine (result.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))

    [<Fact>]
    let ``compile should report unresolved imported system type separately from dependency load failures`` () =
        let program = """
import System.Text.Json.DoesNotExist

fn main: () = do
    var x = DoesNotExist.Parse
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let result =
            Compiler.compile
                { asmName = "JsonImportMissingType"
                  source = program.Trim()
                  outDir = outDir
                  dependencies = [] }

        Assert.False(result.succeeded)
        Assert.Contains(
            result.diagnostics,
            fun diagnostic -> diagnostic.message.Contains("Imported system type 'DoesNotExist' was not found")
        )
        Assert.DoesNotContain(
            result.diagnostics,
            fun diagnostic -> diagnostic.message.Contains("dependency `")
        )
