namespace Atla.Core.Tests.Lowering

open System
open System.Diagnostics
open System.IO
open System.Reflection
open Xunit
open Atla.Compiler

module LoweringTests =
    type SingleCompileRequest =
        { asmName: string
          source: string
          outDir: string
          dependencies: Compiler.ResolvedDependency list }

    let private compileSingle (request: SingleCompileRequest) : Compiler.CompileResult =
        Compiler.compileModules {
            asmName = request.asmName
            modules = [ { moduleName = "main"; source = request.source } ]
            entryModuleName = "main"
            outDir = request.outDir
            dependencies = request.dependencies
        }

    [<Fact>]
    let ``hello`` () =
        let program = """
import System'Console

fn main: () = do
    "Hello, World!" Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "HelloWorld"; source = program.Trim(); outDir = outDir; dependencies = [] }
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
    let ``nullary function call with dot-only syntax compiles and runs`` () =
        let program = """
import System'Console

fn greet (): () = "hello!" Console'WriteLine.

fn main: () = greet.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "NullaryCall"; source = program.Trim(); outDir = outDir; dependencies = [] }
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
    let ``unit argument call should fail for nullary function`` () =
        let program = """
import System'Console

fn greet (): () = "hello!" Console'WriteLine.

fn main: () = () greet.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "UnitArgIsNotNullary"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.False(res.succeeded)
        Assert.Contains(res.diagnostics, fun d -> d.message.Contains("different number of arguments"))

    [<Fact>]
    let ``fizzbuzz program compiles`` () =
        let program = """
import System'Array
import System'Console
import System'Linq'Enumerable
fn fizzbuzz (n: Int): () =
    for i in 1 n Enumerable'Range.
        let s =
            |? i % 15 == 0 => "FizzBuzz"
            |: i % 5 == 0 => "Buzz"
            |: i % 3 == 0 => "Fizz"
            |: else => i'ToString.
        s Console'WriteLine.


fn main: () = do
    let n = Console'ReadLine. Int32'Parse.
    n fizzbuzz.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "FizzBuzz"; source = program.Trim(); outDir = outDir; dependencies = [] }
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
import System'Int32
import System'Console

fn fibonacci (n: Int): Int =
    |? n == 0 => 0
    |: n == 1 => 1
    |: n == 2 => 1
    |: else => (n - 2) fibonacci. + (n - 1) fibonacci.

fn main: () = do
    let n = Console'ReadLine. Int32'Parse.
    n fibonacci. Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "Fibonacci"; source = program.Trim(); outDir = outDir; dependencies = [] }
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

        let res = compileSingle { asmName = "ExitCodeProgram"; source = program.Trim(); outDir = outDir; dependencies = [] }
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
import System'Int32
import System'Console

fn main: () = do
    var n_x = Console'ReadLine.
    n_x = " " n_x'Split.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "SplitOptionalArg"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

        let dllPath = Path.Join(outDir, "SplitOptionalArg.dll")
        Assert.True(File.Exists dllPath)

    [<Fact>]
    let ``array index access reads second element`` () =
        let program = """
import System'Console

fn main: () = do
    let a = Console'ReadLine.
    a !! 1 Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "ArrayIndexAccess"; source = program.Trim(); outDir = outDir; dependencies = [] }
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
import System'Console

fn main: () = do
    let line = Console'ReadLine.
    let a = " " line'Split.
    a !! 0 Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "ArrayIndexAccessSplit"; source = program.Trim(); outDir = outDir; dependencies = [] }
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
    let ``Array String annotated function compiles and runs`` () =
        let program = """
import System'Console

fn count (xs: Array String): Int = xs'Length

fn main: () = do
    let line = Console'ReadLine.
    let a = " " line'Split.
    a count. Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "ArrayStringAnnotated"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

        let dllPath = Path.Join(outDir, "ArrayStringAnnotated.dll")
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
        proc.StandardInput.WriteLine("foo bar baz")
        proc.StandardInput.Close()

        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()

        Assert.Equal(0, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        Assert.Equal("3", stdout.Trim())

    [<Fact>]
    let ``range with array length and index access prints all tokens`` () =
        let program = """
import System'Int32
import System'Console
import System'Linq'Enumerable

fn main: () = do
    let line = Console'ReadLine.
    let a = " " line'Split.
    for i in 0 a'Length Enumerable'Range.
        a !! i Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "RangeArrayLengthLoop"; source = program.Trim(); outDir = outDir; dependencies = [] }
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

        let result = compileSingle { asmName = "CompileResultSuccess"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(result.succeeded)
        Assert.Empty(result.diagnostics)

    [<Fact>]
    let ``compile result should include error diagnostics on failure`` () =
        let program = """
fn main: Int =
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let result = compileSingle { asmName = "CompileResultFailure"; source = program.Trim(); outDir = outDir; dependencies = [] }
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
              compileReferencePaths = [ missingPath ]
              runtimeLoadPaths = [ missingPath ]
              nativeRuntimePaths = [] }

        let result =
            compileSingle
                { asmName = "MissingDependencyProgram"
                  source = program.Trim()
                  outDir = outDir
                  dependencies = [ dependency ] }

        Assert.False(result.succeeded)
        Assert.Contains(result.diagnostics, fun diagnostic -> diagnostic.message.Contains("runtime load assembly not found"))

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
              compileReferencePaths = [ invalidDllPath ]
              runtimeLoadPaths = [ invalidDllPath ]
              nativeRuntimePaths = [] }

        let result =
            compileSingle
                { asmName = "InvalidDependencyProgram"
                  source = program.Trim()
                  outDir = outDir
                  dependencies = [ dependency ] }

        Assert.False(result.succeeded)
        Assert.Contains(result.diagnostics, fun diagnostic -> diagnostic.message.Contains("runtime load assembly is not a valid .NET assembly"))

    [<Fact>]
    let ``compile should resolve imported system type after dependency assembly load`` () =
        let runtimeDir = Path.GetDirectoryName(typeof<string>.Assembly.Location)
        let jsonAssemblyPath = Path.Join(runtimeDir, "System.Text.Json.dll")
        Assert.True(File.Exists(jsonAssemblyPath), $"missing runtime assembly for test: {jsonAssemblyPath}")

        let program = """
import System'Text'Json'JsonNamingPolicy

fn main: () = do
    var policy = JsonNamingPolicy'CamelCase
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let dependency: Compiler.ResolvedDependency =
            { name = "System.Text.Json"
              version = "runtime"
              source = runtimeDir
              compileReferencePaths = [ jsonAssemblyPath ]
              runtimeLoadPaths = [ jsonAssemblyPath ]
              nativeRuntimePaths = [] }

        let result =
            compileSingle
                { asmName = "JsonImportWithDependency"
                  source = program.Trim()
                  outDir = outDir
                  dependencies = [ dependency ] }

        Assert.True(result.succeeded, String.concat Environment.NewLine (result.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))

    [<Fact>]
    let ``compile should use runtimeLoadPaths even when compileReferencePaths are unusable`` () =
        let runtimeDir = Path.GetDirectoryName(typeof<string>.Assembly.Location)
        let jsonAssemblyPath = Path.Join(runtimeDir, "System.Text.Json.dll")
        Assert.True(File.Exists(jsonAssemblyPath), $"missing runtime assembly for test: {jsonAssemblyPath}")

        let program = """
import System'Text'Json'JsonNamingPolicy

fn main: () = do
    var policy = JsonNamingPolicy'CamelCase
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore
        let unusableCompileRefPath = Path.Join(outDir, "missing-compile-ref.dll")

        let dependency: Compiler.ResolvedDependency =
            { name = "System.Text.Json"
              version = "runtime"
              source = runtimeDir
              compileReferencePaths = [ unusableCompileRefPath ]
              runtimeLoadPaths = [ jsonAssemblyPath ]
              nativeRuntimePaths = [] }

        let result =
            compileSingle
                { asmName = "JsonImportRuntimePathPriority"
                  source = program.Trim()
                  outDir = outDir
                  dependencies = [ dependency ] }

        Assert.True(result.succeeded, String.concat Environment.NewLine (result.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))

    [<Fact>]
    let ``compile should report unresolved imported system type separately from dependency load failures`` () =
        let program = """
import System'Text'Json'DoesNotExist

fn main: () = do
    var x = DoesNotExist'Parse
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let result =
            compileSingle
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

    [<Fact>]
    let ``compile should fail when dependency reference simple names conflict`` () =
        let runtimeDir = Path.GetDirectoryName(typeof<string>.Assembly.Location)
        let jsonAssemblyPath = Path.Join(runtimeDir, "System.Text.Json.dll")
        Assert.True(File.Exists(jsonAssemblyPath), $"missing runtime assembly for test: {jsonAssemblyPath}")

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        let depDirA = Path.Join(outDir, "depA")
        let depDirB = Path.Join(outDir, "depB")
        Directory.CreateDirectory(depDirA) |> ignore
        Directory.CreateDirectory(depDirB) |> ignore

        let copiedPathA = Path.Join(depDirA, "System.Text.Json.dll")
        let copiedPathB = Path.Join(depDirB, "System.Text.Json.dll")
        File.Copy(jsonAssemblyPath, copiedPathA, true)
        File.Copy(jsonAssemblyPath, copiedPathB, true)

        let depA: Compiler.ResolvedDependency =
            { name = "dep-a"
              version = "1.0.0"
              source = depDirA
              compileReferencePaths = [ copiedPathA ]
              runtimeLoadPaths = [ copiedPathA ]
              nativeRuntimePaths = [] }

        let depB: Compiler.ResolvedDependency =
            { name = "dep-b"
              version = "1.0.0"
              source = depDirB
              compileReferencePaths = [ copiedPathB ]
              runtimeLoadPaths = [ copiedPathB ]
              nativeRuntimePaths = [] }

        let result =
            compileSingle
                { asmName = "DependencyConflictProgram"
                  source = "fn main: Int = 0"
                  outDir = outDir
                  dependencies = [ depA; depB ] }

        Assert.False(result.succeeded)
        Assert.Contains(result.diagnostics, fun diagnostic -> diagnostic.message.Contains("simple-name conflict"))

    [<Fact>]
    let ``compile should keep dependency diagnostics order deterministic across runs`` () =
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let depA: Compiler.ResolvedDependency =
            { name = "zzz-missing"
              version = "1.0.0"
              source = outDir
              compileReferencePaths = [ Path.Join(outDir, "zzz-missing.dll") ]
              runtimeLoadPaths = [ Path.Join(outDir, "zzz-missing.dll") ]
              nativeRuntimePaths = [] }

        let depB: Compiler.ResolvedDependency =
            { name = "aaa-missing"
              version = "1.0.0"
              source = outDir
              compileReferencePaths = [ Path.Join(outDir, "aaa-missing.dll") ]
              runtimeLoadPaths = [ Path.Join(outDir, "aaa-missing.dll") ]
              nativeRuntimePaths = [] }

        let compileOnce () =
            compileSingle
                { asmName = "DependencyDeterminismProgram"
                  source = "fn main: Int = 0"
                  outDir = outDir
                  dependencies = [ depA; depB ] }

        let run1 = compileOnce ()
        let run2 = compileOnce ()

        Assert.False(run1.succeeded)
        Assert.False(run2.succeeded)

        let messages1 = run1.diagnostics |> List.map (fun diagnostic -> diagnostic.message)
        let messages2 = run2.diagnostics |> List.map (fun diagnostic -> diagnostic.message)
        Assert.Equal<string list>(messages1, messages2)

    [<Fact>]
    let ``function with imported dotnet type as parameter compiles and runs`` () =
        // インポート型（TypeId.Name sid）を関数パラメータに使った場合の CIL 生成を検証する。
        // System.Text.StringBuilder を引数に取る関数を定義し、正常にコンパイル・実行できることを確認する。
        let program = """
import System'Text'StringBuilder
import System'Console

fn process (sb: StringBuilder): () = do
    let _ = "ok" sb'Append.
    sb'ToString. Console'WriteLine.

fn main: () = do
    let sb = StringBuilder.
    sb process.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res =
            compileSingle
                { asmName = "ImportedTypeParam"
                  source = program.Trim()
                  outDir = outDir
                  dependencies = [] }

        Assert.True(res.succeeded, sprintf "Compilation failed: %A" (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "ImportedTypeParam.dll")
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
        Assert.Equal("ok", stdout.Trim())


    [<Fact>]
    let ``compileModules should report ambiguous import when module and type path collide`` () =
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let mainSource = "import Foo'Bar\n\nfn main: Int = 7 Foo'Bar'make."
        let fooBarSource = "fn make (x: Int): Int = x"
        let fooSource = "data Bar = { val: Int }"

        let result =
            Compiler.compileModules {
                asmName = "ImportModulePriority"
                modules =
                    [ { moduleName = "main"; source = mainSource }
                      { moduleName = "Foo.Bar"; source = fooBarSource }
                      { moduleName = "Foo"; source = fooSource } ]
                entryModuleName = "main"
                outDir = outDir
                dependencies = []
            }

        Assert.Contains(result.diagnostics, fun d -> d.message.Contains("ambiguous"))

    [<Fact>]
    let ``compileModules should fallback to type import when module is absent`` () =
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let mainSource =
            "import Foo'Bar\n\nfn id (x: Bar): Bar = x\n\nfn main: () = do\n    let v = Bar { val = 42 }\n    let _ = v id."
        let fooSource = "data Bar = { val: Int }"

        let result =
            Compiler.compileModules {
                asmName = "ImportTypeFallback"
                modules =
                    [ { moduleName = "main"; source = mainSource }
                      { moduleName = "Foo"; source = fooSource } ]
                entryModuleName = "main"
                outDir = outDir
                dependencies = []
            }

        Assert.True(result.succeeded, String.concat Environment.NewLine (result.diagnostics |> List.map (fun d -> d.message)))

    [<Fact>]
    let ``compileModules should allow imported data init via import sub'Person`` () =
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let mainSource =
            "import sub'Person\n\nfn main: () = do\n    let p = Person { name = \"alice\" }\n    let _ = p"
        let subSource = "data Person = { name: String }"

        let result =
            Compiler.compileModules {
                asmName = "ImportSubPersonDataInit"
                modules =
                    [ { moduleName = "main"; source = mainSource }
                      { moduleName = "sub"; source = subSource } ]
                entryModuleName = "main"
                outDir = outDir
                dependencies = []
            }

        Assert.True(result.succeeded, String.concat Environment.NewLine (result.diagnostics |> List.map (fun d -> d.message)))

    [<Fact>]
    let ``compileModules should allow calling imported data type methods via import sub'Person`` () =
        // Regression test: import sub'Person 経由で型をインポートし、そのメソッドを呼び出せることを検証する。
        // 修正前は cross-module import 時に新しい typeSid が割り当てられ、メソッド引数型との不一致で
        // "No overload matched argument count 1" が発生していた。
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let mainSource =
            "import sub\nimport sub'Person\n\nfn main: () = do\n    let p = Person { name = \"alice\" }\n    p'greet."

        let subSource =
            "import System'Console\n\ndata Person = { name: String }\n\nimpl Person\n    fn greet self: () = do\n        self'name Console'WriteLine."

        let result =
            Compiler.compileModules {
                asmName = "ImportSubPersonMethodCall"
                modules =
                    [ { moduleName = "main"; source = mainSource }
                      { moduleName = "sub"; source = subSource } ]
                entryModuleName = "main"
                outDir = outDir
                dependencies = []
            }

        Assert.True(result.succeeded, String.concat Environment.NewLine (result.diagnostics |> List.map (fun d -> d.message)))

    [<Fact>]
    let ``Bool literals compile and return correct values`` () =
        let program = """
import System'Console

fn main: () = do
    True Console'WriteLine.
    False Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "BoolLiterals"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "BoolLiterals.dll")
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
        let lines = stdout.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        Assert.Equal("True", lines.[0].Trim())
        Assert.Equal("False", lines.[1].Trim())

    [<Fact>]
    let ``captured function-typed variable in call position inside closure compiles and runs`` () =
        // Regression test: クロージャー内で捕捉した関数型変数を呼び出す場合に
        // "Unknown method symbol" エラーが発生していたバグの修正を検証する。
        // 修正前は rewriteCapturedRefs が Call(Fn capturedSid, ...) を書き換えず、
        // Layout フェーズが CallSym(capturedSid) を生成し、Gen フェーズで失敗していた。
        let program = """
import System'Console

fn printMsg: () = do
    "called!" Console'WriteLine.

fn applyTwice (callback: () -> ()): () = do
    let doIt = fn () -> callback.
    doIt.
    doIt.

fn main: () = do
    printMsg applyTwice.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "CapturedFnCall"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat System.Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "CapturedFnCall.dll")
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
        let lines = stdout.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        Assert.Equal(2, lines.Length)
        Assert.Equal("called!", lines.[0].Trim())
        Assert.Equal("called!", lines.[1].Trim())

    [<Fact>]
    let ``unary minus on Float variable compiles and produces correct value`` () =
        // 回帰テスト: `-value` で value が Float 変数の場合に
        // "No overload matched for '-'" が発生していたバグの修正を検証する。
        // 修正前は parser が `0 - value` へ脱糖し、Int(0) と Float の型不一致でエラーになっていた。
        let program = """
import System'Console

fn main: () = do
    let value = 3.14
    let invertedValue = -value
    invertedValue'ToString. Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "UnaryMinusFloat"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "UnaryMinusFloat.dll")
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
        let lines = stdout.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        Assert.Equal(1, lines.Length)
        Assert.Equal("-3.14", lines.[0].Trim())

    [<Fact>]
    let ``unary minus on Int variable compiles and produces correct value`` () =
        // 回帰テスト: `-value` で value が Int 変数の場合に正しく動作することを確認する。
        let program = """
import System'Console

fn main: () = do
    let n = 42
    let neg = -n
    neg'ToString. Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "UnaryMinusInt"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "UnaryMinusInt.dll")
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
        let lines = stdout.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        Assert.Equal(1, lines.Length)
        Assert.Equal("-42", lines.[0].Trim())

    [<Fact>]
    let ``role and impl for compiles and executes correctly`` () =
        // role 型を宣言し impl ... for ... で実装した型のメソッドが正しく実行されることを検証する。
        // hello_role example と同等のプログラム。
        let program = """
import System'Console

role Geometry
    fn area self: Float

data Rectangle = { width: Float, height: Float }

impl Geometry for Rectangle
    fn area self: Float =
        self'width * self'height

fn main: () =
    let rect = Rectangle { width = 5.0, height = 10.0 }
    rect'area. Console'WriteLine.
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "HelloRole"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "HelloRole.dll")
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
        Assert.Equal("50", stdout.Trim())

    [<Fact>]
    let ``enum match and constructors compile and execute correctly`` () =
        let program = """
import System'Console

enum Color
    | Black
    | White
    | Rgb { r: Int, g: Int, b: Int }
    | Hsv { h: Int, s: Int, v: Int }

impl Color
    fn red self: Int =
        |@ self
            |: Color'Black => 0
            |: Color'White => 255
            |: Color'Rgb { r, .. } => r
            |: Color'Hsv { h, s, v } => (h * s * v) / 10000

fn main: () =
    let color = Color'Rgb { r = 255, g = 0, b = 0 }
    color'red. Console'WriteLine.
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "HelloEnum"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "HelloEnum.dll")
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
        Assert.Equal("255", stdout.Trim())

    [<Fact>]
    let ``compileModules should allow imported enum constructor and methods via import sub'Color`` () =
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let mainSource =
            """
import System'Console
import sub
import sub'Color

fn main: () = do
    let color = Color'Rgb { r = 255, g = 0, b = 0 }
    color'red. Console'WriteLine.
"""

        let subSource =
            """
enum Color
    | Black
    | Rgb { r: Int, g: Int, b: Int }

impl Color
    fn red self: Int =
        |@ self
            |: Color'Black => 0
            |: Color'Rgb { r, .. } => r
"""

        let result =
            Compiler.compileModules {
                asmName = "ImportEnumMethodCall"
                modules =
                    [ { moduleName = "main"; source = mainSource.Trim() }
                      { moduleName = "sub"; source = subSource.Trim() } ]
                entryModuleName = "main"
                outDir = outDir
                dependencies = []
            }

        Assert.True(result.succeeded, String.concat Environment.NewLine (result.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "ImportEnumMethodCall.dll")
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
        Assert.Equal("255", stdout.Trim())

    [<Fact>]
    let ``generic enum Opt'None unifies with Opt<T> field type`` () =
        // 回帰テスト: Opt'None を Opt<具体型> フィールドに代入するとき
        // "Cannot unify types: Opt and Opt<SomeType>" エラーが発生していたバグの検出用。
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let source = """
import System'Console

enum Opt T
    | None
    | Some { value: T }

data Box =
    { _value: Opt Int }

impl Box
    fn new: Box =
        Box { _value = Opt'None }

    fn get self: Int =
        |@ self'_value
            |: Opt'None => -1
            |: Opt'Some { value } => value

fn main: () = do
    let b = Box'new.
    b'get. Console'WriteLine.
"""

        let res = compileSingle { asmName = "OptNoneUnify"; source = source.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "OptNoneUnify.dll")
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
        Assert.Equal("-1", stdout.Trim())

    [<Fact>]
    let ``generic enum Opt'Some pipeline constructor unifies with Opt<T> field type`` () =
        // 回帰テスト: "arg Type'Case." パイプライン構文で enum case コンストラクターを呼び出すとき
        // "Enum case 'Some' requires a payload initializer" エラーが発生していたバグの検出用。
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let source = """
import System'Console

enum Opt T
    | None
    | Some { value: T }

data Box =
    { _value: Opt Int }

impl Box
    fn new (n: Int): Box =
        Box { _value = n Opt'Some. }

    fn get self: Int =
        |@ self'_value
            |: Opt'None => -1
            |: Opt'Some { value } => value

fn main: () = do
    let b = 42 Box'new.
    b'get. Console'WriteLine.
"""

        let res = compileSingle { asmName = "OptSomePipeline"; source = source.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "OptSomePipeline.dll")
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
        Assert.Equal("42", stdout.Trim())

    [<Fact>]
    let ``generic impl Opt T with wildcard pattern compiles`` () =
        // 回帰テスト: impl Opt T のメソッドが wildcard パターン（Opt'Some _）を使っても
        // CIL 生成エラー "Unresolved meta type is not supported in Gen" が
        // 発生しないことを確認する。
        // 根本原因: impl Opt T の self が型消去後の TypeId.Name optSid を持ち、
        // instantiateEnumRootType が fresh meta を生成し、typeVarSubst = {T -> Meta(m)} となり
        // Positional("_") の boundType が Meta(m) になって CIL 生成でクラッシュしていた。
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let source = """
enum Opt T
    | None
    | Some { value: T }

impl Opt T
    fn count self: Int =
        |@ self
            |: Opt'None => 0
            |: Opt'Some _ => 1

fn main: () = ()
"""

        let res = compileSingle { asmName = "GenericImplWildcard"; source = source.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "GenericImplWildcard.dll")
        Assert.True(File.Exists dllPath)

    [<Fact>]
    let ``generic impl Opt T with named field binding compiles`` () =
        // 回帰テスト: impl Opt T のメソッドが named field binding（Opt'Some { value }）を使っても
        // TypeVar として正しく扱われ CIL 生成が成功することを確認する。
        // Fix 1 により typeVarSubst = {T -> TypeVar "T"} となり boundType = TypeVar "T" → typeof<obj>
        // として型消去され正しく動作する。
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let source = """
enum Opt T
    | None
    | Some { value: T }

impl Opt T
    fn hasPayload self: Int =
        |@ self
            |: Opt'None => 0
            |: Opt'Some { value } => 1

fn main: () = ()
"""

        let res = compileSingle { asmName = "GenericImplNamedField"; source = source.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "GenericImplNamedField.dll")
        Assert.True(File.Exists dllPath)

    [<Fact>]
    let ``impl instance method compiles as CIL instance method not static`` () =
        // 回帰テスト: fn f self で定義した impl メソッドが CIL インスタンスメソッドとして
        // コンパイルされることを検証する。コンパイル後の DLL をリフレクションで検査する。
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let source = """
import System'Console

data Point = { x: Int, y: Int }

impl Point
    fn sum self: Int = self'x + self'y

fn main: () =
    let p = Point { x = 3, y = 4 }
    p'sum. Console'WriteLine.
"""

        let res = compileSingle { asmName = "ImplInstanceMethod"; source = source.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "ImplInstanceMethod.dll")
        Assert.True(File.Exists dllPath)

        // コンパイル済み DLL をリフレクションでロードして Point.sum がインスタンスメソッドであることを確認する。
        let loadContext = System.Runtime.Loader.AssemblyLoadContext("TestImplInstanceMethod", isCollectible = true)
        try
            let asm = loadContext.LoadFromAssemblyPath(dllPath)
            let pointType = asm.GetType("Point")
            Assert.NotNull(pointType)
            let sumMethod = pointType.GetMethod("sum", BindingFlags.Public ||| BindingFlags.Instance)
            Assert.NotNull(sumMethod)
            Assert.False(sumMethod.IsStatic, "sum method must be a CIL instance method, not static")
        finally
            loadContext.Unload()

        // プログラムを実行して正しい結果（7）を返すことを確認する。
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
        Assert.Equal("7", stdout.Trim())
