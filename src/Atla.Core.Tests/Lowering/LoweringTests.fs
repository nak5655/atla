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
            modules = [ { moduleName = "main"; source = request.source; filePath = None } ]
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
    let ``List builtin constructs closed generic and runs without TypeLoadException`` () =
        // import なしで List Int（型位置）と List.（空構築）が使え、
        // 実行時に閉じた List<int> が構築されることを検証する回帰テスト。
        // 旧実装は開いたジェネリック List`1 で newobj を発行し TypeLoadException で落ちていた。
        let program = """
import System'Console

data Bag = { items: List Int }

fn main: () =
    let b = Bag { items = List. }
    1 b'items'Add.
    2 b'items'Add.
    3 b'items'Add.
    b'items'Count Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "ListBuiltin"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded)

        let dllPath = Path.Join(outDir, "ListBuiltin.dll")
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
        Assert.Equal("3", stdout.Trim())



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
    a[1] Console'WriteLine.
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
    a[0] Console'WriteLine.
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
        a[i] Console'WriteLine.
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
    let ``dependency local copy cache should reuse copied path when source is unchanged`` () =
        let runtimeDir = Path.GetDirectoryName(typeof<string>.Assembly.Location)
        let jsonAssemblyPath = Path.Join(runtimeDir, "System.Text.Json.dll")
        Assert.True(File.Exists(jsonAssemblyPath), $"missing runtime assembly for test: {jsonAssemblyPath}")

        try
            DependencyLoader.clearLocalCopyCache()

            let requestDeps = [ ("System.Text.Json", [ jsonAssemblyPath ]) ]
            let first = DependencyLoader.loadDependenciesWithPolicy DependencyLoader.DependencyLoadPolicy.LocalCopyCache requestDeps
            Assert.True(first.succeeded, String.Join(Environment.NewLine, first.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))
            DependencyLoader.unloadDependencies first.loadContext

            let firstCopyPath =
                DependencyLoader.getLocalCopyCacheEntries()
                |> List.find (fun entry -> String.Equals(entry.sourcePath, Path.GetFullPath(jsonAssemblyPath), StringComparison.OrdinalIgnoreCase))
                |> fun entry -> entry.copyPath

            let second = DependencyLoader.loadDependenciesWithPolicy DependencyLoader.DependencyLoadPolicy.LocalCopyCache requestDeps
            Assert.True(second.succeeded, String.Join(Environment.NewLine, second.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))
            DependencyLoader.unloadDependencies second.loadContext

            let sourceEntries =
                DependencyLoader.getLocalCopyCacheEntries()
                |> List.filter (fun entry -> String.Equals(entry.sourcePath, Path.GetFullPath(jsonAssemblyPath), StringComparison.OrdinalIgnoreCase))
            Assert.Single(sourceEntries) |> ignore
            Assert.Equal(firstCopyPath, sourceEntries.Head.copyPath)
        finally
            DependencyLoader.clearLocalCopyCache()

    [<Fact>]
    let ``dependency local copy cache should refresh copied path when source timestamp changes`` () =
        let runtimeDir = Path.GetDirectoryName(typeof<string>.Assembly.Location)
        let jsonAssemblyPath = Path.Join(runtimeDir, "System.Text.Json.dll")
        Assert.True(File.Exists(jsonAssemblyPath), $"missing runtime assembly for test: {jsonAssemblyPath}")

        let tempDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(tempDir) |> ignore
        let sourcePath = Path.Join(tempDir, "System.Text.Json.dll")
        File.Copy(jsonAssemblyPath, sourcePath, overwrite = true)

        try
            DependencyLoader.clearLocalCopyCache()
            let requestDeps = [ ("System.Text.Json", [ sourcePath ]) ]

            let first = DependencyLoader.loadDependenciesWithPolicy DependencyLoader.DependencyLoadPolicy.LocalCopyCache requestDeps
            Assert.True(first.succeeded, String.Join(Environment.NewLine, first.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))
            DependencyLoader.unloadDependencies first.loadContext

            let firstEntries =
                DependencyLoader.getLocalCopyCacheEntries()
                |> List.filter (fun entry -> String.Equals(entry.sourcePath, Path.GetFullPath(sourcePath), StringComparison.OrdinalIgnoreCase))
            let firstCopyPath = Assert.Single(firstEntries).copyPath

            let bumped = File.GetLastWriteTimeUtc(sourcePath).AddSeconds(2.0)
            File.SetLastWriteTimeUtc(sourcePath, bumped)

            let second = DependencyLoader.loadDependenciesWithPolicy DependencyLoader.DependencyLoadPolicy.LocalCopyCache requestDeps
            Assert.True(second.succeeded, String.Join(Environment.NewLine, second.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))
            DependencyLoader.unloadDependencies second.loadContext

            let secondEntries =
                DependencyLoader.getLocalCopyCacheEntries()
                |> List.filter (fun entry -> String.Equals(entry.sourcePath, Path.GetFullPath(sourcePath), StringComparison.OrdinalIgnoreCase))
            Assert.Equal(2, secondEntries.Length)
            Assert.Contains(secondEntries, fun entry -> entry.copyPath = firstCopyPath)
            Assert.Contains(secondEntries, fun entry -> entry.copyPath <> firstCopyPath)
        finally
            DependencyLoader.clearLocalCopyCache()
            if Directory.Exists(tempDir) then
                Directory.Delete(tempDir, recursive = true)

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
                    [ { moduleName = "main"; source = mainSource; filePath = None }
                      { moduleName = "Foo.Bar"; source = fooBarSource; filePath = None }
                      { moduleName = "Foo"; source = fooSource; filePath = None } ]
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
                    [ { moduleName = "main"; source = mainSource; filePath = None }
                      { moduleName = "Foo"; source = fooSource; filePath = None } ]
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
                    [ { moduleName = "main"; source = mainSource; filePath = None }
                      { moduleName = "sub"; source = subSource; filePath = None } ]
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
                    [ { moduleName = "main"; source = mainSource; filePath = None }
                      { moduleName = "sub"; source = subSource; filePath = None } ]
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
    let ``single-precision Float literal with f suffix compiles and produces correct value`` () =
        // `1.0f` 接尾辞リテラルが単精度 Float（float32）として字句解析・型付け・codegen され、
        // Float 同士の加算が正しく実行されることを検証する。
        let program = """
import System'Console

fn main: () = do
    let a = 1.5f
    let b = 2.0f
    let c = a + b
    c'ToString. Console'WriteLine.
"""

        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "SinglePrecisionLiteral"; source = program.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "SinglePrecisionLiteral.dll")
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
        Assert.Equal("3.5", lines.[0].Trim())

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
    fn area self: Double

data Rectangle = { width: Double, height: Double }

impl Geometry for Rectangle
    fn area self: Double =
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
                    [ { moduleName = "main"; source = mainSource.Trim(); filePath = None }
                      { moduleName = "sub"; source = subSource.Trim(); filePath = None } ]
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

    [<Fact>]
    let ``override fn body calls base'Method as non-virtual call (not callvirt)`` () =
        // 回帰テスト: `impl A as B` の `override fn` から `base'Method` を呼んだとき、
        // CIL が `callvirt` で発行されるとオーバーライド側に再ディスパッチして無限再帰になる。
        // `OpCodes.Call`（非仮想）で発行されていれば親クラス（Exception）の実装が直接実行される。
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let source = """
import System'Console
import System'Exception

data MyError = { code: Int }

impl MyError as Exception
    fn new (c: Int): MyError = MyError { code = c }
    override fn ToString self: String = base'ToString.

fn main: () =
    let e = 42 MyError'new.
    e'ToString. Console'WriteLine.
"""

        let res = compileSingle { asmName = "BaseCallNonVirtual"; source = source.Trim(); outDir = outDir; dependencies = [] }
        Assert.True(res.succeeded, String.concat Environment.NewLine (res.diagnostics |> List.map (fun d -> d.message)))

        let dllPath = Path.Join(outDir, "BaseCallNonVirtual.dll")
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

        // `callvirt` 発行のままだと StackOverflowException でプロセスが落ち、ExitCode は 0 以外になる。
        // `call` 発行であれば Exception.ToString が直接呼ばれ、結果は実行時型名（"MyError"）で始まる文字列。
        Assert.Equal(0, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        Assert.StartsWith("MyError", stdout.Trim())

    // ──────────────────────────────────────────────────────────────
    // async / await (PR-3a: AsyncRewrite で同期化されたエンドツーエンド)
    // ──────────────────────────────────────────────────────────────

    [<Fact>]
    let ``async fn returning Task wraps body and produces Task-returning method`` () =
        let program = """
import System'Threading'Tasks'Task

async fn run (): () = ()
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncTaskRet"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message =
                res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncTaskRet.dll")
        Assert.True(File.Exists dllPath)

        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let runMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "run")

        match runMethod with
        | Some mi ->
            // 戻り値型が System.Threading.Tasks.Task であること。
            Assert.Equal("System.Threading.Tasks.Task", mi.ReturnType.FullName)
            // 実際に呼び出して、null でない完了済み Task が返ることを確認する。
            let result = mi.Invoke(null, [||]) :?> System.Threading.Tasks.Task
            Assert.NotNull(result)
            result.Wait()
            Assert.True(result.IsCompletedSuccessfully)
        | None ->
            Assert.True(false, "'run' method not found in generated assembly")

    [<Fact>]
    let ``await inside async fn lowers via GetAwaiter().GetResult() and runs synchronously`` () =
        // 既に完了している Task.CompletedTask を await した上で、後続代入を実行する。
        // PR-3a の同期実装では `awaiter.GetResult()` でブロックするだけで完了し、後続が走る。
        let program = """
import System'Threading'Tasks'Task

async fn run (t: Task): () = await t
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncAwaitSync"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message =
                res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncAwaitSync.dll")
        Assert.True(File.Exists dllPath)

        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let runMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "run")

        match runMethod with
        | Some mi ->
            Assert.Equal("System.Threading.Tasks.Task", mi.ReturnType.FullName)
            let arg = System.Threading.Tasks.Task.CompletedTask :> obj
            let result = mi.Invoke(null, [| arg |]) :?> System.Threading.Tasks.Task
            Assert.NotNull(result)
            result.Wait()
            Assert.True(result.IsCompletedSuccessfully, "async fn 本体は同期実装で完走するべき")
        | None ->
            Assert.True(false, "'run' method not found in generated assembly")

    // ──────────────────────────────────────────────────────────────
    // async / await (PR-3b-2: 状態機械生成)
    // ──────────────────────────────────────────────────────────────

    [<Fact>]
    let ``async fn generates a state machine type implementing IAsyncStateMachine`` () =
        let program = """
import System'Threading'Tasks'Task

async fn run (t: Task): () = await t
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncSMType"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncSMType.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))

        // IAsyncStateMachine を実装する生成型が存在すること。
        let smTypes =
            loaded.GetTypes()
            |> Array.filter (fun t -> typeof<System.Runtime.CompilerServices.IAsyncStateMachine>.IsAssignableFrom(t))
        Assert.True(smTypes.Length >= 1, "should generate at least one IAsyncStateMachine type")

        let smType = smTypes.[0]
        // MoveNext / SetStateMachine を持つこと。
        let allMethods = smType.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
        Assert.Contains(allMethods, (fun m -> m.Name.EndsWith("MoveNext")))
        Assert.Contains(allMethods, (fun m -> m.Name.EndsWith("SetStateMachine")))
        // builder / state フィールドを持つこと。
        let fields = smType.GetFields(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
        Assert.Contains(fields, (fun f -> f.FieldType = typeof<System.Runtime.CompilerServices.AsyncTaskMethodBuilder>))
        Assert.Contains(fields, (fun f -> f.FieldType = typeof<int> && f.Name.Contains("state")))

    [<Fact>]
    let ``async fn returning Task T awaits and yields the result through the state machine`` () =
        let program = """
import System'Threading'Tasks'Task

async fn runT (t: Task Int): Int = await t
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncTaskOfT"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncTaskOfT.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let runMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "runT")

        match runMethod with
        | Some mi ->
            Assert.True(mi.ReturnType.IsGenericType, "return type should be a closed generic Task<T>")
            Assert.Equal(typedefof<System.Threading.Tasks.Task<_>>, mi.ReturnType.GetGenericTypeDefinition())
            Assert.Equal(typeof<int>, mi.ReturnType.GetGenericArguments().[0])
            let arg = System.Threading.Tasks.Task.FromResult(42) :> obj
            let result = mi.Invoke(null, [| arg |]) :?> System.Threading.Tasks.Task<int>
            result.Wait()
            Assert.Equal(42, result.Result)
        | None ->
            Assert.True(false, "'runT' method not found in generated assembly")

    [<Fact>]
    let ``async fn hoists local binding and argument into state machine fields`` () =
        // `let x = await t` のローカル x と引数 t がフィールドへホイストされ、x が結果として返ること。
        let program = """
import System'Threading'Tasks'Task

async fn compute (t: Task Int): Int =
    let x = await t
    x
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncHoist"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncHoist.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let computeMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "compute")

        match computeMethod with
        | Some mi ->
            let arg = System.Threading.Tasks.Task.FromResult(123) :> obj
            let result = mi.Invoke(null, [| arg |]) :?> System.Threading.Tasks.Task<int>
            result.Wait()
            Assert.Equal(123, result.Result)
        | None ->
            Assert.True(false, "'compute' method not found in generated assembly")

    // ──────────────────────────────────────────────────────────────
    // async / await (PR-3b-3: await 境界での真の中断/再開)
    // ──────────────────────────────────────────────────────────────

    [<Fact>]
    let ``async fn suspends on an incomplete Task and resumes when it completes`` () =
        let program = """
import System'Threading'Tasks'Task

async fn run (t: Task): () = await t
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncSuspend"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncSuspend.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let runMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "run")

        match runMethod with
        | Some mi ->
            // まだ完了していない Task を渡す。await 境界で中断し、未完了の Task が返るはず。
            let tcs = System.Threading.Tasks.TaskCompletionSource()
            let result = mi.Invoke(null, [| tcs.Task :> obj |]) :?> System.Threading.Tasks.Task
            Assert.NotNull(result)
            Assert.False(result.IsCompleted, "incomplete な Task を await したら中断して未完了 Task を返すべき")
            // ソース Task を完了させると継続が走り、返り Task も完了する。
            tcs.SetResult()
            Assert.True(result.Wait(5000), "ソース Task 完了後に継続が走り完了するべき")
            Assert.True(result.IsCompletedSuccessfully)
        | None ->
            Assert.True(false, "'run' method not found in generated assembly")

    [<Fact>]
    let ``async fn Task T suspends and yields the result produced after completion`` () =
        let program = """
import System'Threading'Tasks'Task

async fn compute (t: Task Int): Int =
    let x = await t
    x
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncSuspendT"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncSuspendT.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let computeMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "compute")

        match computeMethod with
        | Some mi ->
            let tcs = System.Threading.Tasks.TaskCompletionSource<int>()
            let result = mi.Invoke(null, [| tcs.Task :> obj |]) :?> System.Threading.Tasks.Task<int>
            Assert.False(result.IsCompleted, "incomplete な Task<int> を await したら中断するべき")
            tcs.SetResult(777)
            Assert.True(result.Wait(5000), "完了後に継続が走り結果が得られるべき")
            Assert.Equal(777, result.Result)
        | None ->
            Assert.True(false, "'compute' method not found in generated assembly")

    [<Fact>]
    let ``async fn MoveNext contains AwaitUnsafeOnCompleted and a state field`` () =
        // 生成された MoveNext IL に AwaitUnsafeOnCompleted 呼び出しが含まれることを確認する
        // （= 真の中断/再開コードが出ている）。
        let program = """
import System'Threading'Tasks'Task

async fn run (t: Task): () = await t
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncMoveNextIL"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncMoveNextIL.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let smType =
            loaded.GetTypes()
            |> Array.tryFind (fun t -> typeof<System.Runtime.CompilerServices.IAsyncStateMachine>.IsAssignableFrom(t))
        match smType with
        | Some t ->
            let moveNext =
                t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
                |> Array.tryFind (fun m -> m.Name.EndsWith("MoveNext"))
            match moveNext with
            | Some mn ->
                let body = mn.GetMethodBody()
                Assert.NotNull(body)
                // MoveNext は awaiter を退避するためのローカル/フィールド参照を含み、
                // builder.AwaitUnsafeOnCompleted を呼び出す。メソッド本体の IL が空でないことを確認する。
                let il = body.GetILAsByteArray()
                Assert.True(il.Length > 0, "MoveNext body should contain IL")
            | None -> Assert.True(false, "MoveNext not found on state machine type")
        | None ->
            Assert.True(false, "no IAsyncStateMachine type generated")

    [<Fact>]
    let ``async fn with two sequential awaits suspends twice and yields the final result`` () =
        let program = """
import System'Threading'Tasks'Task

async fn two (a: Task) (b: Task Int): Int =
    await a
    await b
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncTwoAwaits"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncTwoAwaits.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let twoMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "two")

        match twoMethod with
        | Some mi ->
            let tcsA = System.Threading.Tasks.TaskCompletionSource()
            let tcsB = System.Threading.Tasks.TaskCompletionSource<int>()
            let result = mi.Invoke(null, [| tcsA.Task :> obj; tcsB.Task :> obj |]) :?> System.Threading.Tasks.Task<int>
            Assert.False(result.IsCompleted, "最初の await で中断するべき")
            // 1 つ目を完了 → 2 つ目の await で再び中断（まだ未完了）。
            tcsA.SetResult()
            Assert.False(result.IsCompleted, "2 つ目の await で再度中断するべき")
            // 2 つ目を完了 → 全体が完了し b の結果を返す。
            tcsB.SetResult(55)
            Assert.True(result.Wait(5000), "両 Task 完了後に完了するべき")
            Assert.Equal(55, result.Result)
        | None ->
            Assert.True(false, "'two' method not found in generated assembly")

    // ──────────────────────────────────────────────────────────────
    // async / await: 例外伝播（try/catch + SetException）
    // ──────────────────────────────────────────────────────────────

    [<Fact>]
    let ``async fn awaiting a faulted Task returns a faulted Task instead of throwing synchronously`` () =
        let program = """
import System'Threading'Tasks'Task

async fn run (t: Task): () = await t
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncFaulted"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncFaulted.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let runMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "run")

        match runMethod with
        | Some mi ->
            let ex = System.InvalidOperationException("boom")
            let faulted = System.Threading.Tasks.Task.FromException(ex)
            // 呼び出しは同期的に投げず、faulted な Task を返すべき。
            let result = mi.Invoke(null, [| faulted :> obj |]) :?> System.Threading.Tasks.Task
            Assert.NotNull(result)
            // 完了を待つ（faulted）。例外型・メッセージが伝播していることを確認する。
            let agg = Assert.Throws<AggregateException>(fun () -> result.Wait())
            Assert.True(result.IsFaulted, "await した Task が faulted なら返り Task も faulted になるべき")
            let inner = agg.InnerException
            Assert.IsType<System.InvalidOperationException>(inner) |> ignore
            Assert.Equal("boom", inner.Message)
        | None ->
            Assert.True(false, "'run' method not found in generated assembly")

    [<Fact>]
    let ``async fn Task T awaiting a faulted Task propagates the exception into the result Task`` () =
        let program = """
import System'Threading'Tasks'Task

async fn compute (t: Task Int): Int =
    let x = await t
    x
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncFaultedT"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncFaultedT.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let computeMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "compute")

        match computeMethod with
        | Some mi ->
            let ex = System.ArgumentException("bad arg")
            let faulted = System.Threading.Tasks.Task.FromException<int>(ex)
            let result = mi.Invoke(null, [| faulted :> obj |]) :?> System.Threading.Tasks.Task<int>
            let agg = Assert.Throws<AggregateException>(fun () -> result.Wait())
            Assert.True(result.IsFaulted)
            Assert.IsType<System.ArgumentException>(agg.InnerException) |> ignore
        | None ->
            Assert.True(false, "'compute' method not found in generated assembly")

    [<Fact>]
    let ``async fn awaiting an incomplete Task that later faults yields a faulted Task`` () =
        // 中断後に faulted で完了するケース（fast path ではなく、再開経路での例外伝播）。
        let program = """
import System'Threading'Tasks'Task

async fn run (t: Task): () = await t
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncFaultLater"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncFaultLater.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let runMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "run")

        match runMethod with
        | Some mi ->
            let tcs = System.Threading.Tasks.TaskCompletionSource()
            let result = mi.Invoke(null, [| tcs.Task :> obj |]) :?> System.Threading.Tasks.Task
            Assert.False(result.IsCompleted, "未完了 Task で中断するべき")
            tcs.SetException(System.InvalidOperationException("late boom"))
            let agg = Assert.Throws<AggregateException>(fun () -> result.Wait(5000) |> ignore)
            Assert.True(result.IsFaulted, "再開後に faulted になるべき")
            Assert.IsType<System.InvalidOperationException>(agg.InnerException) |> ignore
        | None ->
            Assert.True(false, "'run' method not found in generated assembly")

    // ──────────────────────────────────────────────────────────────
    // async / await: for ループ内の await
    // ──────────────────────────────────────────────────────────────

    [<Fact>]
    let ``async fn awaits inside a for loop and completes after suspension`` () =
        // ループ本体で await し、未完了 Task で中断 → 完了で再開してループを継続・完了する。
        // ループ変数・イテレータが SM フィールドへホイストされ中断を跨いで生存する必要がある。
        let program = """
import System'Threading'Tasks'Task
import System'Linq'Enumerable

async fn loopAwait (t: Task): () =
    for i in 1 3 Enumerable'Range.
        await t
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncForLoop"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncForLoop.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let loopMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "loopAwait")

        match loopMethod with
        | Some mi ->
            // 完了済み Task: 中断せずループ完走。
            let r1 = mi.Invoke(null, [| System.Threading.Tasks.Task.CompletedTask :> obj |]) :?> System.Threading.Tasks.Task
            Assert.True(r1.Wait(5000))
            Assert.True(r1.IsCompletedSuccessfully)
            // 未完了 Task: 1 回目の反復で中断 → 完了で再開しループ継続。
            let tcs = System.Threading.Tasks.TaskCompletionSource()
            let r2 = mi.Invoke(null, [| tcs.Task :> obj |]) :?> System.Threading.Tasks.Task
            Assert.False(r2.IsCompleted, "ループ内 await が未完了なら中断するべき")
            tcs.SetResult()
            Assert.True(r2.Wait(5000), "完了後に再開してループを完走するべき")
            Assert.True(r2.IsCompletedSuccessfully)
        | None ->
            Assert.True(false, "'loopAwait' method not found in generated assembly")

    // ──────────────────────────────────────────────────────────────
    // async / await: async 関数同士の合成（ラウンドトリップ）
    // ──────────────────────────────────────────────────────────────

    [<Fact>]
    let ``async fn can await another async fn (completed) and yield its result`` () =
        let program = """
import System'Threading'Tasks'Task

async fn inner (): Int = 42

async fn outer (): Int = await (inner.)
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncCompose"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncCompose.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let outerMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "outer")

        match outerMethod with
        | Some mi ->
            let result = mi.Invoke(null, [||]) :?> System.Threading.Tasks.Task<int>
            Assert.True(result.Wait(5000), "outer は inner の完了で完了するべき")
            Assert.Equal(42, result.Result)
        | None ->
            Assert.True(false, "'outer' method not found in generated assembly")

    [<Fact>]
    let ``async fn awaiting another async fn that suspends propagates completion through the chain`` () =
        // outer → inner(t) → await t（未完了）。t 完了で inner→outer が連鎖的に完了する。
        let program = """
import System'Threading'Tasks'Task

async fn inner (t: Task Int): Int = await t

async fn outer (t: Task Int): Int = await (t inner.)
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncComposeChain"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncComposeChain.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let outerMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "outer")

        match outerMethod with
        | Some mi ->
            let tcs = System.Threading.Tasks.TaskCompletionSource<int>()
            let result = mi.Invoke(null, [| tcs.Task :> obj |]) :?> System.Threading.Tasks.Task<int>
            Assert.False(result.IsCompleted, "内側 Task が未完了なら outer も中断しているべき")
            tcs.SetResult(7)
            Assert.True(result.Wait(5000), "ソース Task 完了で連鎖的に完了するべき")
            Assert.Equal(7, result.Result)
        | None ->
            Assert.True(false, "'outer' method not found in generated assembly")

    // ──────────────────────────────────────────────────────────────
    // async / await: 式中ネストした await の spilling
    // ──────────────────────────────────────────────────────────────

    [<Fact>]
    let ``async fn spills a single await nested in a call argument`` () =
        // `id(await t)` のように呼び出し引数に await がネストするケース。
        let program = """
import System'Threading'Tasks'Task

async fn id (x: Int): Int = x

async fn outer (t: Task Int): Int = await ((await t) id.)
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncSpillArg"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncSpillArg.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let outerMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "outer")

        match outerMethod with
        | Some mi ->
            // 完了済みのケース。
            let r1 = mi.Invoke(null, [| System.Threading.Tasks.Task.FromResult(9) :> obj |]) :?> System.Threading.Tasks.Task<int>
            Assert.True(r1.Wait(5000))
            Assert.Equal(9, r1.Result)
            // 中断するケース（ネストした await が未完了 Task）。
            let tcs = System.Threading.Tasks.TaskCompletionSource<int>()
            let r2 = mi.Invoke(null, [| tcs.Task :> obj |]) :?> System.Threading.Tasks.Task<int>
            Assert.False(r2.IsCompleted, "ネストした await が未完了なら中断するべき")
            tcs.SetResult(13)
            Assert.True(r2.Wait(5000))
            Assert.Equal(13, r2.Result)
        | None ->
            Assert.True(false, "'outer' method not found in generated assembly")

    [<Fact>]
    let ``async fn spills two awaits in call arguments preserving left-to-right order`` () =
        // `pick(await a, await b)` で 2 つの await が引数にネスト。pick は第2引数を返す。
        // 評価順（a を先に await）が保たれ、b の値が返ることを確認する。
        let program = """
import System'Threading'Tasks'Task

async fn pick (x: Int) (y: Int): Int = y

async fn outer (a: Task Int) (b: Task Int): Int = await ((await a) (await b) pick.)
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncSpillTwo"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncSpillTwo.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let outerMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "outer")

        match outerMethod with
        | Some mi ->
            let tcsA = System.Threading.Tasks.TaskCompletionSource<int>()
            let tcsB = System.Threading.Tasks.TaskCompletionSource<int>()
            let result = mi.Invoke(null, [| tcsA.Task :> obj; tcsB.Task :> obj |]) :?> System.Threading.Tasks.Task<int>
            Assert.False(result.IsCompleted, "最初の await (a) で中断するべき")
            tcsA.SetResult(100)
            Assert.False(result.IsCompleted, "2 つ目の await (b) で再度中断するべき")
            tcsB.SetResult(200)
            Assert.True(result.Wait(5000))
            // pick は第2引数 (b の値) を返す。
            Assert.Equal(200, result.Result)
        | None ->
            Assert.True(false, "'outer' method not found in generated assembly")

    // ──────────────────────────────────────────────────────────────
    // async / await: 条件分岐内の await
    // ──────────────────────────────────────────────────────────────

    [<Fact>]
    let ``async fn awaits inside conditional branches`` () =
        // 各分岐で await する条件式。spilling により分岐内 await が文レベルへ降格される。
        let program = """
import System'Threading'Tasks'Task

async fn choose (c: Bool) (a: Task Int) (b: Task Int): Int =
    |? c => await a
    |: else => await b
"""
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = "AsyncIfBranch"; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, "AsyncIfBranch.dll")
        let loaded = Assembly.LoadFile(Path.GetFullPath(dllPath))
        let chooseMethod =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.tryFind (fun m -> m.Name = "choose")

        match chooseMethod with
        | Some mi ->
            // c=true → a を await。a を未完了で渡し中断→完了で再開。b は完了済みで未使用。
            let tcsA = System.Threading.Tasks.TaskCompletionSource<int>()
            let rTrue = mi.Invoke(null, [| true :> obj; tcsA.Task :> obj; System.Threading.Tasks.Task.FromResult(0) :> obj |]) :?> System.Threading.Tasks.Task<int>
            Assert.False(rTrue.IsCompleted, "選択された分岐 (a) が未完了なら中断するべき")
            tcsA.SetResult(11)
            Assert.True(rTrue.Wait(5000))
            Assert.Equal(11, rTrue.Result)
            // c=false → b を await。
            let tcsB = System.Threading.Tasks.TaskCompletionSource<int>()
            let rFalse = mi.Invoke(null, [| false :> obj; System.Threading.Tasks.Task.FromResult(0) :> obj; tcsB.Task :> obj |]) :?> System.Threading.Tasks.Task<int>
            Assert.False(rFalse.IsCompleted)
            tcsB.SetResult(22)
            Assert.True(rFalse.Wait(5000))
            Assert.Equal(22, rFalse.Result)
        | None ->
            Assert.True(false, "'choose' method not found in generated assembly")

    // ──────────────────────────────────────────────────────────────
    // 比較演算子 < > <= >= と && / || の短絡評価
    // ──────────────────────────────────────────────────────────────

    let private runForStdout (asmName: string) (program: string) : string =
        let outDir = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(outDir) |> ignore

        let res = compileSingle { asmName = asmName; source = program.Trim(); outDir = outDir; dependencies = [] }
        if not res.succeeded then
            let message = res.diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "\n"
            Assert.True(false, $"compile failed: {message}")

        let dllPath = Path.Join(outDir, asmName + ".dll")
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
        proc.StandardInput.Close()
        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()
        Assert.Equal(0, proc.ExitCode)
        Assert.True(String.IsNullOrWhiteSpace stderr, stderr)
        stdout.Trim()

    [<Fact>]
    let ``comparison operators on Int produce correct Bool results`` () =
        let program = """
import System'Console

fn check (a: Int) (b: Int): () = do
    |? a < b => "lt" Console'Write.
    |? a > b => "gt" Console'Write.
    |? a <= b => "le" Console'Write.
    |? a >= b => "ge" Console'Write.

fn main: () = do
    3 5 check.
    5 3 check.
    4 4 check.
"""
        // 3<5: lt,le / 5>3: gt,ge / 4==4: le,ge
        Assert.Equal("ltlegtgelege", runForStdout "CmpInt" program)

    [<Fact>]
    let ``comparison operators on Float produce correct Bool results`` () =
        let program = """
import System'Console

fn main: () = do
    |? 0.5f < 1.0f => "a" Console'Write.
    |? 2.0f > 1.0f => "b" Console'Write.
    |? 1.0f <= 1.0f => "c" Console'Write.
    |? 1.0f >= 2.0f => "d" Console'Write.
    "z" Console'Write.
"""
        // a,b,c が真、d は偽
        Assert.Equal("abcz", runForStdout "CmpFloat" program)

    [<Fact>]
    let ``logical && short-circuits the right operand when left is false`` () =
        let program = """
import System'Console

fn rhs: Bool = do
    "R" Console'Write.
    True

fn main: () = do
    |? False && rhs. => "A" Console'Write.
    |? True && rhs. => "B" Console'Write.
    "z" Console'Write.
"""
        // 1つ目: False && → rhs 評価されない（"R" なし）、条件 false（"A" なし）
        // 2つ目: True && rhs. → rhs 評価され "R"、結果 true → "B"
        Assert.Equal("RBz", runForStdout "ShortCircuitAnd" program)

    [<Fact>]
    let ``logical || short-circuits the right operand when left is true`` () =
        let program = """
import System'Console

fn rhs: Bool = do
    "R" Console'Write.
    True

fn main: () = do
    |? True || rhs. => "A" Console'Write.
    |? False || rhs. => "B" Console'Write.
    "z" Console'Write.
"""
        // 1つ目: True || → rhs 評価されない（"R" なし）、条件 true → "A"
        // 2つ目: False || rhs. → rhs 評価され "R"、結果 true → "B"
        Assert.Equal("ARBz", runForStdout "ShortCircuitOr" program)

    // ──────────────────────────────────────────────────────────────
    // 添字アクセス [i]・添字代入 [i] = x・ジェネリック <T>
    // ──────────────────────────────────────────────────────────────

    [<Fact>]
    let ``index access with bracket syntax reads list element`` () =
        let program = """
import System'Console

fn main: () = do
    let xs: List Int = List.
    10 xs'Add.
    20 xs'Add.
    30 xs'Add.
    xs[1] Console'Write.
"""
        Assert.Equal("20", runForStdout "IndexRead" program)

    [<Fact>]
    let ``index assignment writes list element`` () =
        let program = """
import System'Console

fn main: () = do
    let xs: List Int = List.
    10 xs'Add.
    20 xs'Add.
    xs[0] = 99
    xs[0] Console'Write.
    xs[1] Console'Write.
"""
        // 添字代入後に読み出して検証: xs[0] は 99 に更新、xs[1] は 20 のまま。
        Assert.Equal("9920", runForStdout "IndexAssign" program)

    [<Fact>]
    let ``generic call with angle-bracket syntax`` () =
        let program = """
import System'Activator
import System'Console

fn main: () = do
    let x = Activator'CreateInstance<Int>.
    x Console'Write.
"""
        // CreateInstance<Int> は int の既定値 0 を返す。
        Assert.Equal("0", runForStdout "GenericAngle" program)

    [<Fact>]
    let ``comparison operators are not misparsed as generics`` () =
        let program = """
import System'Console

fn cmp (a: Int) (b: Int): () = do
    |? a < b => "L" Console'Write.
    |? a > b => "G" Console'Write.

fn main: () = do
    3 5 cmp.
    5 3 cmp.
    |? 1 < 2 && 2 > 1 => "B" Console'Write.
"""
        // cmp(3,5)→"L"、cmp(5,3)→"G"、`1<2 && 2>1`→"B"。`<`/`>` がジェネリックに誤認されないことの回帰確認。
        Assert.Equal("LGB", runForStdout "CmpNotGeneric" program)
