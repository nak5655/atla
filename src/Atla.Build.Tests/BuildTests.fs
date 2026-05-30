namespace Atla.Build.Tests

open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Reflection
open System.Text.Json
open Xunit
open Atla.Build
open Atla.Compiler
open Atla.Core.Semantics.Data

module BuildTests =
    let private createTempProjectDir () =
        let path = Path.Join(Path.GetTempPath(), $"atla-build-tests-{Guid.NewGuid():N}")
        Directory.CreateDirectory(path) |> ignore
        path

    let private writeManifest (projectRoot: string) (content: string) =
        File.WriteAllText(Path.Join(projectRoot, "atla.yaml"), content.Trim())

    let private writeReferenceDll (projectRoot: string) (assemblyFileName: string) =
        let tfmDir = Path.Join(projectRoot, "ref", "net8.0")
        Directory.CreateDirectory(tfmDir) |> ignore
        File.WriteAllText(Path.Join(tfmDir, assemblyFileName), "")

    /// YAML の文字列として解釈可能なように、Windows 区切り文字を POSIX 形式へ正規化する。
    let private toYamlPath (path: string) =
        path.Replace("\\", "/")

    /// 生成 DLL を `dotnet` で実行し、標準出力を返す。
    let private runDll (dllPath: string) =
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
        proc.ExitCode, stdout.Trim(), stderr.Trim()

    [<Fact>]
    let ``createEmptyPlan keeps projectRoot and no dependencies`` () =
        let request = { BuildRequest.projectRoot = "/tmp/hello" }
        let plan = BuildSystem.createEmptyPlan request

        Assert.Equal("/tmp/hello", plan.projectRoot)
        Assert.Equal<string>("", plan.projectName)
        Assert.Equal<string>("", plan.projectVersion)
        Assert.Equal(BuildPackageType.Exe, plan.packageType)
        Assert.Empty(plan.dependencies)

    [<Fact>]
    let ``buildProject should parse minimal atla.yaml`` () =
        let projectRoot = createTempProjectDir ()

        writeManifest projectRoot """
package:
  name: "hello"
  version: "0.1.0"
"""

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.True(result.succeeded)
        Assert.Empty(result.diagnostics)

        match result.plan with
        | Some plan ->
            Assert.Equal("hello", plan.projectName)
            Assert.Equal("0.1.0", plan.projectVersion)
            Assert.Equal(Path.GetFullPath(projectRoot), plan.projectRoot)
            Assert.Empty(plan.dependencies)
        | None ->
            Assert.Fail("expected build plan")

    [<Fact>]
    let ``buildProject should parse package type`` () =
        let projectRoot = createTempProjectDir ()

        writeManifest projectRoot """
package:
  name: "hello"
  version: "0.1.0"
  type: "lib"
"""

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.True(result.succeeded)
        Assert.Empty(result.diagnostics)
        Assert.Equal(BuildPackageType.Lib, result.plan.Value.packageType)

    [<Fact>]
    let ``buildProject should parse dll package type`` () =
        let projectRoot = createTempProjectDir ()

        writeManifest projectRoot """
package:
  name: "hello"
  version: "0.1.0"
  type: "dll"
"""

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.True(result.succeeded)
        Assert.Empty(result.diagnostics)
        Assert.Equal(BuildPackageType.Dll, result.plan.Value.packageType)

    [<Fact>]
    let ``buildProject should fail for unsupported package type`` () =
        let projectRoot = createTempProjectDir ()

        writeManifest projectRoot """
package:
  name: "hello"
  version: "0.1.0"
  type: "tool"
"""

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.False(result.succeeded)
        Assert.Contains(result.diagnostics, fun d -> d.message.Contains("package.type"))

    [<Fact>]
    let ``createAtlaLib should generate required container entries`` () =
        let compileOutDir = createTempProjectDir ()
        let outDir = createTempProjectDir ()

        let compileResult =
            Compiler.compileModules {
                asmName = "HelloLibAsm"
                modules = [ { Compiler.ModuleSource.moduleName = "main"; source = "fn main: () = ()"; filePath = None } ]
                entryModuleName = "main"
                outDir = compileOutDir
                dependencies = []
            }

        Assert.True(compileResult.succeeded)

        let packageResult =
            BuildSystem.createAtlaLib "hello" "0.1.0" "HelloLibAsm" compileOutDir outDir [] compileResult

        match packageResult with
        | Result.Error diagnostics ->
            Assert.Fail(String.concat Environment.NewLine (diagnostics |> List.map (fun diagnostic -> diagnostic.message)))
        | Ok atlaLibPath ->
            Assert.True(File.Exists(atlaLibPath))

            use archive = ZipFile.OpenRead(atlaLibPath)
            let entryNames = archive.Entries |> Seq.map (fun entry -> entry.FullName) |> Set.ofSeq
            Assert.Contains("atlalib.json", entryNames)
            Assert.Contains("assemblies/HelloLibAsm.dll", entryNames)
            Assert.Contains("symbols/public.api.json", entryNames)
            Assert.Contains("deps/manifest.lock.json", entryNames)
            Assert.Contains("hashes/sha256sums.txt", entryNames)

            let atlaLibMetadataEntry = archive.GetEntry("atlalib.json")
            Assert.False(isNull atlaLibMetadataEntry)
            use atlaLibMetadataStream = atlaLibMetadataEntry.Open()
            use atlaLibMetadataJson = JsonDocument.Parse(atlaLibMetadataStream)
            let metadataRoot = atlaLibMetadataJson.RootElement
            Assert.Equal("1.0", metadataRoot.GetProperty("formatVersion").GetString())
            Assert.Equal("hello", metadataRoot.GetProperty("package").GetProperty("name").GetString())
            Assert.Equal("0.1.0", metadataRoot.GetProperty("package").GetProperty("version").GetString())
            Assert.Equal("assemblies/HelloLibAsm.dll", metadataRoot.GetProperty("artifacts").GetProperty("assembly").GetString())
            Assert.Equal("symbols/public.api.json", metadataRoot.GetProperty("artifacts").GetProperty("publicApi").GetString())
            Assert.Equal("deps/manifest.lock.json", metadataRoot.GetProperty("artifacts").GetProperty("dependencyLock").GetString())
            Assert.Equal("1.0", metadataRoot.GetProperty("compat").GetProperty("symbolSchemaVersion").GetString())

            let publicApiEntry = archive.GetEntry("symbols/public.api.json")
            Assert.False(isNull publicApiEntry)
            use publicApiStream = publicApiEntry.Open()
            use publicApiJson = JsonDocument.Parse(publicApiStream)
            let publicApiRoot = publicApiJson.RootElement
            Assert.Equal("1.0", publicApiRoot.GetProperty("schemaVersion").GetString())
            let mutable modulesElement = Unchecked.defaultof<JsonElement>
            Assert.True(publicApiRoot.TryGetProperty("modules", &modulesElement))
            let mutable valuesElement = Unchecked.defaultof<JsonElement>
            Assert.True(modulesElement.[0].GetProperty("exports").TryGetProperty("values", &valuesElement))

    [<Fact>]
    let ``buildProject should resolve direct dependencies`` () =
        let rootProject = createTempProjectDir ()
        let depProject = createTempProjectDir ()

        writeManifest depProject """
package:
  name: "dep"
  version: "1.2.3"
"""
        writeReferenceDll depProject "dep.dll"

        let relativePath = Path.GetRelativePath(rootProject, depProject) |> toYamlPath

        writeManifest rootProject $"""
package:
  name: "app"
  version: "0.1.0"
dependencies:
  dep:
    path: "{relativePath}"
"""

        let result = BuildSystem.buildProject { projectRoot = rootProject }

        Assert.True(result.succeeded)

        match result.plan with
        | Some plan ->
            let dependency = Assert.Single(plan.dependencies)
            Assert.Equal("dep", dependency.name)
            Assert.Equal("1.2.3", dependency.version)
            Assert.Equal(Path.GetFullPath(depProject), dependency.source)
        | None ->
            Assert.Fail("expected build plan")

    [<Fact>]
    let ``buildProject should resolve path dependency pointing to atlalib`` () =
        let libCompileOutDir = createTempProjectDir ()
        let libOutDir = createTempProjectDir ()
        let rootProject = createTempProjectDir ()

        let compileResult =
            Compiler.compileModules {
                asmName = "PeopleLibAsm"
                modules = [ { Compiler.ModuleSource.moduleName = "people"; source = "struct Person\n    val name: String\nfn greet (p: Person): String = p'name"; filePath = None } ]
                entryModuleName = "people"
                outDir = libCompileOutDir
                dependencies = []
            }

        Assert.True(compileResult.succeeded)

        let atlaLibPath =
            match BuildSystem.createAtlaLib "peoplelib" "0.1.0" "PeopleLibAsm" libCompileOutDir libOutDir [] compileResult with
            | Ok path -> path
            | Result.Error diagnostics ->
                Assert.Fail(String.concat Environment.NewLine (diagnostics |> List.map (fun diagnostic -> diagnostic.message)))
                ""

        writeManifest rootProject $"""
package:
  name: "app"
  version: "0.1.0"
dependencies:
  people:
    path: "{toYamlPath (Path.GetRelativePath(rootProject, atlaLibPath))}"
"""

        let result = BuildSystem.buildProject { projectRoot = rootProject }
        Assert.True(result.succeeded, String.concat Environment.NewLine (result.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))

        match result.plan with
        | Some plan ->
            let dependency = Assert.Single(plan.dependencies)
            Assert.Equal(Path.GetFullPath(atlaLibPath), dependency.source)
            Assert.Empty(dependency.compileReferencePaths)
            Assert.True(dependency.runtimeLoadPaths |> List.exists File.Exists)
        | None ->
            Assert.Fail("expected build plan")

    [<Fact>]
    let ``compileModules should import data and methods from atlalib dependency`` () =
        let libCompileOutDir = createTempProjectDir ()
        let libOutDir = createTempProjectDir ()
        let appOutDir = createTempProjectDir ()

        let librarySource = """
struct Person
    val name: String
impl Person
    fn greet self: String = self'name
"""

        let libraryResult =
            Compiler.compileModules {
                asmName = "PeopleLibAsm"
                modules = [ { Compiler.ModuleSource.moduleName = "people"; source = librarySource.Trim(); filePath = None } ]
                entryModuleName = "people"
                outDir = libCompileOutDir
                dependencies = []
            }

        Assert.True(libraryResult.succeeded, String.concat Environment.NewLine (libraryResult.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))

        let atlaLibPath =
            match BuildSystem.createAtlaLib "peoplelib" "0.1.0" "PeopleLibAsm" libCompileOutDir libOutDir [] libraryResult with
            | Ok path -> path
            | Result.Error diagnostics ->
                Assert.Fail(String.concat Environment.NewLine (diagnostics |> List.map (fun diagnostic -> diagnostic.message)))
                ""

        let dependency =
            match AtlaLib.resolveRuntimeAssets atlaLibPath with
            | Ok runtimeAssets ->
                let resolvedDependency: Compiler.ResolvedDependency =
                    { name = runtimeAssets.packageName
                      version = runtimeAssets.packageVersion
                      source = atlaLibPath
                      compileReferencePaths = []
                      runtimeLoadPaths = runtimeAssets.runtimeLoadPaths
                      nativeRuntimePaths = runtimeAssets.nativeRuntimePaths }
                resolvedDependency
            | Result.Error diagnostics ->
                Assert.Fail(String.concat Environment.NewLine (diagnostics |> List.map (fun diagnostic -> diagnostic.message)))
                Unchecked.defaultof<Compiler.ResolvedDependency>

        let appSource = """
import System'Console
import people'Person

fn main: () = do
    let p = { name = "alice" } Person.
    let message = p'greet.
    message Console'WriteLine.
"""

        let appResult =
            Compiler.compileModules {
                asmName = "PeopleApp"
                modules = [ { Compiler.ModuleSource.moduleName = "main"; source = appSource.Trim(); filePath = None } ]
                entryModuleName = "main"
                outDir = appOutDir
                dependencies = [ dependency ]
            }

        Assert.True(appResult.succeeded, String.concat Environment.NewLine (appResult.diagnostics |> List.map (fun diagnostic -> diagnostic.message)))

        match BuildSystem.copyDependencies [ dependency ] appOutDir with
        | Result.Error diagnostics ->
            Assert.Fail(String.concat Environment.NewLine (diagnostics |> List.map (fun diagnostic -> diagnostic.message)))
        | Ok _ -> ()

        let exitCode, stdout, stderr = runDll (Path.Join(appOutDir, "PeopleApp.dll"))
        Assert.Equal(0, exitCode)
        Assert.True(String.IsNullOrWhiteSpace(stderr), stderr)
        Assert.Equal("alice", stdout)

    [<Fact>]
    let ``compileModules should import generic enum from atlalib and pattern match on it`` () =
        // ジェネリック enum を atlalib にコンパイルし、import してインスタンス化した変数を
        // match 式でパターンマッチできることを確認する結合テスト。
        let libCompileOutDir = createTempProjectDir ()
        let libOutDir = createTempProjectDir ()
        let appOutDir = createTempProjectDir ()

        // ライブラリ: ジェネリック enum Opt T を定義する。
        let librarySource = """
enum Opt T
    | None
    | Some { value: T }
"""

        let libraryResult =
            Compiler.compileModules {
                asmName = "OptLibAsm"
                modules = [ { Compiler.ModuleSource.moduleName = "opt"; source = librarySource.Trim(); filePath = None } ]
                entryModuleName = "opt"
                outDir = libCompileOutDir
                dependencies = []
            }

        Assert.True(libraryResult.succeeded, String.concat Environment.NewLine (libraryResult.diagnostics |> List.map (fun d -> d.message)))

        let atlaLibPath =
            match BuildSystem.createAtlaLib "optlib" "0.1.0" "OptLibAsm" libCompileOutDir libOutDir [] libraryResult with
            | Ok path -> path
            | Result.Error diagnostics ->
                Assert.Fail(String.concat Environment.NewLine (diagnostics |> List.map (fun d -> d.message)))
                ""

        let dependency =
            match AtlaLib.resolveRuntimeAssets atlaLibPath with
            | Ok runtimeAssets ->
                let resolvedDependency: Compiler.ResolvedDependency =
                    { name = runtimeAssets.packageName
                      version = runtimeAssets.packageVersion
                      source = atlaLibPath
                      compileReferencePaths = []
                      runtimeLoadPaths = runtimeAssets.runtimeLoadPaths
                      nativeRuntimePaths = runtimeAssets.nativeRuntimePaths }
                resolvedDependency
            | Result.Error diagnostics ->
                Assert.Fail(String.concat Environment.NewLine (diagnostics |> List.map (fun d -> d.message)))
                Unchecked.defaultof<Compiler.ResolvedDependency>

        // アプリ: import した Opt T をインスタンス化して match 式でパターンマッチする。
        // Some { value } ブランチは値をそのまま返し、None ブランチは -1 を返す。
        let appSource = """
import System'Console
import opt'Opt

fn unwrapOr (o: Opt Int): Int =
    match o
    | Opt'None -> -1
    | Opt'Some { value } -> value

fn main: () = do
    let a = Opt'Some { value = 99 }
    let b = Opt'None
    let ra = a unwrapOr.
    let rb = b unwrapOr.
    ra Console'WriteLine.
    rb Console'WriteLine.
"""

        let appResult =
            Compiler.compileModules {
                asmName = "OptApp"
                modules = [ { Compiler.ModuleSource.moduleName = "main"; source = appSource.Trim(); filePath = None } ]
                entryModuleName = "main"
                outDir = appOutDir
                dependencies = [ dependency ]
            }

        Assert.True(appResult.succeeded, String.concat Environment.NewLine (appResult.diagnostics |> List.map (fun d -> d.message)))

        match BuildSystem.copyDependencies [ dependency ] appOutDir with
        | Result.Error diagnostics ->
            Assert.Fail(String.concat Environment.NewLine (diagnostics |> List.map (fun d -> d.message)))
        | Ok _ -> ()

        let exitCode, stdout, stderr = runDll (Path.Join(appOutDir, "OptApp.dll"))
        Assert.Equal(0, exitCode)
        Assert.True(String.IsNullOrWhiteSpace(stderr), stderr)
        Assert.Equal("99\n-1", stdout.Trim().Replace("\r\n", "\n"))

    [<Fact>]
    let ``buildProject should fail when atla.yaml is missing`` () =
        let projectRoot = createTempProjectDir ()

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("atla.yaml not found")))
        Assert.True(result.plan.IsNone)

    [<Fact>]
    let ``buildProject should fail for syntax error`` () =
        let projectRoot = createTempProjectDir ()
        writeManifest projectRoot "package: ["

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("parse error")))
        Assert.True(result.plan.IsNone)

    [<Fact>]
    let ``buildProject should fail when required fields are missing`` () =
        let projectRoot = createTempProjectDir ()
        writeManifest projectRoot "package: {}"

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("package.name")))
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("package.version")))
        Assert.True(result.plan.IsNone)

    [<Fact>]
    let ``buildProject should fail when dependency path is missing`` () =
        let rootProject = createTempProjectDir ()

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  missing:
    path: "./deps/missing"
"""

        let result = BuildSystem.buildProject { projectRoot = rootProject }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("dependency path not found")))

    [<Fact>]
    let ``buildProject should fail when dependency graph has cycle`` () =
        let projectA = createTempProjectDir ()
        let projectB = createTempProjectDir ()

        let relativeAToB = Path.GetRelativePath(projectA, projectB) |> toYamlPath
        let relativeBToA = Path.GetRelativePath(projectB, projectA) |> toYamlPath

        writeManifest projectA $"""
package:
  name: "a"
  version: "0.1.0"
dependencies:
  b:
    path: "{relativeAToB}"
"""
        writeReferenceDll projectA "a.dll"

        writeManifest projectB $"""
package:
  name: "b"
  version: "0.1.0"
dependencies:
  a:
    path: "{relativeBToA}"
"""
        writeReferenceDll projectB "b.dll"

        let result = BuildSystem.buildProject { projectRoot = projectA }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("cyclic dependency")))

    [<Fact>]
    let ``buildProject should fail when duplicate package names resolve`` () =
        let rootProject = createTempProjectDir ()
        let depProjectA = createTempProjectDir ()
        let depProjectB = createTempProjectDir ()

        writeManifest depProjectA """
package:
  name: "common"
  version: "1.0.0"
"""
        writeReferenceDll depProjectA "common.dll"

        writeManifest depProjectB """
package:
  name: "common"
  version: "2.0.0"
"""
        writeReferenceDll depProjectB "common.dll"

        let relativeA = Path.GetRelativePath(rootProject, depProjectA) |> toYamlPath
        let relativeB = Path.GetRelativePath(rootProject, depProjectB) |> toYamlPath

        writeManifest rootProject $"""
package:
  name: "app"
  version: "0.1.0"
dependencies:
  commonA:
    path: "{relativeA}"
  commonB:
    path: "{relativeB}"
"""

        let result = BuildSystem.buildProject { projectRoot = rootProject }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("dependency version conflict `common`")))

    (* copyDependencies のテストで使う補助: runtime DLL のみ設定した ResolvedDependency を生成する。 *)
    let private makeRuntimeDep (name: string) (runtimePaths: string list) : Atla.Compiler.Compiler.ResolvedDependency =
        { name = name
          version = "1.0.0"
          source = ""
          compileReferencePaths = []
          runtimeLoadPaths = runtimePaths
          nativeRuntimePaths = [] }

    [<Fact>]
    let ``writeDepsFile should create deps json with app runtime entry`` () =
        let outDir = createTempProjectDir ()
        let result = BuildSystem.writeDepsFile "hello" "0.1.0" "HelloAsm" [] outDir

        match result with
        | Result.Error _ -> Assert.Fail("expected Ok")
        | Ok depsPath ->
            Assert.True(File.Exists(depsPath))
            use json = JsonDocument.Parse(File.ReadAllText(depsPath))
            let root = json.RootElement
            Assert.Equal(".NETCoreApp,Version=v10.0", root.GetProperty("runtimeTarget").GetProperty("name").GetString())
            let targets = root.GetProperty("targets").GetProperty(".NETCoreApp,Version=v10.0")
            let appTarget = targets.GetProperty("hello/0.1.0")
            let mutable runtimeAsset = Unchecked.defaultof<JsonElement>
            Assert.True(appTarget.GetProperty("runtime").TryGetProperty("HelloAsm.dll", &runtimeAsset))

    [<Fact>]
    let ``writeDepsFile should include dependency runtime and native entries`` () =
        let pkgRoot = createTempProjectDir ()
        let outDir = createTempProjectDir ()
        let managedPath = Path.Join(pkgRoot, "lib", "net8.0", "Stride.Engine.dll")
        let nativePath = Path.Join(pkgRoot, "runtimes", "linux-x64", "native", "libSDL2.so")
        Directory.CreateDirectory(Path.GetDirectoryName(managedPath)) |> ignore
        Directory.CreateDirectory(Path.GetDirectoryName(nativePath)) |> ignore
        File.WriteAllText(managedPath, "managed")
        File.WriteAllText(nativePath, "native")

        let dependency : Atla.Compiler.Compiler.ResolvedDependency =
            { name = "Stride.Engine"
              version = "4.3.0.2507"
              source = pkgRoot
              compileReferencePaths = []
              runtimeLoadPaths = [ managedPath ]
              nativeRuntimePaths = [ nativePath ] }

        let result = BuildSystem.writeDepsFile "game_hello" "0.1.0" "GameHello" [ dependency ] outDir

        match result with
        | Result.Error _ -> Assert.Fail("expected Ok")
        | Ok depsPath ->
            use json = JsonDocument.Parse(File.ReadAllText(depsPath))
            let target = json.RootElement.GetProperty("targets").GetProperty(".NETCoreApp,Version=v10.0")
            let depTarget = target.GetProperty("Stride.Engine/4.3.0.2507")
            let mutable managedAsset = Unchecked.defaultof<JsonElement>
            let mutable nativeAsset = Unchecked.defaultof<JsonElement>
            Assert.True(depTarget.GetProperty("runtime").TryGetProperty("Stride.Engine.dll", &managedAsset))
            Assert.True(depTarget.GetProperty("native").TryGetProperty("runtimes/linux-x64/native/libSDL2.so", &nativeAsset))

    [<Fact>]
    let ``copyDependencies should return empty list when dependencies are empty`` () =
        let outDir = createTempProjectDir ()

        let result = BuildSystem.copyDependencies [] outDir

        match result with
        | Ok copied -> Assert.Empty(copied)
        | Result.Error _ -> Assert.Fail("expected Ok")

    [<Fact>]
    let ``copyDependencies should copy DLL when destination does not exist`` () =
        let srcDir = createTempProjectDir ()
        let outDir = createTempProjectDir ()
        let srcPath = Path.Join(srcDir, "dep.dll")
        File.WriteAllText(srcPath, "fake-dll")

        let dep = makeRuntimeDep "dep" [ srcPath ]
        let result = BuildSystem.copyDependencies [ dep ] outDir

        match result with
        | Ok copied ->
            let dstPath = Path.Join(outDir, "dep.dll")
            Assert.Single(copied) |> ignore
            Assert.True(File.Exists(dstPath))
        | Result.Error _ -> Assert.Fail("expected Ok")

    [<Fact>]
    let ``copyDependencies should skip DLL when destination is up-to-date`` () =
        let srcDir = createTempProjectDir ()
        let outDir = createTempProjectDir ()
        let srcPath = Path.Join(srcDir, "dep.dll")
        let dstPath = Path.Join(outDir, "dep.dll")
        let oldTime = DateTime.UtcNow.AddSeconds(-10.0)
        let newTime = DateTime.UtcNow

        (* src を古い時刻、dst を新しい時刻に設定してスキップを確認する。 *)
        File.WriteAllText(srcPath, "fake-dll-old")
        File.SetLastWriteTimeUtc(srcPath, oldTime)
        File.WriteAllText(dstPath, "fake-dll-new")
        File.SetLastWriteTimeUtc(dstPath, newTime)

        let dep = makeRuntimeDep "dep" [ srcPath ]
        let result = BuildSystem.copyDependencies [ dep ] outDir

        match result with
        | Ok copied ->
            Assert.Empty(copied)
            Assert.Equal("fake-dll-new", File.ReadAllText(dstPath))
        | Result.Error _ -> Assert.Fail("expected Ok")

    [<Fact>]
    let ``copyDependencies should copy DLL when source is newer than destination`` () =
        let srcDir = createTempProjectDir ()
        let outDir = createTempProjectDir ()
        let srcPath = Path.Join(srcDir, "dep.dll")
        let dstPath = Path.Join(outDir, "dep.dll")
        let oldTime = DateTime.UtcNow.AddSeconds(-10.0)
        let newTime = DateTime.UtcNow

        (* dst を古い時刻、src を新しい時刻に設定してコピーが実行されることを確認する。 *)
        File.WriteAllText(dstPath, "fake-dll-old")
        File.SetLastWriteTimeUtc(dstPath, oldTime)
        File.WriteAllText(srcPath, "fake-dll-new")
        File.SetLastWriteTimeUtc(srcPath, newTime)

        let dep = makeRuntimeDep "dep" [ srcPath ]
        let result = BuildSystem.copyDependencies [ dep ] outDir

        match result with
        | Ok copied ->
            Assert.Single(copied) |> ignore
            Assert.Equal("fake-dll-new", File.ReadAllText(dstPath))
        | Result.Error _ -> Assert.Fail("expected Ok")

    [<Fact>]
    let ``copyDependencies should return error when source file does not exist`` () =
        let outDir = createTempProjectDir ()
        let missingPath = Path.Join(outDir, "nonexistent.dll")

        let dep = makeRuntimeDep "dep" [ missingPath ]
        let result = BuildSystem.copyDependencies [ dep ] outDir

        match result with
        | Ok _ -> Assert.Fail("expected error")
        | Result.Error diagnostics -> Assert.NotEmpty(diagnostics)

    (* copyDependencies の native runtime DLL コピーテスト用補助: nativeRuntimePaths のみ設定した ResolvedDependency を生成する。 *)
    let private makeNativeDep (name: string) (nativePaths: string list) : Atla.Compiler.Compiler.ResolvedDependency =
        { name = name
          version = "1.0.0"
          source = ""
          compileReferencePaths = []
          runtimeLoadPaths = []
          nativeRuntimePaths = nativePaths }

    [<Fact>]
    let ``copyDependencies should copy native runtime file when destination does not exist`` () =
        let srcDir = createTempProjectDir ()
        let outDir = createTempProjectDir ()
        let srcPath = Path.Join(srcDir, "native.dll")
        File.WriteAllText(srcPath, "fake-native-dll")

        let dep = makeNativeDep "dep" [ srcPath ]
        let result = BuildSystem.copyDependencies [ dep ] outDir

        match result with
        | Ok copied ->
            let dstPath = Path.Join(outDir, "native.dll")
            Assert.Single(copied) |> ignore
            Assert.True(File.Exists(dstPath))
            Assert.Equal("fake-native-dll", File.ReadAllText(dstPath))
        | Result.Error _ -> Assert.Fail("expected Ok")

    [<Fact>]
    let ``copyDependencies should copy both runtime and native runtime files`` () =
        let srcDir = createTempProjectDir ()
        let outDir = createTempProjectDir ()
        let managedPath = Path.Join(srcDir, "managed.dll")
        let nativePath = Path.Join(srcDir, "native.dll")
        File.WriteAllText(managedPath, "fake-managed")
        File.WriteAllText(nativePath, "fake-native")

        let dep : Atla.Compiler.Compiler.ResolvedDependency =
            { name = "dep"
              version = "1.0.0"
              source = ""
              compileReferencePaths = []
              runtimeLoadPaths = [ managedPath ]
              nativeRuntimePaths = [ nativePath ] }

        let result = BuildSystem.copyDependencies [ dep ] outDir

        match result with
        | Ok copied ->
            Assert.Equal(2, List.length copied)
            Assert.True(File.Exists(Path.Join(outDir, "managed.dll")))
            Assert.True(File.Exists(Path.Join(outDir, "native.dll")))
        | Result.Error _ -> Assert.Fail("expected Ok")

    [<Fact>]
    let ``copyDependencies should return error when native runtime source file does not exist`` () =
        let outDir = createTempProjectDir ()
        let missingPath = Path.Join(outDir, "nonexistent-native.dll")

        let dep = makeNativeDep "dep" [ missingPath ]
        let result = BuildSystem.copyDependencies [ dep ] outDir

        match result with
        | Ok _ -> Assert.Fail("expected error")
        | Result.Error diagnostics -> Assert.NotEmpty(diagnostics)

    [<Fact>]
    let ``copyDependencies should preserve runtimes rid native hierarchy for native runtime files`` () =
        (* dep.source が設定されている場合、nativeRuntimePaths は runtimes/<rid>/native/ 階層を保持してコピーされる。 *)
        let pkgRoot = createTempProjectDir ()
        let outDir = createTempProjectDir ()
        let winNativeDir = Path.Join(pkgRoot, "runtimes", "win-x64", "native")
        let linuxNativeDir = Path.Join(pkgRoot, "runtimes", "linux-x64", "native")
        Directory.CreateDirectory(winNativeDir) |> ignore
        Directory.CreateDirectory(linuxNativeDir) |> ignore
        let winSrc = Path.Join(winNativeDir, "libSkiaSharp.dll")
        let linuxSrc = Path.Join(linuxNativeDir, "libSkiaSharp.so")
        File.WriteAllText(winSrc, "fake-win-native")
        File.WriteAllText(linuxSrc, "fake-linux-native")

        let dep : Atla.Compiler.Compiler.ResolvedDependency =
            { name = "skiasharp"
              version = "1.0.0"
              source = pkgRoot
              compileReferencePaths = []
              runtimeLoadPaths = []
              nativeRuntimePaths = [ winSrc; linuxSrc ] }

        let result = BuildSystem.copyDependencies [ dep ] outDir

        match result with
        | Ok copied ->
            Assert.Equal(2, List.length copied)
            Assert.True(File.Exists(Path.Join(outDir, "runtimes", "win-x64", "native", "libSkiaSharp.dll")))
            Assert.True(File.Exists(Path.Join(outDir, "runtimes", "linux-x64", "native", "libSkiaSharp.so")))
            Assert.Equal("fake-win-native", File.ReadAllText(Path.Join(outDir, "runtimes", "win-x64", "native", "libSkiaSharp.dll")))
            Assert.Equal("fake-linux-native", File.ReadAllText(Path.Join(outDir, "runtimes", "linux-x64", "native", "libSkiaSharp.so")))
        | Result.Error _ -> Assert.Fail("expected Ok")

    [<Fact>]
    let ``dependency loader should classify missing chained dependency as warning`` () =
        let dependencyLoaderType = Type.GetType("Atla.Compiler.DependencyLoader, Atla.Core")

        if isNull dependencyLoaderType then
            Assert.Fail("expected Atla.Compiler.DependencyLoader type to exist")

        let methodInfo =
            dependencyLoaderType.GetMethod("toLoadDiagnostic", BindingFlags.NonPublic ||| BindingFlags.Static)

        if isNull methodInfo then
            Assert.Fail("expected DependencyLoader.toLoadDiagnostic to exist")

        let diagnostic =
            methodInfo.Invoke(
                null,
                [| "stride" :> obj
                   "/tmp/Stride.Engine.dll" :> obj
                   FileNotFoundException("missing chained dependency", "SharpDX.Direct3D11") :> obj |]
            )
            :?> Diagnostic

        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.severity)
        Assert.Contains("skipped missing chained dependency", diagnostic.message)

    [<Fact>]
    let ``dependency loader should continue attempting all assemblies after first load failure`` () =
        let tempDir = createTempProjectDir ()
        let bad1 = Path.Join(tempDir, "bad1.dll")
        let bad2 = Path.Join(tempDir, "bad2.dll")
        File.WriteAllText(bad1, "not-a-dotnet-assembly-1")
        File.WriteAllText(bad2, "not-a-dotnet-assembly-2")

        let result = DependencyLoader.loadDependencies [ "stride", [ bad1; bad2 ] ]

        Assert.False(result.succeeded)
        Assert.Equal(2, result.diagnostics |> List.length)
        Assert.All(result.diagnostics, fun d -> Assert.Equal(DiagnosticSeverity.Error, d.severity))
        Assert.Contains(result.diagnostics, fun d -> d.message.Contains(bad1))
        Assert.Contains(result.diagnostics, fun d -> d.message.Contains(bad2))

    [<Fact>]
    let ``dependency loader should fail for bad image format`` () =
        let tempDir = createTempProjectDir ()
        let badAssemblyPath = Path.Join(tempDir, "bad.dll")
        File.WriteAllText(badAssemblyPath, "not-a-dotnet-assembly")

        let result = DependencyLoader.loadDependencies [ "stride", [ badAssemblyPath ] ]

        Assert.False(result.succeeded)
        Assert.True(result.loadContext.IsNone)
        Assert.Single(result.diagnostics) |> ignore
        Assert.Equal(DiagnosticSeverity.Error, result.diagnostics[0].severity)
        Assert.Contains("not a valid .NET assembly", result.diagnostics[0].message)
