namespace Atla.Build.Tests

open System
open System.IO
open Xunit
open Atla.Build

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

    [<Fact>]
    let ``createEmptyPlan keeps projectRoot and no dependencies`` () =
        let request = { BuildRequest.projectRoot = "/tmp/hello" }
        let plan = BuildSystem.createEmptyPlan request

        Assert.Equal("/tmp/hello", plan.projectRoot)
        Assert.Equal<string>("", plan.projectName)
        Assert.Equal<string>("", plan.projectVersion)
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
