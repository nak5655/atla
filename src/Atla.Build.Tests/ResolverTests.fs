namespace Atla.Build.Tests

open System
open System.IO
open Xunit
open Atla.Build

module ResolverTests =
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

    /// YAML の basic string で解釈可能なように、Windows 区切り文字を POSIX 形式へ正規化する。
    let private toYamlPath (path: string) =
        path.Replace("\\", "/")

    let private withNuGetPackagesRoot (packagesRoot: string) (action: unit -> unit) =
        let previous = Environment.GetEnvironmentVariable("NUGET_PACKAGES")
        Environment.SetEnvironmentVariable("NUGET_PACKAGES", packagesRoot)

        try
            action ()
        finally
            Environment.SetEnvironmentVariable("NUGET_PACKAGES", previous)

    let private withEnvironmentVariable (name: string) (value: string) (action: unit -> unit) =
        let previous = Environment.GetEnvironmentVariable(name)
        Environment.SetEnvironmentVariable(name, value)

        try
            action ()
        finally
            Environment.SetEnvironmentVariable(name, previous)

    [<Fact>]
    let ``buildProject should parse dependency with version as nuget-style dependency`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        let expectedPackagePath = Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")
        let expectedDllPath = Path.Join(expectedPackagePath, "ref", "net8.0", "Newtonsoft.Json.dll")
        Directory.CreateDirectory(Path.GetDirectoryName(expectedDllPath)) |> ignore
        File.WriteAllText(expectedDllPath, "")

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  Newtonsoft.Json:
    version: "13.0.3"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.True(result.succeeded)
            Assert.Empty(result.diagnostics)

            match result.plan with
            | Some plan ->
                let dependency = Assert.Single(plan.dependencies)
                Assert.Equal("Newtonsoft.Json", dependency.name)
                Assert.Equal("13.0.3", dependency.version)
                Assert.Equal(Path.GetFullPath(expectedPackagePath), dependency.source)
                Assert.Equal<string list>([ Path.GetFullPath(expectedDllPath) ], dependency.compileReferencePaths)
                Assert.Equal<string list>([ Path.GetFullPath(expectedDllPath) ], dependency.runtimeLoadPaths)
            | None ->
                Assert.Fail("expected build plan")
        )

    [<Fact>]
    let ``buildProject should split compile and runtime assembly selection for nuget and path dependencies`` () =
        let rootProject = createTempProjectDir ()
        let depProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        let nugetRoot = Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")
        let nugetRefDir = Path.Join(nugetRoot, "ref", "net8.0")
        let nugetLibDir = Path.Join(nugetRoot, "lib", "net8.0")
        let pathRefDir = Path.Join(depProject, "ref", "net8.0")
        let pathLibDir = Path.Join(depProject, "lib", "net8.0")
        Directory.CreateDirectory(nugetRefDir) |> ignore
        Directory.CreateDirectory(nugetLibDir) |> ignore
        Directory.CreateDirectory(pathRefDir) |> ignore
        Directory.CreateDirectory(pathLibDir) |> ignore
        File.WriteAllText(Path.Join(nugetRefDir, "Newtonsoft.Json.dll"), "")
        File.WriteAllText(Path.Join(nugetLibDir, "Newtonsoft.Json.dll"), "")
        File.WriteAllText(Path.Join(pathRefDir, "dep-lib.dll"), "")
        File.WriteAllText(Path.Join(pathLibDir, "dep-lib-runtime.dll"), "")
        let relativePath = Path.GetRelativePath(rootProject, depProject) |> toYamlPath

        writeManifest depProject """
package:
  name: "dep-lib"
  version: "0.1.0"
"""

        writeManifest rootProject $"""
package:
  name: "app"
  version: "0.1.0"
dependencies:
  depLocal:
    path: "{relativePath}"
  Newtonsoft.Json:
    version: "13.0.3"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.True(result.succeeded)
            Assert.Empty(result.diagnostics)

            match result.plan with
            | Some plan ->
                let byName = plan.dependencies |> List.map (fun dep -> dep.name, dep) |> Map.ofList
                let localDependency = byName["dep-lib"]
                let nugetDependency = byName["Newtonsoft.Json"]
                Assert.Equal<string list>([ Path.GetFullPath(Path.Join(pathRefDir, "dep-lib.dll")) ], localDependency.compileReferencePaths)
                Assert.Equal<string list>([ Path.GetFullPath(Path.Join(pathLibDir, "dep-lib-runtime.dll")) ], localDependency.runtimeLoadPaths)
                Assert.Equal<string list>([ Path.GetFullPath(Path.Join(nugetRefDir, "Newtonsoft.Json.dll")) ], nugetDependency.compileReferencePaths)
                Assert.Equal<string list>([ Path.GetFullPath(Path.Join(nugetLibDir, "Newtonsoft.Json.dll")) ], nugetDependency.runtimeLoadPaths)
            | None ->
                Assert.Fail("expected build plan")
        )

    [<Fact>]
    let ``buildProject should fail when dependency has no supported reference assembly layout`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        let packageRoot = Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")
        Directory.CreateDirectory(packageRoot) |> ignore

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  Newtonsoft.Json:
    version: "13.0.3"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(result.succeeded)
            Assert.True(result.plan.IsNone)
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("has no supported compile reference assemblies")))
        )

    [<Fact>]
    let ``buildProject should prefer highest tfm within ref for compile and within lib for runtime`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        let packageRoot = Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")
        let refNet8 = Path.Join(packageRoot, "ref", "net8.0")
        let refNetstandard = Path.Join(packageRoot, "ref", "netstandard2.0")
        let libNet10 = Path.Join(packageRoot, "lib", "net10.0")
        Directory.CreateDirectory(refNet8) |> ignore
        Directory.CreateDirectory(refNetstandard) |> ignore
        Directory.CreateDirectory(libNet10) |> ignore
        File.WriteAllText(Path.Join(refNet8, "Newtonsoft.Json.dll"), "")
        File.WriteAllText(Path.Join(refNetstandard, "Newtonsoft.Json.dll"), "")
        File.WriteAllText(Path.Join(libNet10, "Newtonsoft.Json.dll"), "")

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  Newtonsoft.Json:
    version: "13.0.3"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.True(result.succeeded)

            match result.plan with
            | Some plan ->
                let dependency = Assert.Single(plan.dependencies)
                Assert.Equal<string list>([ Path.GetFullPath(Path.Join(refNet8, "Newtonsoft.Json.dll")) ], dependency.compileReferencePaths)
                Assert.Equal<string list>([ Path.GetFullPath(Path.Join(libNet10, "Newtonsoft.Json.dll")) ], dependency.runtimeLoadPaths)
            | None ->
                Assert.Fail("expected build plan")
        )

    [<Fact>]
    let ``buildProject should fail when selected tfm has duplicate simple names`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        let packageRoot = Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")
        let tfmRoot = Path.Join(packageRoot, "ref", "net8.0")
        let duplicateDir = Path.Join(tfmRoot, "alt")
        Directory.CreateDirectory(duplicateDir) |> ignore
        File.WriteAllText(Path.Join(tfmRoot, "Newtonsoft.Json.dll"), "")
        File.WriteAllText(Path.Join(duplicateDir, "Newtonsoft.Json.dll"), "")

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  Newtonsoft.Json:
    version: "13.0.3"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(result.succeeded)
            Assert.True(result.plan.IsNone)
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("ambiguous reference assemblies")))
        )

    [<Fact>]
    let ``buildProject should fail when dependency specifies both path and version`` () =
        let rootProject = createTempProjectDir ()

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  common:
    path: "./deps/common"
    version: "1.2.3"
"""

        let result = BuildSystem.buildProject { projectRoot = rootProject }

        Assert.False(result.succeeded)
        Assert.True(
            result.diagnostics
            |> List.exists (fun d -> d.message.Contains("cannot specify both `path` and `version`"))
        )
        Assert.True(result.plan.IsNone)

    [<Fact>]
    let ``buildProject should fail when path and nuget dependencies resolve to same package name`` () =
        let rootProject = createTempProjectDir ()
        let depProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        let packagePath = Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")
        Directory.CreateDirectory(Path.Join(packagePath, "ref", "net8.0")) |> ignore
        File.WriteAllText(Path.Join(packagePath, "ref", "net8.0", "Newtonsoft.Json.dll"), "")
        let relativePath = Path.GetRelativePath(rootProject, depProject) |> toYamlPath

        writeManifest depProject """
package:
  name: "Newtonsoft.Json"
  version: "9.0.1"
"""
        writeReferenceDll depProject "Newtonsoft.Json.dll"

        writeManifest rootProject $"""
package:
  name: "app"
  version: "0.1.0"
dependencies:
  jsonLocal:
    path: "{relativePath}"
  Newtonsoft.Json:
    version: "13.0.3"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(result.succeeded)
            Assert.True(result.plan.IsNone)
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("dependency version conflict `Newtonsoft.Json`")))
        )

    [<Fact>]
    let ``buildProject should fail when nuget package does not exist in cache`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  Atla.NonExistent.Package.For.Tests.MissingA:
    version: "99.99.99"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(result.succeeded)
            Assert.True(result.plan.IsNone)
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("nuget package restore failed")))
        )

    [<Fact>]
    let ``buildProject should always attempt restore when nuget package does not exist in cache`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  Atla.NonExistent.Package.For.Tests.MissingB:
    version: "99.99.99"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(result.succeeded)
            Assert.True(result.plan.IsNone)
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("nuget package restore failed")))
        )

    [<Fact>]
    let ``buildProject should report restore failure when package cannot be resolved`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  Atla.NonExistent.Package.For.Tests:
    version: "99.99.99"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(result.succeeded)
            Assert.True(result.plan.IsNone)
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("nuget package restore failed")))
        )

    [<Fact>]
    let ``buildProject should fail when transitive nuget versions do not match`` () =
        let rootProject = createTempProjectDir ()
        let depProjectA = createTempProjectDir ()
        let depProjectB = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        Directory.CreateDirectory(Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")) |> ignore
        Directory.CreateDirectory(Path.Join(packagesRoot, "newtonsoft.json", "12.0.3")) |> ignore
        Directory.CreateDirectory(Path.Join(packagesRoot, "newtonsoft.json", "13.0.3", "ref", "net8.0")) |> ignore
        Directory.CreateDirectory(Path.Join(packagesRoot, "newtonsoft.json", "12.0.3", "ref", "net8.0")) |> ignore
        File.WriteAllText(Path.Join(packagesRoot, "newtonsoft.json", "13.0.3", "ref", "net8.0", "Newtonsoft.Json.dll"), "")
        File.WriteAllText(Path.Join(packagesRoot, "newtonsoft.json", "12.0.3", "ref", "net8.0", "Newtonsoft.Json.dll"), "")

        let relativeA = Path.GetRelativePath(rootProject, depProjectA) |> toYamlPath
        let relativeB = Path.GetRelativePath(rootProject, depProjectB) |> toYamlPath

        writeManifest depProjectA """
package:
  name: "dep-a"
  version: "0.1.0"
dependencies:
  Newtonsoft.Json:
    version: "13.0.3"
"""
        writeReferenceDll depProjectA "dep-a.dll"

        writeManifest depProjectB """
package:
  name: "dep-b"
  version: "0.1.0"
dependencies:
  Newtonsoft.Json:
    version: "12.0.3"
"""
        writeReferenceDll depProjectB "dep-b.dll"

        writeManifest rootProject $"""
package:
  name: "app"
  version: "0.1.0"
dependencies:
  depA:
    path: "{relativeA}"
  depB:
    path: "{relativeB}"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(result.succeeded)
            Assert.True(result.plan.IsNone)
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("dependency version conflict `Newtonsoft.Json`")))
        )

    [<Fact>]
    let ``buildProject should allow transitive nuget versions when they match exactly`` () =
        let rootProject = createTempProjectDir ()
        let depProjectA = createTempProjectDir ()
        let depProjectB = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        let packagePath = Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")
        Directory.CreateDirectory(Path.Join(packagePath, "ref", "net8.0")) |> ignore
        File.WriteAllText(Path.Join(packagePath, "ref", "net8.0", "Newtonsoft.Json.dll"), "")

        let relativeA = Path.GetRelativePath(rootProject, depProjectA) |> toYamlPath
        let relativeB = Path.GetRelativePath(rootProject, depProjectB) |> toYamlPath

        writeManifest depProjectA """
package:
  name: "dep-a"
  version: "0.1.0"
dependencies:
  Newtonsoft.Json:
    version: "13.0.3"
"""
        writeReferenceDll depProjectA "dep-a.dll"

        writeManifest depProjectB """
package:
  name: "dep-b"
  version: "0.1.0"
dependencies:
  Newtonsoft.Json:
    version: "13.0.3"
"""
        writeReferenceDll depProjectB "dep-b.dll"

        writeManifest rootProject $"""
package:
  name: "app"
  version: "0.1.0"
dependencies:
  depA:
    path: "{relativeA}"
  depB:
    path: "{relativeB}"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.True(result.succeeded)
            Assert.Empty(result.diagnostics)

            match result.plan with
            | Some plan ->
                let jsonDeps = plan.dependencies |> List.filter (fun dep -> dep.name = "Newtonsoft.Json")
                let jsonDependency = Assert.Single(jsonDeps)
                Assert.Equal("13.0.3", jsonDependency.version)
                Assert.Equal(Path.GetFullPath(packagePath), jsonDependency.source)
            | None ->
                Assert.Fail("expected build plan")
        )

    [<Fact>]
    let ``buildProject should return dependencies in deterministic order`` () =
        let rootProject = createTempProjectDir ()
        let depProjectA = createTempProjectDir ()
        let depProjectZ = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        Directory.CreateDirectory(Path.Join(packagesRoot, "pkgx", "1.0.0")) |> ignore
        Directory.CreateDirectory(Path.Join(packagesRoot, "pkgx", "1.0.0", "ref", "net8.0")) |> ignore
        File.WriteAllText(Path.Join(packagesRoot, "pkgx", "1.0.0", "ref", "net8.0", "PkgX.dll"), "")

        let relativeA = Path.GetRelativePath(rootProject, depProjectA) |> toYamlPath
        let relativeZ = Path.GetRelativePath(rootProject, depProjectZ) |> toYamlPath

        writeManifest depProjectA """
package:
  name: "A"
  version: "0.1.0"
"""
        writeReferenceDll depProjectA "A.dll"

        writeManifest depProjectZ """
package:
  name: "Z"
  version: "0.1.0"
"""
        writeReferenceDll depProjectZ "Z.dll"

        writeManifest rootProject $"""
package:
  name: "app"
  version: "0.1.0"
dependencies:
  zdep:
    path: "{relativeZ}"
  PkgX:
    version: "1.0.0"
  adep:
    path: "{relativeA}"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.True(result.succeeded)
            Assert.Empty(result.diagnostics)

            match result.plan with
            | Some plan ->
                let names = plan.dependencies |> List.map (fun dep -> dep.name)
                Assert.Equal<string list>([ "A"; "PkgX"; "Z" ], names)
            | None ->
                Assert.Fail("expected build plan")
        )

    [<Fact>]
    let ``buildProject should keep diagnostics order deterministic across runs`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  missingPath:
    path: "./deps/missing"
  Missing.Pkg:
    version: "9.9.9"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let run1 = BuildSystem.buildProject { projectRoot = rootProject }
            let run2 = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(run1.succeeded)
            Assert.False(run2.succeeded)

            let messages1 = run1.diagnostics |> List.map (fun d -> d.message)
            let messages2 = run2.diagnostics |> List.map (fun d -> d.message)
            Assert.Equal<string list>(messages1, messages2)
        )

    /// 指定されたパッケージディレクトリに .nuspec ファイルを書き込む。
    let private writeNuspec (packagePath: string) (packageId: string) (version: string) (nuspecBody: string) =
        let nuspecPath = Path.Join(packagePath, $"{packageId.ToLowerInvariant()}.nuspec")
        File.WriteAllText(nuspecPath, nuspecBody.Trim())

    /// 標準的な NuGet パッケージレイアウトを作成し、指定 TFM の lib DLL を生成する。
    let private createNuGetPackage (packagesRoot: string) (packageId: string) (version: string) (tfm: string) : string =
        let packagePath = Path.Join(packagesRoot, packageId.ToLowerInvariant(), version)
        let libDir = Path.Join(packagePath, "lib", tfm)
        Directory.CreateDirectory(libDir) |> ignore
        File.WriteAllText(Path.Join(libDir, $"{packageId}.dll"), "")
        packagePath

    [<Fact>]
    let ``buildProject should resolve transitive nuget dependencies from nuspec`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()

        (* Primary パッケージを作成する。 *)
        let primaryPath = createNuGetPackage packagesRoot "Primary" "1.0.0" "net8.0"

        (* Primary の nuspec に TransitivePkg への依存を記述する。 *)
        writeNuspec primaryPath "Primary" "1.0.0" """
<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">
  <metadata>
    <id>Primary</id>
    <version>1.0.0</version>
    <dependencies>
      <group targetFramework="net8.0">
        <dependency id="TransitivePkg" version="2.0.0" />
      </group>
    </dependencies>
  </metadata>
</package>
"""

        (* 推移的依存パッケージを作成する。 *)
        let _transitivePath = createNuGetPackage packagesRoot "TransitivePkg" "2.0.0" "net8.0"

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  Primary:
    version: "1.0.0"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.True(result.succeeded)
            Assert.Empty(result.diagnostics)

            match result.plan with
            | Some plan ->
                let names = plan.dependencies |> List.map (fun dep -> dep.name) |> List.sort
                Assert.Equal<string list>([ "Primary"; "TransitivePkg" ], names)
            | None ->
                Assert.Fail("expected build plan")
        )

    [<Fact>]
    let ``buildProject should not include duplicate packages when transitive deps overlap`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()

        (* PkgA と PkgB が同じ SharedPkg に依存するダイアモンド依存を作成する。 *)
        let pkgAPath = createNuGetPackage packagesRoot "PkgA" "1.0.0" "net8.0"
        let pkgBPath = createNuGetPackage packagesRoot "PkgB" "1.0.0" "net8.0"
        let _sharedPath = createNuGetPackage packagesRoot "SharedPkg" "3.0.0" "net8.0"

        writeNuspec pkgAPath "PkgA" "1.0.0" """
<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">
  <metadata>
    <id>PkgA</id>
    <version>1.0.0</version>
    <dependencies>
      <group targetFramework="net8.0">
        <dependency id="SharedPkg" version="3.0.0" />
      </group>
    </dependencies>
  </metadata>
</package>
"""

        writeNuspec pkgBPath "PkgB" "1.0.0" """
<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">
  <metadata>
    <id>PkgB</id>
    <version>1.0.0</version>
    <dependencies>
      <group targetFramework="net8.0">
        <dependency id="SharedPkg" version="3.0.0" />
      </group>
    </dependencies>
  </metadata>
</package>
"""

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  PkgA:
    version: "1.0.0"
  PkgB:
    version: "1.0.0"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.True(result.succeeded)
            Assert.Empty(result.diagnostics)

            match result.plan with
            | Some plan ->
                let names = plan.dependencies |> List.map (fun dep -> dep.name) |> List.sort
                (* SharedPkg は重複なく一度だけ含まれること。 *)
                Assert.Equal<string list>([ "PkgA"; "PkgB"; "SharedPkg" ], names)
            | None ->
                Assert.Fail("expected build plan")
        )

    [<Fact>]
    let ``buildProject should report version conflict for transitive nuget dependency`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()

        (* PkgA と PkgB が SharedPkg の異なるバージョンに依存する。 *)
        let pkgAPath = createNuGetPackage packagesRoot "PkgA" "1.0.0" "net8.0"
        let pkgBPath = createNuGetPackage packagesRoot "PkgB" "1.0.0" "net8.0"
        let _sharedV1 = createNuGetPackage packagesRoot "SharedPkg" "1.0.0" "net8.0"
        let _sharedV2 = createNuGetPackage packagesRoot "SharedPkg" "2.0.0" "net8.0"

        writeNuspec pkgAPath "PkgA" "1.0.0" """
<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">
  <metadata>
    <id>PkgA</id>
    <version>1.0.0</version>
    <dependencies>
      <group targetFramework="net8.0">
        <dependency id="SharedPkg" version="1.0.0" />
      </group>
    </dependencies>
  </metadata>
</package>
"""

        writeNuspec pkgBPath "PkgB" "1.0.0" """
<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">
  <metadata>
    <id>PkgB</id>
    <version>1.0.0</version>
    <dependencies>
      <group targetFramework="net8.0">
        <dependency id="SharedPkg" version="2.0.0" />
      </group>
    </dependencies>
  </metadata>
</package>
"""

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  PkgA:
    version: "1.0.0"
  PkgB:
    version: "1.0.0"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(result.succeeded)
            Assert.True(result.plan.IsNone)
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("dependency version conflict `SharedPkg`")))
        )

    [<Fact>]
    let ``buildProject should resolve multi-level transitive nuget dependencies`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()

        (* Root -> Level1 -> Level2 の3段の依存チェーンを作成する。 *)
        let level1Path = createNuGetPackage packagesRoot "Level1" "1.0.0" "net8.0"
        let level2Path = createNuGetPackage packagesRoot "Level2" "1.0.0" "net8.0"
        let _level3 = createNuGetPackage packagesRoot "Level3" "1.0.0" "net8.0"

        writeNuspec level1Path "Level1" "1.0.0" """
<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">
  <metadata>
    <id>Level1</id>
    <version>1.0.0</version>
    <dependencies>
      <group targetFramework="net8.0">
        <dependency id="Level2" version="1.0.0" />
      </group>
    </dependencies>
  </metadata>
</package>
"""

        writeNuspec level2Path "Level2" "1.0.0" """
<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">
  <metadata>
    <id>Level2</id>
    <version>1.0.0</version>
    <dependencies>
      <group targetFramework="net8.0">
        <dependency id="Level3" version="1.0.0" />
      </group>
    </dependencies>
  </metadata>
</package>
"""

        writeManifest rootProject """
package:
  name: "app"
  version: "0.1.0"
dependencies:
  Level1:
    version: "1.0.0"
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.True(result.succeeded)
            Assert.Empty(result.diagnostics)

            match result.plan with
            | Some plan ->
                let names = plan.dependencies |> List.map (fun dep -> dep.name) |> List.sort
                Assert.Equal<string list>([ "Level1"; "Level2"; "Level3" ], names)
            | None ->
                Assert.Fail("expected build plan")
        )
