namespace Atla.Build.Tests

open System
open System.IO
open Xunit
open Atla.Build

module BuildSystemTests =
    [<Fact>]
    let ``createEmptyPlan keeps projectRoot and no dependencies`` () =
        let request = { BuildRequest.projectRoot = "/tmp/hello" }
        let plan = BuildSystem.createEmptyPlan request

        Assert.Equal("/tmp/hello", plan.projectRoot)
        Assert.Equal<string>("", plan.projectName)
        Assert.Equal<string>("", plan.projectVersion)
        Assert.Empty(plan.dependencies)

    let private createTempProjectDir () =
        let path = Path.Join(Path.GetTempPath(), $"atla-build-tests-{Guid.NewGuid():N}")
        Directory.CreateDirectory(path) |> ignore
        path

    let private writeManifest (projectRoot: string) (content: string) =
        File.WriteAllText(Path.Join(projectRoot, "atla.toml"), content.Trim())

    [<Fact>]
    let ``buildProject should parse minimal atla.toml`` () =
        let projectRoot = createTempProjectDir ()

        writeManifest projectRoot """
[package]
name = "hello"
version = "0.1.0"
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
[package]
name = "dep"
version = "1.2.3"
"""

        let relativePath = Path.GetRelativePath(rootProject, depProject)

        writeManifest rootProject $"""
[package]
name = "app"
version = "0.1.0"

[dependencies]
dep = {{ path = "{relativePath}" }}
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
    let ``buildProject should fail when atla.toml is missing`` () =
        let projectRoot = createTempProjectDir ()

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("atla.toml not found")))
        Assert.True(result.plan.IsNone)

    [<Fact>]
    let ``buildProject should fail for syntax error`` () =
        let projectRoot = createTempProjectDir ()
        writeManifest projectRoot "[package"

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("parse error")))
        Assert.True(result.plan.IsNone)

    [<Fact>]
    let ``buildProject should fail when required fields are missing`` () =
        let projectRoot = createTempProjectDir ()
        writeManifest projectRoot "[package]"

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("package.name")))
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("package.version")))
        Assert.True(result.plan.IsNone)

    [<Fact>]
    let ``buildProject should fail when dependency path is missing`` () =
        let rootProject = createTempProjectDir ()

        writeManifest rootProject """
[package]
name = "app"
version = "0.1.0"

[dependencies]
missing = { path = "./deps/missing" }
"""

        let result = BuildSystem.buildProject { projectRoot = rootProject }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("dependency path not found")))

    [<Fact>]
    let ``buildProject should fail when dependency graph has cycle`` () =
        let projectA = createTempProjectDir ()
        let projectB = createTempProjectDir ()

        let relativeAToB = Path.GetRelativePath(projectA, projectB)
        let relativeBToA = Path.GetRelativePath(projectB, projectA)

        writeManifest projectA $"""
[package]
name = "a"
version = "0.1.0"

[dependencies]
b = {{ path = "{relativeAToB}" }}
"""

        writeManifest projectB $"""
[package]
name = "b"
version = "0.1.0"

[dependencies]
a = {{ path = "{relativeBToA}" }}
"""

        let result = BuildSystem.buildProject { projectRoot = projectA }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("cyclic dependency")))

    [<Fact>]
    let ``buildProject should fail when duplicate package names resolve`` () =
        let rootProject = createTempProjectDir ()
        let depProjectA = createTempProjectDir ()
        let depProjectB = createTempProjectDir ()

        writeManifest depProjectA """
[package]
name = "common"
version = "1.0.0"
"""

        writeManifest depProjectB """
[package]
name = "common"
version = "2.0.0"
"""

        let relativeA = Path.GetRelativePath(rootProject, depProjectA)
        let relativeB = Path.GetRelativePath(rootProject, depProjectB)

        writeManifest rootProject $"""
[package]
name = "app"
version = "0.1.0"

[dependencies]
commonA = {{ path = "{relativeA}" }}
commonB = {{ path = "{relativeB}" }}
"""

        let result = BuildSystem.buildProject { projectRoot = rootProject }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("duplicate dependency name")))

    [<Fact>]
    let ``buildProject should parse dependency with version as nuget-style dependency`` () =
        let rootProject = createTempProjectDir ()

        writeManifest rootProject """
[package]
name = "app"
version = "0.1.0"

[dependencies]
"Newtonsoft.Json" = { version = "13.0.3" }
"""

        let result = BuildSystem.buildProject { projectRoot = rootProject }

        Assert.True(result.succeeded)
        Assert.Empty(result.diagnostics)

        match result.plan with
        | Some plan ->
            let dependency = Assert.Single(plan.dependencies)
            Assert.Equal("Newtonsoft.Json", dependency.name)
            Assert.Equal("13.0.3", dependency.version)
            Assert.Equal("nuget:Newtonsoft.Json/13.0.3", dependency.source)
        | None ->
            Assert.Fail("expected build plan")

    [<Fact>]
    let ``buildProject should fail when dependency specifies both path and version`` () =
        let rootProject = createTempProjectDir ()

        writeManifest rootProject """
[package]
name = "app"
version = "0.1.0"

[dependencies]
common = { path = "./deps/common", version = "1.2.3" }
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
        let relativePath = Path.GetRelativePath(rootProject, depProject)

        writeManifest depProject """
[package]
name = "Newtonsoft.Json"
version = "9.0.1"
"""

        writeManifest rootProject $"""
[package]
name = "app"
version = "0.1.0"

[dependencies]
jsonLocal = {{ path = "{relativePath}" }}
"Newtonsoft.Json" = {{ version = "13.0.3" }}
"""

        let result = BuildSystem.buildProject { projectRoot = rootProject }

        Assert.False(result.succeeded)
        Assert.True(result.plan.IsNone)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("duplicate dependency name `Newtonsoft.Json`")))
