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

    [<Fact>]
    let ``buildProject should parse minimal atla.toml`` () =
        let projectRoot = createTempProjectDir ()
        let manifestPath = Path.Join(projectRoot, "atla.toml")

        File.WriteAllText(
            manifestPath,
            """
[package]
name = "hello"
version = "0.1.0"
""".Trim()
        )

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.True(result.succeeded)
        Assert.Empty(result.diagnostics)

        match result.plan with
        | Some plan ->
            Assert.Equal("hello", plan.projectName)
            Assert.Equal("0.1.0", plan.projectVersion)
            Assert.Equal(projectRoot, plan.projectRoot)
            Assert.Empty(plan.dependencies)
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
        let manifestPath = Path.Join(projectRoot, "atla.toml")
        File.WriteAllText(manifestPath, "[package")

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("parse error")))
        Assert.True(result.plan.IsNone)

    [<Fact>]
    let ``buildProject should fail when required fields are missing`` () =
        let projectRoot = createTempProjectDir ()
        let manifestPath = Path.Join(projectRoot, "atla.toml")
        File.WriteAllText(manifestPath, "[package]")

        let result = BuildSystem.buildProject { projectRoot = projectRoot }

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("package.name")))
        Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("package.version")))
        Assert.True(result.plan.IsNone)
