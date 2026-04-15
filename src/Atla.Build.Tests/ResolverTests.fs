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
        File.WriteAllText(Path.Join(projectRoot, "atla.toml"), content.Trim())

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
        Directory.CreateDirectory(expectedPackagePath) |> ignore

        writeManifest rootProject """
[package]
name = "app"
version = "0.1.0"

[dependencies]
"Newtonsoft.Json" = { version = "13.0.3" }
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
            | None ->
                Assert.Fail("expected build plan")
        )

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
        let packagesRoot = createTempProjectDir ()
        let packagePath = Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")
        Directory.CreateDirectory(packagePath) |> ignore
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
[package]
name = "app"
version = "0.1.0"

[dependencies]
"Newtonsoft.Json" = { version = "13.0.3" }
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            let result = BuildSystem.buildProject { projectRoot = rootProject }

            Assert.False(result.succeeded)
            Assert.True(result.plan.IsNone)
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("nuget package not found in cache")))
            Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("ATLA_BUILD_ENABLE_NUGET_RESTORE=1")))
        )

    [<Fact>]
    let ``buildProject should keep auto restore disabled by default`` () =
        let rootProject = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()

        writeManifest rootProject """
[package]
name = "app"
version = "0.1.0"

[dependencies]
"Newtonsoft.Json" = { version = "13.0.3" }
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            withEnvironmentVariable "ATLA_BUILD_ENABLE_NUGET_RESTORE" "0" (fun () ->
                let result = BuildSystem.buildProject { projectRoot = rootProject }

                Assert.False(result.succeeded)
                Assert.True(result.plan.IsNone)
                Assert.True(result.diagnostics |> List.exists (fun d -> d.message.Contains("nuget package not found in cache")))
            )
        )

    [<Fact>]
    let ``buildProject should fail when transitive nuget versions do not match`` () =
        let rootProject = createTempProjectDir ()
        let depProjectA = createTempProjectDir ()
        let depProjectB = createTempProjectDir ()
        let packagesRoot = createTempProjectDir ()
        Directory.CreateDirectory(Path.Join(packagesRoot, "newtonsoft.json", "13.0.3")) |> ignore
        Directory.CreateDirectory(Path.Join(packagesRoot, "newtonsoft.json", "12.0.3")) |> ignore

        let relativeA = Path.GetRelativePath(rootProject, depProjectA)
        let relativeB = Path.GetRelativePath(rootProject, depProjectB)

        writeManifest depProjectA """
[package]
name = "dep-a"
version = "0.1.0"

[dependencies]
"Newtonsoft.Json" = { version = "13.0.3" }
"""

        writeManifest depProjectB """
[package]
name = "dep-b"
version = "0.1.0"

[dependencies]
"Newtonsoft.Json" = { version = "12.0.3" }
"""

        writeManifest rootProject $"""
[package]
name = "app"
version = "0.1.0"

[dependencies]
depA = {{ path = "{relativeA}" }}
depB = {{ path = "{relativeB}" }}
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
        Directory.CreateDirectory(packagePath) |> ignore

        let relativeA = Path.GetRelativePath(rootProject, depProjectA)
        let relativeB = Path.GetRelativePath(rootProject, depProjectB)

        writeManifest depProjectA """
[package]
name = "dep-a"
version = "0.1.0"

[dependencies]
"Newtonsoft.Json" = { version = "13.0.3" }
"""

        writeManifest depProjectB """
[package]
name = "dep-b"
version = "0.1.0"

[dependencies]
"Newtonsoft.Json" = { version = "13.0.3" }
"""

        writeManifest rootProject $"""
[package]
name = "app"
version = "0.1.0"

[dependencies]
depA = {{ path = "{relativeA}" }}
depB = {{ path = "{relativeB}" }}
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

        let relativeA = Path.GetRelativePath(rootProject, depProjectA)
        let relativeZ = Path.GetRelativePath(rootProject, depProjectZ)

        writeManifest depProjectA """
[package]
name = "A"
version = "0.1.0"
"""

        writeManifest depProjectZ """
[package]
name = "Z"
version = "0.1.0"
"""

        writeManifest rootProject $"""
[package]
name = "app"
version = "0.1.0"

[dependencies]
zdep = {{ path = "{relativeZ}" }}
"PkgX" = {{ version = "1.0.0" }}
adep = {{ path = "{relativeA}" }}
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
[package]
name = "app"
version = "0.1.0"

[dependencies]
missingPath = { path = "./deps/missing" }
"Missing.Pkg" = { version = "9.9.9" }
"""

        withNuGetPackagesRoot packagesRoot (fun () ->
            withEnvironmentVariable "ATLA_BUILD_ENABLE_NUGET_RESTORE" "0" (fun () ->
                let run1 = BuildSystem.buildProject { projectRoot = rootProject }
                let run2 = BuildSystem.buildProject { projectRoot = rootProject }

                Assert.False(run1.succeeded)
                Assert.False(run2.succeeded)

                let messages1 = run1.diagnostics |> List.map (fun d -> d.message)
                let messages2 = run2.diagnostics |> List.map (fun d -> d.message)
                Assert.Equal<string list>(messages1, messages2)
            )
        )
