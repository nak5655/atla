namespace Atla.LanguageServer.Tests

open System.IO
open Newtonsoft.Json.Linq
open Xunit
open Atla.LanguageServer.Server
open Atla.LanguageServer.LSPTypes
open Atla.Build
open Atla.Compiler
open Atla.Core.Data

module ServerLifecycleTests =
    /// `Server.tryNormalizeUri` の file URI 仕様に合わせて期待値をOS非依存で組み立てる。
    let private expectedNormalizedFileUri (path: string) =
        let normalizedPath = Path.GetFullPath(path).Replace('\\', '/')
        let normalizedPathForOs =
            if Path.DirectorySeparatorChar = '\\' then
                normalizedPath.ToLowerInvariant()
            else
                normalizedPath
        sprintf "file://%s" normalizedPathForOs

    [<Fact>]
    let ``normalize uri makes file key deterministic`` () =
        let server = Server()

        let normalized = server.TryNormalizeUri("file:///tmp/../tmp/test.atla")

        Assert.Equal(Some(expectedNormalizedFileUri "/tmp/test.atla"), normalized)

    [<Fact>]
    let ``did open, change, close lifecycle publishes diagnostics and clears buffer`` () =
        let published = ResizeArray<string * int>()
        let server =
            Server(fun uri diagnostics ->
                published.Add(uri, diagnostics.Length))

        server.IsAvailablePublishDiagnostics <- true
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]

        let uri = "file:///tmp/lifecycle.atla"
        server.OpenDocument(uri, "fn main: Int = 0")
        server.ChangeDocument(uri, "fn main: Int = 0")

        let tokensBeforeClose = server.Tokenize(uri)
        Assert.NotEmpty(tokensBeforeClose)

        server.CloseDocument(uri)

        let tokensAfterClose = server.Tokenize(uri)
        Assert.Empty(tokensAfterClose)

        let publishedList = published |> Seq.toList
        Assert.True(publishedList.Length >= 3)
        Assert.Equal((uri, 0), publishedList.[0])
        Assert.Equal((uri, 0), publishedList.[1])
        Assert.Equal((uri, 0), publishedList.[2])

    [<Fact>]
    let ``workspace roots gate compilation targets deterministically`` () =
        let published = ResizeArray<string * int>()
        let server =
            Server(fun uri diagnostics ->
                published.Add(uri, diagnostics.Length))

        let content = JObject.Parse("""
        {
          "params": {
            "rootUri": "file:///tmp/workspace",
            "capabilities": {
              "textDocument": {
                "publishDiagnostics": { "relatedInformation": true },
                "semanticTokens": { "tokenTypes": ["keyword", "number", "string", "variable", "type"] }
              }
            }
          }
        }
        """)

        server.Initialize(content) |> ignore

        server.OpenDocument("file:///tmp/workspace/in.atla", "fn main: Int = 0")
        server.OpenDocument("file:///tmp/out.atla", "fn main: Int = 0")

        let publishedList = published |> Seq.toList
        Assert.Contains(publishedList, fun (uri, count) -> uri = "file:///tmp/workspace/in.atla" && count = 0)
        Assert.Contains(publishedList, fun (uri, count) -> uri = "file:///tmp/out.atla" && count = 0)

    [<Fact>]
    let ``initialize falls back to default version when assembly path is empty`` () =
        let server = Server(assemblyLocationResolver = (fun () -> ""))

        let content = JObject.Parse("""
        {
          "params": {
            "capabilities": {
              "textDocument": {
                "publishDiagnostics": { "relatedInformation": true },
                "semanticTokens": { "tokenTypes": ["keyword", "number", "string", "variable", "type"] }
              }
            }
          }
        }
        """)

        let result = server.Initialize(content)
        Assert.Equal("0.0.0", result.serverInfo.version)

    [<Fact>]
    let ``compile and publish injects dependencies resolved from build plan`` () =
        let capturedRequests = ResizeArray<Compiler.CompileRequest>()
        let published = ResizeArray<string * int * string>()
        let tempRoot = Path.Join(Path.GetTempPath(), $"atla-lsp-dependency-inject-{System.Guid.NewGuid():N}")
        let srcDir = Path.Join(tempRoot, "src")
        Directory.CreateDirectory(srcDir) |> ignore
        File.WriteAllText(Path.Join(tempRoot, "atla.yaml"), "package:\n  name: \"app\"\n  version: \"0.1.0\"\n")

        let buildProject (_: BuildRequest) : BuildResult =
            { succeeded = true
              plan =
                Some {
                    projectName = "app"
                    projectVersion = "0.1.0"
                    projectRoot = tempRoot
                    dependencies =
                        [ { name = "Sample.Dependency"
                            version = "1.0.0"
                            source = Path.Join(tempRoot, "deps", "sample")
                            compileReferencePaths = [ Path.Join(tempRoot, "deps", "sample", "ref", "net10.0", "Sample.Dependency.dll") ]
                            runtimeLoadPaths = [ Path.Join(tempRoot, "deps", "sample", "ref", "net10.0", "Sample.Dependency.dll") ] } ]
                  }
              diagnostics = [] }

        let compile (request: Compiler.CompileRequest) : Compiler.CompileResult =
            capturedRequests.Add(request)
            { succeeded = true
              diagnostics = [] }

        let server =
            Server(
                (fun uri diagnostics ->
                    let source =
                        diagnostics
                        |> List.tryHead
                        |> Option.map (fun x -> if isNull x.source then "" else x.source)
                        |> Option.defaultValue ""
                    published.Add(uri, diagnostics.Length, source)),
                buildProjectFn = buildProject,
                compileFn = compile
            )

        server.IsAvailablePublishDiagnostics <- true
        let documentUri = System.Uri(Path.Join(srcDir, "main.atla")).AbsoluteUri
        server.OpenDocument(documentUri, "fn main: Int = 0")

        let request = capturedRequests |> Seq.exactlyOne
        Assert.Single(request.dependencies) |> ignore
        Assert.Equal("Sample.Dependency", request.dependencies.Head.name)
        Assert.Contains(published |> Seq.toList, fun (_, count, _) -> count = 0)

    [<Fact>]
    let ``build failure publishes diagnostics as build source and skips compiler`` () =
        let published = ResizeArray<string * Atla.LanguageServer.LSPTypes.Diagnostic list>()
        let mutable compileCalled = false
        let tempRoot = Path.Join(Path.GetTempPath(), $"atla-lsp-build-fail-{System.Guid.NewGuid():N}")
        let srcDir = Path.Join(tempRoot, "src")
        Directory.CreateDirectory(srcDir) |> ignore
        File.WriteAllText(Path.Join(tempRoot, "atla.yaml"), "package:\n  name: \"broken\"\n")

        let buildProject (_: BuildRequest) : BuildResult =
            { succeeded = false
              plan = None
              diagnostics = [ Atla.Core.Semantics.Data.Diagnostic.Error("manifest parse failed", Span.Empty) ] }

        let compile (_: Compiler.CompileRequest) : Compiler.CompileResult =
            compileCalled <- true
            { succeeded = true
              diagnostics = [] }

        let server =
            Server(
                (fun uri diagnostics -> published.Add(uri, diagnostics)),
                buildProjectFn = buildProject,
                compileFn = compile
            )

        let initializeContent = JObject.Parse($"""
        {{
          "params": {{
            "rootUri": "{System.Uri(tempRoot).AbsoluteUri}",
            "capabilities": {{
              "textDocument": {{
                "publishDiagnostics": {{ "relatedInformation": true }},
                "semanticTokens": {{ "tokenTypes": ["keyword", "number", "string", "variable", "type"] }}
              }}
            }}
          }}
        }}
        """)

        server.Initialize(initializeContent) |> ignore
        let documentUri = System.Uri(Path.Join(srcDir, "main.atla")).AbsoluteUri
        server.OpenDocument(documentUri, "fn main: Int = 0")

        Assert.False(compileCalled)
        let (_, diagnostics) = published |> Seq.last
        let diagnostic = Assert.Single(diagnostics)
        Assert.Equal("atla-build", diagnostic.source)
