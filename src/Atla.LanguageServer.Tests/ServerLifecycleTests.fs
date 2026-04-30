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
    let ``initialize sets completion trigger to apostrophe only`` () =
        let server = Server()
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
        let resultJson = JObject.FromObject(result)
        let triggers =
            resultJson.["capabilities"].["completionProvider"].["triggerCharacters"].Values<string>()
            |> Seq.toList
        Assert.Equal<string list>(["'"], triggers)

    [<Fact>]
    let ``normalize uri makes file key deterministic`` () =
        let server = Server()

        let normalized = server.TryNormalizeUri("file:///tmp/../tmp/test.atla")

        Assert.Equal(Some(expectedNormalizedFileUri "/tmp/test.atla"), normalized)

    [<Fact>]
    let ``normalize uri handles percent-encoded Windows drive letter (d%3A)`` () =
        // VSCode on Windows sends file URIs with the drive-letter colon percent-encoded
        // (e.g. file:///d%3A/repo/src/main.atla instead of file:///d:/repo/src/main.atla).
        // Uri.LocalPath decodes %3A to : but retains a spurious leading '/' on Windows
        // (e.g. /d:/repo/...).  The server must strip that slash so path-based lookups work.
        let server = Server()

        let result = server.TryNormalizeUri("file:///d%3A/repo/src/main.atla")

        // The URI must always resolve to Some (never None) on all platforms.
        Assert.True(result.IsSome, "percent-encoded Windows drive URI should not return None")

        if Path.DirectorySeparatorChar = '\\' then
            // On Windows: the leading slash before the drive letter must not appear in
            // the normalized key.  A well-formed key starts with "file://d:".
            let normalized = result.Value
            Assert.True(
                normalized.StartsWith("file://d:", System.StringComparison.OrdinalIgnoreCase),
                sprintf "On Windows the normalized key must start with 'file://d:', got: %s" normalized
            )

    [<Fact>]
    let ``compile dispatches dependencies when workspace root URI uses percent-encoded drive letter`` () =
        // VSCode on Windows can send workspace root URIs with percent-encoded colons (d%3A).
        // collectWorkspaceRoots must decode these so dependency resolution finds atla.yaml.
        let capturedRequests = ResizeArray<Compiler.CompileModulesRequest>()
        let tempRoot = Path.Join(Path.GetTempPath(), $"atla-lsp-pct-root-{System.Guid.NewGuid():N}")

        try
            let srcDir = Path.Join(tempRoot, "src")
            Directory.CreateDirectory(srcDir) |> ignore
            File.WriteAllText(
                Path.Join(tempRoot, "atla.yaml"),
                "package:\n  name: \"app\"\n  version: \"0.1.0\"\n")

            let buildProject (_: BuildRequest) : BuildResult =
                { succeeded = true
                  plan =
                    Some {
                        projectName = "app"
                        projectVersion = "0.1.0"
                        projectRoot = tempRoot
                        dependencies =
                            [ { name = "Sample"
                                version = "1.0.0"
                                source = ""
                                compileReferencePaths = []
                                runtimeLoadPaths = []
                                nativeRuntimePaths = [] } ]
                      }
                  diagnostics = [] }

            let compile (request: Compiler.CompileModulesRequest) : Compiler.CompileResult =
                capturedRequests.Add(request)
                { succeeded = true; diagnostics = []; hir = None; symbolTable = None }

            let server =
                Server(
                    (fun _ _ -> ()),
                    buildProjectFn = buildProject,
                    compileFn = compile)

            server.IsAvailablePublishDiagnostics <- true

            // Encode ':' as '%3A' only in the path portion of the URI (after "file://").
            // This mimics what VSCode on Windows sends for drive-letter paths.
            // On Linux, file:// URIs contain no colon in the path so encoding is a no-op.
            // Query strings and fragments are not present in file:// source URIs, so
            // replacing every ':' after the scheme prefix is safe here.
            let encodePathColon (uri: string) =
                let prefix = "file://"
                if uri.StartsWith(prefix, System.StringComparison.Ordinal) then
                    prefix + uri.Substring(prefix.Length).Replace(":", "%3A")
                else
                    uri

            let rawRootUri = System.Uri(tempRoot).AbsoluteUri
            let encodedRootUri = encodePathColon rawRootUri

            let initContent = Newtonsoft.Json.Linq.JObject.Parse($"""
            {{
              "params": {{
                "rootUri": "{encodedRootUri}",
                "capabilities": {{
                  "textDocument": {{
                    "publishDiagnostics": {{ "relatedInformation": true }},
                    "semanticTokens": {{ "tokenTypes": ["keyword"] }}
                  }}
                }}
              }}
            }}
            """)

            server.Initialize(initContent) |> ignore

            // Open a document whose URI also uses %3A encoding in the path portion.
            let rawDocUri = System.Uri(Path.Join(srcDir, "main.atla")).AbsoluteUri
            let encodedDocUri = encodePathColon rawDocUri
            server.OpenDocument(encodedDocUri, "fn main: Int = 0")

            // Build system should have been invoked and the dependency injected.
            let request = Assert.Single(capturedRequests)
            Assert.Single(request.dependencies) |> ignore
            Assert.Equal("Sample", request.dependencies.Head.name)
        finally
            if Directory.Exists tempRoot then
                Directory.Delete(tempRoot, recursive = true)

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
        let capturedRequests = ResizeArray<Compiler.CompileModulesRequest>()
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
                            runtimeLoadPaths = [ Path.Join(tempRoot, "deps", "sample", "ref", "net10.0", "Sample.Dependency.dll") ]
                            nativeRuntimePaths = [] } ]
                  }
              diagnostics = [] }

        let compile (request: Compiler.CompileModulesRequest) : Compiler.CompileResult =
            capturedRequests.Add(request)
            { succeeded = true
              diagnostics = []
              hir = None
              symbolTable = None }

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
        Assert.Equal("main", request.entryModuleName)
        Assert.Contains(published |> Seq.toList, fun (_, count, _) -> count = 0)

    [<Fact>]
    let ``project compile keeps main as entry module even when editing non main document`` () =
        let capturedRequests = ResizeArray<Compiler.CompileModulesRequest>()
        let tempRoot = Path.Join(Path.GetTempPath(), $"atla-lsp-entry-main-{System.Guid.NewGuid():N}")
        let srcDir = Path.Join(tempRoot, "src")
        Directory.CreateDirectory(srcDir) |> ignore
        File.WriteAllText(Path.Join(tempRoot, "atla.yaml"), "package:\n  name: \"gui_calc\"\n  version: \"0.1.0\"\n")
        File.WriteAllText(Path.Join(srcDir, "main.atla"), "fn main: Int = 0")

        let buildProject (_: BuildRequest) : BuildResult =
            { succeeded = true
              plan = Some { projectName = "gui_calc"; projectVersion = "0.1.0"; projectRoot = tempRoot; dependencies = [] }
              diagnostics = [] }

        let compile (request: Compiler.CompileModulesRequest) : Compiler.CompileResult =
            capturedRequests.Add(request)
            { succeeded = true; diagnostics = []; hir = None; symbolTable = None }

        let server = Server((fun _ _ -> ()), buildProjectFn = buildProject, compileFn = compile)

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
        server.IsAvailablePublishDiagnostics <- true

        let openedUri = System.Uri(Path.Join(srcDir, "CalculatorWindow.atla")).AbsoluteUri
        server.OpenDocument(openedUri, "data CalculatorWindow = { value: Int }")

        let request = capturedRequests |> Seq.exactlyOne
        Assert.Equal("main", request.entryModuleName)

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

        let compile (_: Compiler.CompileModulesRequest) : Compiler.CompileResult =
            compileCalled <- true
            { succeeded = true
              diagnostics = []
              hir = None
              symbolTable = None }

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

    [<Fact>]
    let ``LSP compile same source twice produces deterministic diagnostics order`` () =
        // 同一ソースを2回コンパイルしたとき、診断の URI・件数・内容が不変であることを検証する（決定性テスト）。
        let collectedFirst = ResizeArray<string * int>()
        let collectedSecond = ResizeArray<string * int>()

        let makeServer (collector: ResizeArray<string * int>) =
            Server(fun uri diagnostics -> collector.Add(uri, diagnostics.Length))

        let source = "fn main: Int = undefinedSymbol"
        let uri = "file:///tmp/determinism-test.atla"

        // 1回目のコンパイル。
        let server1 = makeServer collectedFirst
        server1.IsAvailablePublishDiagnostics <- true
        server1.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]
        server1.OpenDocument(uri, source)

        // 2回目は新しいサーバーインスタンスで同一ソースをコンパイルする。
        let server2 = makeServer collectedSecond
        server2.IsAvailablePublishDiagnostics <- true
        server2.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]
        server2.OpenDocument(uri, source)

        let first = collectedFirst |> Seq.toList
        let second = collectedSecond |> Seq.toList

        Assert.NotEmpty(first)
        Assert.NotEmpty(second)

        // 件数が一致することを確認する。
        Assert.Equal(first.Length, second.Length)

        // URI と診断件数がすべて一致することを確認する。
        List.iter2 (fun (uri1: string, count1: int) (uri2: string, count2: int) ->
            Assert.Equal(uri1, uri2)
            Assert.Equal(count1, count2)) first second
