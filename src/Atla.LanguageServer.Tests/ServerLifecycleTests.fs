namespace Atla.LanguageServer.Tests

open System.IO
open Newtonsoft.Json.Linq
open Xunit
open Atla.LanguageServer.Server

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

        let publishedUris = published |> Seq.map fst |> Seq.toList
        Assert.Contains("file:///tmp/workspace/in.atla", publishedUris)
        Assert.DoesNotContain("file:///tmp/out.atla", publishedUris)

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
