namespace Atla.LanguageServer.Tests

open Newtonsoft.Json.Linq
open Xunit
open Atla.LanguageServer.Server
open Atla.LanguageServer.Program

module ServerTests =

    [<Fact>]
    let ``internal tokenize returns data for simple program`` () =
        let server = Server()
        server.TokenTypes <- [| "keyword"; "number"; "string"; "variable"; "type" |]

        let tokens = server.InternalTokenize("fn main: Int = 0")

        Assert.NotEmpty(tokens)

    [<Fact>]
    let ``tokenize returns empty list when uri is not opened`` () =
        let server = Server()
        server.TokenTypes <- [| "keyword"; "number"; "string"; "variable"; "type" |]

        let tokens = server.Tokenize("file:///not-opened.atla")

        Assert.Empty(tokens)

    [<Fact>]
    let ``initialize capability does not advertise documentHighlightProvider`` () =
        let server = Server()
        let content = JObject.Parse("""
        {
          "params": {
            "capabilities": {
              "textDocument": {
                "semanticTokens": { "tokenTypes": ["keyword", "number", "string", "variable", "type"] },
                "publishDiagnostics": { "relatedInformation": true }
              }
            }
          }
        }
        """)

        let result = server.Initialize(content)

        Assert.False(result.capabilities.documentHighlightProvider)

    [<Fact>]
    let ``normalize uri makes file key deterministic`` () =
        let server = Server()

        let normalized = server.TryNormalizeUri("file:///tmp/../tmp/test.atla")

        Assert.Equal(Some("file:///tmp/test.atla"), normalized)

    [<Fact>]
    let ``did open, change, close lifecycle publishes diagnostics and clears buffer`` () =
        let published = ResizeArray<string * int>()
        let server =
            Server(fun uri diagnostics ->
                published.Add(uri, diagnostics.Length))

        server.IsAvailablePublishDiagnostics <- true
        server.TokenTypes <- [| "keyword"; "number"; "string"; "variable"; "type" |]

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

module ProgramTests =

    [<Fact>]
    let ``shutdown and exit follow lsp exit code rule`` () =
        let state0 = false
        let state1 = nextShutdownState state0 "initialize"
        let state2 = nextShutdownState state1 "shutdown"

        Assert.False(state1)
        Assert.True(state2)
        Assert.Equal(0, exitCodeFromShutdownState state2)
        Assert.Equal(1, exitCodeFromShutdownState false)
