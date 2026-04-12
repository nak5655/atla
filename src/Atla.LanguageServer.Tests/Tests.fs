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

        let tokens = server.InternalTokenize("fn main: () = 1")

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
