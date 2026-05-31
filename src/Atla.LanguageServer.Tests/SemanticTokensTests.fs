namespace Atla.LanguageServer.Tests

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Xunit
open Atla.LanguageServer.Server
open Atla.LanguageServer.LSPTypes

module SemanticTokensTests =
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
    let ``internal tokenize returns data for simple program`` () =
        let server = Server()
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]

        let tokens = server.InternalTokenize("fn main: Int\n    0")

        Assert.NotEmpty(tokens)

    [<Fact>]
    let ``semantic token length includes rightmost character`` () =
        let server = Server()
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]

        let tokens = server.InternalTokenize("fn")

        Assert.Equal<uint32 list>([ 0u; 0u; 2u; 0u; 0u ], tokens)

    [<Fact>]
    let ``tokenize returns empty list when uri is not opened`` () =
        let server = Server()
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]

        let tokens = server.Tokenize("file:///not-opened.atla")

        Assert.Empty(tokens)

    [<Fact>]
    let ``semantic token delta encoding is deterministic across lf crlf and bom`` () =
        let server = Server()
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]

        let lf = "fn main: Int\n    val x = 1\n    \"ok\""
        let crlf = "fn main: Int\r\n    val x = 1\r\n    \"ok\""
        let bom = "\uFEFFfn main: Int\n    val x = 1\n    \"ok\""

        let tokensLf = server.InternalTokenize(lf)
        let tokensCrlf = server.InternalTokenize(crlf)
        let tokensBom = server.InternalTokenize(bom)

        Assert.Equal<uint32 list>(tokensLf, tokensCrlf)
        Assert.Equal<uint32 list>(tokensLf, tokensBom)

    [<Fact>]
    let ``initialize with unknown token types yields empty legend and fallback reason`` () =
        let server = Server()
        let content = JObject.Parse("""
        {
          "params": {
            "capabilities": {
              "textDocument": {
                "semanticTokens": { "tokenTypes": ["operator", "namespace"] }
              }
            }
          }
        }
        """)

        let result = server.Initialize(content)

        Assert.Empty(result.capabilities.semanticTokensProvider.legend.tokenTypes)
        Assert.True(server.SemanticTokensFallbackReason.IsSome)

    [<Fact>]
    let ``semantic tokens response snapshot keeps empty resultId and data focus`` () =
        let payload = SemanticTokens("", [ 0u; 0u; 2u; 0u; 0u ])
        let json = JsonConvert.SerializeObject(payload, Formatting.None)

        Assert.Contains("\"resultId\":\"\"", json)
        Assert.Contains("\"data\":[0,0,2,0,0]", json)

    [<Fact>]
    let ``internal tokenize emits comment token when client advertises comment type`` () =
        let server = Server()
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string"; "comment" |]

        let tokens = server.InternalTokenize("# hi")

        // Single comment spanning columns 0..4 on line 0; encoded with token type index 5.
        Assert.Equal<uint32 list>([ 0u; 0u; 4u; 5u; 0u ], tokens)

    [<Fact>]
    let ``internal tokenize drops comment when client does not advertise comment type`` () =
        let server = Server()
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]

        let tokens = server.InternalTokenize("# hi\nval x = 1")

        // Comment is filtered out by the legend; only `val`, `x`, and `1` remain.
        // Encoded as: val (line 1 col 0, len 3, keyword=0), x (delta col 4, len 1, variable=2), 1 (delta col 4, len 1, number=3).
        Assert.Equal<uint32 list>(
            [
                1u; 0u; 3u; 0u; 0u
                0u; 4u; 1u; 2u; 0u
                0u; 4u; 1u; 3u; 0u
            ],
            tokens
        )
