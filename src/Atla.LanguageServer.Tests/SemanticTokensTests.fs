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

        let tokens = server.InternalTokenize("fn main: Int = 0")

        Assert.NotEmpty(tokens)

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

        let lf = "fn main: Int = do\n    let x = 1\n    \"ok\""
        let crlf = "fn main: Int = do\r\n    let x = 1\r\n    \"ok\""
        let bom = "\uFEFFfn main: Int = do\n    let x = 1\n    \"ok\""

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
                "semanticTokens": { "tokenTypes": ["comment", "operator"] }
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
