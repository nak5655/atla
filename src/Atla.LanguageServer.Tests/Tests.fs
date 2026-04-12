namespace Atla.LanguageServer.Tests

open Xunit
open Atla.LanguageServer.Server

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
