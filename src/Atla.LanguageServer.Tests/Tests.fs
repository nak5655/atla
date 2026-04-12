namespace Atla.LanguageServer.Tests

open System
open Xunit
open Atla.LanguageServer

type ServerTest() =

    [<Fact>]
    member _.TokenizeTest() =
        let server = Server()
        server.tokenTypes <- [| "keyword"; "comment"; "number"; "string"; "variable" |]
        server.isAvailablePublishDiagnostics <- true

        server.compile("", "fn a(b: Int): Int = \"aaa\"#")
        let tokens = server.tokenize("")

        tokens |> ignore