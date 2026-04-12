namespace Atla.LanguageServer.Tests

open Newtonsoft.Json.Linq
open Xunit
open Atla.LanguageServer.LSPMessage

module MessageTests =
    [<Fact>]
    let ``try request id parses number and numeric string`` () =
        let intId = JObject.Parse("""{ "id": 42 }""")
        let stringId = JObject.Parse("""{ "id": "24" }""")

        Assert.Equal(Some 42, tryRequestId intId)
        Assert.Equal(Some 24, tryRequestId stringId)

    [<Fact>]
    let ``message method and params are extracted`` () =
        let content = JObject.Parse("""{ "method": "initialize", "params": { "x": 1 } }""")

        Assert.Equal(Some "initialize", messageMethod content)
        Assert.True(messageParams content |> Option.isSome)
