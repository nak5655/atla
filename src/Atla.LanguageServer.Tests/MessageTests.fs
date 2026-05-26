namespace Atla.LanguageServer.Tests

open System.IO
open System.Text
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

    /// Content-Length は UTF-8 バイト数。本文を ``\r\n\r\n`` 区切りで枠付けする。
    let private frame (json: string) : byte[] =
        let body = Encoding.UTF8.GetBytes json
        let header = Encoding.ASCII.GetBytes(sprintf "Content-Length: %d\r\n\r\n" body.Length)
        Array.append header body

    [<Fact>]
    let ``waitMessageFromStream reads multibyte UTF-8 body and keeps framing aligned`` () =
        // 日本語（マルチバイト UTF-8）を含む didOpen 本文。byte 数 > 文字数 になるため、
        // 旧実装（Content-Length バイト数を「文字数」として読む）ではここでフレーミングが
        // 破綻し、後続メッセージが読めずサーバーが終了していた。
        let didOpenJson =
            """{"method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///g.atla","text":"# 初期パイプ生成\nfn main: () = ()"}}}"""
        let nextJson = """{"id":7,"method":"shutdown","params":{}}"""

        use ms = new MemoryStream(Array.append (frame didOpenJson) (frame nextJson))

        // 1通目: マルチバイト本文が正しく UTF-8 でパースされる。
        match waitMessageFromStream ms with
        | Some(NotificationMsg(_, content)) ->
            Assert.Equal(Some "textDocument/didOpen", messageMethod content)
            let text = content.["params"].["textDocument"].["text"].ToString()
            Assert.Contains("初期パイプ生成", text)
        | other -> Assert.Fail(sprintf "expected didOpen notification, got %A" other)

        // 2通目: 過剰/過少読み取りが無く、後続メッセージもずれずに読める。
        match waitMessageFromStream ms with
        | Some(RequestMsg(_, content)) ->
            Assert.Equal(Some 7, tryRequestId content)
            Assert.Equal(Some "shutdown", messageMethod content)
        | other -> Assert.Fail(sprintf "expected shutdown request, got %A" other)

        // 3通目: 終端では None。
        Assert.True((waitMessageFromStream ms).IsNone)

    [<Fact>]
    let ``waitMessageFromStream returns None on end of stream`` () =
        use ms = new MemoryStream([||])
        Assert.True((waitMessageFromStream ms).IsNone)
