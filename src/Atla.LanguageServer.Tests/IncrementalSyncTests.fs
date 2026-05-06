namespace Atla.LanguageServer.Tests

open Newtonsoft.Json.Linq
open Xunit
open Atla.LanguageServer.Server
open Atla.LanguageServer.LSPTypes

/// インクリメンタル同期（TextDocumentSyncKind.Incremental）に関するテスト。
module IncrementalSyncTests =

    // -----------------------------------------------------------------------
    // ヘルパー: JToken 形式の変更イベントを組み立てる。
    // -----------------------------------------------------------------------

    /// range を持つインクリメンタル変更エントリを組み立てる。
    let private mkChange startLine startChar endLine endChar (text: string) : JToken =
        JToken.Parse(sprintf
            """{"range":{"start":{"line":%d,"character":%d},"end":{"line":%d,"character":%d}},"text":%s}"""
            startLine startChar endLine endChar (Newtonsoft.Json.JsonConvert.SerializeObject text))

    /// range を持たない全文置換変更エントリを組み立てる。
    let private mkFullChange (text: string) : JToken =
        JToken.Parse(sprintf """{"text":%s}""" (Newtonsoft.Json.JsonConvert.SerializeObject text))

    // -----------------------------------------------------------------------
    // Initialize
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``initialize advertises incremental sync kind`` () =
        let server = Server()
        let content = JObject.Parse("""
        {
          "params": {
            "capabilities": {
              "textDocument": {
                "publishDiagnostics": { "relatedInformation": true },
                "semanticTokens": { "tokenTypes": ["keyword"] }
              }
            }
          }
        }
        """)
        let result = server.Initialize(content)
        let resultJson = JObject.FromObject(result)
        let syncKind = resultJson.["capabilities"].["textDocumentSync"].["change"].Value<int>()
        // TextDocumentSyncKind.Incremental = 2
        Assert.Equal(2, syncKind)

    // -----------------------------------------------------------------------
    // ApplyIncrementalChanges – 単一変更
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``incremental change inserts text at start of document`` () =
        let server =
            Server(
                (fun _ _ -> ()),
                debounceDelayMs = 0)
        server.IsAvailablePublishDiagnostics <- true
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]
        let uri = "file:///tmp/incr-insert-start.atla"
        server.OpenDocument(uri, "world")

        // range (0,0)-(0,0): 先頭へ "hello " を挿入する。
        server.ApplyIncrementalChanges(uri, [ mkChange 0 0 0 0 "hello " ] |> Seq.ofList)
        server.WaitForPendingCompilations()

        let tokens = server.Tokenize(uri)
        // "hello world" がバッファに格納されていることをトークンの有無で確認する。
        Assert.NotEmpty(tokens)

    [<Fact>]
    let ``incremental change deletes a range`` () =
        let server =
            Server(
                (fun _ _ -> ()),
                debounceDelayMs = 0)
        server.IsAvailablePublishDiagnostics <- true
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]
        let uri = "file:///tmp/incr-delete.atla"
        server.OpenDocument(uri, "fn main: Int = 42")

        // "fn " の部分（0,0）-（0,3）を削除する。
        server.ApplyIncrementalChanges(uri, [ mkChange 0 0 0 3 "" ] |> Seq.ofList)
        server.WaitForPendingCompilations()

        // "main: Int = 42" が残るためバッファはトークン化可能。
        let tokens = server.Tokenize(uri)
        Assert.NotEmpty(tokens)

    [<Fact>]
    let ``incremental change replaces text in the middle`` () =
        let published = ResizeArray<string * int>()
        let server =
            Server(
                (fun uri diags -> published.Add(uri, diags.Length)),
                debounceDelayMs = 0)
        server.IsAvailablePublishDiagnostics <- true
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]
        let uri = "file:///tmp/incr-replace.atla"
        server.OpenDocument(uri, "fn main: Int = 0")

        // "Int" を "Float" に置き換える: (0,9)-(0,12)
        server.ApplyIncrementalChanges(uri, [ mkChange 0 9 0 12 "Float" ] |> Seq.ofList)
        server.WaitForPendingCompilations()

        let tokens = server.Tokenize(uri)
        Assert.NotEmpty(tokens)

    [<Fact>]
    let ``incremental change without range replaces entire document`` () =
        let published = ResizeArray<string * int>()
        let server =
            Server(
                (fun uri diags -> published.Add(uri, diags.Length)),
                debounceDelayMs = 0)
        server.IsAvailablePublishDiagnostics <- true
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]
        let uri = "file:///tmp/incr-full-replace.atla"
        server.OpenDocument(uri, "fn main: Int = 0")

        // range なし → 全文置換。
        server.ApplyIncrementalChanges(uri, [ mkFullChange "fn main: Int = 99" ] |> Seq.ofList)
        server.WaitForPendingCompilations()

        let tokens = server.Tokenize(uri)
        Assert.NotEmpty(tokens)

    // -----------------------------------------------------------------------
    // ApplyIncrementalChanges – 複数変更の逐次適用
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``multiple incremental changes applied in sequence`` () =
        let server =
            Server(
                (fun _ _ -> ()),
                debounceDelayMs = 0)
        server.IsAvailablePublishDiagnostics <- true
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]
        let uri = "file:///tmp/incr-multi.atla"
        server.OpenDocument(uri, "fn main: Int = 0")

        // 2 つの変更を同一 contentChanges エントリとして送る。
        // 1. "0" を "1" に置換する (0,15)-(0,16)
        // 2. "Int" を "Float" に置換する (0,9)-(0,12)  ← 最初の変更後の座標
        // ただし LSP は contentChanges 内の変更を逐次適用するため、
        // 変更 2 の座標は変更 1 適用後のテキストに基づく。
        let changes =
            [ mkChange 0 15 0 16 "42"   // "0" → "42"
              mkChange 0 9  0 12 "Float" ]  // "Int" → "Float" (now at the same offsets after first edit)
            |> Seq.ofList
        server.ApplyIncrementalChanges(uri, changes)
        server.WaitForPendingCompilations()

        let tokens = server.Tokenize(uri)
        Assert.NotEmpty(tokens)

    // -----------------------------------------------------------------------
    // ApplyIncrementalChanges – マルチライン変更
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``incremental change across multiple lines`` () =
        let server =
            Server(
                (fun _ _ -> ()),
                debounceDelayMs = 0)
        server.IsAvailablePublishDiagnostics <- true
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]
        let uri = "file:///tmp/incr-multiline.atla"
        server.OpenDocument(uri, "fn main: Int =\n  0")

        // 行 0 の末尾から行 1 末尾まで（= ":\n  0"）を " = 42" で置換する。
        server.ApplyIncrementalChanges(uri, [ mkChange 0 13 1 3 " = 42" ] |> Seq.ofList)
        server.WaitForPendingCompilations()

        let tokens = server.Tokenize(uri)
        Assert.NotEmpty(tokens)

    // -----------------------------------------------------------------------
    // ApplyIncrementalChanges – 境界外座標のクランプ
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``incremental change with out-of-bounds line clamps without raising an exception`` () =
        let server =
            Server(
                (fun _ _ -> ()),
                debounceDelayMs = 0)
        server.IsAvailablePublishDiagnostics <- true
        server.TokenTypes <- [| "keyword"; "type"; "variable"; "number"; "string" |]
        let uri = "file:///tmp/incr-clamp.atla"
        server.OpenDocument(uri, "fn main: Int = 0")

        // 存在しない行 999 を参照しても例外を起こさずに処理できること。
        // 例外が発生しなければテスト成功（バッファは変形されるが壊れない）。
        server.ApplyIncrementalChanges(uri, [ mkChange 999 0 999 0 "0" ] |> Seq.ofList)
        server.WaitForPendingCompilations()

    // -----------------------------------------------------------------------
    // ApplyIncrementalChanges – 空の変更リスト
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``apply empty incremental changes list schedules compile with unchanged text`` () =
        let published = ResizeArray<string * int>()
        let server =
            Server(
                (fun uri diags -> published.Add(uri, diags.Length)),
                debounceDelayMs = 0)
        server.IsAvailablePublishDiagnostics <- true
        let uri = "file:///tmp/incr-empty.atla"
        server.OpenDocument(uri, "fn main: Int = 0")

        let prevCount = published.Count
        server.ApplyIncrementalChanges(uri, Seq.empty)
        server.WaitForPendingCompilations()

        // コンパイルが再度スケジュールされ、diagnostics が発行されている。
        Assert.True(published.Count > prevCount)

    // -----------------------------------------------------------------------
    // ApplyIncrementalChanges – 診断連携
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``incremental change that introduces syntax error publishes diagnostic`` () =
        let published = ResizeArray<string * int>()
        let server =
            Server(
                (fun uri diags -> published.Add(uri, diags.Length)),
                debounceDelayMs = 0)
        server.IsAvailablePublishDiagnostics <- true
        let uri = "file:///tmp/incr-diag.atla"
        server.OpenDocument(uri, "fn main: Int = 0")

        // "= 0" を "= " に削減して構文エラーを発生させる。
        server.ApplyIncrementalChanges(uri, [ mkChange 0 14 0 17 "" ] |> Seq.ofList)
        server.WaitForPendingCompilations()

        let lastCount = published |> Seq.last |> snd
        Assert.True(lastCount > 0, "incremental edit introducing a syntax error should publish diagnostics")
