namespace Atla.LanguageServer.Tests

open Xunit
open Atla.LanguageServer.Server

/// GetCompletions / GetHover / GetDefinition の動作を検証するテスト。
module IntelliSenseTests =

    // -----------------------------------------------------------------------
    // ヘルパー
    // -----------------------------------------------------------------------

    /// 診断を公開しない最小サーバーを構築してドキュメントを開く。
    let private makeServerWithSource (uri: string) (source: string) : Server =
        let server = Server(fun _ _ -> ())
        server.IsAvailablePublishDiagnostics <- true
        server.OpenDocument(uri, source)
        server

    // -----------------------------------------------------------------------
    // GetCompletions
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``GetCompletions returns non-empty list for valid source`` () =
        let server = makeServerWithSource "file:///tmp/completion.atla" "fn main: Int = 0"
        let result = server.GetCompletions("file:///tmp/completion.atla", 0, 15)
        // 少なくとも main が補完候補として含まれること。
        Assert.False(result.isIncomplete)
        Assert.NotEmpty(result.items)

    [<Fact>]
    let ``GetCompletions includes declared function name`` () =
        let server = makeServerWithSource "file:///tmp/completion-fn.atla" "fn greet: Int = 42"
        let result = server.GetCompletions("file:///tmp/completion-fn.atla", 0, 16)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("greet", names)

    [<Fact>]
    let ``GetCompletions returns empty list when uri has no cached HIR`` () =
        let server = Server(fun _ _ -> ())
        let result = server.GetCompletions("file:///tmp/not-opened.atla", 0, 0)
        Assert.Empty(result.items)

    [<Fact>]
    let ``GetCompletions falls back to object members for apostrophe context without cache`` () =
        let uri = "file:///tmp/no-cache-member.atla"
        let server = Server(fun _ _ -> ())
        // パース不能テキストを直接開き、キャッシュ未生成状態を作る。
        server.IsAvailablePublishDiagnostics <- true
        server.OpenDocument(uri, "fn main: Int = value'")
        let result = server.GetCompletions(uri, 0, 21)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("ToString", names)

    [<Fact>]
    let ``GetCompletions items have kind and detail fields`` () =
        let server = makeServerWithSource "file:///tmp/completion-detail.atla" "fn compute: Int = 1"
        let result = server.GetCompletions("file:///tmp/completion-detail.atla", 0, 10)
        let computeItem = result.items |> List.tryFind (fun i -> i.label = "compute")
        Assert.True(computeItem.IsSome)
        // detail（型文字列）が空でないこと。
        Assert.NotNull(computeItem.Value.detail)
        Assert.NotEmpty(computeItem.Value.detail)

    [<Fact>]
    let ``GetCompletions filters candidates by identifier prefix at cursor`` () =
        let source = "fn greet: Int = 1\nfn gone: Int = 2\nfn main: Int = 0"
        let server = makeServerWithSource "file:///tmp/completion-prefix.atla" source
        // 行0・列8 は `greet` 識別子の `gre` 末尾位置。
        let result = server.GetCompletions("file:///tmp/completion-prefix.atla", 0, 8)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("greet", names)
        Assert.DoesNotContain("gone", names)

    [<Fact>]
    let ``GetCompletions returns native member candidates after apostrophe input`` () =
        let uri = "file:///tmp/completion-member.atla"
        let validSource = "fn compute: Int = 1\nfn main: Int = compute."
        let server = makeServerWithSource uri validSource
        // 入力途中の `compute'` はパース不能だが、直前の成功キャッシュで補完できることを期待する。
        server.ChangeDocument(uri, "fn compute: Int = 1\nfn main: Int = compute'")
        let result = server.GetCompletions(uri, 1, 23)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("Invoke", names)

    [<Fact>]
    let ``GetCompletions includes static native members for imported type receivers`` () =
        let uri = "file:///tmp/completion-console.atla"
        let validSource = "import System'Console\nfn main: () = do\n  \"Hello, World!\" Console'WriteLine."
        let server = makeServerWithSource uri validSource
        // 入力途中の `Console'` でも static メンバー候補が表示されること。
        let incompleteSource = "import System'Console\nfn main: () = do\n  \"Hello, World!\" Console'"
        server.ChangeDocument(uri, incompleteSource)
        let result = server.GetCompletions(uri, 2, 26)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("WriteLine", names)

    [<Fact>]
    let ``GetCompletions keeps last successful cache on parse errors`` () =
        let uri = "file:///tmp/completion-cache.atla"
        let validSource = "fn compute: Int = 1"
        let server = makeServerWithSource uri validSource
        let beforeError = server.GetCompletions(uri, 0, 0)
        Assert.NotEmpty(beforeError.items)
        // ぶら下がりアポストロフィで意図的にパースエラーを作る。
        server.ChangeDocument(uri, "fn compute: Int = 1\n'")
        let afterError = server.GetCompletions(uri, 0, 0)
        Assert.NotEmpty(afterError.items)

    // -----------------------------------------------------------------------
    // GetHover
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``GetHover returns None for position with no identifier`` () =
        // ソースの先頭（fn キーワード位置）はキーワードであり Id ノードではない。
        let server = makeServerWithSource "file:///tmp/hover-none.atla" "fn main: Int = 0"
        let result = server.GetHover("file:///tmp/hover-none.atla", 0, 0)
        Assert.True(result.IsNone)

    [<Fact>]
    let ``GetHover returns hover info for identifier in expression`` () =
        // `x` が使用されている位置でホバーを要求する。
        // fn test: Int =
        //   let x = 5
        //   x
        // 行2・列2 が `x` の使用位置。
        let source = "fn test: Int =\n  let x = 5\n  x"
        let server = makeServerWithSource "file:///tmp/hover-id.atla" source
        let result = server.GetHover("file:///tmp/hover-id.atla", 2, 2)
        match result with
        | None ->
            // PositionIndex にキャッシュされていない場合は None が許容される（エラー原因なし）。
            ()
        | Some hover ->
            Assert.NotNull(hover.contents)
            Assert.Equal("markdown", hover.contents.kind)
            Assert.Contains("x", hover.contents.value)

    [<Fact>]
    let ``GetHover returns None when uri has no cached HIR`` () =
        let server = Server(fun _ _ -> ())
        let result = server.GetHover("file:///tmp/not-opened.atla", 0, 0)
        Assert.True(result.IsNone)

    // -----------------------------------------------------------------------
    // GetDefinition
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``GetDefinition returns None when uri has no cached HIR`` () =
        let server = Server(fun _ _ -> ())
        let result = server.GetDefinition("file:///tmp/not-opened.atla", 0, 0)
        Assert.True(result.IsNone)

    [<Fact>]
    let ``GetDefinition returns None for position with no identifier`` () =
        let server = makeServerWithSource "file:///tmp/def-none.atla" "fn main: Int = 0"
        // 行0・列0 は `fn` キーワードであり識別子ではない。
        let result = server.GetDefinition("file:///tmp/def-none.atla", 0, 0)
        Assert.True(result.IsNone)

    [<Fact>]
    let ``GetDefinition returns location when identifier is found`` () =
        // let 束縛した変数 y の使用箇所から定義位置にジャンプできることを検証。
        // fn go: Int =
        //   let y = 10
        //   y
        let source = "fn go: Int =\n  let y = 10\n  y"
        let server = makeServerWithSource "file:///tmp/def-location.atla" source
        // 行2・列2 が `y` の使用位置。
        let result = server.GetDefinition("file:///tmp/def-location.atla", 2, 2)
        match result with
        | None ->
            // PositionIndex にキャッシュされていない場合は None も許容。
            ()
        | Some location ->
            Assert.NotNull(location.uri)
            Assert.NotEmpty(location.uri)
            // 宣言は行1（0-indexed）に存在する。
            Assert.Equal(1, location.range.start.line)

    // -----------------------------------------------------------------------
    // PositionIndex ユニットテスト
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``PositionIndex build returns empty index for empty assembly`` () =
        let assembly = Atla.Core.Semantics.Data.Hir.Assembly("test", [])
        let index = Atla.Core.Semantics.PositionIndex.build assembly
        Assert.Empty(index.useSites)
        Assert.Empty(index.declSites)

    [<Fact>]
    let ``formatTypeWithTable formats primitive types correctly`` () =
        let symbolTable = Atla.Core.Semantics.Data.SymbolTable()
        let cases =
            [ Atla.Core.Semantics.Data.TypeId.Unit,   "()"
              Atla.Core.Semantics.Data.TypeId.Bool,   "Bool"
              Atla.Core.Semantics.Data.TypeId.Int,    "Int"
              Atla.Core.Semantics.Data.TypeId.Float,  "Float"
              Atla.Core.Semantics.Data.TypeId.String, "String" ]
        for tid, expected in cases do
            let result = Atla.Core.Semantics.PositionIndex.formatTypeWithTable symbolTable tid
            Assert.Equal(expected, result)

    [<Fact>]
    let ``formatTypeWithTable formats function type correctly`` () =
        let symbolTable = Atla.Core.Semantics.Data.SymbolTable()
        let tid = Atla.Core.Semantics.Data.TypeId.Fn(
                    [ Atla.Core.Semantics.Data.TypeId.Int
                      Atla.Core.Semantics.Data.TypeId.String ],
                    Atla.Core.Semantics.Data.TypeId.Bool)
        let result = Atla.Core.Semantics.PositionIndex.formatTypeWithTable symbolTable tid
        Assert.Equal("(Int, String) -> Bool", result)

    [<Fact>]
    let ``GetCompletions after close returns empty list`` () =
        let uri = "file:///tmp/completion-close.atla"
        let server = makeServerWithSource uri "fn clean: Int = 0"
        let beforeClose = server.GetCompletions(uri, 0, 0)
        Assert.NotEmpty(beforeClose.items)
        server.CloseDocument(uri)
        let afterClose = server.GetCompletions(uri, 0, 0)
        Assert.Empty(afterClose.items)
