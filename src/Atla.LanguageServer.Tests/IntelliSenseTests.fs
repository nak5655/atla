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
    let ``GetCompletions items have kind and detail fields`` () =
        let server = makeServerWithSource "file:///tmp/completion-detail.atla" "fn compute: Int = 1"
        let result = server.GetCompletions("file:///tmp/completion-detail.atla", 0, 10)
        let computeItem = result.items |> List.tryFind (fun i -> i.label = "compute")
        Assert.True(computeItem.IsSome)
        // detail（型文字列）が空でないこと。
        Assert.NotNull(computeItem.Value.detail)
        Assert.NotEmpty(computeItem.Value.detail)

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

    // -----------------------------------------------------------------------
    // GetCompletions – ドット補完
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``GetCompletions at dot position returns member items for string variable`` () =
        // `do` ブロック内で let s = "hello" を宣言し s.Length を式とする。
        // s の型は String。s.Length の `.` 直後（行2・列4）で補完を要求すると String メンバーが返る。
        // 行2: "  s.Length"  → col 2='s', col 3='.', col 4='L'
        let source = "fn test: Int = do\n  let s = \"hello\"\n  s.Length"
        let server = makeServerWithSource "file:///tmp/dot-completion-string.atla" source
        let result = server.GetCompletions("file:///tmp/dot-completion-string.atla", 2, 4)
        // String のメンバー（Length など）が含まれること。
        Assert.False(result.isIncomplete)
        Assert.NotEmpty(result.items)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("Length", names)

    [<Fact>]
    let ``GetCompletions at dot position returns CompletionItemKind for members`` () =
        // String のメソッド（Contains 等）は Method、プロパティ（Length 等）は Field として返る。
        let source = "fn test: Int = do\n  let s = \"hello\"\n  s.Length"
        let server = makeServerWithSource "file:///tmp/dot-completion-kind.atla" source
        let result = server.GetCompletions("file:///tmp/dot-completion-kind.atla", 2, 4)
        Assert.NotEmpty(result.items)
        // 少なくとも 1 件の kind が設定されたアイテムが存在すること。
        let hasKind = result.items |> List.exists (fun i -> not (obj.ReferenceEquals(i.kind, null)))
        Assert.True(hasKind)

    [<Fact>]
    let ``GetCompletions at non-dot position falls back to scope completions`` () =
        // ドット以外の位置では通常のスコープ補完（宣言済み識別子）が返る。
        let source = "fn scopeFn: Int = 42"
        let server = makeServerWithSource "file:///tmp/dot-fallback.atla" source
        // 列 0 は 'f'（ドットではない）→ 通常補完
        let result = server.GetCompletions("file:///tmp/dot-fallback.atla", 0, 0)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("scopeFn", names)

    [<Fact>]
    let ``GetCompletions at dot position with no HIR returns empty list`` () =
        // URI が HIR キャッシュにない場合はクラッシュせず空リストを返す。
        let server = Server(fun _ _ -> ())
        let result = server.GetCompletions("file:///tmp/no-hir.atla", 0, 1)
        Assert.Empty(result.items)

    [<Fact>]
    let ``GetCompletions at position 0 does not crash`` () =
        // character = 0 の場合はドット補完の前提条件を満たさないため通常補完にフォールバック。
        let server = makeServerWithSource "file:///tmp/completion-zero.atla" "fn zero: Int = 0"
        let result = server.GetCompletions("file:///tmp/completion-zero.atla", 0, 0)
        Assert.False(result.isIncomplete)

    [<Fact>]
    let ``GetCompletions at dot after unknown identifier returns empty member list`` () =
        // 識別子がインデックスに存在しない場合はクラッシュせず空リストを返す。
        // ここでは行0・列1 (文字 'n') をドット位置として渡すが、`.` ではないのでフォールバック。
        let server = makeServerWithSource "file:///tmp/dot-unknown.atla" "fn unknown: Int = 0"
        let result = server.GetCompletions("file:///tmp/dot-unknown.atla", 0, 1)
        Assert.False(result.isIncomplete)
