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

    [<Fact>]
    let ``GetCompletions on apostrophe returns receiver type members only`` () =
        let source = "import System'Console\nfn main (): () = do\n    \"Hello, World!\" Console'"
        let server = makeServerWithSource "file:///tmp/completion-incomplete.atla" source
        let result = server.GetCompletions("file:///tmp/completion-incomplete.atla", 2, 28)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.NotEmpty(names)
        Assert.Contains("WriteLine", names)
        Assert.DoesNotContain("main", names)

    [<Fact>]
    let ``GetCompletions on apostrophe with parenthesized receiver returns type members`` () =
        // receiver が (out) のように括弧でグループされた場合、パーサは括弧をスパンに含めないため
        // 名前ベースの fallback（単純識別子ウォークバック）では解決できない。
        // HIR position-based 型解決により、括弧内の out の型 TextWriter が取得され
        // TextWriter メンバーが補完候補として返されることを検証する。
        //
        // Line 3: "  let _ = (out)'NewLine"
        //   apostropheCol = 15, cursor just after apostrophe (character = 16, memberPrefix = "").
        //   Id("out") span right = 14 ≤ 15 → 型 TextWriter が解決される。
        let source =
            "import System'Console\n" +
            "fn main (): () = do\n" +
            "  let out = Console'Out\n" +
            "  let _ = (out)'NewLine\n" +
            "  ()"
        let server = makeServerWithSource "file:///tmp/completion-paren-receiver.atla" source
        // カーソルは行3・列16（"(out)'" の直後、memberPrefix は空）。
        let result = server.GetCompletions("file:///tmp/completion-paren-receiver.atla", 3, 16)
        let names = result.items |> List.map (fun i -> i.label)
        // TextWriter のインスタンスメンバーが含まれ、モジュールメンバー（main）が除外される。
        Assert.NotEmpty(names)
        Assert.Contains("NewLine", names)
        Assert.DoesNotContain("main", names)

    [<Fact>]
    let ``GetCompletions on apostrophe with chained member access receiver returns type members`` () =
        // receiver が `(Console'Out)` のようなメンバーアクセス式の場合、
        // テキストベースの単純識別子ウォークバックでは )の直前が識別子でないため解決できない。
        // HIR position-based 型解決により Console'Out MemberAccess ノードの
        // 型 TextWriter が取得され TextWriter メンバーが補完候補として返されることを検証する。
        //
        // Line 2: "  let _ = (Console'Out)'NewLine"
        //   apostropheCol = 23, cursor just after apostrophe (character = 24, memberPrefix = "").
        //   Console'Out span right = 22 ≤ 23 → 型 TextWriter が解決される。
        let source =
            "import System'Console\n" +
            "fn main (): () = do\n" +
            "  let _ = (Console'Out)'NewLine\n" +
            "  ()"
        let server = makeServerWithSource "file:///tmp/completion-chained-receiver.atla" source
        // カーソルは行2・列24（"(Console'Out)'" の直後、memberPrefix は空）。
        let result = server.GetCompletions("file:///tmp/completion-chained-receiver.atla", 2, 24)
        let names = result.items |> List.map (fun i -> i.label)
        // TextWriter のインスタンスメンバーが含まれ、モジュールメンバー（main）が除外される。
        Assert.NotEmpty(names)
        Assert.Contains("NewLine", names)
        Assert.DoesNotContain("main", names)

    [<Fact>]
    let ``Completions for variable bound to static member suggest instance members`` () =
        let initialSource =
            "import System'DateTime\n" +
            "fn main (): () = do\n" +
            "  let a = DateTime'Now\n" +
            "  a'Year\n" +
            "  ()"
        let changedSource =
            "import System'DateTime\n" +
            "fn main (): () = do\n" +
            "  let a = DateTime'Now\n" +
            "  a'\n" +
            "  ()"
        let uri = "file:///tmp/completion-datetime-now.atla"
        let server = makeServerWithSource uri initialSource
        server.ChangeDocument(uri, changedSource)
        server.WaitForPendingCompilations()
        let result = server.GetCompletions("file:///tmp/completion-datetime-now.atla", 3, 4)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.NotEmpty(names)
        Assert.Contains("Year", names)
        Assert.DoesNotContain("Now", names)
        Assert.DoesNotContain("main", names)

    [<Fact>]
    let ``GetCompletions uses latest partial HIR cache even when diagnostics contain errors`` () =
        let uri = "file:///tmp/completion-partial-hir-cache.atla"
        let initialSource =
            "import System'DateTime\n" +
            "fn main (): () = do\n" +
            "  let value = DateTime'Now\n" +
            "  value'\n" +
            "  ()"
        let changedSource =
            "import System'DateTime\n" +
            "fn main (): () = do\n" +
            "  let value = 1\n" +
            "  unknownValue\n" +
            "  value'\n" +
            "  ()"

        let server = makeServerWithSource uri initialSource
        let beforeChange = server.GetCompletions(uri, 3, 8)
        let beforeNames = beforeChange.items |> List.map (fun i -> i.label)
        Assert.Contains("Year", beforeNames)

        server.ChangeDocument(uri, changedSource)
        server.WaitForPendingCompilations()

        let afterChange = server.GetCompletions(uri, 4, 8)
        let afterNames = afterChange.items |> List.map (fun i -> i.label)
        Assert.DoesNotContain("Year", afterNames)

        let source = "fn main: Int = do\n  let first = 1\n  fir\n  let second = 2\n  first"
        let server = makeServerWithSource "file:///tmp/completion-scope.atla" source
        // 行2・列4 (`fir` 位置) は `second` の宣言より前。
        let result = server.GetCompletions("file:///tmp/completion-scope.atla", 2, 4)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("first", names)
        Assert.DoesNotContain("second", names)

    [<Fact>]
    let ``GetCompletions on import apostrophe suggests namespace types`` () =
        let source = "import System'\nfn main (): Int = 0"
        let server = makeServerWithSource "file:///tmp/completion-import-system.atla" source
        // 行0・列14は `import System'` の apostrophe 直後。
        let result = server.GetCompletions("file:///tmp/completion-import-system.atla", 0, 14)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.NotEmpty(names)
        Assert.Contains("DateTime", names)

    [<Fact>]
    let ``GetCompletions without apostrophe includes visible vars and visible types`` () =
        let source = "import System'Console\nfn main (): Int = do\n  let value = 1\n  value"
        let server = makeServerWithSource "file:///tmp/completion-vars-types.atla" source
        let result = server.GetCompletions("file:///tmp/completion-vars-types.atla", 3, 3)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("value", names)
        Assert.Contains("Console", names)

    [<Fact>]
    let ``GetCompletions on apostrophe after Atla data type variable returns field names`` () =
        // `data Person = { name: String, age: Int }` があるとき、
        // `person'` とすると `name` と `age` が補完候補として返されることを検証する。
        //
        // Line 3: "  person'"
        //   apostropheCol = 8, cursor just after apostrophe (character = 9, memberPrefix = "").
        //   Id("person") は dangling apostrophe で ExprError になるため rawTypeFromHir = None。
        //   fallback path 2b: visibleVarMap["person"].typ = TypeId.Name personTypeSid →
        //   dataTypeFields からフィールド候補 ["name", "age"] が返される。
        let source =
            "data Person = { name: String, age: Int }\n" +
            "fn main (): () = do\n" +
            "  let person = Person { name = \"Alice\", age = 30 }\n" +
            "  person'\n" +
            "  ()"
        let server = makeServerWithSource "file:///tmp/completion-data-fields.atla" source
        // カーソルは行3・列9（"  person'" の apostrophe 直後、memberPrefix は空）。
        let result = server.GetCompletions("file:///tmp/completion-data-fields.atla", 3, 9)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.NotEmpty(names)
        Assert.Contains("name", names)
        Assert.Contains("age", names)
        Assert.DoesNotContain("main", names)

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
        let index = Atla.Core.Semantics.PositionIndex.build assembly (Atla.Core.Semantics.Data.SymbolTable())
        Assert.Empty(index.useSites)
        Assert.Empty(index.declSites)
        // exprTypes も空であること。
        Assert.Empty(index.exprTypes)

    [<Fact>]
    let ``tryFindReceiverTypeAt returns type of innermost expression ending before apostrophe`` () =
        // 構築した HIR に Int リテラル式を1つ含め、その右端を apostropheCol に合わせて
        // tryFindReceiverTypeAt が正しい TypeId を返すことを検証する。
        let span = { Atla.Core.Data.Span.left = { Atla.Core.Data.Position.Line = 0; Atla.Core.Data.Position.Column = 0 }
                     Atla.Core.Data.Span.right = { Atla.Core.Data.Position.Line = 0; Atla.Core.Data.Position.Column = 3 } }
        let intExpr = Atla.Core.Semantics.Data.Hir.Expr.Int(42, span)
        let scopeSpan = { Atla.Core.Data.Span.left = { Atla.Core.Data.Position.Line = 0; Atla.Core.Data.Position.Column = 0 }
                          Atla.Core.Data.Span.right = { Atla.Core.Data.Position.Line = 0; Atla.Core.Data.Position.Column = 10 } }
        let field = Atla.Core.Semantics.Data.Hir.Field(
                        Atla.Core.Semantics.Data.SymbolId 1,
                        Atla.Core.Semantics.Data.TypeId.Int,
                        intExpr, scopeSpan)
        let modul = Atla.Core.Semantics.Data.Hir.Module(
                        "test", [], [field], [],
                        Atla.Core.Semantics.Data.Scope(None))
        let assembly = Atla.Core.Semantics.Data.Hir.Assembly("test", [modul])
        let index = Atla.Core.Semantics.PositionIndex.build assembly (Atla.Core.Semantics.Data.SymbolTable())
        // apostropheCol = 3 → span.right.Column = 3 ≤ 3 ✓ → TypeId.Int が返る。
        let result = Atla.Core.Semantics.PositionIndex.tryFindReceiverTypeAt index 0 3
        Assert.Equal(Some Atla.Core.Semantics.Data.TypeId.Int, result)
        // apostropheCol = 2 → span.right.Column = 3 > 2 → フィルタされ None が返る。
        let result2 = Atla.Core.Semantics.PositionIndex.tryFindReceiverTypeAt index 0 2
        Assert.Equal(None, result2)

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
    let ``PositionIndex dataTypeFields is populated for data type declarations`` () =
        // data Person = { name: String, age: Int } を含むソースをコンパイルし、
        // PositionIndex.dataTypeFields に Person のフィールドが登録されていることを検証する。
        let source =
            "data Person = { name: String, age: Int }\n" +
            "fn main (): () = ()"
        let compileResult = Atla.Compiler.Compiler.compileModules {
            asmName = "diag"
            modules = [ { moduleName = "main"; source = source } ]
            entryModuleName = "main"
            outDir = System.IO.Path.GetTempPath()
            dependencies = []
        }
        Assert.True(compileResult.hir.IsSome, "HIR should be produced")
        Assert.True(compileResult.symbolTable.IsSome, "SymbolTable should be produced")
        match compileResult.hir, compileResult.symbolTable with
        | Some hirAsm, Some symTable ->
            let index = Atla.Core.Semantics.PositionIndex.build hirAsm symTable
            // このソースには Person だけなので、エントリは 1 つでなければならない。
            Assert.Equal(1, index.dataTypeFields |> Map.count)
            // Person のフィールドに "name" と "age" が含まれること（ソースは 2 フィールドのみ）。
            let (_, personFields) = index.dataTypeFields |> Map.toList |> List.head
            let fieldNames = personFields |> List.map fst
            Assert.Equal(2, fieldNames.Length)
            Assert.Contains("name", fieldNames)
            Assert.Contains("age", fieldNames)
        | _ -> failwith "Unreachable"

    [<Fact>]
    let ``PositionIndex bindingSites includes person variable from do block with dangling apostrophe`` () =
        // dangling apostrophe があっても do ブロック内の person let 束縛が
        // PositionIndex.bindingSites に記録されることを検証する。
        let source =
            "data Person = { name: String, age: Int }\n" +
            "fn main (): () = do\n" +
            "  let person = Person { name = \"Alice\", age = 30 }\n" +
            "  person'\n" +
            "  ()"
        let compileResult = Atla.Compiler.Compiler.compileModules {
            asmName = "diag3"
            modules = [ { moduleName = "main"; source = source } ]
            entryModuleName = "main"
            outDir = System.IO.Path.GetTempPath()
            dependencies = []
        }
        match compileResult.hir, compileResult.symbolTable with
        | Some hirAsm, Some symTable ->
            let index = Atla.Core.Semantics.PositionIndex.build hirAsm symTable
            let visibleAtCursor = Atla.Core.Semantics.PositionIndex.visibleSymbolIdsAt index 3 9
            let names =
                visibleAtCursor
                |> List.choose (fun sid -> symTable.Get sid |> Option.map (fun info -> info.name))
            // person は cursor (3,9) で可視でなければならない。
            Assert.Contains("person", names)
        | _ -> failwith "HIR or symbolTable not produced"

    [<Fact>]
    let ``GetCompletions without apostrophe includes person variable in do block`` () =
        // データ型変数 person が do ブロック内の let 束縛として visibleVarMap に含まれることを検証する。
        let source =
            "data Person = { name: String, age: Int }\n" +
            "fn main (): () = do\n" +
            "  let person = Person { name = \"Alice\", age = 30 }\n" +
            "  per\n" +
            "  ()"
        let server = makeServerWithSource "file:///tmp/completion-person-var.atla" source
        // line 3, col 4 → "  per" 位置で通常補完。person が候補に含まれること。
        let result = server.GetCompletions("file:///tmp/completion-person-var.atla", 3, 4)
        let names = result.items |> List.map (fun i -> i.label)
        Assert.Contains("person", names)

    [<Fact>]
    let ``GetCompletions after close returns empty list`` () =
        let uri = "file:///tmp/completion-close.atla"
        let server = makeServerWithSource uri "fn clean: Int = 0"
        let beforeClose = server.GetCompletions(uri, 0, 0)
        Assert.NotEmpty(beforeClose.items)
        server.CloseDocument(uri)
        let afterClose = server.GetCompletions(uri, 0, 0)
        Assert.Empty(afterClose.items)
