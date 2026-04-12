# Plan

## 2026-04-12 プロパティアクセス Lowering 修正（a.Length 実行時失敗の解消）

- [x] `Lowering/Layout.fs` で `Hir.Member.NativeProperty` を値として使用する際、getter 呼び出し命令へ正規化する。
- [x] `Semantics/AnalyzeTests.fs` に `Enumerable.Range 0 a.Length` + `a[i]` の回帰テストを追加する。
- [x] `Lowering/LoweringTests.fs` にユーザー報告コードのコンパイル成功（必要なら実行）回帰テストを追加する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 組み込み `range` 廃止と `Enumerable.Range` 利用への移行

- [x] `Semantics/Resolve.fs` から組み込み `range` 登録を削除する。
- [x] `Parser`/`Semantic`/`Lowering` テストとサンプルコードを `Enumerable.Range` 呼び出しへ更新する。
- [x] 関連ドキュメント（semantic-phase-design）を現行仕様に合わせて更新する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 nullary関数呼び出し `()` の解決修正

- [x] `Semantics/Analyze.fs` の関数適用解析で、`f ()` を 0 引数呼び出しとして正規化した型解決を行う。
- [x] `Semantics` テストに `fn main: () = greet ()` 相当の回帰ケースを追加する。
- [x] `Lowering` テストにユーザー報告コード相当のコンパイル/実行ケースを追加する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 組み込み関数 range 追加

- [x] `Semantics/Resolve.fs` に組み込み `range` を追加し、`System.Linq.Enumerable.Range(int, int)` へ解決できるようにする。
- [x] `Semantics/Analyze.fs` の `for` 解析を拡張し、`IEnumerable` 入力時に `GetEnumerator()` を自動適用して `for i in range 1 20` を受理できるようにする。
- [x] `Parser`/`Semantic`/`Lowering` テストを `range` 利用形へ更新・追加し、回帰を防止する。
- [x] フルテストスイートを実行して変更の妥当性を確認する。

## 2026-04-11 Atla.Cli 実行ファイル名変更（atla.exe）

- [x] `Atla.Cli` の出力アセンブリ名を `atla` に固定し、実行ファイル名を `atla.exe` とする。
- [x] CLIドキュメントの実行ファイル名表記を `atla.exe` に更新する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 Atla.Cli phase4-5（テスト分離と運用仕上げ）

- [x] `Atla.Cli.Tests` プロジェクトを新規作成し、CLIテストを収容する。
- [x] 既存 `Atla.Compiler.Tests/Cli/CliTests.fs` を `Atla.Cli.Tests` へ移行する。
- [x] `Atla.Compiler.Tests` からCLIテスト参照を削除し、solution 構成を更新する。
- [x] フルテストスイートを実行して通過を確認する。
- [x] CLIドキュメントを点検し、必要な追記を行う。

## 2026-04-11 Atla.Cli 最小インターフェイス（phase1-3）

- [x] `Atla.Cli` 実行プロジェクトを追加し、solution に組み込む。
- [x] `build <input.atla>` の最小CLIを実装し、既存 `Compiler.compile` へ接続する。
- [x] 入力検証（存在確認・`.atla`拡張子）と終了コード（成功0/失敗1）を実装する。
- [x] CLIの最小ヘルプとデフォルト値（`--name` / `-o` 省略時）を実装する。
- [x] CLI向けテストを追加してフルテストスイートを実行する。
- [x] CLI利用方法のドキュメントを追加する。

## 2026-04-11 Unit/Void 同値化（Semantic吸収 + Lowering整合）

- [x] `Semantics` で `Unit` と `Native System.Void` を文脈付きで同値化し、`void` 呼び出しを文文脈で `Unit` として許可する。
- [x] `Semantics` で `void` を値文脈（`let`/`var` 右辺など）として扱えないようにし、診断を返す。
- [x] `Lowering/Gen` で `Unit` 返り値メソッドを CLR `void` シグネチャへ正規化し、entrypoint 実行制約を満たす。
- [x] `AnalyzeTests` に `void` 値利用禁止の回帰テストを追加する。
- [x] `doc` 配下に今回の Unit/Void 設計を文書化する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 Unit/Void フェーズ2（Semantic値文脈の厳密化）

- [x] `Analyze.unifyOrError` で `Native System.Void` の許容文脈を `expected = Unit` のみに制限する。
- [x] `let` / `var` / `assign` の個別 `void` 判定を削除し、型統一エラーへ一本化する。
- [x] 既存の `void` 値利用禁止テストを新ルール（型不一致診断）に合わせて更新する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 Unit/Void フェーズ3（Loweringシグネチャ整合の検証強化）

- [x] `Gen` で `TypeId.Unit` 戻り値メソッドが CLR `System.Void` になることを検証する回帰テストを追加する。
- [x] `TypeId.Int` 戻り値が従来どおり `System.Int32` として生成されることを併せて確認する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 Unit/Void フェーズ4（境界ケースのテスト拡充）

- [x] `void` 呼び出しが式文コンテキストでは許可されることを `AnalyzeTests` で追加検証する。
- [x] `main: Int` の実行終了コードが戻り値と一致することを `LoweringTests` で追加検証する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 Unit/Void フェーズ5（完了確認とドキュメント整備）

- [x] `unit-void-design.md` にフェーズ4/5の検証観点（実行終了コード・式文許可）を追記する。
- [x] 追加テストを含めてフルテストスイートの通過を確認し、完了条件を満たす。

## 2026-04-10 DesugarTests の AnalyzeTests への移動

- [x] `Lowering/Desugar/DesugarTests.fs` の do block テストを `Semantics/AnalyzeTests.fs` へ移動する。
- [x] テストプロジェクトの `Compile Include` から旧 `Lowering/Desugar/DesugarTests.fs` を削除する。
- [x] フルテストスイートを実行し、移動後の回帰がないことを確認する。

## 2026-04-10 Testsプロジェクト構成のソース準拠化

- [x] テストディレクトリ名をソース構成（`Syntax` / `Semantics/Data`）に合わせて再配置する。
- [x] `Atla.Compiler.Tests.fsproj` の `Compile Include` を新しいディレクトリ構成順へ更新する。
- [x] フルテストスイートを実行して構成変更のみで回帰がないことを確認する。

## 2026-04-10 Symbol/Type 解決基盤の改善（レビュー対応）

- [x] `SymbolId` を値等価で扱える表現へ変更し、辞書キーとして安定動作させる。
- [x] `Type.unify` の `failwith` を廃止し、`Result` ベースで失敗を返すように変更する。
- [x] 型メタ変数の採番をグローバル状態から解析コンテキストローカルへ移す。
- [x] `SymbolInfo` を不変データ化し、外部バインディング情報を `SymbolKind` から分離する。
- [ ] `Scope.ResolveVar` の `tid` 未使用引数を解消する（利用または削除）。

## 2026-04-09 HIR getErrors メンバ追加

- [x] `Hir` の各型（`Expr`/`Stmt`/`Field`/`Method`/`Type`/`Module`/`Assembly`）に `getErrors` メンバを追加し、再帰的に `Error` を収集する。
- [x] `Analyze.collectExprErrors` を削除し、`Hir.Module.getErrors` を使って診断収集する。
- [x] 対象テストを再実行して変更を確認する。

## 2026-04-09 Error.toString 追加

- [x] `Semantics.Data.Error` に `toString` 関数を追加する。
- [x] `Compile` とテストのエラー文字列化を `Error.toString` 呼び出しへ置き換える。
- [x] 対象テストを実行して変更を確認する。

## 2026-04-09 Semantics.Data.Error 型導入

- [x] `Semantics.Data` 名前空間に `Error` 型（`string` と `Span` を保持）を追加する。
- [x] HIR から収集する診断型を `string` から `Error` に置き換える。
- [x] `Analyze.analyzeModule` 利用側（`Compile` とテスト）を新しい `Error` 型に追従させ、対象テストを実行する。

## 2026-04-09 AnalyzeTestsでAST直接構築へ修正

- [x] `AnalyzeTests.ast to hir should not keep error nodes` から `Lexer` / `Parser` 依存を外す。
- [x] `LoweringTests.hello` 相当のAST（`Import System.Console` と `main` 内 `Console.WriteLine "Hello, World!"`）をテスト内で直接構築する。
- [x] `Result.Error` の場合に明示的に失敗するアサーションを維持し、`AnalyzeTests` を再実行して通過を確認する。

## 2026-04-09 AnalyzeTests入力のhello相当化

- [x] `AnalyzeTests.ast to hir should not keep error nodes` のASTを `LoweringTests.hello` 相当（`import System.Console` と `Console.WriteLine "Hello, World!"` を含む）に変更する。
- [x] `AnalyzeTests` で `Result.Error` が返った場合は明示的にテスト失敗にする。
- [x] `AnalyzeTests` を実行して期待通り通過することを確認する。

## 2026-04-09 helloテスト通過に向けた再整理（HIRエラー残存失敗を考慮）

- [x] Semantic解析後に `hirModule.hasError` を検査し、`ExprError` / `ErrorStmt` が残る場合は `Result.Error` を返して Lowering へ進めない。
- [x] `AnalyzeTests.ast to hir should not keep error nodes` を成功させる（AST->HIR 変換でエラーノードを残さない、または明示エラーとして返す経路を確立する）。
- [x] `Gen.genAssembly` のエントリポイント設定（`main` の `MethodDefinitionHandle` 解決）を修正し、`Entry point not found` を解消する。
- [x] `Layout` のメソッド名解決を点検し、`main` が確実に MIR/Gen に伝播することを保証する。
- [x] `Compile.compile` の `tokens.Head` 直参照を修正し、空トークン入力で例外を出さずに診断を返す。
- [x] `LoweringTests.hello` / `LoweringTests.fibonacci` / `AnalyzeTests.ast to hir should not keep error nodes` を含む全テストを再実行し、失敗要因を段階的に解消する。

## 2026-04-09 HIR hasError を型メンバへ移行

- [x] `Hir` の各型（`Expr`/`Stmt`/`Field`/`Method`/`Type`/`Module`/`Assembly`）に `hasError` メンバを定義する。
- [x] `hasError` は HIR ツリーを再帰的に走査する実装にする。
- [x] テスト側は型メンバ `hasError` を呼ぶように更新し、全体テストを再実行する。

## 2026-04-09 HIR hasError 関数の本体移管

- [x] `Hir` モジュール内の各型に対して `hasError` 判定関数を追加する。
- [x] `AnalyzeTests` のローカルヘルパーを削除し、`Hir.hasError` を利用するように変更する。
- [x] テストスイートを実行し、追加テストが現状失敗することを含む現在の失敗状況を確認する。

## 2026-04-09 HIRエラー残存検知テスト追加

- [x] AST から HIR への変換結果に `Hir.Expr.ExprError` / `Hir.Stmt.ErrorStmt` が残っていないことを検査するテストケースを追加する。
- [x] 現状実装で当該テストが失敗すること（回帰検知の起点になること）を確認する。

## 2026-04-09 helloテストの実行検証化

- [x] `LoweringTests.hello` の現状を確認し、コンパイル成功判定のみになっている箇所を特定する。
- [x] コンパイルしたバイナリ（`HelloWorld.dll`）を実行し、標準出力を検証するテストに書き換える。
- [x] テストを実行し、変更後の `hello` テストが失敗する既知要因（`Entry point not found`）を確認する。

- [x] `Mir.Method` と `Mir.Type` の定義に `name` メンバを追加する。
- [x] `Mir.Method` / `Mir.Type` のコンストラクタ引数を更新し、生成箇所（Layout）を追従させる。
- [x] `Layout` で `Hir.Module.scope` から `SymbolId` に対応する名前を解決して MIR 名に設定する。
- [x] 全テストを実行して影響範囲を確認する。

## 2026-04-08 Gen TypeId 変換対応

- [x] Genモジュールの実装方針を整理し、`TypeId -> System.Type` 変換を追加する計画を明記する。
- [x] Genモジュールに `SymbolId -> TypeBuilder` の変換テーブルを利用する `TypeId -> System.Type` 変換関数を実装する。
- [x] メソッド/コンストラクタ/フィールド生成で上記変換関数を利用するように差し替える。
- [x] Genに対するテストを追加・更新し、型解決の回帰を防止する。
- [x] テストスイートを実行して変更の妥当性を確認する（既存テスト不整合により一部失敗を確認）。

## 2026-04-08 Gen module 化 + Env 導入

- [x] `Gen` を型 (`type Gen`) から `module Gen` に変更し、公開 API を関数ベースに置き換える。
- [x] `Gen.Env` を追加し、`typeBuilders` を保持して `gen` 系関数に明示的に受け渡す。
- [x] 既存呼び出し箇所（`Compile` と `GenTests`）を新 API に追従させる。
- [x] ビルド・テストを実行して回帰がないことを確認する。

## 2026-04-08 Gen コメント方針適用

- [x] `Gen` モジュール内の各主要ブロック（型解決、命令生成、型/モジュール/アセンブリ生成）にコメントを追加する。
- [x] 既存挙動を変えずに可読性のみを改善する。
- [x] ビルドで変更が成立することを確認する。

## 2026-04-08 コメント保持ルールの明文化

- [x] 既存コメントの削除禁止（関連コードに変更がない場合）ルールを `AGENTS.md` に追記する。
- [x] 既存ルールと整合する場所に追記し、運用可能な文言にする。

## 2026-04-08 Gen 削除コメントの復元

- [x] `Gen` モジュールで過去差分で消えた既存コメントを特定する。
- [x] 挙動を変えずに削除コメントを元の意図が分かる形で復元する。
- [x] ビルドで復元後の整合性を確認する。

## 2026-04-08 Testプロジェクト再構成

- [x] 既存コンパイラAPI（`Syntax` / `Semantics` / `Lowering`）と不整合な旧テストを洗い出す。
- [x] `Parsing` / `Lowering.Desugar` / `Lowering.Layout` テストを現行APIに合わせて書き直す。
- [x] テストプロジェクト全体を実行し、ビルドエラーが解消されていることを確認する。

## 2026-04-08 LoweringTests 文字列互換修正

- [x] `LoweringTests` の hello world / fibonacci プログラム文字列を元の内容に戻す。
- [x] 文字列は変更せず、テスト実行経路のみを調整して現行実装で安定して検証できるようにする。
- [x] テストスイートを再実行してエラー解消を確認する。

## 2026-04-08 fibonacciテスト方針変更

- [x] `LoweringTests.fibonacci` を `Compiler.compile` 実行 + `res.IsOk` 確認に戻す。
- [x] フィボナッチのプログラム文字列は変更しない。
- [x] テスト実行結果（失敗許容）を確認する。

## 2026-04-09 SymbolId/TypeId 変数名統一

- [x] `TypeId.Name` のラベル名 `symbolId` を `sid` に変更する。
- [x] 関連コードのビルド/テストを実行し、既存失敗（`LoweringTests.fibonacci`）を確認する。

## 2026-04-09 SymbolId/TypeId 命名をプロジェクト全体で統一

- [x] `SymbolId` 型の変数・引数名を `sid` に統一する。
- [x] `TypeId` 型の変数・引数名を `tid` に統一する。
- [x] テストを実行し、既存失敗（`LoweringTests.fibonacci`）以外の回帰がないことを確認する。

## 2026-04-09 PLAN残タスクの順次解消

- [x] `Layout` のメソッド名解決を `SymbolId.id` ベースの同値判定に修正し、`main` 名が MIR へ確実に伝播することを保証する。
- [x] `Gen.genAssembly` のエントリポイント設定を点検・修正し、`HelloWorld.dll` 実行時の `Entry point not found` を解消する。
- [x] `LoweringTests.hello` / `LoweringTests.fibonacci` / `AnalyzeTests.ast to hir should not keep error nodes` を含む全テストを再実行し、未解決要因を潰す。

## 2026-04-09 レビュー指摘対応（Lexer非変更方針）

- [x] `Syntax/Lexer.fs` の変更を取り消し、Lexer 非変更制約を満たす。
- [x] `Compiler.compile` 側で複数行入力を扱える経路を用意し、`LoweringTests.hello` を含む既存テストを通す。
- [x] `AGENTS.md` に「Lexer/Parserの変更が必要な場合は事前確認する」運用ルールを追記する。
- [x] テストスイートを再実行し、制約下での結果を確認する。

## 2026-04-09 レビュー指摘対応（tokenizeMultiLine 取り消し）

- [x] `Compiler` モジュールの `tokenizeMultiLine` / `shiftSpan` / `shiftTokenLine` を削除し、従来の lexing 経路へ戻す。
- [x] 影響確認としてテストを実行し、必要なら失敗を記録する。

## 2026-04-09 レビュー指摘対応（StringInput修正許可）

- [x] `StringInput.get/next` を修正し、複数行入力を最後まで走査できるようにする。
- [x] `LoweringTests.hello` を含むテストを実行し、回帰がないことを確認する。

## 2026-04-09 レビュー指摘対応（StringInputのCRLF/CR対応）

- [x] `StringInput` で CRLF/CR を正規化し、改行走査が一貫して動作するようにする。
- [x] `LoweringTests.hello` を含むテストを再実行して影響を確認する。

## 2026-04-10 Analyze分割（意味解析/型推論）段階的移行

- [x] `Semantics.Analyze` の責務を棚卸しし、名前解決（意味解析）と型推論の境界をドキュメント化する。
- [x] 既存 `Analyze.analyzeModule` の外部APIを維持したまま、内部処理を `resolveModule` と `inferModule` に分離する。
- [ ] 中間表現（仮称 `ResolvedAst`）を導入し、識別子解決済みだが型未確定の状態を明示する。
- [ ] `Env` を責務別（`ResolveEnv` / `InferEnv`）に分割し、`Scope` と `TypeSubst` の依存を分離する。
- [ ] `failwith` による制御フローを段階的に `Result<_, Error list>` へ置換する。
- [ ] フェーズ境界ごとのテスト（Resolve/Infer）を追加し、既存 `AnalyzeTests` と合わせて回帰を防止する。
- [ ] 上記変更後にテストスイートを実行し、決定性・診断品質・既存Lowering経路への影響を確認する。

## 2026-04-10 Resolve/Infer モジュール分離

- [x] 名前解決に関する関数を `Semantics.Resolve` へ移動し、モジュールスコープ初期化・import解決・関数宣言収集を担わせる。
- [x] 型推論に関する関数を `Semantics.Infer` へ移動し、式/文/メソッド解析と HIR 生成を担わせる。
- [x] `Semantics.Analyze.analyzeModule` の外部APIを維持しつつ、`Resolve.resolveModule` と `Infer.inferModule` を接続する。
- [x] コンパイル順序を保つため `.fsproj` の `Compile Include` を更新する。
- [x] テストスイートを実行して回帰がないことを確認する。

## 2026-04-10 Infer.analyze系のAnalyzeモジュール移管

- [x] `Semantics.Infer` にあるモジュール単位解析関数を `Semantics.Analyze` へ移動し、役割境界を明確化する。
- [x] `Analyze.analyzeModule` から Infer の式/文解析ロジックを呼び出す構成へ更新する。
- [x] テストスイートを実行して回帰がないことを確認する。

## 2026-04-10 Analyze/Resolve/Infer 責務再分離（再修正）

- [x] `Analyze` を AST→（型未確定）HIR 変換の責務に限定し、AST依存ロジックを `Infer` から移管する。
- [x] `Resolve` は名前解決（`SymbolTable`/`Scope` 構築）責務のみを持つことをコード上で維持・確認する。
- [x] `Infer` を「型未確定HIRを受けてTyped HIRを返す」APIに変更する。
- [x] 上記設計を `doc` 配下に設計資料として追加する。
- [x] テストスイートを実行して回帰がないことを確認する。

## 2026-04-10 Analyze.Env の NameEnv / TypeEnv 分割

- [x] `Analyze.Env` を `NameEnv`（名前解決・Symbol操作）と `TypeEnv`（型制約・型解決）へ分割する。
- [x] `Analyze` 内の式/文/メソッド解析ロジックを新しい環境型に追従させる。
- [x] テストを実行して回帰がないことを確認する。

## 2026-04-10 Analyze エラー生成共通化とResult化

- [x] `Analyze` にエラー生成ヘルパー関数を追加し、重複する `Hir.Expr.ExprError` 生成を共通化する。
- [x] `MemberAccess` と `StaticAccess` の解析ロジックを `Result` ベースに切り替える。
- [x] テストを実行して回帰がないことを確認する。

## 2026-04-10 フィボナッチ和プログラムのコンパイルテスト追加

- [x] `LoweringTests` に、指定されたフィボナッチプログラムが `Compiler.compile` で成功することを検証するテストを追加する。
- [x] 生成DLLの存在まで検証し、コンパイル成果物が出力されることを確認する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-10 フィボナッチテストの実行結果検証追加

- [x] `LoweringTests` のフィボナッチテストを、実行時に標準入力 `10` を与えて標準出力が `55` になることを検証する形へ更新する。
- [x] コンパイル成功と生成DLL存在の検証は維持する。
- [x] フルテストスイートを実行して回帰がないことを確認する。
- [x] フィボナッチ実行時の `InvalidProgramException` を解消するため、MIRフレームのローカル/引数型情報をCILローカル宣言へ反映する。

## 2026-04-10 FizzBuzzプログラムのコンパイルテスト追加

- [x] `LoweringTests` に、指定されたFizzBuzzプログラムを `Compiler.compile` へ入力するテストを追加する。
- [x] テストでコンパイル結果（`Result.IsOk`）と生成DLL存在を検証する。
- [x] フルテストスイートを実行し、失敗を含む現状結果を確認する。

## 2026-04-10 for文パース実装（AST構築まで）

- [x] `Syntax/Parser.fs` に `for ... in ...` 文のパーサーを追加し、`Ast.Stmt.For` を構築する。
- [x] `stmt` の分岐に `for` 文を組み込み、`do` ブロック内で `for` 文を解釈できるようにする。
- [x] `ParserTests` に `for` 文がASTとして構築されることを検証するテストを追加する。
- [x] テストスイートを実行し、現状結果を確認する。

## 2026-04-10 for文の `=>` 構文対応（FizzBuzz修正版）

- [x] `Syntax/Parser.fs` の `for` 文パースを `for ... in ... =>` 形式へ対応させる。
- [x] `ParserTests` を修正版FizzBuzz構文に合わせて更新し、`Ast.Stmt.For` 構築を検証する。
- [x] `LoweringTests` のFizzBuzzソースを修正版へ更新する。
- [x] テストを実行してAST構築確認と現状結果を記録する。

## 2026-04-10 LineInput削除（BlockInputへ統一）

- [x] `Syntax/Parser.fs` の `LineInput` を削除し、同等の入力制御を `BlockInput` で実現する。
- [x] `for` 文のiterable解析が行末までに制限されることを維持する。
- [x] 関連テストを実行し、AST構築が維持されることを確認する。

## 2026-04-10 LineInput削除変更の取り消し

- [x] 直前の `LineInput` 削除変更を取り消し、`for` 文パーサーを直前安定状態へ戻す。
- [x] 関連テストを実行し、取り消し後の挙動を確認する。

## 2026-04-10 FizzBuzzテスト入力の更新（`for ... in ...` 形式）

- [x] FizzBuzzテストのAtlaソースを指定された `for ... in ...`（`=>` なし）形式へ更新する。
- [x] `for` 文パーサーを新しいテスト入力でも `Ast.Stmt.For` を構築できるように調整する。
- [x] 関連テストを実行して結果を確認する。

## 2026-04-10 FizzBuzzテスト通過のためのFor文意味解析実装

- [x] `Semantics/Analyze.fs` に `Ast.Stmt.For` 分岐を追加し、`Unsupported statement type` 例外を解消する。
- [x] `LoweringTests.fizzbuzz program compiles` を実行して通過を確認する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-10 直前変更の取り消し

- [x] 直前コミット（FizzBuzz実行テスト対応）の変更を取り消す。
- [x] テストを実行して取り消し後の状態を確認する。

## 2026-04-10 FizzBuzzテストプログラムの入力版更新

- [x] FizzBuzzテストプログラムを指定された `fizzbuzz (n: Int)` + `main` 入力受け取り構成へ変更する。
- [x] `fn ... = for ...` 形式をパースできるようにパーサーを調整する。
- [x] `Int32.Parse` を import なしでも解決できるよう、意味解析前の型解決スコープを調整する。
- [x] テストを実行して変更後の状態を確認する。

## 2026-04-10 FizzBuzz実行結果検証テスト化

- [x] `LoweringTests.fizzbuzz program compiles` を、入力 `15` を与えて標準出力を検証するテストへ変更する。
- [x] 期待される FizzBuzz 出力（1..15）をアサートする。
- [x] テストスイートを実行し、現状結果（失敗許容）を確認する。

## 2026-04-10 FizzBuzzテスト通過に向けた段階実行（タスク1-3）

- [x] `PLANS.md` に段階実行タスク（1,2,3）を追記する。
- [x] `LoweringTests.fizzbuzz program compiles` を単体実行し、現状の成否を確認する。
- [x] `ParserTests.fileModule parses fizzbuzz for statement` を単体実行し、構文段階の成否を確認する。

## 2026-04-10 FizzBuzzタスク4: 意味解析の穴埋め

- [x] `Ast.Stmt.For` を意味解析で no-op にせず、iterable/ループ変数/本体を解析した HIR へ変換する。
- [x] for文本体で参照されるループ変数（`i`）のシンボルと型を解決できるようにする。
- [x] for文をMIRへレイアウトできるように lowering を拡張し、`MoveNext` / `Current` による反復を生成する。
- [x] `LoweringTests.fizzbuzz program compiles` を再実行して結果を確認する（現状: 実行時 `NullReferenceException` で失敗）。

## 2026-04-10 TypeId変換の案3（コンテキスト付き変換API）適用

- [x] `TypeId` モジュールに、プリミティブ/Native向け `tryToRuntimeSystemType` を追加する。
- [x] `TypeId.Name` 解決を注入できる `tryResolveToSystemType` を追加する。
- [x] `Semantics/Analyze.fs` と `Lowering/Layout.fs` の重複変換ロジックを新APIへ置換する。
- [x] `Lowering/Gen.fs` の `TypeId.Name` 解決も新APIへ寄せる。
- [x] 関連テスト（最低: parser + fizzbuzz lowering）を実行して結果を確認する（parserは成功、fizzbuzz lowering は exit code 134 で失敗）。

## 2026-04-10 タスク5: Loweringの整合性確認

- [x] `for` 文を含むプログラムで、AST -> Semantic -> HIR -> MIR の連結が成立することをテストで検証する。
- [x] 生成されたMIRに `MoveNext` / `Current` とループ制御命令（ラベル/ジャンプ）が含まれることを確認する。
- [x] 追加テストを実行して結果を確認する。

## 2026-04-11 `Console.ReadLine ()` 呼び出し解決の修正

- [x] `Semantics/Analyze.fs` の呼び出し可能式解析で、`Console.ReadLine ()` のような static member 呼び出しを `Hir.Callable.NativeMethod` として解決できるようにする。
- [x] `Semantics` テストに `Console.ReadLine ()` を含む回帰ケースを追加する。
- [x] `Lowering` テストにユーザー報告コード相当のコンパイル成功ケースを追加する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 配列インデックスアクセス `a[0]` 対応

- [x] `Syntax/Parser.fs` を更新し、`expr[index]` 構文を AST として構築できるようにする。
- [x] `Syntax` テストに `a[0]` を含む回帰ケースを追加する。
- [x] `Semantics/Analyze.fs` でインデックスアクセスをネイティブ配列/インデクサ呼び出しへ解決できるようにする。
- [x] `Lowering` テストにユーザー報告コード（`Split` + `a[0]`）のコンパイル成功ケースを追加する。
- [x] ドキュメントを更新し、インデックスアクセス構文を明記する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-11 array index access 実行検証テスト強化

- [x] `LoweringTests` の `array index access compiles` を実行検証付きテストへ更新する。
- [x] 入力 `"1 2 3"` に対して `a[1]` の結果が `2` であることをアサートする。
- [x] 対象テストを実行して通過を確認する。

## 2026-04-11 インデックスアクセス実行テストの入力コード差し替え

- [x] `LoweringTests` のインデックスアクセス実行テストを指定コード（`Console.ReadLine` + `a[1]`）へ置き換える。
- [x] 対象テストを実行して通過を確認する。

## 2026-04-11 `Split` 結果配列のインデックスアクセス実行不具合修正

- [x] `Semantics/Analyze.fs` の indexer 解決を見直し、1 次元配列では `System.Array.GetValue(int)` を優先して実行時に安全な呼び出しへ正規化する。
- [x] `LoweringTests` にユーザー報告コード（`(Console.ReadLine ()).Split " "` + `a[0]`）の実行検証付き回帰テストを追加する。
- [x] ドキュメント（`doc/semantic-phase-design.md`）の indexer 解決仕様を現実装に合わせて更新する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-12 LanguageServer 機能復旧（フェーズ分割）

- [x] Phase 0: LanguageServer 復旧タスクをフェーズ単位で `PLANS.md` に確定する。
- [x] Phase 1: `Atla.LanguageServer` の旧 API 依存を現行 `Atla.Core` API に合わせて解消し、ビルドを通す。
- [x] Phase 1: `Atla.LanguageServer.Tests` を現行 `Server` 公開 API に追従させ、最小テストを通す。
- [x] Phase 1: LanguageServer 関連プロジェクトのテストを実行して結果を確認する（`Atla.LanguageServer.Tests` は成功、`dotnet test src/Atla.slnx` は既存 `Atla.Build` 側のコンパイルエラーで失敗）。


### Phase 2: LSP プロトコル準拠の強化

- [x] `Program.fs` の未対応 request 分岐（現在 `_ -> ()`）を JSON-RPC エラー応答へ変更し、クライアント待ち状態を防ぐ。
- [x] `LSPMessage.waitMessage` のヘッダー/本文解析を堅牢化し、EOF・Content-Length 欠落・不正 JSON 時に安全に失敗できるようにする。
- [x] `initialize` 応答 capability を見直し、実装済み機能のみを明示する。
- [x] `initialize` / `shutdown` / `exit` の往復を統合テスト化する。

### Phase 3: ドキュメント同期と診断配信の安定化

- [ ] `didOpen` / `didChange` / `didClose` のバッファライフサイクルを明確化し、Close 時にメモリと診断状態を適切に解放する。
- [ ] URI 正規化（OS 差・ワークスペース外ファイル）を整理し、コンパイル対象判定を決定的にする。
- [ ] コンパイル成功時に空 diagnostics を必ず送るルールをテストで固定する。
- [ ] 失敗時 diagnostics の粒度（lex/parse/semantic）を段階的に分離できるよう変換レイヤーを導入する。

### Phase 4: Diagnostics 品質向上

- [ ] `LSPTypes.Diagnostic` を拡張し、`severity` / `source` / `code` を扱えるようにする。
- [ ] `Span.Empty` 固定の暫定実装を縮退し、取得可能な span を優先して range へ反映する。
- [ ] 診断メッセージの安定化（決定性と順序）をテストで保証する。
- [ ] 代表的な失敗ケース（未解決識別子/型不一致/構文エラー）の LSP 診断スナップショットを追加する。

### Phase 5: Semantic Tokens 精度改善

- [ ] `InternalTokenize` のトークン種別マッピングを見直し、`keyword` / `type` / `variable` / `number` / `string` の判定を仕様化する。
- [ ] 複数行・空行・先頭 BOM・CRLF 入力で delta encoding が壊れないことを回帰テストで保証する。
- [ ] クライアントが未サポート token type を通知した場合のフォールバック挙動を固定する。
- [ ] semantic tokens の JSON 応答をスナップショットテスト化する。

### Phase 6: テスト基盤と回帰防止

- [ ] `Atla.LanguageServer.Tests` を message レイヤー/サーバー状態遷移/診断配信/トークン化の観点で分割する。
- [ ] stdin/stdout ベースの軽量 E2E テストを追加し、LSP フレーミングを実運用に近い形で検証する。
- [ ] 異常系テスト（不正ヘッダー、不正 JSON、未知メソッド、空本文）を追加する。
- [ ] LanguageServer 変更時に必ず実行するテストコマンドを `PLANS.md` または `doc` に明記する。

### Phase 7: リリース準備

- [ ] エディタ接続手順（起動コマンド、stdio 設定、サンプルプロジェクト）をドキュメント化する。
- [ ] 既知制約（未実装 LSP メソッド、診断精度の制限）を整理して明示する。
- [ ] CI で `Atla.LanguageServer` / `Atla.LanguageServer.Tests` を必須チェックにする。
- [ ] フェーズ完了条件（ビルド成功・テスト成功・E2E 成功）を満たしたら完了マークを更新する。

## 2026-04-12 Atla.Build コンパイルエラー解消

- [x] `Atla.Build/Program.fs` の `Compiler.compile` 参照を現行 namespace/module 構成に合わせて解決する。
- [x] `dotnet build src/Atla.Build/Atla.Build.fsproj` を通し、`Atla.Build` 単体ビルドの成功を確認する。
- [x] `dotnet test src/Atla.Build.Tests/Atla.Build.Tests.fsproj` と `dotnet test src/Atla.slnx` を実行し、影響範囲を確認する。

## 2026-04-12 LanguageServer Phase2 実施

- [x] 未対応 request に対する JSON-RPC エラー応答を実装する。
- [x] `waitMessage` の EOF / Content-Length / JSON 解析の異常系耐性を実装する。
- [x] `initialize` capability を現実装に合わせて調整する。
- [x] `initialize` -> `shutdown` -> `exit` の遷移をテストで検証する。
- [x] LanguageServer テストとソリューション全体テストを実行して結果を確認する。
