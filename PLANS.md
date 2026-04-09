# Plan

## 2026-04-09 helloテスト通過に向けた再整理（HIRエラー残存失敗を考慮）

- [x] Semantic解析後に `hirModule.hasError` を検査し、`ExprError` / `ErrorStmt` が残る場合は `Result.Error` を返して Lowering へ進めない。
- [x] `AnalyzeTests.ast to hir should not keep error nodes` を成功させる（AST->HIR 変換でエラーノードを残さない、または明示エラーとして返す経路を確立する）。
- [ ] `Gen.genAssembly` のエントリポイント設定（`main` の `MethodDefinitionHandle` 解決）を修正し、`Entry point not found` を解消する。
- [ ] `Layout` のメソッド名解決を点検し、`main` が確実に MIR/Gen に伝播することを保証する。
- [x] `Compile.compile` の `tokens.Head` 直参照を修正し、空トークン入力で例外を出さずに診断を返す。
- [ ] `LoweringTests.hello` / `LoweringTests.fibonacci` / `AnalyzeTests.ast to hir should not keep error nodes` を含む全テストを再実行し、失敗要因を段階的に解消する。

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
