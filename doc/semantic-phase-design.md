# Semantic Phase Design (Resolve / Analyze / Infer)

## 目的

意味解析フェーズの責務を以下の3モジュールへ明確に分離する。

1. `Resolve`: 名前解決のみ
2. `Analyze`: AST から（型未確定を含む）HIR への変換のみ
3. `Infer`: 型未確定 HIR を Typed HIR へ確定するのみ

これにより、フェーズ境界を明示し、各モジュールの関心事を単一化する。

## モジュール責務

### Resolve

- 入力: `AST.Module`
- 出力: `ResolvedModule`（`moduleScope` と `fnDecls`）
- 担当:
  - import 解決
  - 組み込み型/演算子のスコープ登録
  - `SymbolTable` と `Scope` の構築
- 非担当:
  - HIR 構築
  - 型推論

### Analyze

- 入力: `AST.Module` + `ResolvedModule`（内部で `Resolve.resolveModule` を利用）
- 出力: 型未確定を含む `HIR.Module`
- 担当:
  - AST ノードを HIR ノードへ変換
  - 変数/型シンボル参照を `Scope`/`SymbolTable` で解決して HIR に反映
  - （推論前段としての）型メタ変数付き HIR 生成
- 非担当:
  - HIR の最終型確定

### Infer

- 入力: 型未確定を含む `HIR.Module`
- 出力: Typed `HIR.Module`
- 担当:
  - `TypeSubst` を使った型解決（`Type.resolve`）
  - HIR ツリー全体の型情報の正規化
- 非担当:
  - AST 走査
  - 名前解決

## データフロー

`AST.Module`
→ `Resolve.resolveModule`（名前解決 / SymbolTable 構築）
→ `Analyze`（AST→HIR 変換）
→ `Infer.inferModule`（型確定）
→ Typed `HIR.Module`

## 設計上の利点

- フェーズ単位でのテスト容易性向上
- 境界が明確なため回帰時の原因特定が容易
- `Resolve` / `Analyze` / `Infer` の独立改善が可能

## `Enumerable.Range` の扱い

- `Resolve` は組み込み関数を追加せず、`import System.Linq.Enumerable` を通じた通常の静的メソッド解決を利用する。
- `Analyze` の `for` 文解析は、`MoveNext`/`Current` を直接持つ反復子だけでなく、`GetEnumerator()` を持つ `IEnumerable` も受理し、必要に応じて `GetEnumerator()` 呼び出しを HIR に明示化する。
- これにより `for i in Enumerable.Range 1 20` のような書き方でも、Lowering には常に反復子形（`MoveNext`/`Current`）が渡る。

## ネイティブメソッド呼び出し（optional 引数）の扱い

- `Analyze` のネイティブメンバー解決では、まず「引数個数が完全一致する候補」を優先し、存在しない場合のみ optional 引数を末尾に持つ候補を許可する。
- optional 引数が省略された呼び出しは、既定値を HIR 引数として補完してから `Hir.Expr.Call` を構築する。
- これにより、`n_x.Split " "` のような 1 引数呼び出しでも `System.String.Split(string, StringSplitOptions=...)` を解決できる。
