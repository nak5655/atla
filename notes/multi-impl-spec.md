# `data` に対する `impl` 個数制約仕様（更新提案）

## 目的
- `impl` の配置自由度を確保しつつ、宣言の一意性を保つ。
- 既存パイプライン `AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL` の境界を維持する。

## 提案スコープ
- 対象: `data` 型への `impl` 宣言（`impl TypeName` / `impl TypeName for Role`）の個数制約と診断。
- 非対象: `class`/`interface` など未導入型、拡張メソッド、別モジュール orphan `impl`。

## 構文ルール
- 既存構文を維持する。
- `impl` 内要素は引き続き `fn` のみを許可する。

## 意味規則（確定案）
1. **対象型制約**
   - `impl` 対象は `data` 型のみ。
2. **`for` なし `impl` の個数制約**
   - 同一型 `T` に対する `impl T`（`for` なし）は **最大 1 つ**。
3. **`for` あり `impl` の個数制約**
   - 同一組 `(T, Role)` に対する `impl T for Role` は **最大 1 つ**。
   - `Role` が異なれば別枠として共存可能。
4. **メソッド重複判定**
   - 同一 `impl` ブロック内・ブロック間を問わず、最終候補集合で「メソッド名 + 引数型列（`this` を除く）+ 戻り値型」が重複したらエラー。
5. **`this` 規則**
   - `this` は先頭引数で必須。
   - `this` の型は `impl` 対象型 `T` と一致必須。
6. **解決順序**
   - 呼び出し候補は「型本体メソッド + 許可された `impl` 群」の和集合。
   - 既存の型適合ベース解決を適用し、最良候補が一意でない場合は曖昧エラー。
7. **決定的診断順序**
   - `対象型未定義 -> Role 不正/未解決 -> impl 個数制約違反 -> this 不正 -> メソッド重複 -> 呼び出し時 NoMatch/Ambiguous`。

## フェーズ境界への影響
- AST: `Decl.Impl` ノードは個別保持（集約しない）。
- Semantic Analysis: `(T, Option<Role>)` 単位で個数制約を検証後、メソッド集合を検証・統合する。
- HIR: `impl` 専用ノードを残さず、既存メソッド表現へ正規化する。
- Frame Allocation/MIR/CIL: 変更不要（既存メソッド lowering を再利用）。

## 診断コード（案）
- `E_IMPL_TARGET_NOT_DATA`: `impl` 対象が `data` 型ではない。
- `E_IMPL_ROLE_NOT_FOUND`: `for Role` の `Role` が未解決。
- `E_IMPL_DUPLICATE_BLOCK_DEFAULT`: `impl T`（`for` なし）が複数定義された。
- `E_IMPL_DUPLICATE_BLOCK_FOR_ROLE`: `impl T for Role` が同一 `Role` で複数定義された。
- `E_IMPL_THIS_TYPE_MISMATCH`: `this` の型が `impl` 対象型と不一致。
- `E_IMPL_DUPLICATE_METHOD_SIGNATURE`: 同一シグネチャが重複定義された。

## テスト観点（実装時）
- 正常系:
  - `impl T` 単独 1 件で成功。
  - `impl T for Reader` と `impl T for Writer` の共存成功。
- 異常系:
  - `impl T` を 2 回定義して個数制約診断。
  - `impl T for Reader` を 2 回定義して個数制約診断。
  - `this` 型不一致診断。
  - 同一シグネチャ重複診断。
- 回帰:
  - 既存単一 `impl` の挙動維持。
  - HIR/MIR へ `impl` 糖衣が残らないことを確認。


## 実装計画（提案）
1. **変更制御の確認**
   - `Lexer.fs` / `Parser.fs` 変更の明示承認を取得してから着手する。
2. **AST/Parser 更新**
   - `impl T` と `impl T for Role` の両方を構文木に保持し、span を失わない。
3. **Resolver 更新**
   - `T`/`Role` の解決を行い、未解決時は確定した診断コードを返す。
4. **Semantic 集約・制約検証**
   - `(T, Option<Role>)` 単位で impl を集約し、
     - `impl T` は 1 件まで
     - `impl T for Role` は Role ごと 1 件まで
     を検証する。
5. **メソッド重複検証**
   - 集約後の有効 impl 群に対してシグネチャ重複を検証する。
6. **HIR 正規化**
   - `impl` 専用ノードを HIR へ持ち込まず、既存メソッド表現に統合する。
7. **テスト実装**
   - Parser: `impl T` 重複 / `impl T for Role` 重複 / Role 未解決。
   - Semantic: `this` 不一致 / シグネチャ重複 / 診断順序。
   - Lowering: HIR/MIR/CIL 非退行。
8. **総合検証**
   - 全テストプロジェクト実行で非退行を確認する。

## 未決事項
- 戻り値型を重複判定に含めるか（現提案は含める）。
- 将来 `impl A for B`（継承）導入時に、Role 付き `impl` の探索優先度をどう定義するか。
