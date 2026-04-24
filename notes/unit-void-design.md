# Unit / Void 設計メモ（2026-04-11）

## 目的

`Unit` と CLR `System.Void` の扱いをフェーズ境界ごとに明確化し、
以下を同時に満たす。

- `Console.WriteLine` のような `void` 呼び出しを文脈に応じて扱えること。
- `void` を値として束縛・代入する誤用を意味解析で検出すること。
- CIL の entrypoint 要件（戻り値が `void` / `int` / `uint`）を満たすこと。

## 方針

1. 言語上の意味としては `unit` と `void` を同値として扱う。
2. ただし同値化は文脈依存とし、Semantic で吸収する。
   - 式文（副作用目的）では `void` 呼び出しを `unit` として許可。
   - 値文脈（`let` / `var` / `assign` の右辺）では `void` を禁止。
3. `unit -> void` の CLR シグネチャ整合は Lowering/Gen 側で吸収する。
   - メソッド戻り値が `TypeId.Unit` のとき、CIL メソッド定義は `System.Void` を使う。

## フェーズ責務

### Semantic

- `Type.unify` / `Type.canUnify` で `Unit` と `Native System.Void` の単一化を許可。
- `Analyze.unifyOrError` で `Native System.Void` の許容文脈を `expected = Unit` のみに制限。
- `ExprStmt` は `Unit` 期待型で解析し、副作用呼び出しのみを許容。
- 値文脈で `void` が現れた場合は型不一致（`Cannot unify types`）として診断化。

### Lowering / Gen

- HIR/MIR の `TypeId.Unit` 返り値メソッドを CIL では `System.Void` で定義。
- これにより `main` エントリポイントが CLR 制約に一致する。

## テスト方針

- 既存の `AnalyzeTests` / `LoweringTests` の回帰を維持。
- `void` の値利用禁止（束縛・引数渡し）の回帰テストを追加。
- `void` 呼び出しが式文コンテキストでは成功することを回帰テストで検証する。
- `main: Int` の戻り値がプロセス終了コードへ反映されることを実行テストで検証する。
