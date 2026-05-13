# `.atlalib` 実装仕様（推奨 v1）

## 1. 目的

`.atlalib` は、Atla のライブラリ配布物を **import 解決可能な単一ファイル** として扱うための配布フォーマットである。

本仕様の目的は次の 3 点に限定する。

1. 配布用成果物のコンテナ形式を固定する。
2. import 側が必要とする公開シンボル情報の表現を固定する。
3. 依存関係の復元と実行時ロードに必要なメタデータを固定する。

本仕様はコンパイラの段階境界を崩さない。

`AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL`

- `.atlalib` 生成はコンパイラ phase ではなく **パッケージング段** とし、Semantic Analysis 直後に固定された公開シンボル表現と、CIL 出力後に得られる assembly artifact を束ねて生成する。
- `.atlalib` import は **ソース AST を再解釈しない**。
- import 側は `.atlalib` の公開シンボル表現を読み、Semantic Analysis 用の解決済み情報へ復元する。

---

## 2. `package.type` の意味

`atla.yaml` の `package.type` は以下を受理する。

- `exe`: 実行可能成果物を生成する。
- `lib`: `.atlalib` を生成する。
- `dll`: `.dll` を生成する。

### 2.1 `lib`

- `lib` はユーザー可視の最終成果物として `.atlalib` のみを出力する。
- 内部的に DLL / PDB を生成してよい。
- ビルド結果の表示は `Generated: <name>.atlalib` のみとする。

### 2.2 `dll`

- `dll` は `Generated: <name>.dll` のみを出力する。
- `.atlalib` は生成しない。

---

## 3. コンテナ形式

`.atlalib` は ZIP コンテナとする。

理由:

- 実装が単純である。
- ストリームでメタデータを先に検査しやすい。
- 将来の署名、追加 IR、複数成果物同梱に拡張しやすい。

### 3.1 レイアウト

```text
/
  atlalib.json
  assemblies/
    <assembly-name>.dll
    <assembly-name>.pdb                (任意)
  symbols/
    public.api.json
  deps/
    manifest.lock.json
  hashes/
    sha256sums.txt
  signature/
    package.sig                        (任意)
```

### 3.2 v1 必須エントリ

v1 の必須エントリは以下とする。

- `atlalib.json`
- `assemblies/<assembly-name>.dll`
- `symbols/public.api.json`
- `deps/manifest.lock.json`
- `hashes/sha256sums.txt`

---

## 4. `atlalib.json`

`atlalib.json` はコンテナ識別と互換性判定のための最上位メタデータである。

### 4.1 必須項目

- `formatVersion`
- `package.name`
- `package.version`
- `compiler.name`
- `compiler.version`
- `compiler.targetFramework`
- `artifacts.assembly`
- `artifacts.publicApi`
- `artifacts.dependencyLock`
- `compat.languageAbi`
- `compat.symbolSchemaVersion`

### 4.2 推奨 JSON 例

```json
{
  "formatVersion": "1.0",
  "package": {
    "name": "mylib",
    "version": "1.2.3"
  },
  "compiler": {
    "name": "atla",
    "version": "0.1.0",
    "targetFramework": "net10.0"
  },
  "artifacts": {
    "assembly": "assemblies/mylib.dll",
    "publicApi": "symbols/public.api.json",
    "dependencyLock": "deps/manifest.lock.json"
  },
  "compat": {
    "languageAbi": "atla-abi-1",
    "symbolSchemaVersion": "1.0"
  }
}
```

### 4.3 解釈規則

- `formatVersion` は **ZIP 内レイアウトとメタデータ構造** の互換性を表す。
- `languageAbi` は **Atla 言語レベルの import/呼び出し/型互換性** を表す。
- `symbolSchemaVersion` は **`symbols/public.api.json` の JSON 形状** の互換性を表す。

`formatVersion` と `symbolSchemaVersion` は別軸で管理する。

---

## 5. `symbols/public.api.json`

`symbols/public.api.json` は import 解決専用の公開シンボル表現である。

このファイルは単なる API 一覧ではなく、**import 側が Semantic Analysis で必要とする最小の解決済み公開情報** を持たなければならない。

### 5.1 設計原則

- 未解決識別子を含めない。
- AST の糖衣構文を持ち込まない。
- import 側が **型・フィールド・メソッド・継承情報** を復元できる。
- コンパイル時の一時的な `SymbolId` をそのまま外部契約にしない。
- 参照は **パッケージ内で安定な論理 ID** で表現する。

### 5.2 最上位構造

```json
{
  "schemaVersion": "1.0",
  "modules": [
    {
      "name": "Foo",
      "exports": {
        "values": [],
        "types": []
      }
    }
  ]
}
```

### 5.3 モジュール export

各モジュールは以下を export する。

- 公開値関数
- 公開型
- 型に属する公開フィールド
- 型に属する公開メソッド
- `impl` から復元すべき基底型情報
- enum / role の種別情報

### 5.4 `exportId` の形式

`exportId` はパッケージ内で安定な論理 ID であり、同一ソース定義からは常に同じ値が生成されなければならない。

v1 では次の文字列形式を使う。

- モジュール: `module:<module-name>`
- 値関数: `value:<module-name>:<value-name>`
- 型: `type:<module-name>:<type-name>`
- フィールド: `field:<module-name>:<type-name>:<field-name>`
- メソッド: `method:<module-name>:<owner-type-name>:<method-name>:<instance|static>`
- enum case: `enumCase:<module-name>:<type-name>:<case-name>`

生成規則:

- 宣言名はソース上の公開名をそのまま使う。
- Compiler 内部の `SymbolId` や連番を埋め込まない。
- JSON 出力順は `module-name`, `type-name`, `member-name` の辞書順で決定する。
- hidden field のような import 復元専用要素を公開する場合は、`field:<module-name>:<type-name>:__hidden__:<field-name>` のように予約セグメントを含めて通常公開名と衝突しないようにする。
- v1 で予約する内部セグメントは `__hidden__` のみとし、`__internal__` や `__private__` など他の接頭辞を暗黙予約してはならない。

### 5.5 値 export

値 export は少なくとも以下を持つ。

- `name`
- `exportId`
- `kind` (`function` など)
- `signature`

### 5.6 型 export

型 export は少なくとも以下を持つ。

- `name`
- `exportId`
- `kind` (`data` / `enum` / `role`)
- `typeParameters`
- `fields`
- `methods`
- `baseType` (存在する場合)
- `delegatedByFieldName` (存在する場合)

### 5.7 enum の要件

enum は data と同列の曖昧な表現にしてはならない。

少なくとも以下を持つ。

- `cases`
- 各 case の `name`
- `tag`
- payload の有無
- payload フィールド定義

### 5.8 role の要件

role は通常の data と区別できなければならない。

少なくとも以下を持つ。

- `kind = "role"`
- role method のシグネチャ

### 5.9 `impl` 復元用情報

クロスモジュール import 後もメンバー解決を維持するため、型 export は必要に応じて以下を持つ。

- `.NET` 基底型または Atla 基底型を表す `baseType`
- 委譲 `impl ... by ...` 用の `delegatedByFieldName`
- import 側が instance / static を区別するための method 属性

### 5.10 推奨型表現

型参照は以下のいずれかで表す。

- 組み込み型
- パッケージ内公開型参照
- .NET 型参照
- 関数型
- 型適用
- 型変数

例:

```json
{
  "kind": "packageType",
  "module": "Foo",
  "name": "Bar"
}
```

```json
{
  "kind": "nativeType",
  "fullName": "System.Exception"
}
```

```json
{
  "kind": "function",
  "args": [
    { "kind": "builtin", "name": "Int" }
  ],
  "return": { "kind": "builtin", "name": "String" }
}
```

### 5.11 このファイルで保証すること

`symbols/public.api.json` は、import 側が次を再構築できることを保証する。

- `import Foo'Bar` がモジュール import か型 import かの判断に必要な公開名
- 同一完全名に対する module/type 衝突の検出
- 型 alias の実体化
- クロスモジュールの data / enum 初期化
- クロスモジュールの型メソッド解決
- `impl ... as ...` および `impl ... for ... by ...` の継承・委譲解決

---

## 6. `deps/manifest.lock.json`

`deps/manifest.lock.json` は import / 実行時ロードに必要な依存関係の決定済み記録である。

このファイルは「依存が存在した」という宣言ではなく、**どの実体をロードすべきか** を再現可能にするための lock である。

### 6.1 必須項目

依存ごとに少なくとも以下を持つ。

- `id`
- `version`
- `source`
- `contentHash`
- `runtimeAssets`
- `nativeAssets`

### 6.2 `source` の表現

`source` は自由文字列にしてはならない。

v1 では次のいずれかの URI 風表現を使う。

- `nuget:<feed-url>#<package-id>`
- `path:<normalized-absolute-or-project-relative-path>`

例:

- `nuget:https://api.nuget.org/v3/index.json#Some.Package`
- `path:../local-packages/Some.Package`

### 6.3 推奨構造

```json
{
  "dependencies": [
    {
      "id": "Some.Package",
      "version": "1.2.3",
      "source": "nuget:https://api.nuget.org/v3/index.json#Some.Package",
      "contentHash": "sha256:...",
      "runtimeAssets": [
        ["lib/net10.0/Some.Package.dll", "sha256:..."]
      ],
      "nativeAssets": []
    }
  ]
}
```

### 6.4 解釈規則

- `.atlalib` は依存パッケージの DLL 全体を必ずしも内包しない。
- import 側は `manifest.lock.json` を使って依存を復元または検証し、実際のロード対象 DLL 群を確定する。
- `runtimeAssets` は import 時の .NET 型解決と実行時ロードの両方に使う。
- `nativeAssets` は実行時配置に使う。

---

## 7. `hashes/sha256sums.txt`

`hashes/sha256sums.txt` はコンテナ内エントリ整合性の検証用である。

- 形式は `<sha256>  <path>` とする。
- 少なくとも v1 必須エントリは全件記載する。
- 行順は決定的でなければならない。

---

## 8. import 解決仕様

本節は `.atlalib` 利用時の import 解決契約を定義する。

### 8.1 import 解決の優先順位

`import A'B` は次の順で解決する。

1. 同一コンパイル要求内の Atla 定義
2. 依存 `.atlalib` が export する Atla 定義
3. 依存 DLL 群から解決される .NET 型

同一優先順位層の中で、同じ完全名 `A.B` に対して Atla モジュールと Atla 型が同時に見つかった場合、**優先して選ばず曖昧エラー** とする。

推奨診断形式:

- code: `E_IMPORT_AMBIGUOUS_NAME`
- message: `import 'A.B' is ambiguous in dependency layer '<layer>': both module and type were found`

理由:

- 後方で同名モジュールまたは同名型が追加されたときの静かな意味変化を防げる。
- import の意味変化をすべて compile error に寄せた方が実装と利用者の両方にとって追跡しやすい。
- v1 の import 構文には module/type の明示ディスアンビギュエーション構文がないため、曖昧時は失敗させる方が安全である。

### 8.2 import 側の必須処理

`.atlalib` を入力として採用する import 実装は、少なくとも以下を行う。

1. `.atlalib` を発見する。
2. `atlalib.json` を読み、`formatVersion` / `languageAbi` / `symbolSchemaVersion` を検証する。
3. `hashes/sha256sums.txt` を検証する。
4. `manifest.lock.json` を読み、依存 runtime asset を復元または検証する。
5. `public.api.json` を読み、公開シンボル表現を import 用内部表現へ変換する。
6. 依存 DLL をロードして .NET 型解決可能な状態を作る。
7. Atla モジュール import / 型 import / .NET 型 import を優先順位どおりに解決する。

### 8.3 import 側が再構築すべき内部情報

import 側は少なくとも以下を内部的に再構築する必要がある。

- モジュール別 export 一覧
- 型 alias から実体型への対応
- クロスモジュール member 解決に必要な型メタデータ
- enum case 情報
- role 情報
- baseType / delegatedByFieldName

### 8.4 import 失敗条件

以下はエラーとする。

- `formatVersion` のメジャー不一致
- `languageAbi` 不一致
- `symbolSchemaVersion` の非互換
- `hashes/sha256sums.txt` 検証失敗
- `manifest.lock.json` と実際の依存 asset 不一致
- import 対象シンボルが `public.api.json` に存在しない

依存ロード中の連鎖的な欠損は Warning 扱いとして継続してよいが、次の手順で **有効 runtime asset** を判定し、その結果に従って継続可否を決める。

1. `manifest.lock.json` に列挙された候補 asset ごとに、ファイル存在確認を行う。
2. 存在する asset について記録済みハッシュと一致するか検証する。
3. ハッシュ一致 asset ごとに assembly load を試みる。
4. load 成功、または chained dependency 欠損 Warning のみで load context に保持できた asset を **有効 runtime asset** とみなす。

- 当該依存について少なくとも 1 つの runtime asset ロード経路が有効である。
- 利用中の import 文が要求する .NET 型またはネイティブメンバー解決に失敗していない。

次のいずれかに該当した時点で **最終 import 解決失敗** とみなし、エラーで停止する。

- import 対象の完全名が最終的に Atla モジュール / Atla 型 / .NET 型のいずれにも解決されない。
- import 自体は成功したが、その後の型解決またはメンバー解決で必要な .NET 型が解決できない。
- 依存ごとの候補 runtime asset がすべて無効で、必要なロードコンテキストを構築できない。

---

## 9. 生成フロー（`package.type: lib`）

推奨フローは以下とする。

1. 通常どおり Semantic Analysis から CIL 生成まで実行する。
2. Semantic Analysis 完了結果から import 用公開シンボル表現を抽出する。
3. 依存 lock を生成する。
4. `atlalib.json` を生成する。
5. ZIP コンテナへ必須エントリを格納する。
6. `hashes/sha256sums.txt` を生成する。
7. 出力整合性を検証する。
8. `Generated: <name>.atlalib` を表示する。

---

## 10. 互換性ポリシー

### 10.1 バージョン軸

- `formatVersion`: コンテナ構造の互換性
- `languageAbi`: Atla 言語 ABI / import 契約の互換性
- `symbolSchemaVersion`: `public.api.json` 形状の互換性

### 10.2 互換性判定

- `formatVersion` のメジャー不一致: ロード拒否
- `languageAbi` 不一致: ロード拒否
- `symbolSchemaVersion` のメジャー不一致: ロード拒否
- `public.api.json` の破壊的変更: `symbolSchemaVersion` メジャー更新必須
- import 契約を壊す言語変更: `languageAbi` 更新必須

---

## 11. 非目標

v1 では以下を目標にしない。

- HIR / MIR の同梱
- 増分コンパイルキャッシュの標準化
- 署名検証の厳格運用
- 依存パッケージ本体の完全同梱
- 旧フォーマットとの互換レイヤー

---

## 12. 今後の拡張候補

- `ir/hir.bin`, `ir/mir.bin` の任意格納
- 署名と証明書チェーン検証
- ソースインデックスやドキュメントの同梱
- マルチターゲット成果物の同梱
- import 用公開シンボルのバイナリ表現追加
