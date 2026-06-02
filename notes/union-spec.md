# union 仕様メモ（2026-06-01）

## 対象

Atla 言語の `union` 型の構文・意味・lowering・制約をまとめる。

`union` は旧 `enum` を完全に置き換える代数的データ型（タグ付き直和）であり、
.NET の **抽象クラス（ルート型）+ 派生クラス（バリアント）** へ lower される。
タグ整数とペイロード型による表現（旧 enum）ではなく、ネイティブな継承階層を用いる。

---

## 1. 構文

### 1.1 基本形

```atla
union Color
    struct Rgb: Color
        val r: Int
        val g: Int
        val b: Int

    object Black: Color
```

- `union <Name>` がルート型を宣言する。
- ルート型本体に `val` を書くと、**全バリアントが継承する共有フィールド**になる。
- バリアントは union 本体内に並べる。各バリアントは `: <ParentUnion>` で親を明示する。
- バリアントには 2 種類ある。
  - **`struct` バリアント**: 自身のフィールド（`val ...`）を持つ。
  - **`object` バリアント**: 自身のフィールドを持たない単集合値。継承フィールドがある場合は
    `name = expr` 形式で初期値を供給する。

```atla
union Color
    val alpha: Int

    object RichBlack: Color
        alpha = 255          # 継承フィールド alpha の初期値

    struct Rgb: Color
        val r: Int
```

### 1.2 ジェネリック union

```atla
union Opt T
    object None: Opt
    struct Some: Opt
        val value: T
```

- 型パラメータはルート名の後ろに並べる（例: `union Opt T`）。
- 全バリアントはルートの型パラメータを共有する。
- フィールド型に型パラメータ（`T`）を使用できる。

### 1.3 ネスト union

union のバリアントとして、さらに別の `union` を宣言できる。

```atla
union Color
    val alpha: Int

    struct Rgb: Color
        val r: Int

    union HueColor: Color
        val h: Int
        val s: Int

        struct Hsv: HueColor
            val v: Int

        struct Hsl: HueColor
            val l: Int
```

- ネスト union は親 union を基底型とする独立した型を生成する（再帰的なクラス階層）。
- リーフバリアントの修飾名は多段になる（例: `Color'HueColor'Hsv`）。
- 共有フィールドは継承チェーンを辿って合成される
  （`Hsv` は `HueColor` の `h, s` と `Color` の `alpha` を継承する）。

### 1.4 extendable union

```atla
extendable union Color
    val alpha: Int

    object RichBlack: Color
        alpha = 255

    struct Rgb: Color
        val r: Int

# union 本体外で追加するバリアント（external variant）
struct Cmyk: Color
    val k: Int

object Transparent: Color
    alpha = 0
```

- `extendable` 修飾を付けた union は、本体外で `: <Union>` を付けたバリアントを追加できる。
- `extendable` union は **match の網羅性チェックを行わない**（外部で任意にバリアントが
  追加されうるため、すべてのケースを静的に把握できない）。

---

## 2. 値構築

| 形 | 例 | 用途 |
|---|---|---|
| ブレース構築 | `Color'Rgb { r = 255, g = 0, b = 0 }` | フィールドを名前指定。継承フィールドも同じブレースで供給。 |
| concatenative 構築 | `value Opt'Some.` | own フィールド数と引数数が一致する単純バリアント（位置引数）。 |
| 0 引数値 | `Opt'None` | object バリアント、または全フィールドが object 初期値で供給されるバリアント。 |

- 多段修飾名で構築する（ネスト: `Color'HueColor'Hsv { h = .., s = .., v = .., alpha = .. }`）。
- concatenative 構築は **own フィールドのみ位置対応**する。継承フィールドを持つバリアントは
  ブレース構築（`{ ... }`）を使う必要がある。
- ジェネリックバリアントの型パラメータは構築引数 / 期待型から推論・具体化される。

---

## 3. パターンマッチ

```atla
match self
| Color'Rgb { r, .. } -> r
| Color'HueColor'Hsv { h, s, v, .. } -> (h * s * v) / 10000
| Color'Black -> 0
```

- バリアント名は多段修飾名を取りうる（`Color'HueColor'Hsv`）。
- フィールド束縛は `{ field1, field2, .. }`（`..` で残りのフィールドを無視）。
- object バリアント / フィールド束縛不要のバリアントは `Color'Black` のように書く。
- **網羅性チェック**: `extendable` でない union は全バリアントを尽くさなければならない。
  尽くしていない場合は "Non-exhaustive match"。
  - ネスト union の場合、リーフバリアントまで尽くす必要がある。
- **重複アーム**: 同一バリアントを複数回マッチするとエラー。

### let-else / var-else

```atla
fn printValue (o: Opt Int): ()
    val Opt'Some x = o
    | else -> return
    x Console'WriteLine.
```

- RHS が指定バリアントにマッチしたらフィールドを束縛して継続。
- マッチしなければ `| else -> ...` を実行。else 末尾は `return` / `break` / `continue` で
  発散しなければならない。
- `val` は束縛変数がイミュータブル、`var` はミュータブル。
- RHS の型はルート型でもバリアント型でもよい。ジェネリックの型パラメータは RHS の
  具体型引数から解決する。

---

## 4. サブタイピング

バリアント型はルート型のサブタイプであり、ルート型が期待される位置へ **upcast** できる。

```atla
struct Box
    val _value: Opt Int

impl Box
    fn new: Box
        { _value = Opt'None } Box.          # Opt'None -> Opt<int>

fn printValue (o: Opt Int): ()
    ...

(1 Opt'Some.) printValue.                    # Opt'Some<int> -> Opt<int>（関数引数位置）
```

`unifyOrError` のサブタイプ判定が次を許容する。

- 非ジェネリック: バリアント `Name` → ルート `Name`（baseType 連鎖を辿る）。
- ジェネリック: `App(Variant, args)` → `App(Root, args)`（head のサブタイプ + 型引数 unify）、
  および `App(Root, args)` ↔ `Name(Variant)` の組み合わせ。
- 関数型 `Fn`: 引数位置は反変、戻り値位置は共変。これにより
  「バリアント値を、ルート型を期待する関数へ渡す」呼び出しが型検査を通る。

旧 enum はバリアントをルート型へ lower していたため、この upcast を暗黙に提供していた。
union ではバリアントが独立した型になるため、サブタイプ判定として明示的に扱う。

---

## 5. Lowering

### 5.1 型表現

- **ルート型**: union 本体の共有フィールドを持つ **abstract class**（`isAbstract=true`）。
  ネスト union は親 union を基底型に持つ。
- **各バリアント**: `baseType = ルート型` の具象クラス。**自身のフィールドのみ**を保持し、
  共有フィールドは継承で共有する（メンバー解決が baseType 連鎖を辿る）。
- object バリアントの継承フィールド初期値は、構築時に親フィールドへ代入する形で供給される。

### 5.2 match の lowering

- match は **`isinst` 型テストの if 連鎖**へ lower される。
- 各アームを「スクルーティニーが当該バリアント型か」の `Hir.Expr.TypeTest` で分岐し、
  マッチ時に `castclass` 相当のキャスト後にフィールドを束縛する。
- 最後のアームはフォールスルー（else）として扱われ、型テストを省く。

### 5.3 メタデータ（AnalyzeEnv）

- `DataTypeDef.unionInfo : UnionTypeDef option` — ルート型にのみ設定される。
  - `UnionTypeDef.isExtendable` — 網羅性チェックの有無。
  - `UnionTypeDef.variants : UnionVariantDef list` — 宣言順のバリアント一覧（網羅性の基準集合）。
- `UnionVariantDef` — `name`（修飾なしバリアント名）, `typeSid`, `isUnion`（ネスト union か）,
  `objectFieldInits`（object バリアントの継承フィールド初期値）。
- バリアントの `DataTypeDef` は `baseType = Some(ルート型)` を持ち、`unionInfo = None`。

---

## 6. フェーズ別実装メモ

### Lexer / Parser
- キーワード: `union`, `struct`, `object`, `extendable`。
- ルート宣言 `unionDecl`、バリアント（`struct`/`object`/ネスト `union`）、external variant を解析。
- 構築式は共有の `Ast.Expr.EnumInit`、パターンは共有の `Ast.Pattern.Enum` を再利用する
  （命名は歴史的経緯。union/旧 enum で共通の AST ノード）。

### Resolve
- `ResolvedUnionDecl` にルート型 SID、型パラメータ、修飾名、親 union SID、
  バリアント名→SID マップ、external variant を格納する。
- `registerUnion` がネスト union を再帰的に登録する。

### Analyze
- `resolvedModule.unionDecls` を fold し、ルート型（abstract class）と各バリアント型を
  `Hir.Type` / `DataTypeDef` として生成する（§5.1）。
- インポートした union は import 経路で root + バリアント DataTypeDef へ再構築する
  （エクスポート済みの `type:` / `field:` SID を再利用）。

### ExprAnalyze
- `Ast.Expr.EnumInit`: union バリアントのブレース / 0 引数構築。
- `Ast.Expr.Apply`: concatenative 構築 `arg ... Union'Variant.`。
- `Ast.Expr.MemberAccess`: `Union'Variant` 値構築（ブレースなし）。
- `match`: `unionInfo` を辿り isinst if 連鎖へ lower、網羅性 / 重複チェック。
- `let-else` / `var-else`: isinst テスト + castclass フィールド束縛へデシュガー。
- `unifyOrError`: バリアント → ルートのサブタイプ判定（§4）。

---

## 7. 既知の制約

- **`.atlalib` パッケージへの union export 未対応**: `Atla.Build` の型 export は現状
  `data` / `role` のみを出力する。`compileModules`（ソース全体を同時コンパイル）経由の
  クロスモジュール union は動作するが、コンパイル済み `.atlalib` 依存として配布した
  union は import 側で再構築されない。
