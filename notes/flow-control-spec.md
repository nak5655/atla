# フロー制御・ループ制御 仕様メモ（2026-05-28）

## 対象

Atla 言語のフロー制御文とループ制御文の構文・意味・制約をまとめる。

- `return expr` — 関数からの早期リターン
- `break` — ループからの早期脱出
- `continue` — ループの次のイテレーションへスキップ
- `for x in iterable` — イテラブルに対するループ
- `while cond` — 条件付きループ
- `let-else` / `var-else` — パターンマッチ付き束縛（else で発散必須）

---

## 構文

### return

```atla
fn unwrap (o: Opt Int): Int =
    let Opt'Some v = o
    | else -> return -1
    v
```

- 関数本体（do ブロック含む）のどこでも使用可能。
- `return expr` で式の値を返し、残りの文は実行されない。
- 戻り値の型は宣言済み関数戻り型と単一化される。

### break

```atla
for i in items
    if | i == stop =>
        break
    i process.

var x = 0
while x < 100
    if | x == 3 =>
        break
    x = x + 1
```

- `for` または `while` の本体内でのみ有効。
- ループ外での使用はコンパイルエラー。

### continue

```atla
for i in items
    if | i == skip =>
        continue
    i process.

var i = 0
while i < 10
    i = i + 1
    if | i % 2 == 0 =>
        continue
    i print.
```

- `for` または `while` の本体内でのみ有効。
- `for`: 次の `MoveNext()` 呼び出しへスキップ（ループ先頭へジャンプ）。
- `while`: 条件の再評価点へジャンプ（ループ先頭へジャンプ）。
- ループ外での使用はコンパイルエラー。

### for

```atla
for i in collection
    i process.
```

- `collection` は `IEnumerable` 互換型（`MoveNext()` + `Current` を持つ型）。
- 反復変数 `i` のスコープはループ本体のみ（ループ外からは参照不可）。
- `for` 本体内で `break` / `continue` が使用可能。

### while

```atla
while cond
    body
```

- `cond` は `Bool` 型。
- 本体は `cond` が `True` の間繰り返し実行される。
- `while False` の場合、本体は一度も実行されない。
- `while` 本体内で `break` / `continue` が使用可能。

### let-else / var-else

```atla
let Type'Case x = expr
| else -> return defaultValue

var Type'Case x = expr
| else -> return defaultValue
```

- RHS が指定ケースにマッチした場合、フィールドを束縛して後続処理を継続する。
- マッチしなかった場合、`| else -> body` を実行する。
- `else` ブランチの末尾は `return` / `break` / `continue` のいずれかで発散しなければならない（強制）。
- 発散がない場合はコンパイルエラー。
- `let`: 束縛変数はイミュータブル。`var`: 束縛変数はミュータブル。
- パターンは enum パターンのみ（`Type'Case`、または `Type'Case { field }`）。

---

## 制約まとめ

| 文 | 使用可能な場所 | else 発散 | 備考 |
|---|---|---|---|
| `return expr` | 関数本体（do ブロック含む） | — | 型は宣言戻り型と単一化 |
| `break` | `for` / `while` 本体 | — | 最内ループのみ対象 |
| `continue` | `for` / `while` 本体 | — | 最内ループのみ対象 |
| `for x in e` | 任意の文脈 | — | `e` は `IEnumerable` 互換 |
| `while cond` | 任意の文脈 | — | `cond` は `Bool` |
| `let-else` / `var-else` | 任意の文脈 | 必須 | else 末尾が発散しないとエラー |

---

## フェーズ別実装メモ

### AST

- `Ast.Stmt.Return(expr, span)` — return 文。
- `Ast.Stmt.Break(span)` / `Ast.Stmt.Continue(span)` — ループ制御。
- `Ast.Stmt.For(varName, iterable, body, span)` — for ループ。
- `Ast.Stmt.While(cond, body, span)` — while ループ。
- `Ast.Stmt.LetElse(pattern, value, elseBranch, span)` / `Ast.Stmt.VarElse(...)` — パターン束縛+分岐。

### Semantic Analysis（ExprAnalyze）

- `return expr`: `analyzeExpr` で式を解析し `Hir.Stmt.Return` を生成。戻り型は呼び出し元が期待型として渡す。
- `break` / `continue`: そのまま `Hir.Stmt.Break` / `Hir.Stmt.Continue` へ変換。スコープ検証は Layout フェーズ。
- `for`: イテラブル型の `IEnumerable` 互換性検査、反復変数のサブスコープ宣言。
- `while`: 条件式を `TypeId.Bool` で解析。本体は外側スコープで解析（反復変数なし）。
- `let-else` / `var-else`: else ブランチ末尾の発散チェック。非発散時は `ErrorStmt` を末尾に追加。フィールド束縛は外側 nameEnv に宣言（ループ後でも参照可能）。

### HIR

- `Hir.Stmt.Return(value, span)` — ユーザー向け return（async 状態機械用の `ClosedHir.Stmt.Return` とは別）。
- `Hir.Stmt.For` / `Hir.Stmt.While` / `Hir.Stmt.Break` / `Hir.Stmt.Continue` はそのまま HIR に存在。
- `let-else` / `var-else` は HIR では `Hir.Stmt.If` + `Hir.Stmt.Let` の組み合わせにデシュガーされる（専用ノードなし）。
  - スクルーティニー束縛を `Hir.Expr.Block` 条件式内に埋め込むことで `analyzeStmt` の単一 `Hir.Stmt` 制約を回避。

### Closure Conversion

- `Hir.Stmt.Return` → `ClosedHir.Stmt.ReturnValue(value, span)`（値付き return）。
- `Hir.Stmt.While` → `ClosedHir.Stmt.While`（構造を保持）。
- `Hir.Stmt.Break` / `Hir.Stmt.Continue` → そのまま。

### AsyncRewrite

- `await` を含む `for` / `while` は Label/Goto 形式に展開（状態機械の resume ポイント生成のため）。
- `while` 展開形式:
  ```
  Label(loopStart)
  If(cond, [], [Goto(loopEnd)])   // cond が False → loopEnd へ
  body
  Goto(loopStart)
  Label(loopEnd)
  ```
- 展開時に本体内の `break` → `Goto(loopEnd)`、`continue` → `Goto(loopStart)` に置き換える。
- ネストしたループの `break` / `continue` は最内ループのみ対象（外側ループには踏み込まない）。

### Layout（MIR 生成）

- **for**: `MarkLabel(start) → CallAssign(cond, MoveNext) → JumpTrue/JumpFalse → MarkLabel(body) → CallAssign(current, Current) → Assign(var) → bodyIns → Jump(start) → MarkLabel(end)`
- **while**: `MarkLabel(start) → condIns → JumpFalse(cond, end) → bodyIns → Jump(start) → MarkLabel(end)`
- **break**: `Jump(loopEnd)` — `loopLabels` スタックの先頭から `loopEndId` を取得。
- **continue**:
  - `for`: `Jump(loopStart)` — `MoveNext()` 呼び出しへ戻る。
  - `while`: `Jump(loopStart)` — 条件再評価へ戻る。
- **return**: `RetValue(value)` → CIL `ret` 命令（値をスタックに積んで戻る）。
- `loopLabels` はスタック構造で、ネストしたループに対応する。

### CIL（Gen）

- `Mir.Ins.Jump` → `OpCodes.Br_S` / `Br`
- `Mir.Ins.JumpFalse` → `OpCodes.Brfalse_S` / `Brfalse`
- `Mir.Ins.JumpTrue` → `OpCodes.Brtrue_S` / `Brtrue`
- `Mir.Ins.MarkLabel` → `ILGenerator.MarkLabel`
- `Mir.Ins.RetValue` → スタックに値を積んで `ret`

---

## 設計上の注意点

### CIL ローカルはメソッドスコープ

CIL ローカル変数はメソッド全体でスコープを持つため、`if` ブランチ内や `while` 条件式内で宣言された一時変数も、後続のコードから参照可能。`let-else` のフィールド束縛が then ブランチ内の `Hir.Stmt.Let` として表現されても、ループ後の文から正しく参照できるのはこのため。

### break / continue のスコープ検証

スコープの正しさ（ループ外での使用禁止）は Layout フェーズで `loopLabels` スタックが空かどうかで検証する（`[] -> Error("'break' used outside of a loop")`）。Semantic フェーズでは検証しない。

### while の条件は毎イテレーション再評価

MIR では条件式の命令列 `condIns` が `MarkLabel(loopStart)` の直後に配置されるため、`Jump(loopStart)` によるループバック時に毎回再評価される。一時レジスタはメソッドスコープだが上書きされるため問題なし。

### let-else の発散チェック

`else` ブランチの末尾文が `Hir.Stmt.Return` / `Hir.Stmt.Break` / `Hir.Stmt.Continue` でなければ `ErrorStmt` を末尾に付加してコンパイルエラーとする。HIR レベルで処理するため、Layout 以降は通常の `If` 文として扱われる。
