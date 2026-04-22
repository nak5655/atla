# Plan

## 2026-04-22 保守性・責務分離の改善（フェーズ単位タスク）

前セッションの設計調査で特定された問題を、コンパイラパイプラインのフェーズ順に整理したタスク一覧。
各タスクは独立したコミット単位として実装する。

---

### フェーズ 1: HIR 型から変換後ノードを分離する

**問題**: `Hir.Expr` に `EnvFieldLoad` / `ClosureCreate` という変換後専用ノードが混在しており、
HIR が「型付きソース意味論 IR」という役割を逸脱している（AGENTS.md §5 HIR Rules 違反）。
意味解析フェーズ（`Infer.fs` 等）がクロージャー変換の実装詳細を知らなければならない状態になっている。

**実装内容**:
- [x] `PLANS.md` に本タスク計画を追記する（このエントリ）。
- [x] `src/Atla.Core/Semantics/Data/` 配下に `ClosedHir.fs` を新設し、
  `ClosedHir.Expr` 型を定義する（`Hir.Expr` の全ケース＋`EnvFieldLoad`/`ClosureCreate`）。
- [x] `Hir.Expr` から `EnvFieldLoad` / `ClosureCreate` を削除し、`Hir.Expr` をソース意味論のみに戻す。
- [x] `ClosureConversion` の入力型を `Hir.Assembly`、出力型を `ClosedHir.Assembly` に変更する。
  - `rewriteExpr` / `rewriteStmt` の戻り型を `ClosedHir.Expr` / `ClosedHir.Stmt` に変える。
- [x] `Infer.fs` の `EnvFieldLoad` / `ClosureCreate` に対するパターンマッチ pass-through を削除する。
- [x] `Layout.fs` の入力型を `ClosedHir.Assembly` に変更し、`layoutExpr` が `ClosedHir.Expr` を受け取るよう更新する。
- [x] `Atla.Core.Tests` をビルド・テスト実行して全テストが通ることを確認する。

---

### フェーズ 2: `closureInvokeMethods` を HIR から分離する

**問題**: `Hir.Module` のオプション引数として `closureInvokeMethods: Map<int, int>` が混入しており、
HIR のデータ定義がクロージャー変換の産物（`liftedMethodSid -> envTypeSid` マッピング）を保持している。
これは HIR の責務を超えた変換フェーズ固有情報である。

**実装内容**:
- [x] `PLANS.md` に本タスク計画を追記する（このエントリ）。
- [x] フェーズ 1 完了後、`ClosedHir.Module` に `closureInvokeMethods: Map<int, int>` を正式フィールドとして定義する
  （`Hir.Module` のオプション引数は削除する）。
- [x] `ClosureConversion.preprocessAssembly` の戻り値（`ClosedHir.Assembly`）が
  `closureInvokeMethods` を `ClosedHir.Module` フィールドとして返すよう変更する。
- [x] `Layout.layoutModule` が `ClosedHir.Module.closureInvokeMethods` を参照するよう変更する
  （`hirModule.closureInvokeMethods` の参照先を `ClosedHir.Module` に切り替える）。
- [x] `Atla.Core.Tests` をビルド・テスト実行して全テストが通ることを確認する。

---

### フェーズ 3: `ClosureConversion` をコンパイルパイプラインへ明示的に組み込む

**問題**: `Layout.layoutAssembly` 内部で `ClosureConversion.preprocessAssembly` を直接呼び出しており、
クロージャー変換フェーズがパイプライン上から不可視になっている（AGENTS.md §3「All phase boundaries MUST be explicit module boundaries」違反）。

**実装内容**:
- [x] `PLANS.md` に本タスク計画を追記する（このエントリ）。
- [x] `Layout.layoutAssembly` から `ClosureConversion.preprocessAssembly` の呼び出しを削除する。
  - `Layout.layoutAssembly` の入力型を `ClosedHir.Assembly`（フェーズ 1・2 の成果物）に変更する。
  - 残留 Lambda チェック（`hasLambdaExpr`）は `ClosedHir.Expr` に対して実行するよう修正する。
- [x] `Compile.fs` のパイプラインに `ClosureConversion.preprocessAssembly` を明示的に追加する。
  ```fsharp
  |> Result.bind ClosureConversion.preprocessAssembly
  |> Result.bind (Layout.layoutAssembly request.asmName)
  ```
- [x] `Atla.Core.Tests` をビルド・テスト実行して全テストが通ることを確認する。

---

### フェーズ 4: `Mir.Frame` / `Mir.Label` の可変状態を解消する

**問題**: `Mir.Frame` は `mutable` フィールドを持つクラス、`Mir.Label` も内部状態が可変であり、
AGENTS.md §6「mutable bindings MUST NOT be used in compiler phases」に直接違反している。
副作用ベースのレジスタ割り当てにより、フェーズの純粋性が保証できない。

**実装内容**:
- [x] `PLANS.md` に本タスク計画を追記する（このエントリ）。
- [x] `Mir.Frame` を不変レコードに置き換える。
  - `addArg` / `addLoc` は新しいフレームを返す純粋関数として再定義する。
  - `Layout.fs` 内でフレームを引き回している箇所を、状態渡し（fold パターン）で書き直す。
- [x] `Mir.Label` の可変フィールド（`_label`, `_ilOffset`）を、
  ラベル解決を後段（Gen フェーズ）の責務として切り出す形に変更する。
  - Gen フェーズで `Dictionary<LabelId, Label>` を管理し、命令生成時に解決する。
- [x] `Atla.Core.Tests` をビルド・テスト実行して全テストが通ることを確認する。

---

### フェーズ 5: `Layout.fs` / `ClosureConversion.fs` のエラー処理を `Result` へ統一する

**問題**: `Layout.fs` と `ClosureConversion.fs` 全体で `failwith` / `failwithf` が制御フローに多用されており、
AGENTS.md §8「Exceptions MUST NOT be used for control flow」に違反している。
エラー箇所のスパン情報が失われ、診断品質が低下する。

**実装内容**:
- [x] `PLANS.md` に本タスク計画を追記する（このエントリ）。
- [x] `Layout.fs` の `layoutExpr` / `layoutStmt` の戻り型を `Result<LayoutState * KNormal, Diagnostic>` / `Result<LayoutState * Mir.Ins list, Diagnostic>` に変更する。
  - `failwith` を `Result.Error (Diagnostic.Error(..., span))` に置き換える。
  - 呼び出し側を `Result.Error e -> Result.Error e` / `Ok (state, kn) -> ...` でチェーンする。
  - `mapFoldResult` / `collectResults` ヘルパーを追加してリスト走査時の Result 伝播を簡潔にする。
- [x] `layoutMethod` / `layoutInvokeMethod` / `layoutModule` の戻り型も Result に変更する。
  - `layoutModule` は `Result<Mir.Module, Diagnostic list>` を返す（複数エラーを集約）。
  - `layoutAssembly` の try-catch を除去し、Result チェーンに統一する。
- [x] `ClosureConversion.fs` の `rewriteExpr` 系で `failwith` が残っている箇所を確認 → 該当なし（既に 0 件）。
- [x] `Atla.Core.Tests` に診断スパン付きエラーを検証するテストを追加する（`ExprError`・`ErrorStmt`・未定義変数代入の 3 ケース）。
- [x] `Atla.Core.Tests` をビルド・テスト実行して全テストが通ることを確認する（90 → 93 テスト）。

---

### フェーズ 6: HIR トラバーサルを共通 fold/map インフラで統合する

**問題**: `ClosureConversion.fs` に `collectFreeVarsExpr`・`rewriteExpr`・`rewriteCapturedRefs`・
`collectMaxSymbolIdExpr` の4系統の式ツリートラバーサルが独立して実装されており、
新しい `Hir.Expr` ケースを追加するたびに4箇所すべての修正が必要になる。

**実装内容**:
- [x] `PLANS.md` に本タスク計画を追記する（このエントリ）。
- [x] `src/Atla.Core/Semantics/Data/Hir.fs`（またはヘルパーモジュール）に
  `mapExpr : (Expr -> Expr) -> Expr -> Expr` および
  `foldExpr : ('a -> Expr -> 'a) -> 'a -> Expr -> 'a` を追加する。
  - 対応する `mapStmt` / `foldStmt` も追加する。
  - `ClosedHir.fs` にも同様の `mapExpr`/`foldExpr`/`mapStmt`/`foldStmt` を追加する。
- [x] `ClosureConversion.fs` の各トラバーサルを上記インフラを用いて書き直す。
  - `collectFreeVarsExpr` → コンテキスト依存（bound set がラムダごとに変化）のため明示再帰を維持。
  - `rewriteExpr` → Hir.Expr→ClosedHir.Expr の型をまたぐ変換＋状態渡しのため明示再帰を維持。
  - `rewriteCapturedRefs`/`rewriteCapturedRefsStmt` → `ClosedHir.mapExpr`/`ClosedHir.mapStmt` で書き直す。
  - `collectMaxSymbolIdExpr`/`collectMaxSymbolIdStmt` → `Hir.foldExpr`/`Hir.foldStmt` で書き直す。
- [x] `Atla.Core.Tests` をビルド・テスト実行して全テストが通ることを確認する。

---

### フェーズ 7: SymbolId 採番器を一元化する

**問題**: `Analyze.fs` が `SymbolTable.NextId()` を使い、
`ClosureConversion` が `ConversionState.nextSymbolId`（`maxInModule + 1` から独立採番）を管理している。
型で衝突防止が保証されておらず、フェーズ追加時にリスクになる。

**実装内容**:
- [x] `PLANS.md` に本タスク計画を追記する（このエントリ）。
- [x] `ClosureConversion.preprocessAssembly` の引数に `SymbolTable` を追加し、
  `allocateSymbolId` が `SymbolTable.NextId()` を使うよう変更する。
- [x] `ConversionState.nextSymbolId` フィールドと `collectMaxSymbolId` 系の関数を削除する。
- [x] `Compile.fs` で `ClosureConversion.preprocessAssembly` の呼び出しに `symbolTable` を渡す。
- [x] `Atla.Core.Tests` をビルド・テスト実行して全テストが通ることを確認する。

---

### フェーズ 8: `foldExprWithCtx` の追加と `collectFreeVarsExpr` のフェーズ分離

**問題（問題1）**: `collectFreeVarsExpr` が `foldExpr` で表現できない Reader 方向の文脈依存（Lambda 境界で
`bound` をリセット）を持ち、既存の線形アキュムレーター型 `foldExpr` と根本的に非互換。

**問題（問題2）**: `rewriteExpr` が `bound` / `bindings` / `globalSymbols` を直列スレッドしながら
Hir→ClosedHir の型越え変換を行っており、捕捉解析と構造変換が混在している。

**実装内容**:
- [x] `PLANS.md` に本タスク計画を追記する（このエントリ）。
- [x] `Hir.fs` に `foldExprWithCtx` / `foldStmtWithCtx` を追加する（Reader 文脈付き畳み込みインフラ）。
  - `descend`: 各 Expr ノードへ降りる前に文脈を更新（Lambda 境界で bound をリセット等）。
  - `afterStmt`: Block 内 Stmt 処理後に文脈を更新（Let 束縛の逐次追加等）。
  - `leaf`: リーフノードで値を生成。`merge` / `zero`: 兄弟結果を合成。
- [x] `ClosedHir.fs` に同様の `foldExprWithCtx` / `foldStmtWithCtx` を追加する（`EnvFieldLoad` / `ClosureCreate` もリーフ扱い）。
- [x] `ClosureConversion.fs` の `collectFreeVarsExpr`・`collectFreeVarsStmts`・`collectFreeVarsStmt`（3関数）を、
  `ClosedHir.foldExprWithCtx` を使った 1 関数 `collectFreeVarsExpr` に置き換える。
  - HIR 版の自由変数収集は `collectFreeVarsHirExpr`（`Hir.foldExprWithCtx` ベース）として追加。
- [x] キャプチャー解析（Phase 1）を `rewriteExpr` から独立した純粋関数 `buildCaptureMap` として抽出する。
  - `Hir.foldExprWithCtx` を用いて各 Lambda の自由変数を算出し、外側 bindings で型情報を解決。
  - 戻り値: `Map<Span, CapturedVarMetadata list * int list>`（型不明 sid はエラー報告用 int list）。
- [x] Phase 2 `rewriteExpr` のシグネチャから `bound` / `bindings` / `globalSymbols` を除去し、
  代わりに `captureMap: Map<Span, CapturedVarMetadata list * int list>` を受け取る形に簡略化する。
- [x] `rewriteStmt` / `rewriteStmts` も同様に `bound` / `bindings` スレッドを除去する。
- [x] `rewriteMethods` で Phase 1（`buildCaptureMap`）→ Phase 2（`rewriteExpr`）の順に呼び出すよう更新する。
- [x] `Atla.Core.Tests` をビルド・テスト実行して全テストが通ることを確認する（93 テスト全通過）。

---

**問題**: `rewriteStmts` / `rewriteMethods` で `@ [item]` 形式の末尾追加を繰り返しており、
文数・メソッド数に対して O(n²) のリスト連結コストが発生している。

**実装内容**:
- [ ] `PLANS.md` に本タスク計画を追記する（このエントリ）。
- [ ] `ClosureConversion.fs` の `rewriteStmts` / `rewriteMethods` を
  逆順蓄積＋`List.rev` パターンへ書き直す。
- [ ] 同様のパターンが他のファイル（`Layout.fs` 等）に存在しないか確認し、あれば同様に修正する。
- [ ] `Atla.Core.Tests` をビルド・テスト実行して全テストが通ることを確認する。

---

## 2026-04-21 クロージャー実装タスク（追加5件バッチ）

### 実施内容
- [x] `ClosureConversion` で捕捉変数メタデータ（`sid` / `isMutable` / `typ`）を収集する内部モデルを追加する。
- [x] 捕捉ラムダ診断に mutable 捕捉シンボル一覧（`mutable=[...]`）を含める。
- [x] 自由変数判定を修正し、外側束縛を捕捉対象として扱う（ラムダ引数のみを束縛集合へ入れる）。
- [x] `Layout` に「closure conversion 後の残留 Lambda」を検出する前提チェックを追加する。
- [x] `Atla.Core.Tests` に回帰テストを追加/更新する（mutable 捕捉診断、決定性、for-scope/mutable capture 解析）。

## 2026-04-21 C#互換寄りクロージャー仕様との差分解消タスク（env-class本実装）

### 目的
- 現状の「非捕捉ラムダのみ lambda lifting / 捕捉ラムダは診断失敗」から、C#互換寄り仕様（env-class方式）へ移行する。
- 捕捉ラムダを `AST -> Semantic -> HIR -> Frame Allocation -> MIR -> CIL` の既存パイプライン内で正しく lowering する。

### 現状との差分（要解消）
- 捕捉ラムダは `ClosureConversion` で診断失敗となり、成功パスが存在しない。
- mutable 変数捕捉（by variable, 共有セル）仕様が未実装。
- env-class の生成・delegate target バインド ABI が未実装。

### 仕様（C#互換寄りで固定）
- 捕捉は「値」ではなく「変数」単位で行う。
  - immutable は値保持可。
  - mutable は共有セル（box/ref）経由で読み書きする。
- 捕捉順序は `SymbolId` 昇順で固定し、env フィールド順・初期化順・補助メソッド引数順へ同一順序を適用する。
- 非捕捉ラムダは現状どおり static delegate 化する。
- 捕捉ラムダは `env-instance + lifted invoke` で delegate を構築する。
- ループ反復変数捕捉は反復ごとの新規束縛として扱う（C#互換）。

### 実装タスク
- [x] `PLANS.md` に本タスク計画を追記する。
- [x] `Lowering/ClosureConversion.fs`
  - [x] 捕捉ラムダ失敗診断を成功変換へ置換する。
  - [x] env-class 用メタデータ（env type symbol, captured field list, mutable-cell 要否）を生成する。
  - [x] ラムダ本体を lifted method へ変換し、env 参照アクセスへ書き換える（`EnvFieldLoad`）。
- [x] `Lowering/Data/Mir.fs`
  - [x] env-class 生成に必要な MIR 表現（`NewEnv` / `StoreEnvField` / `LoadEnvField`）を拡張する。
  - [x] target 付き delegate 生成を表現できる値/命令を追加する。
- [x] `Lowering/Layout.fs`
  - [x] env インスタンス生成・captured 値の格納・delegate 構築を出力する（`ClosureCreate` / `EnvFieldLoad` lowering）。
  - [x] closure invoke メソッドを `Mir.Type.methods` にルーティングする（`closureInvokeMethods` マップ使用）。
  - [x] 既存 `Hir.Expr.Lambda` failwith 到達を排除する（前処理後の不変条件として保証）。
- [x] `Lowering/Gen.fs`
  - [x] target 付き delegate 生成 IL（`ldarg/ldloc target; ldftn; newobj`）を実装する。
  - [x] env-class メンバー生成と参照解決を実装する（`typeCtors` / `fieldBuilders`）。
  - [x] env-class invoke インスタンスメソッド定義（`MethodAttributes.Public`）と `methodBuilders` 登録を実装する。
- [x] `Semantics/Data/Hir.fs`
  - [x] `Hir.Expr.EnvFieldLoad` と `Hir.Expr.ClosureCreate` を追加する。
  - [x] `Hir.Module` に `closureInvokeMethods: Map<int,int>` をオプション引数で追加する。
- [x] `Semantics/Infer.fs`
  - [x] 新 HIR ノード（`EnvFieldLoad` / `ClosureCreate`）の pass-through を追加する。
- [x] テスト
  - [x] `Atla.Core.Tests/Lowering/LayoutTests.fs`: captured lambda の成功 lowering ケースを追加。
  - [x] 既存テストを新動作（env-class 変換成功）に合わせて更新する。

## 2026-04-21 クロージャーlowering実装（非捕捉ラムダのlambda lifting）

### 目的
- `Hir.Expr.Lambda` が `Layout` で `failwith` になる現状を解消し、Frame Allocation 前段で lowering を実施する。
- まずは決定的で安全な最小単位として「非捕捉ラムダ」を lambda lifting で lowering し、捕捉ラムダは明示診断を維持する。
- 今後の env-class 方式拡張に必要な `Hir.Arg` の `SymbolId` 連携を導入する。

### 仕様
- `Hir.Arg` は `sid: SymbolId` を保持する。
- `ClosureConversion.preprocessAssembly` は以下を行う。
  - ラムダ本体を再帰変換し、非捕捉ラムダを module-level method へ lambda lifting する。
  - 生成メソッドのシンボルは module 内で一意な連番を採番する（決定的）。
  - ラムダ式は `Hir.Expr.Id(liftedSid, lambdaType, span)` へ置換する。
  - 捕捉ラムダは `methodSid` と `captured` を含む診断を返して失敗する。
- `Layout` は `ClosureConversion` 後の HIR を前提に lowering し、非捕捉ラムダを通常の `FnDelegate` 経路で MIR 化する。

### 実装内容
- [x] `PLANS.md` に本タスク計画を追記する。
- [x] `src/Atla.Core/Semantics/Data/Hir.fs` の `Hir.Arg` に `sid` を追加する。
- [x] `src/Atla.Core/Semantics/Analyze.fs` と `src/Atla.Core/Semantics/Infer.fs` のラムダ引数生成を `sid` 対応へ更新する。
- [x] `src/Atla.Core/Lowering/ClosureConversion.fs` を「非捕捉ラムダのlambda lifting」実装へ更新する。
- [x] `src/Atla.Core.Tests/Lowering/LayoutTests.fs` に非捕捉ラムダlowering成功の回帰テストを追加する。
- [x] `dotnet test src/Atla.slnx` を実行する。

## 2026-04-21 C#互換を意識したクロージャー方針の明文化と `Ast.Expr.Lambda` の意味解析対応

### 目的
- C#互換を意識した env-class 方式（変数捕捉・決定的な捕捉順序）を実装方針として明文化する。
- `Ast.Expr.Lambda` が `Analyze` フェーズで `Unsupported expression type` になる現状を解消し、HIR の `Lambda` へ変換できるようにする。

### 仕様
- クロージャー方針（実装方針として明文化）:
  - ラムダは env-class 方式を前提とし、捕捉は「値」ではなく「変数」単位で扱う。
  - 捕捉順序は `SymbolId` 昇順で決定的に扱う（既存前処理と整合）。
  - 非捕捉ラムダは静的 delegate 生成、捕捉ラムダは env インスタンスをターゲットにした delegate 生成を想定する。
- `Analyze.analyzeExpr` に `Ast.Expr.Lambda` ケースを追加する。
  - 期待型が `Fn` かつ引数個数が一致する場合は、その引数型/戻り型をラムダ解析に利用する。
  - それ以外の場合は引数型と戻り型に fresh meta を割り当てる。
  - ラムダ引数はラムダ本体専用スコープに `Arg` として宣言し、本文を解析する。
  - 最後に `tid` とラムダの関数型を `unify` し、失敗時は `ExprError` を返す。
  - ラムダ引数名の重複は明示診断（`ExprError`）を返す。

### 実装内容
- [x] `PLANS.md` に本タスク計画を追記する。
- [x] `src/Atla.Core/Semantics/Analyze.fs` に `Ast.Expr.Lambda` の意味解析を追加する。
- [x] `src/Atla.Core.Tests/Semantics/AnalyzeTests.fs` にラムダ解析のユニットテストを追加する。
- [x] `dotnet test src/Atla.slnx` を実行する。

## 2026-04-21 無名関数構文 `fn arg1 arg2 -> expr` の導入（パーサー範囲）

### 目的
- 式コンテキストで無名関数（lambda）を定義できるようにする。
- 構文は `fn arg1 arg2 -> expr` を基本とし、明示ユニット引数 `fn () -> expr` も許可する。
- lambda は `expr` の最上位レイヤーで受理する。

### 仕様
- `Ast.Expr` に `Lambda(args: string list, body: Expr, span: Span)` を追加する。
- `Parser.expr` は `lambdaExpr <|> binopExpr` の順で解析する。
- lambda の引数は次のみ許可する。
  - `Id` の1個以上並び（例: `fn x y -> ...`）
  - 明示ユニット引数 `()`（例: `fn () -> ...`）
- `fn -> expr` は空引数として `Ast.Expr.Error` を返す。
- 引数重複（例: `fn x x -> ...`）は `Ast.Expr.Error` を返す。
- 今回はテスト範囲を `ParserTests` のみに限定する。
- `Hir.Expr.Lambda` の Lowering は既存の `failwith` を維持する（今回非対応）。

### 実装内容
- [x] `PLANS.md` に本タスク計画を追記する。
- [x] `src/Atla.Core/Syntax/Data/Ast.fs` に `Ast.Expr.Lambda` を追加する。
- [x] `src/Atla.Core/Syntax/Parser.fs` に lambda パーサーを追加し、`expr` 最上位で受理する。
- [x] `src/Atla.Core.Tests/Syntax/ParserTests.fs` に lambda の正常系・異常系テストを追加する。
- [x] `dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj --filter "FullyQualifiedName~ParserTests"` を実行する。

## 2026-04-20 native-only NuGet パッケージ（lib/ref なし）の推移的解決対応

### 目的
- `Avalonia.Win32` 等の `runtimes/*/native/` のみを提供し `lib/` や `ref/` を一切持たない NuGet パッケージ（native-only package）が推移的依存として解決されたとき、`visitTransitiveDependency` に黙って無視されてネイティブファイルがコピーされない問題を修正する。
- `examples/gui` をビルドすると `runtimes/win-x64/native/av_libglesv2.dll` がコピーされなかった直接原因。

### 根本原因
- `tryCollectDependencyAssemblyPaths` は `tryCollectCompileReferenceAssemblyPaths`/`tryCollectRuntimeLoadAssemblyPaths` のどちらか一方でも失敗すると全体を `Result.Error` として返す。
- `lib/` も `ref/` も持たないパッケージはどちらも失敗し `Result.Error` となる。
- `visitTransitiveDependency` は `Result.Error` を黙って無視（`state` を返す）するため native ファイルが収集されない。

### 仕様
- `tryCollectDependencyAssemblyPaths` でマネージドアセット収集に失敗しても `runtimes/*/native/` にファイルが存在する場合は `Ok([], [], nativeFiles)` を返す（native-only パッケージとして成功扱い）。
- `_._` プレースホルダを持つパッケージ（`lib/<tfm>/_._`）は従来通り `Ok []` を返す既存ロジックで処理される。

### 実装内容
- [x] `PLANS.md`: 仕様をドキュメントに記録する。
- [x] `Atla.Build/Resolver.fs`: `tryCollectDependencyAssemblyPaths` に native-only フォールバックを追加する。
- [x] `Atla.Build.Tests/ResolverTests.fs`: `lib/ref` なし native-only 推移的 NuGet パッケージの回帰テストを追加する。

## 2026-04-20 すべてのプラットフォームのネイティブランタイムコピー対応

### 目的
- `atla build` でネイティブランタイム（`runtimes/*/native/` 配下のファイル）を現在の実行環境の RID のみでなく、すべてのプラットフォーム分コピーするよう変更する。
- SkiaSharp 等のクロスプラットフォームパッケージが複数の RID（`win-x64`, `linux-x64`, `osx-x64` 等）に対してネイティブライブラリを提供する場合、すべてを収集・コピーすることで配布物が自己完結する。

### 仕様
- `collectNativeRuntimePaths` は `runtimes/*/native/` 配下のすべてのファイルを収集する（RID を絞らない）。
- `copyDependencies` はネイティブランタイムファイルを `dep.source` からの相対パスを保持したまま `outDir` 配下に配置する（例: `outDir/runtimes/win-x64/native/libSkiaSharp.dll`）。
  - `dep.source` が空の場合は従来通りフラットコピーにフォールバックする。
- `copyIfNewer` は宛先の親ディレクトリを自動作成する（サブディレクトリ構造を再現するため）。

### 実装内容
- [x] `PLANS.md`: 仕様をドキュメントに記録する。
- [x] `Atla.Build/Resolver.fs`: `collectNativeRuntimePaths` を RID 全列挙に変更する（`getNativeRuntimeIdCandidates` を削除）。
- [x] `Atla.Build/Build.fs`: `copyIfNewer` に宛先ディレクトリ自動作成を追加し、`copyDependencies` でネイティブパスの相対パス保持コピーを実装する。
- [x] `Atla.Build.Tests/BuildTests.fs`: `runtimes/<rid>/native/` 階層保持コピーの新テストを追加する。
- [x] `Atla.Build.Tests/ResolverTests.fs`: 複数 RID 収集のテスト（NuGet / path 依存）を追加する。

## 2026-04-20 ネイティブのみ提供パッケージ（`lib/<tfm>/_._`）のサポート

### 目的
- `lib/<tfm>/_._` プレースホルダのみを持ち `runtimes/<rid>/native/` にネイティブアセットを提供する NuGet パッケージ（例: `SkiaSharp.NativeAssets.Linux`）を `atla build` の依存として使えるようにする。
- 現在はそのようなパッケージが `tryCollectDllsFromDirectory` で `*.dll` を見つけられずエラーとなり、依存解決に失敗する。

### 仕様
- TFM ディレクトリが存在するが `.dll` ファイルが 0 件の場合（`_._` プレースホルダのみ等）は `Ok []`（対応 TFM だがマネージドアセットなし）として扱い、エラーとしない。
  - 以前の挙動: `Some(Result.Error "has no dll candidates")` → 依存解決失敗
  - 変更後の挙動: `Some(Ok [])` → `compileReferencePaths = []`, `runtimeLoadPaths = []`, `nativeRuntimePaths = [native files]` で成功
- `runtimes/<rid>/native/` のネイティブファイルは引き続き `collectNativeRuntimePaths` で収集し、`nativeRuntimePaths` に設定する。
- `copyDependencies` がネイティブファイルを出力ディレクトリへコピーする既存ロジックはそのまま使用する。

### 実装内容
- [x] `PLANS.md`: 仕様をドキュメントに記録する。
- [x] `Atla.Build/Resolver.fs`: `tryCollectFromRoot` 内の「TFM ディレクトリは存在するが DLL なし」のケースをエラーから `Ok []` に変更する。
- [x] `Atla.Build.Tests/ResolverTests.fs`: ネイティブのみパッケージ（`lib/<tfm>/_._` + `runtimes/<rid>/native/`）の解決成功テストを追加する。

## 2026-04-20 ネイティブランタイム DLL のコピー対応

### 目的
- `atla build` でネイティブランタイム DLL（NuGet パッケージの `runtimes/<rid>/native/` 配下や path 依存プロジェクトの同構造ディレクトリに含まれるファイル）が出力ディレクトリにコピーされない問題を解消する。

### 仕様
- `ResolvedDependency` に `nativeRuntimePaths: string list` フィールドを追加する。
  - NuGet / path 両依存の解決時に `runtimes/<current-rid>/native/` 配下の全ファイルを収集して設定する。
  - `runtimes/` ディレクトリが存在しない依存では空リスト `[]` を設定する。
  - RID フォールバック: `<os>-<arch>` 形式の場合は `<os>` を追加候補として収集する（例: `win-x64` → `["win-x64"; "win"]`）。
  - 複数 RID 候補で同一ファイル名が存在する場合は最初に見つかったものを採用する。
- `BuildSystem.copyDependencies` を `runtimeLoadPaths` に加えて `nativeRuntimePaths` も outDir にコピーするよう更新する。

### 実装内容
- [x] `PLANS.md`: 仕様をドキュメントに記録する。
- [x] `Atla.Core/Compile.fs`: `ResolvedDependency` に `nativeRuntimePaths: string list` フィールドを追加する。
- [x] `Atla.Build/Resolver.fs`: `collectNativeRuntimePaths` を追加し、`tryCollectDependencyAssemblyPaths` の返り値に含める。NuGet / path 両依存の `ResolvedDependency` 構築時に設定する。
- [x] `Atla.Build/Build.fs`: `copyDependencies` で `nativeRuntimePaths` も outDir にコピーする。
- [x] `Atla.Core.Tests/Lowering/LoweringTests.fs`: 既存テストの `ResolvedDependency` に `nativeRuntimePaths = []` を追加する。
- [x] `Atla.Build.Tests/BuildTests.fs`: `makeRuntimeDep` 更新、ネイティブ DLL コピーのユニットテストを追加する。
- [x] `Atla.Build.Tests/ResolverTests.fs`: `runtimes/<rid>/native/` からのネイティブパス収集テストを追加する。
- [x] `Atla.LanguageServer.Tests/ServerLifecycleTests.fs`: 既存テストの `ResolvedDependency` に `nativeRuntimePaths = []` を追加する。

## 2026-04-20 NuGet 推移的依存パッケージの解決

### 目的
- NuGet パッケージが `.nuspec` に列挙する推移的依存パッケージを再帰的に解決し、出力ディレクトリへコピーする DLL が不足する問題を解消する。

### 仕様
- `Resolver.fs` に `tryReadTransitiveNuGetDependencies (packageId: string) (packagePath: string) : Result<DependencySpec list, Diagnostic list>` を追加する。
  - `PackageFolderReader` で抽出済みパッケージディレクトリから `.nuspec` を読み取る。
  - `tfmPriority` と同じ優先順位で最適な `<group>` を選択し、含まれる `<dependency>` を `NuGetDependency` に変換して返す。
  - `.nuspec` に依存グループが存在しない場合は `Ok []` を返す。
  - 例外は `Diagnostic.Error` に変換して `Result.Error` で返す。
- `visitDependency` の `NuGetDependency` ケースで、パッケージを初めて解決したとき推移的依存を再帰的に `visitDependency` で処理する。
  - 既に `resolvedByName` に登録済みのパッケージは再処理しない（無限ループ防止）。

### 実装内容
- [x] `PLANS.md`: 仕様をドキュメントに記録する。
- [x] `Atla.Build/Resolver.fs`: `tryReadTransitiveNuGetDependencies` を追加し、`visitDependency` で呼び出す。
- [x] `Atla.Build.Tests/ResolverTests.fs`: 推移的依存解決のユニットテストを追加する。

## 2026-04-20 依存 DLL コピー処理の追加

### 目的
- `atla build` で生成された DLL と同じ出力ディレクトリに依存 DLL が存在しない問題を解消する。
- 依存 DLL のコピーは、ソースが宛先より新しい場合（または宛先が存在しない場合）のみ実行する。

### 仕様
- `BuildSystem.copyDependencies (dependencies: Compiler.ResolvedDependency list) (outDir: string) : Result<string list, Diagnostic list>`
  - `ResolvedDependency.runtimeLoadPaths` に含まれる全 DLL を `outDir` へコピーする。
  - コピー条件: 宛先ファイルが存在しない、または `File.GetLastWriteTimeUtc(src) > File.GetLastWriteTimeUtc(dst)`。
  - 成功時はコピーしたファイルのパスリストを `Ok` で返す。
  - コピー時に IO 例外が発生した場合は `Diagnostic.Error` を含む `Result.Error` を返す。
  - エラーは全ファイル分まとめて返す（途中でショートサーキットしない）。
- `Atla.Console` の `build` コマンドは、コンパイル成功後に `copyDependencies` を呼び出す。
  - コピーされた DLL のパスを `Copied: <path>` 形式で標準出力に出力する。
  - コピー失敗時はエラーを標準エラーへ出力し終了コード 1 を返す。

### 実装内容
- [x] `PLANS.md`: 仕様をドキュメントに記録する。
- [x] `Atla.Build/Build.fs`: `BuildSystem.copyDependencies` を追加する。
- [x] `Atla.Console/Program.fs`: コンパイル後に `copyDependencies` を呼び出す。
- [x] `Atla.Build.Tests/BuildTests.fs`: `copyDependencies` のユニットテストを追加する。

## 2026-04-19 example/gui ビルド成功に必要な機能実装

### 目的
- `examples/gui` のビルドが 3 つのエラーで失敗していた問題を解消する。
  1. `Expression is not a generic callable target at [11:17, 11:50)` ← インポートパス誤り＋GenericApply エラー伝播欠如
  2. `Undefined type 'unknown' at [12:18, 12:42)` ← 上記の連鎖エラー
  3. `Undefined type 'unknown' at [13:4, 13:17)` ← 上記の連鎖エラー
- インポート型（`TypeId.Name sid`）を関数パラメータとして使った場合に CIL 生成が失敗する問題を修正する。

### 実装内容
- [x] `examples/gui/src/main.atla`: `import Avalonia.Controls.AppBuilder` → `import Avalonia.AppBuilder`（正しい名前空間）
- [x] `examples/gui/src/main.atla`: `fn main ... : Int` → `fn main ... : ()`（`AppBuilder.Start` は void を返す）
- [x] `Semantics/Analyze.fs`: `GenericApply` で `analyzedTarget` が `ExprError` のとき、元のエラーをそのまま伝播する（汎用メッセージで隠さない）
- [x] `Lowering/Gen.fs`: `Env` に `symbolTable: SymbolTable` フィールドを追加し、`resolveType` で `TypeId.Name sid` を `SymbolTable` 経由で `SystemTypeRef` → `System.Type` に解決する
- [x] `Compile.fs`: `Gen.genAssembly` に `symbolTable` を渡すよう変更
- [x] `Atla.Core.Tests/Lowering/LoweringTests.fs`: インポート型をパラメータに持つ関数のコンパイルテストを追加
- [x] `Atla.Console.Tests/Build/BuildTests.fs`: gui リグレッションテストを成功期待（exit code 0）に更新

## 2026-04-19 UnifyError.toMessage を Analyze.fs へ移動・エラー伝播バグの修正

### 目的
- `Type.fs` の `UnifyError.toMessage` は `Analyze.fs` からしか参照されないため移動し、`formatTypeForDisplay` を利用して人間が読みやすい型名（"int", "string" 等）でメッセージを生成する。
- do ブロックの末尾式が既に `ExprError` の場合、余分な "Cannot unify" エラーが報告されるバグを修正する。

### 実装内容
- [x] `Semantics/Data/Type.fs`: `UnifyError.toMessage` モジュール関数を削除する。
- [x] `Semantics/Analyze.fs`: `formatTypeForDisplay` の直後に `formatUnifyError` 関数を追加する（`nameEnv`/`typeEnv` を受け取り、`formatTypeForDisplay` で型を表示する）。
- [x] `Semantics/Analyze.fs`: `unifyOrError` に `nameEnv` パラメータを追加し、`formatUnifyError` を使うよう更新する。
- [x] `Semantics/Analyze.fs`: `unifyOrError` の全呼び出し箇所（9箇所）に `nameEnv` を追加する。
- [x] `Semantics/Analyze.fs`: do ブロック解析で末尾式が `ExprError` の場合、`unifyOrError` をスキップして根本エラーを伝播する。
- [x] `Atla.Core.Tests/Semantics/AnalyzeTests.fs`: 型不一致エラーメッセージに人間が読みやすい型名が含まれることを検証するテストを追加する。
- [x] `Atla.Core.Tests/Semantics/AnalyzeTests.fs`: ブロック末尾が ExprError のとき余分な "Cannot unify" が出ないことを検証するテストを追加する。
- [x] `dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj --filter "FullyQualifiedName~AnalyzeTests"` を実行する。（26/26 passed）

## 2026-04-19 第一級関数（first-class functions）対応

### 目的
- atla 関数を他の atla 関数の引数として渡せるようにする。
- インポートした .NET 関数（デリゲート型パラメータを持つメソッド）にも渡せるようにする。

### 実装内容
- [x] `Syntax/Data/Ast.fs`: `Ast.TypeExpr.Arrow` を追加（例: `Int -> Int`）。
- [x] `Syntax/Parser.fs`: `->` キーワードを型式で右結合にパース。
- [x] `Semantics/Data/Type.fs`: `TypeId.Fn` → .NET デリゲート型（`Func<>`, `Action<>`, `Converter<>`）への変換を追加。`canUnify` / `unify` で `Fn ↔ Native delegate` の統一を実装。
- [x] `Semantics/Data/Hir.fs`: `Hir.Method` に `args: (SymbolId * TypeId) list` を追加。
- [x] `Semantics/Analyze.fs`: `resolveTypeExpr` で `Ast.TypeExpr.Arrow` を処理。`analyzeMethod` で引数 SymbolId を収集。.NET デリゲート型パラメータへ渡す関数参照の型を具体化。
- [x] `Semantics/Infer.fs`: `args` を型推論後も伝播。
- [x] `Lowering/Data/Mir.fs`: `Mir.Value.FnDelegate` を追加（グローバル関数をデリゲートとして参照）。
- [x] `Lowering/Layout.fs`: `layoutMethod` で宣言順に引数を frame へ事前登録。`Hir.Expr.Id` が Fn 型で frame にない場合 `FnDelegate` を生成。`Hir.Callable.Fn` が frame にある場合はデリゲート経由の `Invoke` 呼び出しを発行。
- [x] `Lowering/Gen.fs`: `resolveType` で `TypeId.Fn` をデリゲート型へ解決。`genValue` で `FnDelegate` を処理（`ldnull; ldftn; newobj` を発行）。
- [x] テスト: 矢印型パース、`Hir.Method.args` 順序、高階関数の意味解析・レイアウト、`Fn ↔ delegate canUnify`、CIL 生成検証。

## 2026-04-18 `Expression is not callable` 診断に原因情報を追加

- [x] `PLANS.md` に本対応の計画を記録する。
- [x] `Semantics/Analyze.fs` の call 解析失敗時に、非呼び出し対象の式種別と解決済み型を診断へ含める。
- [x] `Atla.Core.Tests` に診断メッセージ回帰テストを追加する。
- [x] `dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj --filter "FullyQualifiedName~AnalyzeTests"` を実行する。

## 2026-04-18 `examples/gui` の `Expression is not callable` 原因特定と修正

- [x] `examples/gui/src/main.atla` を再ビルドして、報告された診断を再現する。
- [x] `examples/gui/src/main.atla` の import パスと Avalonia API 呼び出しを点検し、コンパイラ診断との対応を確認する。
- [x] `TypeId.Name` で保持される外部型に対する runtime 型解決経路を意味解析へ追加し、誤診断を減らす。
- [x] generic 型引数解決でも `TypeId.Name` を runtime 型へ解決できるよう修正する。
- [x] 回帰確認として `Atla.Core.Tests` に回帰テストを追加する。
- [x] 回帰確認としてフルテストスイート（`dotnet test src/Atla.slnx`）を実行する。
- [x] 変更内容を要約し、原因と解決策を日本語で報告する。

## 2026-04-19 exprStmt が同インデント次行を Apply 引数に取り込むバグの修正

### 根本原因

`term2` は `Many1 (term1 ())` で関数適用を解析する。
`do` / `fn` ブロック内では `BlockInput` がオフサイド列より深くインデントされたすべてのトークンを可視にするため、
同じインデントレベルの次行先頭トークンも `term1` として消費され、複数文が誤って1つの Apply 式にまとまる。

例:
```
do
    window.Show ()
    app.Run window   ← window.Show の引数として誤解析される
```

### 対応

`term2` で先頭 `term1`（関数部分）を解析した後、引数パース用の `callInput` を
`BlockInput(input, head.span.left)` で作成する。
これにより:
- head と同じ行のトークン → 可視（引数として解析できる）
- head より深くインデントされた後続行 → 可視（継続引数として許容）
- head と同じ列・またはそれより左の後続行先頭 → 不可視（別文として扱われる）

変更ファイル:
- `Syntax/Parser.fs`: `term2` の実装を直接パーサ関数スタイルに書き替え
- `Atla.Core.Tests/Syntax/ParserTests.fs`: 回帰テストを追加

## 2026-04-19 example/gui コンストラクタ未解決バグの修正

### 根本原因

1. **コンストラクタ戻り型バグ**（`Semantics/Analyze.fs`）
   `NativeConstructorGroup` / `NativeConstructor` ケースがコンストラクタの戻り型として
   新鮮な型メタ変数 (`callRetType`) を返していた。
   `let window = Window ()` のように `let` で束縛すると `window` の型が未解決のまま残り、
   その後の `window.Show ()` で `resolveRuntimeSystemType` が失敗してエラー連鎖が起きる。

2. **パーサーの貪欲な関数適用**（`Syntax/Parser.fs`、確認済み・別課題）
   `do` ブロック内の `exprStmt` は `Many1 term1` を使うため、同じインデントレベルの
   複数行を一つの Apply 式として解析してしまう。
   例: `window.Show ()` と `app.Run window` が `Apply(window.Show, [(), app.Run, window])` になる。

3. **例示コードの不正な Avalonia API 使用**
   - `app.Run window` → `Application` に `Run(Window)` は存在しない
   - `config.Start appMain args` → `AppMainDelegate` デリゲートが必要（Atla の関数は渡せない）

### 対応

- `Analyze.fs`: `NativeConstructorGroup` / `NativeConstructor` の戻り型を
  `TypeId.fromSystemType ctorInfo.DeclaringType` に修正
- `Hir.fs` / `Infer.fs` / `Layout.fs` / `Mir.fs` / `Gen.fs`:
  参照型のオプショナル引数デフォルト値 `null` を渡すため `Null` リテラルを HIR に追加
- `Analyze.fs`: `tryDefaultArgExpr` を拡張し参照型の `null` デフォルト値を処理
- `examples/gui/src/main.atla`: 正しい Avalonia API に書き直し
  - `appMain`: `app.Run window` → 削除（Window.Show の後にそのまま終了）
  - `main`: デリゲート不要の `StartWithClassicDesktopLifetime` を使用・戻り型を `Int` に変更
- テスト追加: コンストラクタ戻り型の回帰テスト

## 2026-04-18 依存解決の用途別分離（compile参照 / runtimeロード）実装タスク

### フェーズ0: 設計確定

## 2026-04-21 クロージャー変換（Frame Allocation 前処理）導入

### 目的
- Frame Allocation の前処理としてクロージャー変換フェーズを追加する。
- 「closure = 自由変数ありラムダ」を明示し、検出と前処理責務を専用モジュールへ分離する。
- 変換前提（環境オブジェクト + invoke 関数）を踏まえ、後続フェーズへ明示的な診断を返せるようにする。

### 実装内容
- [x] `Lowering/ClosureConversion.fs` を追加し、`Hir.Assembly -> PhaseResult<Hir.Assembly>` の前処理エントリを実装する。
- [x] 自由変数検出ロジックを追加し、自由変数ありラムダを決定的に検出する。
- [x] `Lowering/Layout.fs` から `ClosureConversion.preprocessAssembly` を呼び出し、Frame Allocation 前に実行する。
- [x] 自由変数ありラムダを検出した場合、対象メソッドと捕捉変数IDを含む診断を返す。
- [x] `Atla.Core.Tests/Lowering/LayoutTests.fs` にクロージャー前処理の回帰テストを追加する。
- [x] `ResolvedDependency` を `compileReferencePaths` と `runtimeLoadPaths` の2系統へ拡張する方針を確定する。
- [x] `compileReferencePaths` の選定規則を `ref > lib`、`runtimeLoadPaths` の選定規則を `lib > ref` として明文化する。
- [x] 既存パイプライン（`AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL`）を崩さない受け渡し境界を定義する。

### フェーズ1: モデル拡張（Atla.Core / Atla.Build）
- [x] `Atla.Core.Compile` の依存入力モデルに `compileReferencePaths` / `runtimeLoadPaths` を追加する。
- [x] `Atla.Build` の `ResolvedDependency` 生成経路を新モデルへ移行する。
- [x] 既存呼び出し側（Console / LanguageServer / Tests）のコンストラクタ/変換コードを新モデルへ更新する。

### フェーズ2: Resolver 分離実装
- [x] `Resolver` に compile参照選定関数（`ref > lib`）と runtimeロード選定関数（`lib > ref`）を分離実装する。
- [x] TFM 優先順位と診断メッセージを両経路で統一し、順序決定性を維持する。
- [x] package ごとに compile/runtime の両パスが決定的順で返ることを保証する。

### フェーズ3: DependencyLoader 連携
- [x] `DependencyLoader.loadDependencies` が `runtimeLoadPaths` のみを入力として扱うように変更する。
- [x] runtimeロード対象に `ref/` が混入した場合の診断（または自動フォールバック）方針を実装する。
- [x] 依存ロード失敗診断に「compile参照とruntimeロードのどちらで失敗したか」を含める。

### フェーズ4: Compile パイプライン接続
- [x] `Compile.compile` で dependency 入力のマッピングを新モデルへ更新する。
- [x] Semantic 直前ロードの既存順序・失敗ハンドリングを維持する。
- [x] CIL 生成側が compile参照情報を必要とする場合の受け渡し口を整理する（現時点不要なら明記のみ）。

### フェーズ5: テスト
- [x] `Atla.Build.Tests` に compile/runtime 選定差分（`ref > lib` / `lib > ref`）の単体テストを追加する。
- [x] `Atla.Core.Tests` に Avalonia 相当ケースの runtimeロード成功回帰テストを追加する。
- [x] `Atla.Console.Tests` に `examples/gui` ビルド回帰テストを追加する。
- [x] 決定性テスト（同一入力でパス順・診断順が不変）を Build/Core で追加する。

### フェーズ6: ドキュメント
- [x] `README.md` に依存解決の2系統（compile参照/runtimeロード）を追記する。
- [x] `doc/build-system-phase1.md` に Resolver の選定規則と理由（参照専用DLLの実ロード回避）を追記する。
- [x] 既存の NuGet/依存ロード説明との整合性を確認する。

### 完了条件
- [ ] `dotnet run --project src/Atla.Console -- build examples/gui` が成功する。
- [x] `dotnet test src/Atla.Build.Tests/Atla.Build.Tests.fsproj` が成功する。
- [x] `dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj` が成功する。
- [x] `dotnet test src/Atla.Console.Tests/Atla.Console.Tests.fsproj` が成功する。
- [x] `dotnet test src/Atla.slnx` が成功する。

## 2026-04-18 `examples/gui` 差し戻し + エラー原因調査

- [x] `PLANS.md` に差し戻しと調査の計画を追記する。
- [x] 直前コミット（`Fix examples/gui build by using minimal entrypoint`）の変更を取り消す。
- [x] `dotnet run --project src/Atla.Console -- build examples/gui` を再実行し、報告されたエラーを再現する。
- [x] CIL 生成経路を調査し、`Avalonia.Application` エラーの発生要因を特定する（修正は行わない）。
- [x] 影響確認として `dotnet test src/Atla.Console.Tests/Atla.Console.Tests.fsproj` を実行する。

## 2026-04-18 `examples/gui` ビルド通過化（Avalonia制約の回避）

- [x] `PLANS.md` に本対応の実装計画を追記する。
- [x] `examples/gui/src/main.atla` を現行バックエンドでビルド可能な最小エントリポイントへ差し替える。
- [x] `examples/gui` の目的と制約（Avalonia API 呼び出しを一時的に外す理由）をドキュメントへ追記する。
- [x] `dotnet run --project src/Atla.Console -- build examples/gui` を実行し、ビルド通過を確認する。
- [x] フルテストスイート（`dotnet test src/Atla.slnx`）を実行する。

## 2026-04-18 GUI向け構文更新（フェーズ4: ドキュメント/サンプル）

- [x] `PLANS.md` に本バッチ（フェーズ4）の実装計画を記録する。
- [x] `README.md` の言語メモに generic 呼び出し `target[typeArgs]` と index `expr !! index` を追記する。
- [x] 破壊的変更として `a[b]` 廃止（`a !! b` へ移行）をドキュメントに明記する。
- [x] `doc/semantic-phase-design.md` の index 記法説明を `expr !! index` 前提へ更新する。
- [x] `examples/gui/src/main.atla` を `AppBuilder.Configure[Application] ()` 構文へ更新する。
- [x] `dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj --filter "FullyQualifiedName~ParserTests"` を実行する。

## 2026-04-18 GUI向け呼び出し解決拡張（フェーズ2-3: Semantic/Test）

- [x] `PLANS.md` に本バッチ（フェーズ2-3）の実装計画を記録する。
- [x] `Ast.Expr.GenericApply` を Semantic で解釈し、`target[typeArgs]` が呼び出し可能な `Hir.Expr` に解決されるようにする。
- [x] import した .NET 型に対してコンストラクタ呼び出し（`TypeName ()`）ができるよう、名前解決で ctor グループを変数シンボルとして公開する。
- [x] メンバー解決で拡張メソッド候補を探索し、`receiver.ExtensionMethod (...)` の解決経路を追加する。
- [x] `AnalyzeTests` に generic 呼び出し・コンストラクタ呼び出し・拡張メソッド呼び出しの回帰テストを追加する。
- [x] `dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj --filter "FullyQualifiedName~AnalyzeTests"` を実行する。
- [x] `dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj --filter "FullyQualifiedName~ParserTests"` を実行する。

## 2026-04-18 GUI向け構文更新（フェーズ1: Parser/AST）

- [x] `PLANS.md` に本バッチ（フェーズ1）の実装計画を記録する。
- [x] Generic呼び出し構文を `target[typeArgs]` へ変更し、`Ast.Expr.GenericApply` として表現できるようにする。
- [x] indexアクセス構文を `expr !! index` へ変更し、Parser で `Ast.Expr.IndexAccess` として扱う。
- [x] 後方互換は考慮せず、既存の `a[b]` 構文を廃止する。
- [x] `ParserTests` を新構文へ更新し、generic postfix と index `!!` の回帰テストを追加する。

## 2026-04-18 型適用の一般化（Array 専用制約の解除）

- [x] `TypeId` に汎用型適用ノードを追加し、型解決/単一化/解決処理で保持できるようにする。
- [x] `Analyze.resolveTypeExpr` を更新し、`Ast.TypeExpr.Apply` を `Array` 以外でも `TypeId.App` として解決する。
- [x] 既存の `Array String` は `TypeId.App(Native System.Array, [String])` として扱い、ランタイム配列解決互換を維持する。
- [x] セマンティクステストを更新し、`String Int` などの非Array型適用が `TypeId.App` として保持されることを検証する。
- [x] フルテストスイート（`dotnet test src/Atla.slnx`）を実行する。

## 2026-04-18 Array String 対応（フェーズ7-8）

- [x] フェーズ7: `Array String` の型適用に対する異常系（引数不足/引数過多）診断テストを追加する。
- [x] フェーズ7: AST/HIR/MIR 境界で `Array String` 型情報が保持されるスナップショット相当テストを追加する。
- [x] フェーズ8: `README.md` に `Array String` 型注釈（`String` は大文字始まり）を追記する。
- [x] フルテストスイート（`dotnet test src/Atla.slnx`）を実行する。

## 2026-04-18 Array String 対応（フェーズ5-6）

- [x] フェーズ5: `Array String` 型引数を持つ関数が HIR→MIR で配列型シグネチャを保持することを Lowering テストで検証する。
- [x] フェーズ6: `TypeId.App(Native System.Array, [TypeId.String])` が CIL 生成時に `System.String[]` パラメータへ変換されることを Gen テストで検証する。
- [x] フェーズ6: `Array String` 型注釈を使うプログラムの compile→run E2E テストを追加し、ランタイム挙動を確認する。
- [x] フルテストスイート（`dotnet test src/Atla.slnx`）を実行する。

## 2026-04-18 Array String 対応フォローアップ（小文字プリミティブ別名の撤回）

- [x] フェーズ3実装から小文字プリミティブ別名解決を削除する。
- [x] `Array String` の意味解析テストを仕様どおり検証する。
- [x] フルテストスイート（`dotnet test src/Atla.slnx`）を実行する。

## 2026-04-18 Array String 対応（フェーズ3-4）

- [x] フェーズ3: `Analyze.resolveTypeExpr` で `Ast.TypeExpr.Apply`（`Array String`）を解決し、型引数数エラーを診断化する。
- [x] フェーズ3: （撤回済み）小文字プリミティブ型名（`string` など）の意味解析対応は行わない。
- [x] フェーズ4: `TypeId` に配列表現を追加し、単一化・解決・ランタイム型変換で配列型を扱えるようにする。
- [x] フェーズ4: `Array String` の意味解析回帰テストを追加する。
- [x] フルテストスイート（`dotnet test src/Atla.slnx`）を実行する。

## 2026-04-18 Array String 対応（フェーズ0-2）

- [x] フェーズ0: 仕様確定（文法は空白区切り、互換文法は対象外、要素アクセスは既存文法を採用）を記録する。
- [x] フェーズ1: `Ast.TypeExpr` に型適用ノードを追加し、`Array String` を構文木で表現可能にする。
- [x] フェーズ2: `Parser.typeExpr` を拡張し、空白区切り型適用（`Array String`）を解析可能にする。
- [x] フェーズ2: `ParserTests` に `Array String` の回帰テストを追加する。
- [x] フルテストスイート（`dotnet test src/Atla.slnx`）を実行する。

## 2026-04-18 buildProject 診断順序の決定性修正（一時フォルダ名の非決定性排除）

- [x] `Resolver.tryRunRestore` の一時ダウンロード先を実行ごと GUID ではなく package/version 由来の決定的なパスへ変更する。
- [x] restore 失敗時診断に実行ごとのランダムパスが混入しないことを `Atla.Build.Tests` で回帰テストとして固定する。
- [x] `dotnet test src/Atla.Build.Tests/Atla.Build.Tests.fsproj --filter "buildProject should keep diagnostics order deterministic across runs"` を実行して再現ケースの非退行を確認する。

## 2026-04-17 Atla.Build 自動NuGet取得の既定化（環境変数廃止）

- [x] `ATLA_BUILD_ENABLE_NUGET_RESTORE` 判定を削除し、NuGetキャッシュ未存在時は常にNuGet.Client取得を試行する。
- [x] キャッシュ未存在時の診断文言から環境変数有効化案内を削除し、取得失敗理由を返す仕様へ統一する。
- [x] `Atla.Build.Tests` のフラグ前提テストを新仕様（常時取得）に合わせて更新する。
- [x] README から `ATLA_BUILD_ENABLE_NUGET_RESTORE` の説明を削除し、既定挙動を記載する。
- [x] `dotnet test src/Atla.Build.Tests/Atla.Build.Tests.fsproj` と `dotnet test src/Atla.slnx` を実行して回帰を確認する。

## 2026-04-17 Atla.Build NuGet.Client 直呼び移行（一時csproj廃止）

- [x] `Atla.Build` に NuGet.Client 系パッケージ参照を追加し、`dotnet restore` 外部プロセス依存を除去する。
- [x] `Resolver` の自動取得経路を NuGet.Client API ベースへ置換し、一時 `restore.csproj` を生成しないようにする。
- [x] 取得失敗時の診断を既存 `Result`/`Diagnostic` 形に正規化し、既定挙動（未キャッシュ時失敗・有効時のみ自動取得）を維持する。
- [x] `Atla.Build.Tests` に NuGet.Client 自動取得経路の回帰テストを追加し、既存失敗系テストとの整合を確認する。
- [x] README の `ATLA_BUILD_ENABLE_NUGET_RESTORE` 説明を実装実態（NuGet.Client 自動取得）に合わせて更新する。
- [x] `dotnet test src/Atla.Build.Tests/Atla.Build.Tests.fsproj` と `dotnet test src/Atla.slnx` を実行して回帰なしを確認する。

## 2026-04-16 Atla.Build manifest YAML完全移行

- [x] フェーズ0: 方針確定（`atla.yaml` のみサポート、Toml関連処理を削除、診断文言を `atla.yaml` に統一、YAMLライブラリは `YamlDotNet 17.0.1` を採用）。
- [x] フェーズ1: `Atla.Build` の manifest パース実装を TOML から YAML へ置換し、`atla.yaml` 固定で build できるようにする。
- [x] フェーズ2: 依存解決・参照側（LSP/CLI）での manifest 探索名を `atla.yaml` に統一する。
- [x] フェーズ3: テストを `atla.yaml` 前提へ更新する（TOML回帰テストは作成しない）。
- [x] フェーズ4: ドキュメントを YAML 前提記述へ全面置換する（廃止告知は記載しない）。
- [x] フェーズ5: フルテストスイート実行で回帰なしを確認する。

## 2026-04-16 NuGet依存DLLロード実装（analyzeModule直前で実行）

### フェーズ0: 方針確定（推奨案で決定）

- [x] 決定事項: 依存DLLロードは `Compiler.compile` 内で `Analyze.analyzeModule` 呼び出し直前に実行する。
- [x] 決定事項: ロード失敗は警告継続ではなく `Diagnostic.Error` で fail-fast する。
- [x] 決定事項: 参照DLL探索は `ref/` 優先、次に `lib/` を探索する。
- [x] 決定事項: TFM優先順位は `net10.0 > net9.0 > net8.0 > netstandard2.1 > netstandard2.0` とする。
- [x] 決定事項: 同名アセンブリ競合（同一simple nameの複数version）は診断エラーで失敗させる。
- [x] 決定事項: ロード順・診断順は決定的（ソート済み）でなければならない。

### フェーズ1: モデル拡張（Build -> Compiler 受け渡し）

- [x] `ResolvedDependency` または `CompileRequest` に「参照対象DLL一覧」を保持するモデルを追加する。
- [x] `Atla.Build` 側で NuGet package root から「最終的に採用したDLLパス」を計算して BuildPlan に反映する。
- [x] path依存についても参照DLL抽出の規約を明文化し、NuGet依存と同じデータ形へ正規化する。

### フェーズ2: NuGet DLL選定ロジック実装（Atla.Build）

- [x] `Resolver` に `ref/` -> `lib/` 探索を追加し、TFM優先順位に従って DLL を選定する。
- [x] 候補なし/候補過多/破損パスに対する構造化診断を追加する。
- [x] 依存ごとの選定結果を決定的順序で返す。

### フェーズ3: DLLロード実装（Atla.Compiler）

- [x] `DependencyLoader`（新規モジュール）を追加し、`Analyze.analyzeModule` 直前で依存DLLをロードする。
- [x] ロード処理は `AssemblyLoadContext` ベース（将来の隔離・解放を見据えた設計）で実装する。
- [x] 失敗理由（ファイル欠損/BadImageFormat/依存連鎖不足）を `Diagnostic` に変換する。

### フェーズ4: 意味解析連携

- [x] 依存DLLロード後に `Resolve.tryResolveSystemType` で型解決できることを統合テストで保証する。
- [x] `import` 解決エラーで「型未存在」と「依存ロード失敗」を識別可能な診断メッセージへ改善する。

### フェーズ5: テスト

- [x] `Atla.Build.Tests` に DLL選定（TFM優先・ref/lib優先・異常系）の単体テストを追加する。
- [x] `Atla.Core.Tests` に analyze直前ロード経路の統合テスト（成功/失敗/競合）を追加する。
- [x] 回帰テストとして同一入力でロード順・診断順が不変であることを検証する。
- [x] フェーズ5実施時は `dotnet test src/Atla.Build.Tests/Atla.Build.Tests.fsproj` と `dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj` を先行実行して回帰を確認する。

### LSPサーバー経路への dependencies 注入タスク（新規）

- [x] 実装前に現行 `Server.compileAndPublish` の依存注入欠落経路を確認し、`BuildSystem` 連携方針を確定する。
- [x] `Atla.LanguageServer.Server.compileAndPublish` に `BuildSystem.buildProject` を組み込み、`Compiler.compile` へ `plan.dependencies` を渡す経路を追加する。
- [x] `didOpen` / `didChange` の URI からプロジェクトルート（`atla.toml` 起点）を決定するルールを追加し、ワークスペース外・manifest未検出時のフォールバック挙動を定義する。
- [x] Build失敗（manifest不正/依存解決失敗）と Compile失敗（lex/parse/semantic/依存ロード失敗）を識別して LSP diagnostics へ反映する変換レイヤーを追加する。
- [x] `Atla.LanguageServer.Tests` に dependencies 注入の統合テストを追加する（成功: build plan dependencies が compile request へ注入される、失敗: build diagnostics が `atla-build` source で配信され compile をスキップ）。
- [x] LSP経路での決定性（同一入力で依存解決順・診断順が不変）を回帰テストで固定する。

### 完了条件

- [x] `dotnet test src/Atla.Build.Tests/Atla.Build.Tests.fsproj` が成功する。
- [x] `dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj` が成功する。
- [x] `dotnet test src/Atla.slnx` が成功する。

## 2026-04-15 コメント規約追記（関数・ブロック必須）

- [x] `AGENTS.md` に「関数および処理ブロックには必ずコメントを付与する」ルールを追加する。
- [x] 既存の開発ワークフロー規約と矛盾しない位置に追記する。
- [x] フルテストスイートを実行し、ドキュメント変更のみで非退行を確認する。

## 2026-04-15 Atla.Build コメント整備（ブロック単位）

- [x] `src/Atla.Build/Build.fs` の主要処理ブロック（manifest検証・依存解析・build entry）にブロックコメントを追加する。
- [x] `src/Atla.Build/Resolver.fs` の主要処理ブロック（NuGet解決・依存木走査・競合診断）にブロックコメントを追加する。
- [x] フルテストスイートを実行し、コメント追加のみで挙動非退行を確認する。

## 2026-04-15 Atla.Build NuGet依存解決 フェーズ0-1（仕様確定 + manifest拡張）

- [x] フェーズ0: 依存種別の優先度を `path > version(nuget)` として明確化する。
- [x] フェーズ1: `[dependencies]` で `version` 指定時に NuGet 依存として解釈できるようにする（例: `Newtonsoft.Json = { version = "13.0.3" }`）。
- [x] フェーズ1: `path` と `version` の同時指定を診断エラーにする。
- [x] `Atla.Build.Tests` に `version` 指定の受理（現時点は未解決診断）と同時指定エラーの回帰テストを追加する。

## 2026-04-15 Atla.Build NuGet依存解決 フェーズ2（BuildPlan解決モデル拡張）

- [x] `version` 指定の NuGet 依存を `BuildPlan.dependencies` の `ResolvedDependency` として返せるようにする。
- [x] NuGet依存の `source` を決定的な表現（`nuget:<packageId>/<version>`）で保持する。
- [x] path依存とnuget依存が同一パッケージ名へ解決される場合は重複診断にする。
- [x] `Atla.Build.Tests` に NuGet解決成功ケースと path+nuget 同名衝突ケースを追加する。

## 2026-04-15 Atla.Build NuGet依存解決 フェーズ3（ローカルキャッシュ解決基盤）

- [x] NuGet依存を `NUGET_PACKAGES`（未設定時は `~/.nuget/packages`）から解決する。
- [x] 依存がキャッシュに存在しない場合は構造化診断で失敗させる。
- [x] NuGet依存の `ResolvedDependency.source` を実体ディレクトリ（絶対パス）にする。
- [x] `Atla.Build.Tests` にキャッシュ存在/不存在ケースを追加する。

## 2026-04-15 Atla.Build リファクタ（Resolver分割）

- [x] `Build.fs` から依存解決ロジックを `Resolver.fs` に分離する。
- [x] `Atla.Build.fsproj` のコンパイル順序に `Resolver.fs` を追加する。
- [x] 既存テストで振る舞い非退行を確認する。

## 2026-04-15 Atla.Build NuGet依存解決 フェーズ4（競合解決: 厳密一致）

- [x] 同一依存名の解決結果について、version 不一致を明示的な競合診断として扱う。
- [x] version 一致かつ source 一致の場合のみ重複を許可（再訪として統合）する。
- [x] version 一致でも source 不一致の場合は重複依存名診断で失敗させる。
- [x] `Atla.Build.Tests` に transitive NuGet の version 不一致/一致ケースを追加する。

## 2026-04-15 Atla.Build NuGet依存解決 フェーズ5（自動Restore導線）

- [x] NuGetキャッシュ不在時に `ATLA_BUILD_ENABLE_NUGET_RESTORE=1` で自動 `dotnet restore` を試行できるようにする。
- [x] 自動Restoreが無効な既定動作を維持し、診断に有効化方法を含める。
- [x] `Atla.Build.Tests` に既定無効動作と診断メッセージの回帰テストを追加する。

## 2026-04-15 Atla.Build NuGet依存解決 フェーズ6（テスト構造分割）

- [x] `BuildSystemTests.fs` を `BuildTests.fs`（manifest/build経路）と `ResolverTests.fs`（NuGet/競合解決経路）に分割する。
- [x] `Atla.Build.Tests.fsproj` の `Compile Include` を新しいテスト構成へ更新する。
- [x] 分割後の `Atla.Build.Tests` とフルテストスイート通過を確認する。

## 2026-04-15 Atla.Build NuGet依存解決 フェーズ7（README整備）

- [x] ルート `README.md` を追加し、CLIインターフェイス（`build <projectRoot>`）を記載する。
- [x] NuGet関連の環境変数（`NUGET_PACKAGES`, `ATLA_BUILD_ENABLE_NUGET_RESTORE`）をREADMEに明記する。
- [x] 既存 `doc/cli-interface.md` と整合する最小プロジェクト構成・実行例を記載する。

## 2026-04-15 Atla.Build NuGet依存解決 フェーズ8（決定性検証）

- [x] 依存解決結果の並び順が決定的であることを `ResolverTests` で検証する。
- [x] 診断メッセージ順が同一入力で再現可能であることを `ResolverTests` で検証する。
- [x] `Atla.Build.Tests` とフルテストスイート通過でフェーズ完了を確認する。

## 2026-04-15 Atla.Buildプロジェクト追加（atla.toml依存解決 + Console連携）

### 2026-04-15 実装バッチ（依存解決 + Console/Core連携）

### 2026-04-15 フォローアップ（新API完全移行）

### 2026-04-15 フォローアップ2（Tomlyn最新版適用）

- [x] Toml パース実装が Tomlyn 利用であることを維持し、独自パーサーを導入しない。
- [x] `Atla.Build` の Tomlyn 参照を最新版へ更新する。
- [x] テストを実行して API 互換性と回帰なしを確認する。


- [x] `Atla.Core.Compiler` の旧 `compile(asmName, source, outDir)` を削除し、`CompileRequest` ベースAPIへ統一する。
- [x] `Atla.Console` / `Atla.LanguageServer` / `Atla.Core.Tests` を新API呼び出しへ全面移行する。
- [x] `Atla.Build` の依存型を `Atla.Core` 側の `ResolvedDependency` へ統一し、Consoleでの型変換を削除する。
- [x] フルテストスイートを再実行して移行完了を確認する。


- [x] `Atla.Build` で `[dependencies]`（ローカルpath依存）を解決し、循環依存・欠損パス・重複依存を診断として返す。
- [x] `Atla.Core.Compile` に依存入力モデルを追加し、既存呼び出し互換を維持する。
- [x] `Atla.Console` の `build` を `build <projectRoot>` に切り替え、`Atla.Build` 経由で解決済み依存を `Atla.Core.Compile` に渡す。
- [x] `Atla.Console.Tests` / `Atla.Build.Tests` を拡張し、成功/失敗経路と依存診断を検証する。
- [x] `doc/cli-interface.md` を更新し、新CLI入力仕様を反映する。
- [x] フルテストスイートを実行する。


- [x] フェーズ1: `Atla.Build` の責務と `atla.toml` 最小仕様を確定する。
  - [x] `atla.toml` は当面 `[package]` のみを対象にし、`name` / `version` を必須とする。
  - [x] 想定最小構成は以下とする:
    - [x] `[package]`
    - [x] `name = "hello"`
    - [x] `version = "0.1.0"`
  - [x] `dependencies` は将来フェーズで NuGet パッケージ解決に対応する（本フェーズでは未実装）。
  - [x] 責務境界は `Atla.Build = manifest読取/依存解決`、`Atla.Core = コンパイル` を維持する。
  - [x] `Atla.Core.Compile` へ渡す依存モデルは `ResolvedDependency list` を採用する。
- [x] フェーズ2: `Atla.Build` / `Atla.Build.Tests` のプロジェクト雛形を追加する。
  - [x] `src/Atla.Build/Atla.Build.fsproj` を追加し、solution (`src/Atla.slnx`) に組み込む。
  - [x] `src/Atla.Build.Tests/Atla.Build.Tests.fsproj` を追加し、solution (`src/Atla.slnx`) に組み込む。
  - [x] 最小のビルド確認テストを追加し、プロジェクト参照が正しいことを検証する。
- [x] フェーズ3: `Atla.Build` で `atla.toml` 解析を実装する（Tomlyn使用）。
  - [x] `Tomlyn` を `Atla.Build` に追加し、`atla.toml` の `[package]`/`name`/`version` を検証する。
  - [x] `BuildSystem.buildProject` を実装し、成功時 `BuildPlan`、失敗時 `Diagnostic list` を返す。
  - [x] `Atla.Build.Tests` に正常系・異常系（ファイル欠損、構文エラー、必須項目欠落）を追加する。
- [x] `Atla.Build` に `atla.toml` パーサーを実装し、構文/必須項目不足を診断として返す。
- [x] `Atla.Build` に依存解決を実装し、循環依存・欠損パス・重複依存を診断として返す。
- [x] `Atla.Build` の公開API（`buildProject`）を追加し、解決済み依存情報を返せるようにする（依存解決本体は次フェーズ）。
- [x] `Atla.Console` の `build` コマンド入力をプロジェクトルート前提へ更新し、`Atla.Build` を呼び出す。
- [x] `Atla.Console` から `Atla.Core.Compile` へ解決済み依存を渡す経路を追加する。
- [x] `Atla.Core.Compile` の入力モデルを拡張し、依存情報を受け取れるようにする（LanguageServer等の既存呼び出しは互換維持または追従）。
- [x] `Atla.Build.Tests` を追加し、まずは最小のプロジェクト疎通テストを配置する（`atla.toml` パース/依存解決の正常系・異常系は次フェーズで拡張）。
- [x] `Atla.Console.Tests` を更新し、`build <projectRoot>` 経路の成功/失敗を検証する。
- [x] AST/HIR/MIR のスナップショットと診断検証を含む関連テストを追加/更新する。
- [x] `doc/cli-interface.md` を更新し、`build` の新しい入力と `atla.toml` 運用を明記する。
- [x] フルテストスイートを実行し、決定性・フェーズ不変条件・回帰なしを確認する。

## 2026-04-13 LSPシンタックスハイライト右端1文字欠落の修正

- [x] `SourceString.join` の span 終端計算を見直し、トークン長が1文字短くならないように修正する。
- [x] 右端欠落を再現・防止する回帰テスト（少なくとも span と semantic token length）を追加する。
- [x] 関連テストとフルテストスイートを実行して回帰がないことを確認する。

## 2026-04-13 Language Server 初期化時の空パス例外修正

- [x] `Atla.LanguageServer.Server.Initialize` のサーバー版数取得処理で、アセンブリパスが空文字の場合に例外を出さずフォールバック値を返すようにする。
- [x] 空パス入力を再現する回帰テストを `Atla.LanguageServer.Tests` に追加する。
- [x] 既存を含むテストを実行して回帰がないことを確認する。

## 2026-04-12 診断モデル刷新（Warning/Info を成功時にも返す）

- [x] `Semantics.Data` に `Diagnostic` DU を新設し、`Error | Warning | Info` を表現できるようにする（`span` と `message` を保持）。
- [x] `Semantics.Data.Error` 型を削除し、コンパイラ内部の診断表現を `Diagnostic` に統一する（互換レイヤは作らない）。
- [x] `Hir` の `getErrors` / `hasError` を `getDiagnostics` / `hasError` 相当に再設計し、Error 以外も収集できるようにする。
- [x] `Analyze` / `Infer` の戻り値を `Diagnostic list` ベースへ更新し、成功時も非 Error 診断を保持できる形にする。
- [x] `Compiler.compile` の戻り値を `CompileResult` 型へ変更し、`diagnostics` を常に保持する。
- [x] `Atla.Build`（CLI）を `CompileResult` に追従させ、成功時でも Warning/Info を表示できるようにする。
- [x] `Atla.LanguageServer` を `CompileResult` + `Diagnostic` に追従させ、LSP 診断 severity を `Error/Warning/Information` へ正しくマッピングする。
- [x] 今回は診断コード（error code）を導入しない。コード体系の設計・付与は将来タスクとして保留する。
- [x] テストを更新・追加し、以下を検証する：
  - [x] コンパイル成功 + Warning/Info の診断返却
  - [x] コンパイル失敗 + Error 診断返却
  - [x] CLI/LSP での severity 反映
  - [x] 診断順序の決定性

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
- [x] `Scope.ResolveVar` の `tid` 未使用引数を解消する（利用または削除）。

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
- [x] フェーズ境界ごとのテスト（Resolve/Infer）を追加し、既存 `AnalyzeTests` と合わせて回帰を防止する。
- [x] 上記変更後にテストスイートを実行し、決定性・診断品質・既存Lowering経路への影響を確認する。

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

## 2026-04-12 failwith削減とCompile層準拠のフェーズ結果統一

- [x] `Semantics.Data` に Compile層と整合する共通フェーズ結果型（`succeeded` + `diagnostics` + `value`）を追加する。
- [x] `Resolve` / `Analyze` / `Layout` / `Gen` の公開APIをフェーズ結果型へ移行し、例外をDiagnosticへ変換する経路を追加する。
- [x] `Compile.compile` を新しいフェーズ結果型連結へ更新し、成功時診断も維持する。
- [x] 関連ユニットテストを更新し、フルテストを実行して回帰がないことを確認する。

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

- [x] 決定事項: `didOpen` / `didChange` / `didClose` の状態遷移は `Server` に集約し、`didClose` では `publishDiagnostics(uri, [])` 後にバッファを解放する。
- [x] 決定事項: URI は正規化して内部キー化し、`file://` かつ（workspace 指定時は）workspace 配下のみをコンパイル対象とする。
- [x] 決定事項: diagnostics 生成は変換レイヤー（Compile結果 -> LSP Diagnostics）を必須化し、段階的に lex/parse/semantic 粒度へ拡張可能な形にする。
- [x] `didOpen` / `didChange` / `didClose` のバッファライフサイクルを明確化し、Close 時にメモリと診断状態を適切に解放する。
- [x] URI 正規化（OS 差・ワークスペース外ファイル）を整理し、コンパイル対象判定を決定的にする。
- [x] コンパイル成功時に空 diagnostics を必ず送るルールをテストで固定する。
- [x] 失敗時 diagnostics の粒度（lex/parse/semantic）を段階的に分離できるよう変換レイヤーを導入する。

### Phase 4: Diagnostics 品質向上

- [x] `LSPTypes.Diagnostic` を拡張し、`severity` / `source` / `code` を扱えるようにする。
- [x] `Span.Empty` 固定の暫定実装を縮退し、取得可能な span を優先して range へ反映する。
- [x] 診断メッセージの安定化（決定性と順序）をテストで保証する。
- [x] 代表的な失敗ケース（未解決識別子/型不一致/構文エラー）の LSP 診断スナップショットを追加する。

### Phase 5: Semantic Tokens 精度改善

- [x] 決定事項: 互換性方針は「現時点で本来あるべき仕様」を優先し、Semantic Tokens の破壊的変更を許可する（クライアント互換より仕様整合を優先）。
- [x] 決定事項: token 種別は `keyword` / `type` / `variable` / `number` / `string` の5種を正とし、`InternalTokenize` はこの5種へ正規化する（未分類は送信しない）。
- [x] 決定事項: delta encoding の基準入力として LF / CRLF / BOM 付き入力を正式サポートし、同一意味入力で同一トークン列を返す決定性を必須条件にする。
- [x] 決定事項: クライアント capability が空または未知 token type のみの場合、サーバーは空データを返し、`window/logMessage` でフォールバック理由を通知する。
- [x] 決定事項: semantic tokens 応答スナップショットは `resultId` を固定値（空文字）として `data` 配列を主検証対象にする。
- [x] `InternalTokenize` のトークン種別マッピングを見直し、`keyword` / `type` / `variable` / `number` / `string` の判定を仕様化する。
- [x] 複数行・空行・先頭 BOM・CRLF 入力で delta encoding が壊れないことを回帰テストで保証する。
- [x] クライアントが未サポート token type を通知した場合のフォールバック挙動を固定する。
- [x] semantic tokens の JSON 応答をスナップショットテスト化する。

### Phase 6: テスト基盤と回帰防止

- [x] 決定事項: `Atla.LanguageServer.Tests` を `Message` / `ServerLifecycle` / `Diagnostics` / `SemanticTokens` / `Program` の5モジュールに分割し、責務境界を固定する。
- [x] 決定事項: E2E テストは stdin/stdout の実フレーミング（`Content-Length`）を必須検証対象にし、`initialize -> didOpen -> semanticTokens -> shutdown -> exit` を最小正常系とする。
- [x] 決定事項: 異常系 E2E は `不正ヘッダー` / `Content-Length欠落` / `不正JSON` / `未知request` / `空本文` を最低セットとし、終了コードと応答有無を固定する。
- [x] 決定事項: LanguageServer 変更時の必須コマンドは `dotnet test src/Atla.LanguageServer.Tests/Atla.LanguageServer.Tests.fsproj` と `dotnet test src/Atla.slnx` の2つに固定する。
- [x] `Atla.LanguageServer.Tests` を message レイヤー/サーバー状態遷移/診断配信/トークン化の観点で分割する。
- [x] stdin/stdout ベースの軽量 E2E テストを追加し、LSP フレーミングを実運用に近い形で検証する。
- [x] 異常系テスト（不正ヘッダー、不正 JSON、未知メソッド、空本文）を追加する。
- [x] LanguageServer 変更時に必ず実行するテストコマンドを `PLANS.md` または `doc` に明記する。

### Phase 7: リリース準備

- [x] 決定事項: リリース時ドキュメントは「対応済み LSP メソッド」「未対応メソッド」「既知制約」「推奨クライアント設定」を必須セクションとして持つ。
- [x] 決定事項: 既知制約は機能単位（diagnostics / semantic tokens / sync）で列挙し、回避策がある場合は必ず併記する。
- [x] 決定事項: CI 必須チェックは `Atla.LanguageServer` build と `Atla.LanguageServer.Tests` test を required とし、失敗時はマージ不可とする。
- [x] 決定事項: Phase 7 完了条件は「ローカル full test 成功」「E2E 正常系/異常系成功」「ドキュメント更新完了」の3条件同時達成とする。
- [x] エディタ接続手順（起動コマンド、stdio 設定、サンプルプロジェクト）をドキュメント化する。
- [x] 既知制約（未実装 LSP メソッド、診断精度の制限）を整理して明示する。
- [x] CI で `Atla.LanguageServer` / `Atla.LanguageServer.Tests` を必須チェックにする。
- [x] フェーズ完了条件（ビルド成功・テスト成功・E2E 成功）を満たしたら完了マークを更新する。

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

## 2026-04-13 Atla.Console / Atla.LanguageServer の self-contained application 対応

- [x] `Atla.Console.fsproj` と `Atla.LanguageServer.fsproj` に self-contained publish 向けプロパティを追加する。
- [x] 利用者向けドキュメントに self-contained publish 手順を追記する。
- [x] テストスイートを実行して回帰がないことを確認する。

## 2026-04-13 self-contained 単一実行ファイル（exe）出力対応

- [x] `Atla.Console` / `Atla.LanguageServer` の publish 設定を単一実行ファイル出力（single-file）向けに拡張する。
- [x] ドキュメントに「exe 単独実行」向けの publish 手順と実行例を追記する。
- [x] フルテストスイートを実行して回帰がないことを確認する。

## 2026-04-16 ルートAtla.slnx追加変更の取り消し

- [x] ルート追加した `Atla.slnx` を削除し、solution エントリポイントを `src/Atla.slnx` のみに戻す。
- [x] `README.md` のテスト実行手順を `dotnet test src/Atla.slnx` 前提へ戻す。
- [x] `src/Atla.slnx` に対するフルテストを実行して非退行を確認する。

## 2026-04-16 Windowsでのcycle依存テスト失敗修正

- [x] `buildProject should fail when dependency graph has cycle` の失敗を再現し、Windows特有の `atla.toml` 文字列エスケープ問題を特定する。
- [x] テストデータ生成をOS非依存な書き方へ修正し、TOMLパースが安定するようにする。
- [x] `Atla.Build.Tests` と `src/Atla.slnx` のテストを実行して回帰がないことを確認する。

## 2026-04-16 ResolveTests のWindowsパス区切り問題修正

- [x] `ResolverTests` で `Path.GetRelativePath` を TOML へ埋め込む箇所を洗い出し、Windowsで不正エスケープになる経路を再現・特定する。
- [x] `ResolverTests` 側にも TOML 向けパス正規化を適用し、OS非依存で同一テストデータを生成する。
- [x] `Atla.Build.Tests` と `src/Atla.slnx` を実行して回帰がないことを確認する。

## 2026-04-16 LanguageServer URI正規化テストのWindows差分修正

- [x] `ServerLifecycleTests.normalize uri makes file key deterministic` の期待値がOS依存になっている箇所を特定する。
- [x] 期待値をOS非依存（正規化関数の仕様準拠）へ修正し、Windows/Unixで同一意図を検証できるようにする。
- [x] `Atla.LanguageServer.Tests` と `src/Atla.slnx` のテストを実行して回帰がないことを確認する。
