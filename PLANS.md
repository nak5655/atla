# 実行計画

このファイルは OpenAI Codex の execution-plan スタイルに従い、意図的に簡潔に保っています。
詳細な履歴計画や設計メモは `notes/` 配下に保存します。

## 目的
- リポジトリ直下の計画を軽量に保つ。
- ここではアクティブな作業と直近の実行詳細のみを追跡する。

## スコープ
- 対象: アクティブな実装タスク、実行順序、リスク、検証手順。
- 対象外: 長期アーカイブ履歴、完了済みの深掘り調査（`notes/` に移動）。

## 制約
- コンパイラのフェーズ順序 `AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL` を維持する。
- フェーズ境界を明示的かつ決定的に保つ。
- 不変条件に影響する変更には必ずテストを追加・更新する。

## 計画
### アクティブタスク (2026-04-26): `impl A for B by b` 委譲メンバー解決

#### ミッション
- 新構文 `impl A for B by b` を導入し、`A` から `b` フィールド経由で `B` のメンバーへ委譲解決できるようにする。
- 既存の `impl A` / `impl A for B` との後方互換を維持する。
- AST -> Semantic Analysis -> HIR の境界でのみ糖衣を処理し、下流フェーズ（Frame Allocation/MIR/CIL）の契約を変更しない。

#### 実行ステップ
1. AST `Decl.Impl` を拡張し、`byFieldName` を保持できるようにする。
2. Lexer/Parser を拡張し、`impl <Target> [for <Base>] [by <Field>]` を決定的に解析する。
3. Resolve で `by` 指定の妥当性（`for` 必須・対象 data フィールド存在）を検証する。
4. Analyze で data メンバー探索失敗時に `by` フィールド経由の委譲探索を実行し、data/native 双方のメンバー解決へ接続する。
5. Parser/Semantics テストを追加し、`for + by` の正常系と回帰を検証する。
6. フルテストスイートを実行して非退行を確認する。

### アクティブタスク (2026-04-26): `impl B for A` サブタイプ関係の導入

#### ミッション
- 新構文 `impl B for A` を導入し、`for` が `B <: A` のサブタイプ関係を作る仕様を実装する。
- AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL の各フェーズで基底型情報を隣接フェーズに明示的に伝搬する。
- 既存の `impl B`（`for` なし）を後方互換で維持する。

#### 実行ステップ
1. `Ast.Decl.Impl` を拡張し、`forTypeName`（基底型名）を保持できるようにする。
2. Parser を拡張し、`impl <Target> [for <Base>]` を決定的に解析する。
3. Resolver で `for` 指定時の基底型解決と循環継承検証を追加する。
4. Semantic Analysis にサブタイプ判定ヘルパーを追加し、型適合判定と data メンバー解決で継承チェーンを考慮する。
5. HIR/ClosedHir/MIR の `Type` に `baseType` を追加し、Layout/ClosureConversion/Infer で情報を保持する。
6. CIL 生成で `DefineType` 時に基底型を指定し、実行時型階層へ反映する。
7. Parser/Semantics/Lowering の回帰テストを追加し、`impl B for A` の成功系・失敗系を検証する。
8. フルテストスイートを実行して非退行を確認する。

### アクティブタスク (2026-04-26): 単項マイナス + Float ビルトイン演算子対応

#### ミッション
- `-1.0` / `-x` などの単項マイナスを Parser で受理し、決定的に AST へ正規化する。
- 既存のビルトイン算術演算子に Float 系シグネチャを追加し、`Float` 計算が意味解析を通過できるようにする。
- 既存の AST -> Semantic Analysis -> HIR 契約を維持し、下流フェーズ契約を変更しない。

#### 実行ステップ
1. Parser に単項マイナス解析レイヤーを導入し、負の数値リテラルと一般式（`-expr`）を受理する。
2. 既存の二項演算パースに単項マイナスレイヤーを接続し、演算子優先順位を維持する。
3. SymbolTable のビルトイン演算子定義へ Float 用シグネチャ（`+ - * / ==`）を追加する。
4. 同名演算子の複数シンボルに対して、期待型で候補を絞り込めるように名前解決ロジックを調整する。
5. Parser/Semantics テストに単項マイナス・Float 演算ケースを追加する。
6. `examples/data` を再ビルドして再現エラーが解消したことを確認する。
7. フルテストスイートを実行し、既存仕様の非退行を確認する。

### アクティブタスク (2026-04-26): 呼び出し時オーバーロード解決の型適合化

#### ミッション
- `NativeMethodGroup` 呼び出し時の候補選択を arity 優先のみから型適合優先へ拡張し、`Console'WriteLine` のような多重オーバーロードを決定的に解決する。
- `Ast.Expr.Id` 側の同名候補絞り込みとの一貫性を確保し、診断（No match / Ambiguous）を安定化する。

#### 実行ステップ
1. Analyze の `Apply` 経路に「引数型 + 返り値期待型」で method 候補を評価するヘルパーを導入する。
2. optional 引数は型適合判定と整合する形で補助的に扱い、既存機能を維持する。
3. `NativeMethodGroup` 選択で型適合スコアを用いた一意選択を実装し、同点候補は曖昧診断へフォールバックする。
4. `Console'WriteLine` を使う回帰テストを追加し、`examples/data` を元の出力コードへ戻してビルドを検証する。
5. フルテストスイートを実行して既存パイプライン不変条件の非退行を確認する。

### アクティブタスク (2026-04-26): `examples/data` の出力行復元

#### ミッション
- `examples/data/src/main.atla` の `Console'WriteLine` 呼び出しを復元し、前回変更で導入された `_evaluated` 束縛への置換を取り消す。

#### 実行ステップ
1. `main` 末尾行を `5.0 line'evaluate. Console'WriteLine.` へ戻す。
2. `examples/data` のビルド挙動を確認する。

### アクティブタスク (2026-04-26): MemberAccess の NativeMethodGroup 保持

#### ミッション
- MemberAccess 段階で複数 Native メソッド候補が存在する場合に即時 Ambiguous へ落とさず、`Hir.Member` として method-group を保持して Apply 段階で最終選択する。
- `Console'WriteLine` のような多重オーバーロードで、実引数型確定後の決定的解決を可能にする。

#### 実行ステップ
1. `Hir.Member` へ `NativeMethodGroup of MethodInfo list` ケースを追加する。
2. Analyze の MemberAccess 解決で、method 候補が複数件のとき `NativeMethodGroup` を返す経路を実装する。
3. `exprAsCallable` で `Member.NativeMethodGroup` を `Callable.NativeMethodGroup` へ変換する。
4. 既存の Apply 側 overload 選択ロジックを再利用して最終候補を確定する。
5. `Console'WriteLine` を含む回帰テストと `examples/data` ビルドで動作確認する。

### アクティブエピック (2026-04-24): ドット専用呼び出し + アポストロフィメンバーアクセス

#### ミッション
- HIR 以降の下流フェーズに不要な影響を出さず、新しい表層構文を導入する。
- 新構文:
  - 関数呼び出しは `.` のみを使用。
  - メンバーアクセスは `'` を使用。

#### 凍結済み言語ルール
- `x f.` => `f(x)`。
- `a b f.` => `f(a, b)`。
- `x f. g.` => `g(f(x))`（左から右に評価）。
- `f.` は有効（ゼロ引数呼び出し）。
- `x .` は有効（呼び出し可能式 `x` への直接ゼロ引数呼び出し）。
- `a'b` => `a.b`。
- メンバーアクセスは一次式として束縛: `a'b c.` => `(a.b)(c)`。
- 無効な形式:
  - `x f` はエラー（呼び出しに `.` が不足）。
  - `a'` はエラー（メンバー識別子が不足）。

#### スコープ境界
- AST/Parser/Semantics: 対象。
- HIR/Frame Allocation/MIR/CIL: 意図的な構造変更は行わず、非退行検証のみ実施。

### アクティブタスク (2026-04-24): Import パス区切りの変更（`.` -> `'`）

#### ミッション
- `import` のパス構文をドット区切り識別子からアポストロフィ区切り識別子へ変更する。
- 旧ドット区切り import 構文は無効化する（互換レイヤーなし）。

#### 実行ステップ
1. Parser の import 文法を `import A'B'C` を解析できるように更新する。
2. 旧 `import A.B` がパース時に失敗することを確認する。
3. テストとサンプルソースをアポストロフィ区切り import に移行する。
4. 構文テストを重点実行した後、フルスイートを確認する。

### アクティブタスク (2026-04-25): 複数引数ドット専用呼び出しのパース

#### ミッション
- パイプライン形式で callee の前に複数引数を渡せるよう、ドット専用呼び出しのパースを拡張する。
- 正準 AST 形状（`Ast.Expr.Apply`）を維持し、Semantic/HIR 契約の変更を避ける。

#### 実行ステップ
1. `term2` のパースループを更新し、最終 callee と末尾 `.` の前に 1 個以上の引数 term を収集する。
2. 既存挙動（ゼロ引数 `f.`, `x .` と連鎖呼び出し `x f. g.`）を維持する。
3. 2 個以上の引数呼び出しと、複数引数形式でのドット欠落診断について parser テストを追加する。
4. 複数引数ドット呼び出しが正準 `Hir.Expr.Call` に lower されることを semantic 回帰テストで確認する。
5. 未対応構文に依存していたサンプルソースを更新する。
6. フルテストスイートを実行する。

### アクティブタスク (2026-04-25): メンバーアクセス代入（`window'Width = 320`）対応

#### ミッション
- 代入先として識別子だけでなくアポストロフィメンバーアクセス式も許可する。
- HIR/MIR に parser 由来の糖衣を持ち込まず、Semantic Analysis でメンバー代入を lower してフェーズ境界を維持する。

#### 実行ステップ
1. 代入文パースを拡張し、`=` の左辺で l-value 式（`identifier` または `member-access`）を受け付ける。
2. 不正 l-value（呼び出し結果、リテラル、ぶら下がりアポストロフィ等）に対して明示的な parser 診断を維持する。
3. AST の代入表現を更新し、代入先式と安定した span を保持する。
4. Semantic Analysis では識別子代入を既存 `Hir.Stmt.Assign` として lower する。
5. Semantic Analysis ではメンバー代入を正準 setter セマンティクスへ lower する:
   - setter を持つインスタンスメンバー/プロパティ -> setter への `Hir.Expr.Call` を `Hir.Stmt.ExprStmt` で包む
   - フィールド書き込み -> call ベースの lowering で不十分な場合のみ、専用 HIR 書き込みノードの導入/更新を検討する
6. 読み取り専用プロパティ、setter 不在、アクセス不可メンバー、型不一致の診断順序を決定的に維持する。
7. `window'Width = 320` を含む、有効/無効 l-value の parser 境界テストを追加する。
8. 成功ケースの lowering と失敗ケースの期待診断を semantic 回帰テストで検証する。
9. フルテストスイートを実行し、AST/HIR/MIR 不変条件の非退行を確認する。


### アクティブタスク (2026-04-25): CIL の static literal field 生成修正

#### ミッション
- static literal field（特に enum メンバー）を `ldsfld` で読み出す経路を解消し、定数即値として CIL へ正規化する。
- Layout/HIR の境界は変更せず、MIR -> CIL 変換責務の範囲で根本修正する。

#### 実行ステップ
1. `Gen.genValue` の `Mir.Value.FieldVal` 分岐に `field.IsLiteral` 判定を追加する。
2. literal は `GetRawConstantValue()` を使って型別に `ldc.*` / `ldstr` / `ldnull` へ変換する。
3. enum literal は underlying type へ展開して整数即値として発行する。
4. 非 literal static field のみ `ldsfld` を維持し、インスタンス field は既存 `ldfld` 経路を維持する。
5. Lowering.Gen テストへ enum literal 回帰ケースを追加し、GUI サンプル再発を防止する。
6. CIL フェーズノートへ literal field の取り扱い規約を追記する。
7. フルテストを実行し、診断と不変条件の非退行を確認する。

### アクティブタスク (2026-04-25): インスタンス呼び出しの Layout 受け渡し修正

#### ミッション
- `Hir.Expr.Call` が保持する `instance` を Layout で欠落させず MIR 呼び出し引数へ反映する。
- CIL 生成前に `instance :: args` の順序を保証し、`callvirt` のスタック不整合を防止する。

#### 実行ステップ
1. Layout の call lowering で instance 式を先に正規化し、引数命令列へ安定順序で結合する。
2. メソッドオーバーロード解決時は instance を除いた実引数数で候補選択する。
3. インスタンスメソッド呼び出しが MIR で receiver を先頭引数として保持する回帰テストを追加する。
4. 該当 Lowering テストを実行し、既存の phase 境界不変条件に退行がないことを確認する。

### アクティブタスク (2026-04-25): unit 単独実引数の 0 引数正規化撤去

#### ミッション
- 呼び出し解析で `unit` 単独実引数を 0 引数へ暗黙変換する互換ロジックを撤去する。
- 0 引数呼び出しは dot-only (`f.` / `x .`) のみを正規経路とし、実引数 `()` は 1 引数として扱う。

#### 実行ステップ
1. Semantic Analysis の `Ast.Expr.Apply` 解析から `[Hir.Expr.Unit _] -> []` 正規化を削除する。
2. `() greet.` のような unit 実引数呼び出しが 0 引数関数に対して失敗する回帰テストを追加する。
3. dot-only の正規 0 引数呼び出し（`greet.`）が引き続き成功することを既存テストで確認する。
4. 旧仕様を示すテスト名/メッセージ（unit argument syntax）を新仕様に合わせて更新する。
5. フルテストスイートを実行し、診断順序と phase 境界不変条件の非退行を確認する。

### アクティブタスク (2026-04-25): テストコード中の旧構文を新構文へ統一

#### ミッション
- テスト内に残る旧表層構文（ドット区切り import / ドットメンバー呼び出し）を新構文へ完全移行する。
- 旧構文残存による非本質的なテスト失敗を排除し、構文移行後の期待動作だけを検証する。

#### 実行ステップ
1. テストファイルを走査し、Atla ソース文字列中の `import A.B` と `A.B x` 形式を検出する。
2. `import A'B` と `x A'B.` など新構文へ置換する。
3. 旧構文を直接前提にするテスト名/期待値を新仕様に合わせて更新する。
4. 影響するテストプロジェクト（Core/Console）を再実行して成功を確認する。

### アクティブタスク (2026-04-25): `data` 宣言と初期化構文の導入（struct lowering）

#### ミッション
- `examples/data` 系サンプルをコンパイル可能にするため、`data` 宣言を表層構文として受理する。
- `data` を Semantic Analysis で正準 struct + constructor 呼び出しへ lower し、既存 HIR/MIR/CIL パイプラインを再利用する。
- フィールドは `public` かつ可変を既定とし、自動 `Equals` / `ToString` 生成や immutable 前提は導入しない。

#### 実行ステップ
1. 受理文法を次で固定する:
   - 宣言: `data Person = { name: String, age: Int }`（フィールド区切りは `,`）
   - 初期化: `Person { name = "Alice", age = 20 }`（名前付きのみ、必須フィールド省略不可）
   - フィールド型注釈は必須、宣言側に初期化式は持たせない。
2. **変更制御**: `src/Atla.Core/Syntax/Lexer.fs` / `src/Atla.Core/Syntax/Parser.fs` の編集前に明示承認を取得する（AGENTS.md ルール）。
3. Parser で `data` 宣言 AST ノードと `data` 初期化 AST ノードを追加し、span を保持した決定的診断（不足フィールド、重複、区切り不正）を実装する。
4. Semantic Analysis で `data` 宣言を struct シンボルへ展開し、`public mutable` フィールド定義と constructor シグネチャを型付き HIR に正規化する。
5. Semantic Analysis で `data` 初期化構文を既存 constructor 呼び出しに lower し、宣言順に並ぶ引数列へ変換する（未知/重複/不足/型不一致は構造化診断）。
6. HIR 不変条件（完全型付け、未解決識別子なし、糖衣構文なし）を検証する回帰テストを追加する。
7. Parser/Semantic の境界テストを追加する:
   - 正常系: `data` 宣言 + 初期化成功
   - 異常系: 重複フィールド、未知フィールド、必須フィールド欠落、型不一致
   - スナップショット: AST/HIR 形状の固定化
8. MIR/CIL への影響が struct lowering に限定されることを確認し、`data` 由来コードの最終 CIL 実行回帰テストを追加する。
9. `examples/data` サンプル、言語仕様ドキュメント、フェーズノートを更新し、`dotnet test` フルスイートで非退行を確認する。

### アクティブタスク (2026-04-26): `impl` 構文の導入（型メソッド束縛）

#### ミッション
- `examples/data/data.atla` に追加された `impl TypeName ...` 構文をパース・意味解析できるようにし、既存フェーズ境界を壊さずにコンパイル可能にする。
- `impl` 内の `fn` は最終的に正準 HIR メソッドへ lower し、Frame Allocation/MIR/CIL へは既存の関数呼び出し経路で受け渡す。
- 仕様確定（2026-04-26）:
  - 対象は `data` 型のみ
  - `this` は必須
  - `this` は先頭引数で明示
  - 同名メソッド・同一型への複数 `impl` はエラー

#### 実行ステップ
1. 受理仕様を固定する（最小スコープ）:
   - 形: `impl TypeName` ブロック配下に `fn` 宣言のみ許可。
   - `impl` 対象型は同一モジュール内 `data` または `import` 済み型に限定（拡張メソッドは将来課題）。
   - メソッド本体内の `this` 解決ルール（暗黙/明示）を明文化し、曖昧さを禁止。
2. **変更制御**: `src/Atla.Core/Syntax/Lexer.fs` / `src/Atla.Core/Syntax/Parser.fs` 変更が必要なため、着手前に明示承認を取得する（AGENTS.md ルール）。
3. AST を拡張し、`Decl.Impl`（型名 + メソッド群 + span）を追加する。`impl` 内要素は構文段階で `fn` のみに制限し、決定的診断を返す。
4. Parser で `impl` 宣言をトップレベル宣言として受理する。オフサイドルールと span 付与を既存 `fn`/`data` と同一方針に揃える。
5. Resolve で `impl` 対象型の存在検証を行い、未定義型/重複メソッド名/不正シグネチャを構造化診断で返す。
6. Analyze で `impl` メソッドを型に束縛された HIR メソッドへ正規化する:
   - レシーバ引数（`this`）を明示的な先頭引数として確定。
   - メンバー呼び出し `value'method.` を既存 `Hir.Expr.Call` 形状へ lower。
   - HIR に parser 糖衣（`impl` 専用ノード）を残さない。
7. Lowering 非退行を確認する（Frame Allocation/MIR/CIL の新分岐は原則追加しない）。必要なら symbol 解決のみ最小修正する。
8. テストを追加・更新する:
   - Parser: `impl` 正常系/異常系（空ブロック、不正要素、未知型）。
   - Semantics: `impl` メソッド解決、`this` 束縛、重複/型不一致診断順序。
   - Lowering: `impl` メソッド呼び出しが既存 MIR/CIL 経路で動作する回帰。
   - Snapshot: AST/HIR 形状の固定化（`impl` 糖衣が HIR に残らないこと）。
9. `examples/data` とフェーズノート（AST/Semantic/HIR）を更新し、`dotnet test` フルスイートで完了判定する。

#### リスクと対策
- `this` 解決を式解析へ直結すると未解決識別子が HIR へ漏れるリスク:
  - 対策: Semantic Analysis でのみ `this` を導入し、Parser は予約語として受理するだけに留める。
- `impl` を HIR 新ノードで保持してしまうリスク:
  - 対策: Analyze で必ず既存 `Hir.Method` 群へ正規化し、下流へは同一契約で渡す。
- 診断順序の不安定化リスク:
  - 対策: `対象型未定義 -> メソッド重複 -> シグネチャ不正 -> 本体型不一致` の順序を固定する。

#### リスクと対策
- Parser の糖衣が HIR へ漏れるリスク:
  - 対策: semantic 正規化点を単一点に固定し、HIR では struct/ctor 呼び出しの正準ノードのみ許可する。
- 初期化構文の自由度増加で診断順序が不安定化するリスク:
  - 対策: 診断優先順位を `重複 -> 未知 -> 必須不足 -> 型不一致` に固定し、テストで順序を凍結する。
- 既存 struct/ctor 機構との二重実装リスク:
  - 対策: lowering は既存 constructor 解決ロジックを再利用し、`data` 専用の MIR/CIL 分岐を追加しない。

#### 実行ステップ
1. `.` 呼び出しと `'` メンバーアクセスの文法/Parser を、ソース span を保ったまま更新する。
2. 無効形式（`x f`, `a'`, 不正な後置 call/member 連鎖）の parser 診断を追加・調整する。
3. Semantics で解析結果を既存の正準 call/member 形状に正規化する。
4. 既存 HIR lowering 契約を再利用し、可能な限り新 HIR バリアントを導入しない。
5. 正常系/異常系の parser + semantic 単体テストを追加する。
6. AST/HIR/MIR スナップショットテストを追加・更新して回帰をカバーする。
7. フルテストスイートを実行し、診断順序が決定的であることを確認する。
8. ユーザー向け構文ドキュメントを更新する。

#### テストマトリクス
- 正常系:
  - `x f.`
  - `x f. g.`
  - `f.`
  - `x .`
  - `a'b`
  - `a'b c.`
- 異常系:
  - `x f`
  - `a'`
  - 不正な後置 call/member 連鎖
- 境界:
  - AST スナップショット安定性
  - HIR スナップショットが正準（Call/MemberAccess）を維持
  - MIR 非退行

#### リスクと対策
- 式末尾におけるドットトークン曖昧性:
  - 対策: 厳密な後置 call パースと一次式境界の明確化。
- Parser 変更による広範なテスト破損:
  - 対策: まず parser テストを段階実施し、その後に semantic 正規化とスナップショット検証を行う。
- HIR 契約ドリフトの混入:
  - 対策: リファクタ前に回帰テストで HIR 形状アサーションを固定する。

#### 完了条件（エピック）
- 警告ゼロでビルド成功。
- フルテスト合格。
- 診断が構造化・決定的であること。
- 正規化後の形で HIR/MIR/CIL 挙動が回帰同等であること。
- 最終構文に合わせてドキュメント更新済みであること。

## 予期しない発見・知見
- （実装中に見つかった予期しない事項を記録する。）
- 2026-04-25: 既存 `data` 構文は `data Name =` + オフサイド列挙であり、今回合意した `{ ... }` + `,` 区切り仕様とは非互換だったため、Parser とサンプルを同時更新する必要があった。
- 2026-04-25: `examples/gui` の失敗は `WindowStartupLocation'CenterScreen` のような enum static literal field を CIL で `ldsfld` した際に `System.NotSupportedException` が発生することが原因だった。
- 2026-04-25: `Atla.Core.Tests` のフルスイートには旧 call/member 構文サンプル（例: `f x`, `a.b()`）がまだ多く残っており、複数引数ドット呼び出し対応後でも、関連外の parser/semantic 失敗が発生する。
- 2026-04-25: メンバー代入の lowering は、既存 HIR のまま安全に扱うため現時点では「プロパティ setter 呼び出し」へ正規化し、フィールド代入は明示エラーとして扱う方針にした。
- 2026-04-25: 旧構文由来のテスト失敗は、テスト内サンプルコードを dot-only call / apostrophe member-access 構文へ移行することで解消できた（実装コード側で旧構文互換を追加しない方針を維持）。
- 2026-04-25: `Layout.layoutExpr` の `ClosedHir.Expr.Call` 分岐で `instance` が破棄されており、インスタンスメソッド呼び出し時に receiver 未積載の不正 IL（`InvalidProgramException`）が発生することを確認した。
- 2026-04-26: Lexer の keyword 判定が単語境界を見ておらず、`intercept` などが `in` + `tercept` に分割される問題を確認したため、単語境界チェックを導入した。

### アクティブタスク (2026-04-26): examples 回帰テストの整合（`gui_hello` / `data`）

#### ミッション
- `Atla.Console.Tests` の examples 回帰テストを現行ディレクトリ構成に合わせ、`examples/gui_hello` を正しくビルド検証できるようにする。
- `examples/data` のビルドが成功することを確認する回帰テストを追加する。

#### 実行ステップ
1. examples パス探索ロジックの `gui` 固定参照を `gui_hello` へ更新する。
2. `build should succeed for examples gui_hello` テストへ名称/期待成果物を更新する。
3. `build should succeed for examples data` テストを追加し、`examples/data` で `atla build` 成功と DLL 出力を検証する。
4. `dotnet test src/Atla.Console.Tests/Atla.Console.Tests.fsproj` を実行して回帰確認する。


### アクティブタスク (2026-04-26): `data` 複数 `impl` 制約の実装計画

#### ミッション
- 現状「同一型への複数 `impl` はエラー」となっている制約を見直し、`data` 型に対して複数 `impl` ブロックを許可する。
- 実装着手前に、重複メソッド・解決順序・診断の決定性を含む仕様を先に凍結する。
- AST/HIR/MIR の既存不変条件を壊さない最小差分設計を採用する。

#### 実行ステップ
1. 事前確認: Parser/Lexer 変更が必要なため、着手前に明示承認を取得する（AGENTS.md ルール）。
2. AST/Parser: `impl T` と `impl T for Role` を AST に保持し、span と構文診断の決定性を維持する。
3. Resolver: `T` と `Role` の存在解決を追加し、未解決時は `E_IMPL_TARGET_NOT_DATA` / `E_IMPL_ROLE_NOT_FOUND` を返す。
4. Semantic 集約: `(T, Option<Role>)` 単位で `impl` をグルーピングし、`impl T` は 1 件、`impl T for Role` は Role ごと 1 件の制約を検証する。
5. Semantic 重複検証: 許可されたブロック集合に対し、メソッドシグネチャ重複（name + args + return）を検証する。
6. HIR 正規化: `impl` 糖衣を残さず既存 `Hir.Method` 群へ正規化し、下流（Frame Allocation/MIR/CIL）の変更を回避する。
7. テスト追加: parser/semantic/lowering の正常系・異常系・スナップショットを追加する（`impl T` 重複、`impl T for Role` 重複を必須化）。
8. 検証: `dotnet test` の全テストプロジェクトを実行し、診断順序の決定性と非退行を確認する。

## 検証
- 警告ゼロでビルド成功。
- フルテスト合格。
- lowering 各段階で IR 不変条件が維持される。

## 決定ログ
- 2026-04-24: ルートの過大な計画を簡潔な実行テンプレートへ置き換えた。
- 2026-04-24: 履歴計画内容を `notes/plans-archive.md` へ移動した。
- 2026-04-24: AGENTS.md を ExecPlan 形式ガイダンスに変換した。
- 2026-04-24: `notes/plans-archive.md` からフェーズ別設計メモを `notes/phases/` へ抽出開始した。
- 2026-04-24: `notes/phases/` に Closure Conversion フェーズの明示ドキュメントを追加し、フェーズ索引順を調整した。
- 2026-04-24: ドット専用関数呼び出し（`.`）とアポストロフィメンバーアクセス（`'`）の構文計画を凍結し、左から右の call 連鎖と HIR+ 変更なし方針を確定した。
- 2026-04-24: parser 側の構文切替（アポストロフィメンバーアクセス + ドット専用 call 連鎖）を実装し、syntax parser テストを新構文へ更新した。
- 2026-04-24: Step 1/2 残作業を完了（ぶら下がりアポストロフィの明示診断追加、モジュール全体パースの EOI 必須化、`Ast.Expr.Error` / `Ast.Stmt.Error` を generic unsupported-type エラーへ潰さず semantic へ伝播）。
- 2026-04-24: Step 4/5 を完了（dot/apostrophe 構文が正準 `Hir.Expr.Call` に lower されることを確認する semantic 非退行テスト、および一次束縛 member call の parser 境界テストを追加）。
- 2026-04-24: Step 5 のカバレッジを拡張（左から右の連鎖 dot call `x f. g.`、識別子ゼロ引数 dot call `f.`、連鎖/ゼロ引数 dot-only プログラムの semantic 成功確認）。
- 2026-04-24: `examples/` のソースを dot-only call と apostrophe member-access 表記に移行し、サンプルを新表層構文に一致させた。
- 2026-04-24: `a::b` parser 構文経路を削除し、メンバーアクセスを apostrophe 専用（`a'b`）に統一、テスト/サンプルを更新した。
- 2026-04-24: `import a.b` を `import a'b` へ置換する import 構文移行タスクを開始し、旧ドット import を拒否する方針を明示した。
- 2026-04-25: 代入文の AST を `Assign(name, ...)` から `Assign(targetExpr, ...)` へ拡張し、`window'Width = 320` のようなメンバー代入を Semantic Analysis で setter 呼び出しへ lower する実装を開始した。
- 2026-04-25: `Atla.Core.Tests` の旧呼び出し/メンバー構文サンプルを新構文へ更新し、`dotnet test src/Atla.Core.Tests/Atla.Core.Tests.fsproj` が全件成功する状態へ復帰した。
- 2026-04-25: Layout の call lowering で `instance` を `args` 先頭へ結合する修正方針を採用し、メソッド候補選択は receiver を除く引数数で評価する方針を確定した。
- 2026-04-25: static literal field（enum 定数含む）は `ldsfld` ではなく即値ロードへ変換する CIL 修正方針を採用した。

## 参照
- 履歴計画: `notes/plans-archive.md`
- 技術ノート索引: `notes/README.md`
