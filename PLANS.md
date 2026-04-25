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
- 2026-04-25: `examples/gui` の失敗は `WindowStartupLocation'CenterScreen` のような enum static literal field を CIL で `ldsfld` した際に `System.NotSupportedException` が発生することが原因だった。
- 2026-04-25: `Atla.Core.Tests` のフルスイートには旧 call/member 構文サンプル（例: `f x`, `a.b()`）がまだ多く残っており、複数引数ドット呼び出し対応後でも、関連外の parser/semantic 失敗が発生する。
- 2026-04-25: メンバー代入の lowering は、既存 HIR のまま安全に扱うため現時点では「プロパティ setter 呼び出し」へ正規化し、フィールド代入は明示エラーとして扱う方針にした。
- 2026-04-25: 旧構文由来のテスト失敗は、テスト内サンプルコードを dot-only call / apostrophe member-access 構文へ移行することで解消できた（実装コード側で旧構文互換を追加しない方針を維持）。
- 2026-04-25: `Layout.layoutExpr` の `ClosedHir.Expr.Call` 分岐で `instance` が破棄されており、インスタンスメソッド呼び出し時に receiver 未積載の不正 IL（`InvalidProgramException`）が発生することを確認した。

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
