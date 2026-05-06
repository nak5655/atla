/// ソース位置からシンボル情報への逆引きインデックスと型フォーマット機能を提供する。
module Atla.Core.Semantics.PositionIndex

open Atla.Core.Data
open Atla.Core.Semantics.Data

// ---------------------------------------------------------------------------
// 型フォーマット
// ---------------------------------------------------------------------------

/// TypeId を人間が読める文字列にフォーマットする。
/// シンボルIDの解決には `resolve` 関数を使う（外部から注入）。
let rec formatType (resolve: SymbolId -> string) (tid: TypeId) : string =
    match tid with
    | TypeId.Unit -> "()"
    | TypeId.Bool -> "Bool"
    | TypeId.Int -> "Int"
    | TypeId.Float -> "Float"
    | TypeId.String -> "String"
    | TypeId.App(TypeId.Native t, [ elem ]) when t = typeof<System.Array> ->
        sprintf "Array<%s>" (formatType resolve elem)
    | TypeId.App(head, args) ->
        sprintf "%s<%s>" (formatType resolve head) (args |> List.map (formatType resolve) |> String.concat ", ")
    | TypeId.Name sid -> resolve sid
    | TypeId.Fn([], ret) -> sprintf "() -> %s" (formatType resolve ret)
    | TypeId.Fn([ arg ], ret) -> sprintf "%s -> %s" (formatType resolve arg) (formatType resolve ret)
    | TypeId.Fn(args, ret) ->
        sprintf "(%s) -> %s"
            (args |> List.map (formatType resolve) |> String.concat ", ")
            (formatType resolve ret)
    | TypeId.Meta _ -> "?"
    | TypeId.Native t -> t.Name
    | TypeId.Error msg -> sprintf "error(%s)" msg

/// SymbolTable を使って TypeId をフォーマットする。
let formatTypeWithTable (symbolTable: SymbolTable) (tid: TypeId) : string =
    let resolve sid =
        match symbolTable.Get sid with
        | Some info -> info.name
        | None -> "?"
    formatType resolve tid

// ---------------------------------------------------------------------------
// インデックス型
// ---------------------------------------------------------------------------

/// HIR 内で識別子が使用されている箇所の情報。
type UseSite =
    { /// 使用箇所のソーススパン。
      span: Span
      /// 参照されているシンボルのID。
      symbolId: SymbolId }

/// ソース位置からシンボル情報への逆引きインデックス。
type PositionIndex =
    { /// 識別子の全使用箇所リスト（HIR の Id ノードから収集）。
      useSites: UseSite list
      /// シンボルIDから宣言スパンへのマップ（SymbolId.id をキーとする）。
      declSites: Map<int, Span>
      /// ローカル束縛の可視範囲情報（位置ベース補完フィルタに使用）。
      bindingSites: BindingSite list
      /// モジュールのトップレベルスコープ（補完候補の列挙に使用）。
      moduleScope: Scope
      /// 全式ノードの (span, TypeId) ペア（apostrophe 補完の型解決に使用）。
      exprTypes: (Span * TypeId) list }

/// ローカル束縛（引数/let/for）の可視範囲情報。
and BindingSite =
    { /// 束縛されるシンボルID。
      symbolId: SymbolId
      /// 束縛宣言のスパン。
      declSpan: Span
      /// 当該束縛が可視な字句スコープ全体のスパン。
      scopeSpan: Span }

// ---------------------------------------------------------------------------
// ヘルパー
// ---------------------------------------------------------------------------

/// (line, col) がスパン内に含まれるかを判定する（左閉・右開）。
let private spanContains (line: int) (col: int) (span: Span) : bool =
    let sl = span.left.Line
    let sc = span.left.Column
    let el = span.right.Line
    let ec = span.right.Column
    (line > sl || (line = sl && col >= sc)) &&
    (line < el || (line = el && col < ec))

// ---------------------------------------------------------------------------
// PositionIndex 構築（内部アキュムレータ型）
// ---------------------------------------------------------------------------

/// `build` 関数が使用する不変アキュムレータ。
/// F# のコーディング規約（AGENTS.md §6）に従い可変バインディングは使用しない。
type private BuildState =
    { useSites:   UseSite list
      declSites:  Map<int, Span>
      bindingSites: BindingSite list
      /// 全式ノードの (span, TypeId) ペア（position-based 型解決に使用）。
      exprTypes:  (Span * TypeId) list }

/// 空のアキュムレータ。
let private emptyState : BuildState =
    { useSites    = []
      declSites   = Map.empty
      bindingSites = []
      exprTypes   = [] }

/// 宣言スパンをアキュムレータに追加する（同一 SymbolId の場合は最初の登録のみ保持）。
let private addDecl (sid: SymbolId) (span: Span) (state: BuildState) : BuildState =
    let (SymbolId id) = sid
    if state.declSites |> Map.containsKey id then state
    else { state with declSites = state.declSites |> Map.add id span }

/// 使用箇所をアキュムレータに追加する。
let private addUse (sid: SymbolId) (span: Span) (state: BuildState) : BuildState =
    { state with useSites = { span = span; symbolId = sid } :: state.useSites }

/// ローカル束縛の可視範囲情報を追加する。
let private addBindingSite (sid: SymbolId) (declSpan: Span) (scopeSpan: Span) (state: BuildState) : BuildState =
    { state with
        bindingSites =
            { symbolId = sid
              declSpan = declSpan
              scopeSpan = scopeSpan } :: state.bindingSites }

/// 式ノードを再帰的に走査してアキュムレータを更新する。
let rec private walkExpr (scopeSpan: Span) (expr: Hir.Expr) (state: BuildState) : BuildState =
    // エラーノードを除く全式の (span, TypeId) を収集する（position-based 型解決用）。
    let state =
        match expr with
        | Hir.Expr.ExprError _ -> state
        | _ -> { state with exprTypes = (expr.span, expr.typ) :: state.exprTypes }
    match expr with
    | Hir.Expr.Id(sid, _, span) ->
        addUse sid span state
    | Hir.Expr.Call(_, instance, args, _, _) ->
        let state1 = instance |> Option.fold (fun s e -> walkExpr scopeSpan e s) state
        args |> List.fold (fun s e -> walkExpr scopeSpan e s) state1
    | Hir.Expr.Lambda(args, _, body, _, span) ->
        let lambdaScope = span
        let state1 =
            args
            |> List.fold
                (fun s (arg: Hir.Arg) ->
                    s
                    |> addDecl arg.sid arg.span
                    |> addBindingSite arg.sid arg.span lambdaScope)
                state
        walkExpr lambdaScope body state1
    | Hir.Expr.MemberAccess(_, instance, _, _) ->
        instance |> Option.fold (fun s e -> walkExpr scopeSpan e s) state
    | Hir.Expr.Block(stmts, body, _, span) ->
        let blockScope = span
        let state1 = stmts |> List.fold (fun s stmt -> walkStmt blockScope stmt s) state
        walkExpr blockScope body state1
    | Hir.Expr.If(cond, thenBr, elseBr, _, _) ->
        state |> walkExpr scopeSpan cond |> walkExpr scopeSpan thenBr |> walkExpr scopeSpan elseBr
    | Hir.Expr.Unit _
    | Hir.Expr.Bool _
    | Hir.Expr.Int _
    | Hir.Expr.Float _
    | Hir.Expr.String _
    | Hir.Expr.Null _
    | Hir.Expr.ExprError _ -> state

/// 文ノードを走査してアキュムレータを更新する。
and private walkStmt (scopeSpan: Span) (stmt: Hir.Stmt) (state: BuildState) : BuildState =
    match stmt with
    | Hir.Stmt.Let(sid, _, value, span) ->
        state
        |> addDecl sid span
        |> addBindingSite sid span scopeSpan
        |> walkExpr scopeSpan value
    | Hir.Stmt.Assign(_, value, _) ->
        walkExpr scopeSpan value state
    | Hir.Stmt.StoreField(_, _, _, value, _) ->
        walkExpr scopeSpan value state
    | Hir.Stmt.ExprStmt(expr, _) ->
        walkExpr scopeSpan expr state
    | Hir.Stmt.For(sid, _, iterable, body, span) ->
        let forScope =
            body
            |> List.tryLast
            |> Option.map (fun stmt ->
                match stmt with
                | Hir.Stmt.Let (_, _, _, bodySpan)
                | Hir.Stmt.Assign (_, _, bodySpan)
                | Hir.Stmt.StoreField (_, _, _, _, bodySpan)
                | Hir.Stmt.ExprStmt (_, bodySpan)
                | Hir.Stmt.For (_, _, _, _, bodySpan)
                | Hir.Stmt.ErrorStmt (_, bodySpan) -> bodySpan)
            |> Option.defaultValue span
        let state1 =
            state
            |> addDecl sid span
            |> addBindingSite sid span forScope
            |> walkExpr scopeSpan iterable
        body |> List.fold (fun s stmt -> walkStmt forScope stmt s) state1
    | Hir.Stmt.ErrorStmt _ -> state

/// 単一モジュールを走査してアキュムレータを更新する。
let private walkModule (modul: Hir.Module) (state: BuildState) : BuildState =
    let state1 =
        modul.fields |> List.fold (fun s field ->
            s |> addDecl field.sym field.span |> walkExpr field.span field.body) state
    let state2 =
        modul.methods |> List.fold (fun s method ->
            s |> addDecl method.sym method.span |> walkExpr method.span method.body) state1
    modul.types |> List.fold (fun s typ ->
        // Hir.Type はスパンを持たないため Span.Empty を使用する。
        let s1 = addDecl typ.sym Span.Empty s
        let s2 =
            typ.fields |> List.fold (fun sField field ->
                sField |> addDecl field.sym field.span |> walkExpr field.span field.body) s1
        typ.methods |> List.fold (fun sMethod methodInfo ->
            sMethod |> addDecl methodInfo.sym methodInfo.span |> walkExpr methodInfo.span methodInfo.body) s2) state2

// ---------------------------------------------------------------------------
// PositionIndex 構築（公開エントリポイント）
// ---------------------------------------------------------------------------

/// HIR アセンブリを走査して PositionIndex を構築する。
let build (assembly: Hir.Assembly) : PositionIndex =
    // 全モジュールをフォールドで処理し、不変アキュムレータを更新する。
    let finalState =
        assembly.modules |> List.fold (fun s modul -> walkModule modul s) emptyState

    // トップレベルスコープ：最後のモジュールのスコープを補完候補として使用する。
    let topScope =
        assembly.modules
        |> List.tryLast
        |> Option.map (fun m -> m.scope)
        |> Option.defaultWith (fun () -> Scope(None))

    { useSites    = finalState.useSites
      declSites   = finalState.declSites
      bindingSites = finalState.bindingSites
      moduleScope = topScope
      exprTypes   = finalState.exprTypes }

// ---------------------------------------------------------------------------
// クエリ関数
// ---------------------------------------------------------------------------

/// 指定した (line, col) 位置にある識別子の SymbolId を返す。
/// 複数の使用箇所が重なる場合は最初に見つかったものを返す。
let tryFindSymbolAt (index: PositionIndex) (line: int) (col: int) : SymbolId option =
    index.useSites
    |> List.tryFind (fun e -> spanContains line col e.span)
    |> Option.map (fun e -> e.symbolId)

/// 指定した SymbolId の宣言スパンを返す。
let tryFindDeclSpan (index: PositionIndex) (sid: SymbolId) : Span option =
    let (SymbolId id) = sid
    index.declSites |> Map.tryFind id

/// 指定位置で可視なローカル束縛シンボルを返す。
let visibleSymbolIdsAt (index: PositionIndex) (line: int) (col: int) : SymbolId list =
    index.bindingSites
    |> List.filter (fun binding ->
        spanContains line col binding.scopeSpan
        && (binding.declSpan.left.Line < line
            || (binding.declSpan.left.Line = line && binding.declSpan.left.Column <= col)))
    |> List.map (fun binding -> binding.symbolId)

/// ソースの行・列を整数オフセットに変換するためのスケーリング係数。
/// 1 行あたりの最大列数は 1_000_000 未満とみなす（現実的なソースファイルはこの上限内に収まる）。
[<Literal>]
let private ColumnMultiplier = 1_000_000

/// 指定した (line, col) 位置を包含する最小スパンの式の TypeId を返す。
/// position-based 型解決（ホバー等）に使用する。
/// 最小スパン（最内側の式）を優先するため、スパン幅の昇順で先頭を返す。
let tryFindTypeAt (index: PositionIndex) (line: int) (col: int) : TypeId option =
    index.exprTypes
    |> List.filter (fun (span, _) -> spanContains line col span)
    |> List.sortBy (fun (span, _) ->
        // 最小スパン（最内側）を優先：行をまたぐ式も正しく比較できるよう絶対オフセット差で近似する。
        let startOff = span.left.Line  * ColumnMultiplier + span.left.Column
        let endOff   = span.right.Line * ColumnMultiplier + span.right.Column
        endOff - startOff)
    |> List.tryHead
    |> Option.map snd

/// apostrophe 補完専用: 指定行で `apostropheCol` の位置以前に終わる式の中で
/// 最も後方（右端が apostropheCol に最も近い）の式の TypeId を返す。
/// receiver が `(expr)'` のように括弧でグループされている場合でも
/// パーサがパーレンをスパンに含めないため、内側の式の右端で検索する。
/// 同じ右端なら左端が最も小さい（最も外側の）式を優先する。
let tryFindReceiverTypeAt (index: PositionIndex) (line: int) (apostropheCol: int) : TypeId option =
    index.exprTypes
    |> List.filter (fun (span, _) ->
        // 同一行で apostrophe 以前に終わる式のみを対象とする。
        span.right.Line = line &&
        span.right.Column <= apostropheCol)
    |> List.sortBy (fun (span, _) ->
        // 右端が最も後方（apostropheCol に最も近い）ものを優先。
        // 同じ右端なら最も広い（外側の）式を優先（左端が最も小さいもの）。
        -(span.right.Line * ColumnMultiplier + span.right.Column),
        span.left.Line * ColumnMultiplier + span.left.Column)
    |> List.tryHead
    |> Option.map snd
