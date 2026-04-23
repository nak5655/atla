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
      /// モジュールのトップレベルスコープ（補完候補の列挙に使用）。
      moduleScope: Scope }

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
// PositionIndex 構築
// ---------------------------------------------------------------------------

/// HIR アセンブリを走査して PositionIndex を構築する。
let build (assembly: Hir.Assembly) : PositionIndex =
    let useSites = System.Collections.Generic.List<UseSite>()
    let declSites = System.Collections.Generic.Dictionary<int, Span>()

    /// シンボルの宣言スパンを登録する（初回のみ）。
    let addDecl (sid: SymbolId) (span: Span) =
        let (SymbolId id) = sid
        if not (declSites.ContainsKey id) then
            declSites[id] <- span

    /// 式ノードを再帰的に走査し、Id 使用箇所と宣言箇所を収集する。
    let rec walkExpr (expr: Hir.Expr) =
        match expr with
        | Hir.Expr.Id(sid, _, span) ->
            useSites.Add { span = span; symbolId = sid }
        | Hir.Expr.Call(_, instance, args, _, _) ->
            instance |> Option.iter walkExpr
            args |> List.iter walkExpr
        | Hir.Expr.Lambda(args, _, body, _, _) ->
            args |> List.iter (fun (arg: Hir.Arg) -> addDecl arg.sid arg.span)
            walkExpr body
        | Hir.Expr.MemberAccess(_, instance, _, _) ->
            instance |> Option.iter walkExpr
        | Hir.Expr.Block(stmts, body, _, _) ->
            stmts |> List.iter walkStmt
            walkExpr body
        | Hir.Expr.If(cond, thenBr, elseBr, _, _) ->
            walkExpr cond
            walkExpr thenBr
            walkExpr elseBr
        | Hir.Expr.Unit _
        | Hir.Expr.Int _
        | Hir.Expr.Float _
        | Hir.Expr.String _
        | Hir.Expr.Null _
        | Hir.Expr.ExprError _ -> ()

    /// 文ノードを走査し、宣言箇所と使用箇所を収集する。
    and walkStmt (stmt: Hir.Stmt) =
        match stmt with
        | Hir.Stmt.Let(sid, _, value, span) ->
            addDecl sid span
            walkExpr value
        | Hir.Stmt.Assign(_, value, _) ->
            walkExpr value
        | Hir.Stmt.ExprStmt(expr, _) ->
            walkExpr expr
        | Hir.Stmt.For(sid, _, iterable, body, span) ->
            addDecl sid span
            walkExpr iterable
            body |> List.iter walkStmt
        | Hir.Stmt.ErrorStmt _ -> ()

    // トップレベルスコープ（補完候補用）。
    // 最後に処理したモジュールのスコープを使用する。
    let mutable topScope = Scope(None)

    for modul in assembly.modules do
        topScope <- modul.scope
        for field in modul.fields do
            addDecl field.sym field.span
            walkExpr field.body
        for method in modul.methods do
            addDecl method.sym method.span
            walkExpr method.body
        for typ in modul.types do
            // Hir.Type はスパンを持たないため Span.Empty を使用する。
            addDecl typ.sym Span.Empty
            for field in typ.fields do
                addDecl field.sym field.span
                walkExpr field.body

    { useSites  = useSites |> Seq.toList
      declSites = declSites |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
      moduleScope = topScope }

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
