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
    // 循環/過深な型グラフでの StackOverflow を、キャッチ可能な
    // InsufficientExecutionStackException に変換してログ捕捉を可能にする。
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
    match tid with
    | TypeId.Unit -> "()"
    | TypeId.Bool -> "Bool"
    | TypeId.Int -> "Int"
    | TypeId.Float -> "Float"
    | TypeId.Double -> "Double"
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
    // 型パラメータ（TypeVar）は型パラメータ名をそのまま表示する。
    | TypeId.TypeVar name -> name
    | TypeId.VarargFn(fixedArgs, elemType, ret) ->
        let prefix =
            if fixedArgs.IsEmpty then ""
            else (fixedArgs |> List.map (formatType resolve) |> String.concat " -> ") + " -> "
        sprintf "(%s%s... -> %s)" prefix (formatType resolve elemType) (formatType resolve ret)
    | TypeId.ByRef inner -> sprintf "ref %s" (formatType resolve inner)

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
      exprTypes: (Span * TypeId) list
      /// Atla データ型のフィールド情報: SymbolId.id → (フィールド名, TypeId) のリスト。
      /// `TypeId.Name typeSid` を受け手型とする apostrophe 補完でフィールド候補を返すために使用する。
      dataTypeFields: Map<int, (string * TypeId) list>
      /// ローカル変数の解決済み TypeId: SymbolId.id → TypeId。
      /// symbol table の meta 変数は型推論後に解決されないため、HIR 式の型を直接格納する。
      /// let/for/lambda 引数の bundle 型が TypeId.Meta のままでも、
      /// RHS 式の .typ から具体型（例: TypeId.Name personSid）が得られる。
      varTypes: Map<int, TypeId> }

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
      exprTypes:  (Span * TypeId) list
      /// ローカル変数の解決済み TypeId: SymbolId.id → TypeId（HIR 式の .typ から取得）。
      varTypes:   Map<int, TypeId> }

/// 空のアキュムレータ。
let private emptyState : BuildState =
    { useSites    = []
      declSites   = Map.empty
      bindingSites = []
      exprTypes   = []
      varTypes    = Map.empty }

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

/// ローカル変数の解決済み型を varTypes マップに登録する。
/// HIR 式の `.typ` を直接格納することで、symbol table の未解決 meta を回避する。
let private addVarType (sid: SymbolId) (tid: TypeId) (state: BuildState) : BuildState =
    let (SymbolId id) = sid
    { state with varTypes = state.varTypes |> Map.add id tid }

/// 式ノードを再帰的に走査してアキュムレータを更新する。
let rec private walkExpr (scopeSpan: Span) (expr: Hir.Expr) (state: BuildState) : BuildState =
    // 循環/過深な HIR での StackOverflow を、キャッチ可能な例外に変換する。
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
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
                    |> addBindingSite arg.sid arg.span lambdaScope
                    // Lambda 引数の型は arg.typ から直接取得する（symbol table の meta を回避）。
                    |> addVarType arg.sid arg.typ)
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
    | Hir.Expr.Await(operand, _, _) ->
        walkExpr scopeSpan operand state
    | Hir.Expr.Unit _
    | Hir.Expr.Bool _
    | Hir.Expr.Int _
    | Hir.Expr.Float _
    | Hir.Expr.Double _
    | Hir.Expr.String _
    | Hir.Expr.Null _
    | Hir.Expr.ExprError _ -> state

/// 文ノードを走査してアキュムレータを更新する。
and private walkStmt (scopeSpan: Span) (stmt: Hir.Stmt) (state: BuildState) : BuildState =
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
    match stmt with
    | Hir.Stmt.Let(sid, _, value, span) ->
        state
        |> addDecl sid span
        |> addBindingSite sid span scopeSpan
        // value.typ は DataInit などで直接 TypeId.Name に解決されているため、
        // symbol table の meta 変数を介さず型を記録する。
        |> addVarType sid value.typ
        |> walkExpr scopeSpan value
    | Hir.Stmt.Assign(_, value, _) ->
        walkExpr scopeSpan value state
    | Hir.Stmt.StoreField(_, _, _, value, _) ->
        walkExpr scopeSpan value state
    | Hir.Stmt.StoreNativeField(receiver, _, value, _) ->
        state |> walkExpr scopeSpan receiver |> walkExpr scopeSpan value
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
                | Hir.Stmt.StoreNativeField (_, _, _, bodySpan)
                | Hir.Stmt.ExprStmt (_, bodySpan)
                | Hir.Stmt.For (_, _, _, _, bodySpan)
                | Hir.Stmt.If (_, _, _, bodySpan)
                | Hir.Stmt.Break bodySpan
                | Hir.Stmt.Continue bodySpan
                | Hir.Stmt.ErrorStmt (_, bodySpan) -> bodySpan)
            |> Option.defaultValue span
        let state1 =
            state
            |> addDecl sid span
            |> addBindingSite sid span forScope
            |> addVarType sid iterable.typ
            |> walkExpr scopeSpan iterable
        body |> List.fold (fun s stmt -> walkStmt forScope stmt s) state1
    | Hir.Stmt.If(cond, thenBody, elseBody, _) ->
        let state1 = walkExpr scopeSpan cond state
        let state2 = thenBody |> List.fold (fun s stmt -> walkStmt scopeSpan stmt s) state1
        elseBody |> List.fold (fun s stmt -> walkStmt scopeSpan stmt s) state2
    | Hir.Stmt.Break _ -> state
    | Hir.Stmt.Continue _ -> state
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
/// `symbolTable` はデータ型フィールドの名前解決に使用する。
let build (assembly: Hir.Assembly) (symbolTable: SymbolTable) : PositionIndex =
    // 全モジュールをフォールドで処理し、不変アキュムレータを更新する。
    let finalState =
        assembly.modules |> List.fold (fun s modul -> walkModule modul s) emptyState

    // トップレベルスコープ：最後のモジュールのスコープを補完候補として使用する。
    let topScope =
        assembly.modules
        |> List.tryLast
        |> Option.map (fun m -> m.scope)
        |> Option.defaultWith (fun () -> Scope(None))

    // データ型のフィールド情報を構築する: SymbolId.id → (フィールド名, TypeId) リスト。
    // SymbolTable からフィールドの名前を引き、フィールドを持つ型のみを登録する。
    // シンボルテーブル上のフィールド名は "TypeName.fieldName" の形式で格納されているため、
    // 最後の '.' 以降の部分（非修飾名）のみを補完候補ラベルとして使用する。
    let getUnqualifiedFieldName (qualifiedName: string) : string =
        let lastDot = qualifiedName.LastIndexOf('.')
        if lastDot >= 0 then qualifiedName.Substring(lastDot + 1) else qualifiedName

    let dataTypeFields =
        assembly.modules
        |> List.collect (fun modul -> modul.types)
        |> List.choose (fun hirType ->
            let (SymbolId typId) = hirType.sym
            let fields =
                hirType.fields
                |> List.choose (fun field ->
                    symbolTable.Get field.sym
                    |> Option.map (fun fieldInfo ->
                        // "Person.name" → "name" に正規化する。
                        getUnqualifiedFieldName fieldInfo.name, field.typ))
            if fields.IsEmpty then None
            else Some(typId, fields))
        |> Map.ofList

    { useSites       = finalState.useSites
      declSites      = finalState.declSites
      bindingSites   = finalState.bindingSites
      moduleScope    = topScope
      exprTypes      = finalState.exprTypes
      dataTypeFields = dataTypeFields
      varTypes       = finalState.varTypes }

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
