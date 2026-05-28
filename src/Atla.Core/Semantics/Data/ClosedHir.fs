namespace Atla.Core.Semantics.Data

open System.Reflection
open Atla.Core.Data

/// クロージャー変換後 HIR。`Hir` の全構造に加え、クロージャー変換が導入する
/// `EnvFieldLoad` / `ClosureCreate` ノードを保持する。
/// `Hir.Expr` は「型付きソース意味論 IR」の役割に特化しており、
/// 変換後専用ノードはこのモジュールが正式に所有する。
module ClosedHir =
    /// Hir.Arg を透過的に参照する（クロージャー変換で構造変更なし）。
    type Arg = Hir.Arg

    /// Hir.Callable を透過的に参照する（クロージャー変換で構造変更なし）。
    type Callable = Hir.Callable

    /// Hir.Member を透過的に参照する（クロージャー変換で構造変更なし）。
    type Member = Hir.Member

    /// クロージャー変換後の式。`Hir.Expr` の全ケースに加え、
    /// env-class クロージャー変換が生成する `EnvFieldLoad` / `ClosureCreate` を含む。
    type Expr =
        | Unit of span: Span
        | Bool of value: bool * span: Span
        | Int of value: int * span: Span
        | Float of value: float32 * span: Span
        | Double of value: float * span: Span
        | String of value: string * span: Span
        | Null of tid: TypeId * span: Span
        | Id of sid: SymbolId * tid: TypeId * span: Span
        | Call of func: Callable * instance: Expr option * args: Expr list * tid: TypeId * span: Span
        | Lambda of args: Arg list * ret: TypeId * body: Expr * tid: TypeId * span: Span
        | MemberAccess of mem: Member * instance: Expr option * tid: TypeId * span: Span
        | Block of stmts: Stmt list * expr: Expr * tid: TypeId * span: Span
        | If of cond: Expr * thenBranch: Expr * elseBranch: Expr * tid: TypeId * span: Span
        /// `await operand` 式（ClosedHir 段階では Hir.Expr.Await を素通し）。状態機械生成は PR-3 で行う。
        | Await of operand: Expr * tid: TypeId * span: Span
        | ExprError of message: string * errTyp: TypeId * span: Span
        // env-class クロージャーフィールド参照。lifted invoke method の第一引数（env インスタンス）からフィールドを読む。
        // envArgSid: env インスタンスが格納された引数の SymbolId（lifted method 内の Arg(0)）
        // capturedSid: 捕捉変数（= env フィールド）の SymbolId
        | EnvFieldLoad of envArgSid: SymbolId * capturedSid: SymbolId * tid: TypeId * span: Span
        // env-class クロージャー生成式。env インスタンスを生成し、捕捉変数を格納し、bound delegate を返す。
        // envTypeSid: env クラスの型 SymbolId
        // methodSid: lifted invoke メソッドの SymbolId
        // captured: 捕捉変数の (SymbolId * TypeId * isMutable) リスト（SymbolId 昇順）
        | ClosureCreate of envTypeSid: SymbolId * methodSid: SymbolId * captured: (SymbolId * TypeId * bool) list * tid: TypeId * span: Span
        /// `&target` — マネージドポインタ（`T&`）を生成する式。
        /// target は `Id`（ローカル/引数）または `MemberAccess(DataField | NativeField, Some instance)` を想定。
        /// tid は `TypeId.ByRef inner`。AsyncRewrite 等の lowering で導入され、ユーザー言語からは生成されない。
        | AddrOf of target: Expr * tid: TypeId * span: Span

        member this.typ =
            match this with
            | Unit _ -> TypeId.Unit
            | Bool _ -> TypeId.Bool
            | Int _ -> TypeId.Int
            | Float _ -> TypeId.Float
            | Double _ -> TypeId.Double
            | String _ -> TypeId.String
            | Null (t, _) -> t
            | Id (_, t, _) -> t
            | Call (_, _, _, t, _) -> t
            | Lambda (_, _, _, t, _) -> t
            | MemberAccess (_, _, t, _) -> t
            | Block (_, _, t, _) -> t
            | If (_, _, _, t, _) -> t
            | Await (_, t, _) -> t
            | ExprError (_, t, _) -> t
            | EnvFieldLoad (_, _, t, _) -> t
            | ClosureCreate (_, _, _, t, _) -> t
            | AddrOf (_, t, _) -> t

        member this.span =
            match this with
            | Unit span -> span
            | Bool (_, span) -> span
            | Int (_, span) -> span
            | Float (_, span) -> span
            | Double (_, span) -> span
            | String (_, span) -> span
            | Null (_, span) -> span
            | Id (_, _, span) -> span
            | Call (_, _, _, _, span) -> span
            | Lambda (_, _, _, _, span) -> span
            | MemberAccess (_, _, _, span) -> span
            | Block (_, _, _, span) -> span
            | If (_, _, _, _, span) -> span
            | Await (_, _, span) -> span
            | ExprError (_, _, span) -> span
            | EnvFieldLoad (_, _, _, span) -> span
            | ClosureCreate (_, _, _, _, span) -> span
            | AddrOf (_, _, span) -> span

    /// クロージャー変換後の文。`Hir.Stmt` と同構造だが `ClosedHir.Expr` を参照する。
    and Stmt =
        | Let of sid: SymbolId * isMutable: bool * value: Expr * span: Span
        | Assign of sid: SymbolId * value: Expr * span: Span
        | StoreField of instanceExpr: Expr * typeSid: SymbolId * fieldSid: SymbolId * value: Expr * span: Span
        /// ネイティブ（.NET）フィールドへの書き込み。`Hir.Stmt.StoreNativeField` と同構造。
        | StoreNativeField of receiver: Expr * field: FieldInfo * value: Expr * span: Span
        | ExprStmt of expr: Expr * span: Span
        | For of sid: SymbolId * tid: TypeId * iterable: Expr * body: Stmt list * span: Span
        | If of cond: Expr * thenBody: Stmt list * elseBody: Stmt list * span: Span
        /// for ループからの早期脱出（Layout で内側ループの脱出ラベルへの `Mir.Ins.Jump` へ下す）。
        | Break of span: Span
        /// for ループの次の反復へスキップ（Layout で内側ループの先頭ラベルへの `Mir.Ins.Jump` へ下す）。
        | Continue of span: Span
        /// 非構造化制御フロー用のラベル定義（Layout で `Mir.Ins.MarkLabel` へ下す）。
        /// AsyncRewrite の状態機械生成（resume ポイント等）が導入する。labelId はメソッド内で一意。
        | Label of labelId: int * span: Span
        /// 無条件ジャンプ（Layout で `Mir.Ins.Jump` へ下す）。
        | Goto of labelId: int * span: Span
        /// メソッドからの即時 return（戻り値なし。Layout で `Mir.Ins.Ret` へ下す）。
        /// 状態機械 MoveNext の await 中断点で使用する。
        | Return of span: Span
        /// メソッドから値を返す（Layout で値をスタックに積んで `Mir.Ins.Ret` へ下す）。
        /// ユーザー言語の `return expr` 文から生成される。
        | ReturnValue of value: Expr * span: Span
        /// 保護領域（try）から領域外ラベルへ脱出する（Layout で `Mir.Ins.Leave` へ下す）。
        /// CIL では try/catch の中から `ret`/`br` で抜けられないため、状態機械 MoveNext の
        /// await 中断点（try 内）からメソッド末尾へ抜けるのに使用する。
        | Leave of labelId: int * span: Span
        /// try/catch（catch は単一の例外型）。Layout で `Mir.Ins.TryCatch` へ下す。
        /// catchVarSid は catch 節で捕捉した例外を束縛する変数（型は catchType）。
        | TryCatch of tryBody: Stmt list * catchType: System.Type * catchVarSid: SymbolId * catchBody: Stmt list * span: Span
        | ErrorStmt of message: string * span: Span

    /// クロージャー変換後のフィールド定義。
    type Field(sid: SymbolId, tid: TypeId, body: Expr, span: Span) =
        member this.sym = sid
        member this.typ = tid
        member this.body = body
        member this.span = span

    /// クロージャー変換後のメソッド定義。本体が `ClosedHir.Expr` で表現される。
    type Method(sid: SymbolId, args: (SymbolId * TypeId) list, body: Expr, tid: TypeId, overrideTarget: MethodInfo option, isAsync: bool, span: Span) =
        member this.sym = sid
        /// メソッドの引数リスト（宣言順に (SymbolId, TypeId) の組で保持）。
        member this.args = args
        member this.body = body
        member this.typ = tid
        /// `override` 付きメソッドの場合、上書き対象となる親 .NET クラスの MethodInfo。
        member this.overrideTarget = overrideTarget
        /// `async` 修飾子が付いていたかどうか。状態機械生成は PR-3 で行う。
        member this.isAsync = isAsync
        member this.span = span

    /// クロージャー変換後の型定義。
    type Type(sid: SymbolId, isInterface: bool, baseType: TypeId option, typeParams: string list, fields: Field list, methods: Method list) =
        member this.sym = sid
        /// この型がインターフェイス（role 宣言から生成）であるかを示す。
        member this.isInterface = isInterface
        member this.baseType = baseType
        /// 型パラメータ名のリスト（例: `enum Opt T` では `["T"]`）。非ジェネリックの場合は空リスト。
        member this.typeParams = typeParams
        member this.fields = fields
        member this.methods = methods

    /// クロージャー変換後のモジュール。`closureInvokeMethods` は必須フィールドとして保持する。
    type Module(name: string, types: Type list, fields: Field list, methods: Method list, scope: Scope, closureInvokeMethods: Map<int, int>) =
        member this.name = name
        member this.types = types
        member this.fields = fields
        member this.methods = methods
        member this.scope = scope
        /// クロージャー変換で生成された invoke メソッドの (liftedMethodSid -> envTypeSid) マッピング。
        /// Layout で invoke メソッドを env-class の Mir.Type.methods へルーティングするために使用する。
        member this.closureInvokeMethods = closureInvokeMethods

    /// クロージャー変換後のアセンブリ。
    type Assembly(name: string, modules: Module list) =
        member this.name = name
        member this.modules = modules

    // ─────────────────────────────────────────────
    // 共通トラバーサルインフラ
    // 新しい Expr / Stmt ケースを追加した場合はこの2関数のみを更新すれば良い。
    // ─────────────────────────────────────────────

    /// `Expr` ツリー全体を bottom-up で変換する。
    /// 各ノードの子を再帰変換した後、`f` を適用して新しいノードを返す。
    let rec mapExpr (f: Expr -> Expr) (expr: Expr) : Expr =
        let mapped =
            match expr with
            | Unit _
            | Bool _
            | Int _
            | Float _
            | Double _
            | String _
            | Null _
            | Id _
            | ExprError _
            | EnvFieldLoad _
            | ClosureCreate _ -> expr
            | Call (func, instance, args, tid, span) ->
                Call(func, instance |> Option.map (mapExpr f), args |> List.map (mapExpr f), tid, span)
            | Lambda (args, ret, body, tid, span) ->
                Lambda(args, ret, mapExpr f body, tid, span)
            | MemberAccess (mem, instance, tid, span) ->
                MemberAccess(mem, instance |> Option.map (mapExpr f), tid, span)
            | Block (stmts, body, tid, span) ->
                Block(stmts |> List.map (mapStmt f), mapExpr f body, tid, span)
            | Expr.If (cond, thenBranch, elseBranch, tid, span) ->
                Expr.If(mapExpr f cond, mapExpr f thenBranch, mapExpr f elseBranch, tid, span)
            | Await (operand, tid, span) ->
                Await(mapExpr f operand, tid, span)
            | AddrOf (target, tid, span) ->
                AddrOf(mapExpr f target, tid, span)
        f mapped

    /// `Stmt` に含まれる全 `Expr` を bottom-up で変換する。
    and mapStmt (f: Expr -> Expr) (stmt: Stmt) : Stmt =
        match stmt with
        | Let (sid, isMutable, value, span) -> Let(sid, isMutable, mapExpr f value, span)
        | Assign (sid, value, span) -> Assign(sid, mapExpr f value, span)
        | StoreField (instanceExpr, typeSid, fieldSid, value, span) ->
            StoreField(mapExpr f instanceExpr, typeSid, fieldSid, mapExpr f value, span)
        | StoreNativeField (receiver, field, value, span) ->
            StoreNativeField(mapExpr f receiver, field, mapExpr f value, span)
        | ExprStmt (expr, span) -> ExprStmt(mapExpr f expr, span)
        | For (sid, tid, iterable, body, span) ->
            For(sid, tid, mapExpr f iterable, body |> List.map (mapStmt f), span)
        | Stmt.If (cond, thenBody, elseBody, span) ->
            Stmt.If(mapExpr f cond, thenBody |> List.map (mapStmt f), elseBody |> List.map (mapStmt f), span)
        | TryCatch (tryBody, catchType, catchVarSid, catchBody, span) ->
            TryCatch(tryBody |> List.map (mapStmt f), catchType, catchVarSid, catchBody |> List.map (mapStmt f), span)
        | ReturnValue (value, span) -> ReturnValue(mapExpr f value, span)
        | Break _ | Continue _ | Label _ | Goto _ | Return _ | Leave _ -> stmt
        | ErrorStmt _ -> stmt

    /// `Expr` ツリー全体を pre-order（トップダウン）で畳み込む。
    /// 各ノードを訪問した順に `f` を累積し、最終アキュムレーターを返す。
    let rec foldExpr (f: 'a -> Expr -> 'a) (acc: 'a) (expr: Expr) : 'a =
        let acc' = f acc expr
        match expr with
        | Unit _
        | Bool _
        | Int _
        | Float _
        | Double _
        | String _
        | Null _
        | Id _
        | ExprError _
        | EnvFieldLoad _
        | ClosureCreate _ -> acc'
        | Call (_, instance, args, _, _) ->
            let acc'' = instance |> Option.fold (foldExpr f) acc'
            args |> List.fold (foldExpr f) acc''
        | Lambda (_, _, body, _, _) -> foldExpr f acc' body
        | MemberAccess (_, instance, _, _) ->
            instance |> Option.fold (foldExpr f) acc'
        | Block (stmts, body, _, _) ->
            let acc'' = stmts |> List.fold (foldStmt f) acc'
            foldExpr f acc'' body
        | Expr.If (cond, thenBranch, elseBranch, _, _) ->
            let acc'' = foldExpr f acc' cond
            let acc''' = foldExpr f acc'' thenBranch
            foldExpr f acc''' elseBranch
        | Await (operand, _, _) -> foldExpr f acc' operand
        | AddrOf (target, _, _) -> foldExpr f acc' target

    /// `Stmt` に含まれる全 `Expr` を pre-order で畳み込む。
    and foldStmt (f: 'a -> Expr -> 'a) (acc: 'a) (stmt: Stmt) : 'a =
        match stmt with
        | Let (_, _, value, _)
        | Assign (_, value, _)
        | ExprStmt (value, _) -> foldExpr f acc value
        | StoreField (instanceExpr, _, _, value, _) ->
            foldExpr f (foldExpr f acc instanceExpr) value
        | StoreNativeField (receiver, _, value, _) ->
            foldExpr f (foldExpr f acc receiver) value
        | For (_, _, iterable, body, _) ->
            let acc' = foldExpr f acc iterable
            body |> List.fold (foldStmt f) acc'
        | Stmt.If (cond, thenBody, elseBody, _) ->
            let acc' = foldExpr f acc cond
            let acc'' = thenBody |> List.fold (foldStmt f) acc'
            elseBody |> List.fold (foldStmt f) acc''
        | TryCatch (tryBody, _, _, catchBody, _) ->
            let acc' = tryBody |> List.fold (foldStmt f) acc
            catchBody |> List.fold (foldStmt f) acc'
        | ReturnValue (value, _) -> foldExpr f acc value
        | Break _ | Continue _ | Label _ | Goto _ | Return _ | Leave _ -> acc
        | ErrorStmt _ -> acc

    // ─────────────────────────────────────────────
    // Reader 文脈付き畳み込みインフラ
    // foldExpr と異なり「上から下（Reader 方向）」に文脈を伝搬できる。
    // Lambda 境界で bound をリセットする等のコンテキスト依存走査に使用する。
    // EnvFieldLoad / ClosureCreate はリーフとして扱う（子ノードなし）。
    // ─────────────────────────────────────────────

    /// `Expr` ツリーを Reader 文脈（`ctx`）を保持しながら畳み込む。
    /// - `descend`: 各 Expr ノードに降りる前に文脈を更新（例: Lambda 境界で bound をリセット）
    /// - `afterStmt`: Block 内 Stmt 処理後に文脈を更新（例: Let 束縛の逐次追加）
    /// - `leaf`: リーフノード（Unit / Int / ... / EnvFieldLoad / ClosureCreate）で値を生成
    /// - `merge` / `zero`: 兄弟ノードの結果を合成
    let rec foldExprWithCtx
        (descend: 'ctx -> Expr -> 'ctx)
        (afterStmt: 'ctx -> Stmt -> 'ctx)
        (leaf: 'ctx -> Expr -> 'acc)
        (merge: 'acc -> 'acc -> 'acc)
        (zero: 'acc)
        (ctx: 'ctx)
        (expr: Expr) : 'acc =
        let ctx' = descend ctx expr
        match expr with
        | Unit _ | Bool _ | Int _ | Float _ | Double _ | String _ | Null _ | Id _ | ExprError _
        | EnvFieldLoad _ | ClosureCreate _ ->
            leaf ctx' expr
        | Call (_, instance, args, _, _) ->
            let instAcc =
                instance
                |> Option.map (foldExprWithCtx descend afterStmt leaf merge zero ctx')
                |> Option.defaultValue zero
            let argsAcc =
                args
                |> List.map (foldExprWithCtx descend afterStmt leaf merge zero ctx')
                |> List.fold merge zero
            merge instAcc argsAcc
        | Lambda (_, _, body, _, _) ->
            // ctx' はこの Lambda ノードへ `descend` を適用した後の文脈（例: bound がリセット済み）
            foldExprWithCtx descend afterStmt leaf merge zero ctx' body
        | MemberAccess (_, instance, _, _) ->
            instance
            |> Option.map (foldExprWithCtx descend afterStmt leaf merge zero ctx')
            |> Option.defaultValue zero
        | Block (stmts, body, _, _) ->
            // Stmt を逐次処理し、afterStmt で文脈を更新しながら結果を蓄積する。
            let stmtsAcc, ctxAfterStmts =
                stmts
                |> List.fold
                    (fun (acc, c) stmt ->
                        let stmtAcc = foldStmtWithCtx descend afterStmt leaf merge zero c stmt
                        merge acc stmtAcc, afterStmt c stmt)
                    (zero, ctx')
            let bodyAcc = foldExprWithCtx descend afterStmt leaf merge zero ctxAfterStmts body
            merge stmtsAcc bodyAcc
        | Expr.If (cond, thenBranch, elseBranch, _, _) ->
            [ cond; thenBranch; elseBranch ]
            |> List.map (foldExprWithCtx descend afterStmt leaf merge zero ctx')
            |> List.fold merge zero
        | Await (operand, _, _) ->
            foldExprWithCtx descend afterStmt leaf merge zero ctx' operand
        | AddrOf (target, _, _) ->
            foldExprWithCtx descend afterStmt leaf merge zero ctx' target

    /// `Stmt` 内の全 `Expr` を Reader 文脈（`ctx`）を保持しながら畳み込む。
    /// For ループの反復変数は `afterStmt` に合成 `Let` を渡してボディの文脈へ追加する。
    and foldStmtWithCtx
        (descend: 'ctx -> Expr -> 'ctx)
        (afterStmt: 'ctx -> Stmt -> 'ctx)
        (leaf: 'ctx -> Expr -> 'acc)
        (merge: 'acc -> 'acc -> 'acc)
        (zero: 'acc)
        (ctx: 'ctx)
        (stmt: Stmt) : 'acc =
        match stmt with
        | Let (_, _, value, _) | Assign (_, value, _) | ExprStmt (value, _) ->
            foldExprWithCtx descend afterStmt leaf merge zero ctx value
        | StoreField (instanceExpr, _, _, value, _) ->
            merge
                (foldExprWithCtx descend afterStmt leaf merge zero ctx instanceExpr)
                (foldExprWithCtx descend afterStmt leaf merge zero ctx value)
        | StoreNativeField (receiver, _, value, _) ->
            merge
                (foldExprWithCtx descend afterStmt leaf merge zero ctx receiver)
                (foldExprWithCtx descend afterStmt leaf merge zero ctx value)
        | For (sid, _, iterable, body, span) ->
            let iterAcc = foldExprWithCtx descend afterStmt leaf merge zero ctx iterable
            // For 反復変数 sid をボディ用の文脈へ追加する。
            // `afterStmt` が `Let` を認識して bound を拡張するため、合成 Let でトリガーする。
            // 値は `Expr.Unit span` をプレースホルダーとして使用し、sid の束縛追加のみが目的。
            let innerCtx = afterStmt ctx (Let(sid, false, Expr.Unit span, span))
            let bodyAcc =
                body
                |> List.fold
                    (fun (acc, c) s ->
                        merge acc (foldStmtWithCtx descend afterStmt leaf merge zero c s), afterStmt c s)
                    (zero, innerCtx)
                |> fst
            merge iterAcc bodyAcc
        | Stmt.If (cond, thenBody, elseBody, _) ->
            let condAcc = foldExprWithCtx descend afterStmt leaf merge zero ctx cond
            let thenAcc =
                thenBody
                |> List.fold
                    (fun (acc, c) s ->
                        merge acc (foldStmtWithCtx descend afterStmt leaf merge zero c s), afterStmt c s)
                    (zero, ctx)
                |> fst
            let elseAcc =
                elseBody
                |> List.fold
                    (fun (acc, c) s ->
                        merge acc (foldStmtWithCtx descend afterStmt leaf merge zero c s), afterStmt c s)
                    (zero, ctx)
                |> fst
            merge condAcc (merge thenAcc elseAcc)
        | TryCatch (tryBody, _, _, catchBody, _) ->
            let tryAcc =
                tryBody
                |> List.fold (fun (acc, c) s -> merge acc (foldStmtWithCtx descend afterStmt leaf merge zero c s), afterStmt c s) (zero, ctx)
                |> fst
            let catchAcc =
                catchBody
                |> List.fold (fun (acc, c) s -> merge acc (foldStmtWithCtx descend afterStmt leaf merge zero c s), afterStmt c s) (zero, ctx)
                |> fst
            merge tryAcc catchAcc
        | ReturnValue (value, _) -> foldExprWithCtx descend afterStmt leaf merge zero ctx value
        | Break _ | Continue _ | Label _ | Goto _ | Return _ | Leave _ -> zero
        | ErrorStmt _ -> zero
