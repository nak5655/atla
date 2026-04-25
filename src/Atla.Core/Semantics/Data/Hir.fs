namespace Atla.Core.Semantics.Data

open System.Reflection
open Atla.Core.Data

module Hir =
    type Arg(sid: SymbolId, name: string, tid: TypeId, span: Span) =
        member this.sid = sid
        member this.name = name
        member this.typ = tid
        member this.span = span

    type Callable =
        | Fn of SymbolId
        | DataConstructor of typeSid: SymbolId * fieldSids: SymbolId list
        | BuiltinOperator of Builtins.Operators
        | NativeMethod of MethodInfo
        | NativeMethodGroup of MethodInfo list
        | NativeConstructor of ConstructorInfo
        | NativeConstructorGroup of ConstructorInfo list

    type Member =
        | NativeField of FieldInfo
        | NativeProperty of PropertyInfo
        | NativeMethod of MethodInfo

    type Expr =
        | Unit of span: Span
        | Int of value: int * span: Span
        | Float of value: float * span: Span
        | String of value: string * span: Span
        | Null of tid: TypeId * span: Span
        | Id of sid: SymbolId * tid: TypeId * span: Span
        | Call of func: Callable * instance: Expr option * args: Expr list * tid: TypeId * span: Span
        | Lambda of args: Arg list * ret: TypeId * body: Expr * tid: TypeId * span: Span
        | MemberAccess of mem: Member * instance: Expr option * tid: TypeId * span: Span
        | Block of stmts: Stmt list * expr: Expr * tid: TypeId * span: Span
        | If of cond: Expr * thenBranch: Expr * elseBranch: Expr * tid: TypeId * span: Span
        | ExprError of message: string * errTyp: TypeId * span: Span

        member this.typ =
            match this with
            | Unit _ -> TypeId.Unit
            | Int _ -> TypeId.Int
            | Float _ -> TypeId.Float
            | String _ -> TypeId.String
            | Null (t, _) -> t
            | Id (_, t, _) -> t
            | Call (_, _, _, t, _) -> t
            | Lambda (_, _, _, t, _) -> t
            | MemberAccess (_, _, t, _) -> t
            | Block (_, _, t, _) -> t
            | If (_, _, _, t, _) -> t
            | ExprError (_, t, _) -> t

        member this.span =
            match this with
            | Unit (span) -> span
            | Int (_, span) -> span
            | Float (_, span) -> span
            | String (_, span) -> span
            | Null (_, span) -> span
            | Id (_, _, span) -> span
            | Call (_, _, _, _, span) -> span
            | Lambda (_, _, _, _, span) -> span
            | MemberAccess (_, _, _, span) -> span
            | Block (_, _, _, span) -> span
            | If (_, _, _, _, span) -> span
            | ExprError (_, _, span) -> span

        member this.hasError =
            this.getDiagnostics |> List.exists (fun diagnostic -> diagnostic.isError)

        member this.getDiagnostics : Diagnostic list =
            match this with
            | ExprError (message, _, span) -> [ Diagnostic.Error(message, span) ]
            | Block (stmts, body, _, _) ->
                (stmts |> List.collect (fun stmt -> stmt.getDiagnostics)) @ body.getDiagnostics
            | If (cond, thenBranch, elseBranch, _, _) ->
                cond.getDiagnostics @ thenBranch.getDiagnostics @ elseBranch.getDiagnostics
            | Call (_, instance, args, _, _) ->
                let instanceErrors =
                    instance
                    |> Option.map (fun expr -> expr.getDiagnostics)
                    |> Option.defaultValue []
                instanceErrors @ (args |> List.collect (fun expr -> expr.getDiagnostics))
            | Lambda (_, _, body, _, _) -> body.getDiagnostics
            | MemberAccess (_, instance, _, _) ->
                instance
                |> Option.map (fun expr -> expr.getDiagnostics)
                |> Option.defaultValue []
            | _ -> []

    and Stmt =
        | Let of sid: SymbolId * isMutable: bool * value: Expr * span: Span
        | Assign of sid: SymbolId * value: Expr * span: Span
        | ExprStmt of expr: Expr * span: Span
        | For of sid: SymbolId * tid: TypeId * iterable: Expr * body: Stmt list * span: Span
        | ErrorStmt of message: string * span: Span

        member this.hasError =
            this.getDiagnostics |> List.exists (fun diagnostic -> diagnostic.isError)

        member this.getDiagnostics : Diagnostic list =
            match this with
            | ErrorStmt (message, span) -> [ Diagnostic.Error(message, span) ]
            | Let (_, _, value, _)
            | Assign (_, value, _)
            | ExprStmt (value, _) -> value.getDiagnostics
            | For (_, _, iterable, body, _) ->
                iterable.getDiagnostics @ (body |> List.collect (fun stmt -> stmt.getDiagnostics))

    type Field(sid: SymbolId, tid: TypeId, body: Expr, span: Span) =
        member this.sym = sid
        member this.typ = tid
        member this.body = body
        member this.span = span
        member this.hasError = body.hasError
        member this.getDiagnostics = body.getDiagnostics

    type Method(sid: SymbolId, args: (SymbolId * TypeId) list, body: Expr, tid: TypeId, span: Span) =
        member this.sym = sid
        // メソッドの引数リスト（宣言順に (SymbolId, TypeId) の組で保持）。
        member this.args = args
        member this.body = body
        member this.typ = tid
        member this.span = span
        member this.hasError = body.hasError
        member this.getDiagnostics = body.getDiagnostics

    type Type(sid: SymbolId, fields: Field list) =
        member this.sym = sid
        member this.fields = fields
        member this.hasError = fields |> List.exists (fun field -> field.hasError)
        member this.getDiagnostics = fields |> List.collect (fun field -> field.getDiagnostics)

    type Module(name: string, types: Type list, fields: Field list, methods: Method list, scope: Scope) =
        member this.name = name
        member this.types = types
        member this.fields = fields
        member this.methods = methods
        member this.scope = scope
        member this.hasError =
            (fields |> List.exists (fun field -> field.hasError))
            || (methods |> List.exists (fun method -> method.hasError))
            || (types |> List.exists (fun typ -> typ.hasError))
        member this.getDiagnostics =
            (fields |> List.collect (fun field -> field.getDiagnostics))
            @ (methods |> List.collect (fun method -> method.getDiagnostics))
            @ (types |> List.collect (fun typ -> typ.getDiagnostics))

    type Assembly(name: string, modules: Module list) =
        member this.name = name
        member this.modules = modules
        member this.hasError = modules |> List.exists (fun modul -> modul.hasError)
        member this.getDiagnostics = modules |> List.collect (fun modul -> modul.getDiagnostics)

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
            | Int _
            | Float _
            | String _
            | Null _
            | Id _
            | ExprError _ -> expr
            | Call (func, instance, args, tid, span) ->
                Call(func, instance |> Option.map (mapExpr f), args |> List.map (mapExpr f), tid, span)
            | Lambda (args, ret, body, tid, span) ->
                Lambda(args, ret, mapExpr f body, tid, span)
            | MemberAccess (mem, instance, tid, span) ->
                MemberAccess(mem, instance |> Option.map (mapExpr f), tid, span)
            | Block (stmts, body, tid, span) ->
                Block(stmts |> List.map (mapStmt f), mapExpr f body, tid, span)
            | If (cond, thenBranch, elseBranch, tid, span) ->
                If(mapExpr f cond, mapExpr f thenBranch, mapExpr f elseBranch, tid, span)
        f mapped

    /// `Stmt` に含まれる全 `Expr` を bottom-up で変換する。
    and mapStmt (f: Expr -> Expr) (stmt: Stmt) : Stmt =
        match stmt with
        | Let (sid, isMutable, value, span) -> Let(sid, isMutable, mapExpr f value, span)
        | Assign (sid, value, span) -> Assign(sid, mapExpr f value, span)
        | ExprStmt (expr, span) -> ExprStmt(mapExpr f expr, span)
        | For (sid, tid, iterable, body, span) ->
            For(sid, tid, mapExpr f iterable, body |> List.map (mapStmt f), span)
        | ErrorStmt _ -> stmt

    /// `Expr` ツリー全体を pre-order（トップダウン）で畳み込む。
    /// 各ノードを訪問した順に `f` を累積し、最終アキュムレーターを返す。
    let rec foldExpr (f: 'a -> Expr -> 'a) (acc: 'a) (expr: Expr) : 'a =
        let acc' = f acc expr
        match expr with
        | Unit _
        | Int _
        | Float _
        | String _
        | Null _
        | Id _
        | ExprError _ -> acc'
        | Call (_, instance, args, _, _) ->
            let acc'' = instance |> Option.fold (foldExpr f) acc'
            args |> List.fold (foldExpr f) acc''
        | Lambda (_, _, body, _, _) -> foldExpr f acc' body
        | MemberAccess (_, instance, _, _) ->
            instance |> Option.fold (foldExpr f) acc'
        | Block (stmts, body, _, _) ->
            let acc'' = stmts |> List.fold (foldStmt f) acc'
            foldExpr f acc'' body
        | If (cond, thenBranch, elseBranch, _, _) ->
            let acc'' = foldExpr f acc' cond
            let acc''' = foldExpr f acc'' thenBranch
            foldExpr f acc''' elseBranch

    /// `Stmt` に含まれる全 `Expr` を pre-order で畳み込む。
    and foldStmt (f: 'a -> Expr -> 'a) (acc: 'a) (stmt: Stmt) : 'a =
        match stmt with
        | Let (_, _, value, _)
        | Assign (_, value, _)
        | ExprStmt (value, _) -> foldExpr f acc value
        | For (_, _, iterable, body, _) ->
            let acc' = foldExpr f acc iterable
            body |> List.fold (foldStmt f) acc'
        | ErrorStmt _ -> acc

    // ─────────────────────────────────────────────
    // Reader 文脈付き畳み込みインフラ
    // foldExpr と異なり「上から下（Reader 方向）」に文脈を伝搬できる。
    // Lambda 境界で bound をリセットする等のコンテキスト依存走査に使用する。
    // ─────────────────────────────────────────────

    /// `Expr` ツリーを Reader 文脈（`ctx`）を保持しながら畳み込む。
    /// - `descend`: 各 Expr ノードに降りる前に文脈を更新（例: Lambda 境界で bound をリセット）
    /// - `afterStmt`: Block 内 Stmt 処理後に文脈を更新（例: Let 束縛の逐次追加）
    /// - `leaf`: リーフノード（Unit / Int / Float / String / Null / Id / ExprError）で値を生成
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
        | Unit _ | Int _ | Float _ | String _ | Null _ | Id _ | ExprError _ ->
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
        | If (cond, thenBranch, elseBranch, _, _) ->
            [ cond; thenBranch; elseBranch ]
            |> List.map (foldExprWithCtx descend afterStmt leaf merge zero ctx')
            |> List.fold merge zero

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
        | ErrorStmt _ -> zero
