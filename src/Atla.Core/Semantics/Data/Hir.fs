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
        // env-class クロージャーフィールド参照。lifted invoke method の第一引数（env インスタンス）からフィールドを読む。
        // envArgSid: env インスタンスが格納された引数の SymbolId（lifted method 内の Arg(0)）
        // capturedSid: 捕捉変数（= env フィールド）の SymbolId
        | EnvFieldLoad of envArgSid: SymbolId * capturedSid: SymbolId * tid: TypeId * span: Span
        // env-class クロージャー生成式。env インスタンスを生成し、捕捉変数を格納し、bound delegate を返す。
        // envTypeSid: env クラスの型 SymbolId
        // methodSid: lifted invoke メソッドの SymbolId
        // captured: 捕捉変数の (SymbolId * TypeId * isMutable) リスト（SymbolId 昇順）
        | ClosureCreate of envTypeSid: SymbolId * methodSid: SymbolId * captured: (SymbolId * TypeId * bool) list * tid: TypeId * span: Span

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
            | EnvFieldLoad (_, _, t, _) -> t
            | ClosureCreate (_, _, _, t, _) -> t

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
            | EnvFieldLoad (_, _, _, span) -> span
            | ClosureCreate (_, _, _, _, span) -> span

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
            | EnvFieldLoad _ -> []
            | ClosureCreate _ -> []
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

    type Module(name: string, types: Type list, fields: Field list, methods: Method list, scope: Scope, ?closureInvokeMethods: Map<int, int>) =
        member this.name = name
        member this.types = types
        member this.fields = fields
        member this.methods = methods
        member this.scope = scope
        // クロージャー変換で生成された invoke メソッドの (liftedMethodSid -> envTypeSid) マッピング。
        // Layout で invoke メソッドを env-class の Mir.Type.methods へルーティングするために使用する。
        member this.closureInvokeMethods = defaultArg closureInvokeMethods Map.empty
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
