namespace Atla.Compiler.Semantics.Data

open System.Reflection
open Atla.Compiler.Data

module Hir =
    type Arg(name: string, tid: TypeId, span: Span) =
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
            | Id (_, _, span) -> span
            | Call (_, _, _, _, span) -> span
            | Lambda (_, _, _, _, span) -> span
            | MemberAccess (_, _, _, span) -> span
            | Block (_, _, _, span) -> span
            | If (_, _, _, _, span) -> span
            | ExprError (_, _, span) -> span

    and Stmt =
        | Let of sid: SymbolId * isMutable: bool * value: Expr * span: Span
        | Assign of sid: SymbolId * value: Expr * span: Span
        | ExprStmt of expr: Expr * span: Span
        | ErrorStmt of message: string * span: Span

    type Field(sid: SymbolId, tid: TypeId, body: Expr, span: Span) =
        member this.sym = sid
        member this.typ = tid
        member this.body = body
        member this.span = span

    type Method(sid: SymbolId, body: Expr, tid: TypeId, span: Span) =
        member this.sym = sid
        member this.body = body
        member this.typ = tid
        member this.span = span

    type Type(sid: SymbolId, fields: Field list) =
        member this.sym = sid
        member this.fields = fields

    type Module(name: string, types: Type list, fields: Field list, methods: Method list, scope: Scope) =
        member this.name = name
        member this.types = types
        member this.fields = fields
        member this.methods = methods
        member this.scope = scope

    type Assembly(name: string, modules: Module list) =
        member this.name = name
        member this.modules = modules
