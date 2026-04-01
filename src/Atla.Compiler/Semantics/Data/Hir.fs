namespace Atla.Compiler.Semantics.Data

open Atla.Compiler.Data

module Hir =
    type Arg(name: string, typ: TypeId, span: Span) =
        member this.name = name
        member this.typ = typ
        member this.span = span

    type Expr =
        | Unit of span: Span
        | Int of value: int * span: Span
        | Float of value: float * span: Span
        | String of value: string * span: Span
        | Id of sym: SymbolId * typ: TypeId * span: Span
        | Apply of func: Expr * arg: Expr * typ: TypeId * span: Span
        | Lambda of args: Arg list * ret: TypeId * body: Expr * scope: Scope * lambdaTyp: TypeId * span: Span
        | MemberAccess of receiver: Expr * memberName: string * typ: TypeId * span: Span
        | Block of stmts: Stmt list * expr: Expr * scope: Scope * typ: TypeId * span: Span
        | If of cond: Expr * thenBranch: Expr * elseBranch: Expr * typ: TypeId * span: Span
        | ExprError of message: string * errTyp: TypeId * span: Span

        member this.typ =
            match this with
            | Unit _ -> TypeId.Unit
            | Int _ -> TypeId.Int
            | Float _ -> TypeId.Float
            | String _ -> TypeId.String
            | Id (_, t, _) -> t
            | Apply (_, _, t, _) -> t
            | Lambda (_, _, _, _, t, _) -> t
            | MemberAccess (_, _, t, _) -> t
            | Block (_, _, _, t, _) -> t
            | If (_, _, _, t, _) -> t
            | ExprError (_, t, _) -> t

        member this.span =
            match this with
            | Unit (span) -> span
            | Int (_, span) -> span
            | Float (_, span) -> span
            | String (_, span) -> span
            | Id (_, _, span) -> span
            | Apply (_, _, _, span) -> span
            | Lambda (_, _, _, _, _, span) -> span
            | MemberAccess (_, _, _, span) -> span
            | Block (_, _, _, _, span) -> span
            | If (_, _, _, _, span) -> span
            | ExprError (_, _, span) -> span

    and Stmt =
        | Let of sym: SymbolId * isMutable: bool * value: Expr * span: Span
        | Assign of sym: SymbolId * value: Expr * span: Span
        | ExprStmt of expr: Expr * span: Span
        | ErrorStmt of message: string * span: Span

    type Field(sym: SymbolId, typ: TypeId, body: Expr, span: Span) =
        member this.sym = sym
        member this.typ = typ
        member this.body = body
        member this.span = span

    type Type(sym: SymbolId, fields: Field list) =
        member this.sym = sym
        member this.fields = fields

    type Module(name: string, types: Type list, fields: Field list, scope: Scope) =
        member this.name = name
        member this.types = types
        member this.fields = fields
        member this.scope = scope

    type Assembly(name: string, modules: Module list) =
        member this.name = name
        member this.modules = modules
