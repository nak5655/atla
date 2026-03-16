namespace Atla.Compiler.Hir

open Atla.Compiler.Types

module Hir =
    // Expression HIR as a discriminated union
    type Expr =
        abstract member typ: TypeCray with get, set

    // Statements
    type Stmt =
        | Let of name: string * isMutable: bool * value: Expr * span: Span
        | Assign of name: string * value: Expr * span: Span
        | ExprStmt of expr: Expr * span: Span
        | ErrorStmt of message: string * span: Span

    // Type expressions
    type TypeExpr =
        | Id of name: string * span: Span
        | Import of path: string list * span: Span

    type FnArg =
        | Unit of span: Span
        | Named of name: string * typeExpr: TypeExpr * span: Span

    // Declarations
    type Decl =
        | Def of name: string * expr: Expr * span: Span
        | TypeDef of name: string * typeExpr: TypeExpr * span: Span
        | DeclError of message: string * span: Span

    type Module(name: string, decls: Decl list) =
        member this.name = name
        member this.decls = decls

    type Assembly(modules: Module list) =
        member this.modules = modules
        
    module Expr =
        type Unit(span: Span) =
            let mutable typ = TypeCray.Unit
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type Int(value:int, span: Span) =
            let mutable typ = TypeCray.Int
            member this.value = value
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type Float(value: float, span: Span) =
            let mutable typ = TypeCray.Float
            member this.value = value
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type String(value: string, span: Span) =
            let mutable typ = TypeCray.String
            member this.value = value
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type Id(name: string, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.name = name
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type Apply(func: Expr, args: Expr list, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.func = func
            member this.args = args
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type Fn(args: FnArg list, ret: TypeExpr, body: Expr, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.args = args
            member this.ret = ret
            member this.body = body
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type MemberAccess(receiver: Expr, memberName: string, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.receiver = receiver
            member this.memberName = memberName
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type Block(stmts: Stmt list, expr: Expr, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.stmts = stmts
            member this.expr = expr
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type If(cond: Expr, thenBranch: Expr, elseBranch: Expr option, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.cond = cond
            member this.thenBranch = thenBranch
            member this.elseBranch = elseBranch
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type Error(message: string, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.message = message
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v
