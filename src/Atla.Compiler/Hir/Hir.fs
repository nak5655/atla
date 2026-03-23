namespace Atla.Compiler.Hir

open Atla.Compiler.Types

module Hir =
    // Expression HIR as a discriminated union
    type Expr =
        abstract member typ: TypeCray with get, set

    // Statements
    type Stmt = interface end

    // Type expressions
    type TypeExpr =
        | Unit of span: Span
        | Id of name: string * span: Span
        | Import of path: string list * span: Span

    type FnArg =
        abstract member span: Span

    // Declarations
    type Decl =
        | Fn of name: string * args: FnArg list * ret: TypeExpr * body: Expr * scope: Scope * span: Span
        | TypeDef of name: string * typeExpr: TypeExpr * span: Span
        | DeclError of message: string * span: Span

    type Module(name: string, decls: Decl list, scope: Scope) =
        member this.name = name
        member this.decls = decls
        member this.scope = scope

    type Assembly(modules: Module list, scope: Scope) =
        member this.modules = modules
        member this.scope = scope

    module FnArg =
        type Unit(span: Span) =
            member this.span = span
            interface FnArg with
                member this.span = span
        type Named(name: string, typeExpr: TypeExpr, span: Span) =
            let mutable _typ = TypeCray.Unknown
            member this.name = name
            member this.typeExpr = typeExpr
            member this.span = span
            member this.typ
                with get() = _typ
                and set(v) = _typ <- v
            interface FnArg with
                member this.span = span
        
    module Expr =
        type Unit(span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type Int(value:int, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.value = value
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type Float(value: float, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.value = value
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type String(value: string, span: Span) =
            let mutable typ = TypeCray.Unknown
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

        type Fn(args: FnArg list, ret: TypeExpr, body: Expr, scope: Scope, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.args = args
            member this.ret = ret
            member this.body = body
            member this.scope = scope
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

        type Block(stmts: Stmt list, scope: Scope, span: Span) =
            let mutable typ = TypeCray.Unknown
            member this.stmts = stmts
            member this.scope = scope
            member this.span = span
            interface Expr with
                member this.typ
                    with get() = typ
                    and set(v) = typ <- v

        type If(cond: Expr, thenBranch: Expr, elseBranch: Expr, span: Span) =
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

    module Stmt =
        type Let(name: string, isMutable: bool, value: Expr, span: Span) =
            member this.name = name
            member this.isMutable = isMutable
            member this.value = value
            member this.span = span
            interface Stmt

        type Assign(name: string, value: Expr, span: Span) =
            member this.name = name
            member this.value = value
            member this.span = span
            interface Stmt

        type ExprStmt(expr: Expr, span: Span) =
            member this.expr = expr
            member this.span = span
            interface Stmt

        type ErrorStmt(message: string, span: Span) =
            member this.message = message
            member this.span = span
            interface Stmt
