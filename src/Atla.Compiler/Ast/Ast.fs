namespace Atla.Compiler.Ast

open Atla.Compiler.Types

module Ast =
    type Expr =
        abstract member span: Span
        
    type Stmt =
        abstract member span: Span

    module Expr = 
        type Unit(span: Span) =
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Int(value: int, span: Span) =
            member this.value = value
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Float(value: float, span: Span) =
            member this.value = value
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type String(value: string, span: Span) =
            member this.value = value
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Id(name: string, span: Span) =
            member this.name = name
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type DoExpr(stmts: Stmt list, span: Span) =
            member this.stmts = stmts
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Error(message: string, span: Span) =
            member this.message = message
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span
    
    module Stmt =
        type Let(name: string, value: Expr, span: Span) =
            member this.name = name
            member this.value = value
            member this.span = span
            interface Stmt with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Var(name: string, value: Expr, span: Span) =
            member this.name = name
            member this.value = value
            member this.span = span
            interface Stmt with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Assign(name: string, value: Expr, span: Span) =
            member this.name = name
            member this.value = value
            member this.span = span
            interface Stmt with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type ExprStmt(expr: Expr, span: Span) =
            member this.expr = expr
            member this.span = span
            interface Stmt with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Return(expr: Expr, span: Span) =
            member this.expr = expr
            member this.span = span
            interface Stmt with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type For(varName: string, iterable: Expr, body: Stmt list, span: Span) =
            member this.varName = varName
            member this.iterable = iterable
            member this.body = body
            member this.span = span
            interface Stmt with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Error(message: string, span: Span) =
            member this.message = message
            member this.span = span
            interface Stmt with
                member this.span = span
            interface HasSpan with
                member this.span = span
