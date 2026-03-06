namespace Atla.Compiler.Ast

open Atla.Compiler.Types

module Ast =
    type Expr =
        abstract member span: Span
        
    type Stmt =
        abstract member span: Span
        
    type TypeExpr =
        abstract member span: Span

    type DataItem =
        abstract member span: Span

    type Decl =
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

        type MemberAccess(target: Expr, memberName: string, span: Span) =
            member this.target = target
            member this.memberName = memberName
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Apply(func: Expr, args: Expr list, span: Span) =
            member this.func = func
            member this.args = args
            member this.span = span
            interface Expr with
                member this.span = this.span
            interface HasSpan with
                member this.span = this.span

        type Block(stmts: Stmt list, span: Span) =
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
                
    module TypeExpr =
        type Id(name: string, span: Span) =
            member this.name = name
            member this.span = span
            interface TypeExpr with
                member this.span = span
            interface HasSpan with
                member this.span = span

    module DataItem =
        type Field(name:string, typeExpr: TypeExpr, span: Span) =
            member this.name = name
            member this.typeExpr = typeExpr
            member this.span = span
            interface DataItem with
                member this.span = span
            interface HasSpan with
                member this.span = span

    module Decl =
        type Import(path: string list, span: Span) =
            member this.path = path
            member this.span = span
            interface Decl with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Data(name: string, items: DataItem list, span: Span) =
            member this.name = name
            member this.items = items
            member this.span = span
            interface Decl with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Error(message: string, span: Span) =
            member this.message = message
            member this.span = span
            interface Decl with
                member this.span = span
            interface HasSpan with
                member this.span = span

    type Module(decls: Decl list, stmts: Stmt list) =
        member this.decls = decls
        member this.stmts = stmts
