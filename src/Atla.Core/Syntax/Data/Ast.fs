namespace Atla.Core.Syntax.Data

open Atla.Core.Data

module Ast =
    type Expr =
        abstract member span: Span
        
    type Stmt =
        abstract member span: Span
        
    type TypeExpr =
        abstract member span: Span

    type DataItem =
        abstract member span: Span

    type FnArg =
        abstract member span: Span

    type Decl =
        abstract member span: Span

    type DataInitField =
        abstract member span: Span

    type IfBranch =
        abstract member span: Span

    module IfBranch =
        type Then(cond: Expr, body: Expr, span: Span) =
            member this.cond = cond
            member this.body = body
            member this.span = span
            interface IfBranch with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Else(body: Expr, span: Span) =
            member this.body = body
            member this.span = span
            interface IfBranch with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Error(message: string, span: Span) =
            member this.message = message
            member this.span = span
            interface IfBranch with
                member this.span = span
            interface HasSpan with
                member this.span = span

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

        type Bool(value: bool, span: Span) =
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

        type MemberAccess(receiver: Expr, memberName: string, span: Span) =
            member this.receiver = receiver
            member this.memberName = memberName
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type IndexAccess(receiver: Expr, index: Expr, span: Span) =
            member this.receiver = receiver
            member this.index = index
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span
                
        type StaticAccess(typeName: string, memberName: string, span: Span) =
            member this.typeName = typeName
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

        type GenericApply(func: Expr, typeArgs: TypeExpr list, span: Span) =
            member this.func = func
            member this.typeArgs = typeArgs
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

        type If(branches: IfBranch list, span: Span) =
            member this.branches = branches
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        // 無名関数式（例: fn x y -> x + y）を表す。
        type Lambda(args: string list, body: Expr, span: Span) =
            member this.args = args
            member this.body = body
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

        /// `TypeName { field = value, ... }` 形式の data 初期化式。
        type DataInit(typeName: string, fields: DataInitField list, span: Span) =
            member this.typeName = typeName
            member this.fields = fields
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

        type Assign(target: Expr, value: Expr, span: Span) =
            member this.target = target
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
        type Unit(span: Span) =
            member this.span = span
            interface TypeExpr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Id(name: string, span: Span) =
            member this.name = name
            member this.span = span
            interface TypeExpr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        // 空白区切りの型適用（例: Array String）を表す。
        type Apply(head: TypeExpr, args: TypeExpr list, span: Span) =
            member this.head = head
            member this.args = args
            member this.span = span
            interface TypeExpr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        // 関数型（例: Int -> Int）を表す。右結合で再帰的に構成される。
        type Arrow(arg: TypeExpr, ret: TypeExpr, span: Span) =
            member this.arg = arg
            member this.ret = ret
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

    module DataInitField =
        /// data 初期化式の単一フィールド代入（`name = expr`）。
        type Field(name: string, value: Expr, span: Span) =
            member this.name = name
            member this.value = value
            member this.span = span
            interface DataInitField with
                member this.span = span
            interface HasSpan with
                member this.span = span


    module FnArg =
        type Unit(span: Span) =
            member this.span = span
            interface FnArg with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Named(name: string, typeExpr: TypeExpr, span: Span) =
            member this.name = name
            member this.typeExpr = typeExpr
            member this.span = span
            interface FnArg with
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

        type Fn(name: string, args: FnArg list, ret: TypeExpr, body: Expr, span: Span) =
            member this.name = name
            member this.args = args
            member this.ret = ret
            member this.body = body
            member this.span = span
            interface Decl with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Impl(typeName: string, forTypeName: string option, byFieldName: string option, methods: Fn list, span: Span) =
            member this.typeName = typeName
            member this.forTypeName = forTypeName
            member this.byFieldName = byFieldName
            member this.methods = methods
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

    type Module(decls: Decl list) =
        member this.decls = decls
