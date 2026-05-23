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

    type EnumCase =
        abstract member span: Span

    type FnArg =
        abstract member span: Span

    type Decl =
        abstract member span: Span

    type DataInitField =
        abstract member span: Span

    type PatternField =
        abstract member span: Span

    type Pattern =
        abstract member span: Span

    type MatchArm =
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

        /// `await Expr` 形式の式。`async fn` の本体内でのみ使用でき、
        /// オペランドは `Task` または `Task T` 型でなければならない（Analyze で検査）。
        type Await(operand: Expr, span: Span) =
            member this.operand = operand
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

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
        type Lambda(args: FnArg list, body: Expr, span: Span) =
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

        /// `EnumType'CaseName { field = value, ... }` 形式の enum case 初期化式。
        type EnumInit(typeName: string, caseName: string, fields: DataInitField list, span: Span) =
            member this.typeName = typeName
            member this.caseName = caseName
            member this.fields = fields
            member this.span = span
            interface Expr with
                member this.span = span
            interface HasSpan with
                member this.span = span

        /// `match expr | Pattern -> expr ...` 形式の match 式。
        type Match(scrutinee: Expr, arms: MatchArm list, span: Span) =
            member this.scrutinee = scrutinee
            member this.arms = arms
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


        type CompoundAssignOp =
            | Add
            | Sub
            | Mul
            | Div

        type CompoundAssign(op: CompoundAssignOp, target: Expr, value: Expr, span: Span) =
            member this.op = op
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

        type If(branches: IfBranch list, span: Span) =
            member this.branches = branches
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

    module EnumCase =
        type Field(name:string, typeExpr: TypeExpr, span: Span) =
            member this.name = name
            member this.typeExpr = typeExpr
            member this.span = span
            interface HasSpan with
                member this.span = span

        type Case(name: string, fields: Field list, span: Span) =
            member this.name = name
            member this.fields = fields
            member this.span = span
            interface EnumCase with
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

    module PatternField =
        type Named(name: string, span: Span) =
            member this.name = name
            member this.span = span
            interface PatternField with
                member this.span = span
            interface HasSpan with
                member this.span = span

        /// `| Type'Case varName ->` 形式の位置引数バインディング。
        /// ケースの n 番目のフィールドを varName に束縛する。
        type Positional(varName: string, span: Span) =
            member this.varName = varName
            member this.span = span
            interface PatternField with
                member this.span = span
            interface HasSpan with
                member this.span = span

    module Pattern =
        /// `TypeName'CaseName` または `TypeName'CaseName { x, .. }` 形式の enum pattern。
        type Enum(typeName: string, caseName: string, fields: PatternField list, hasRest: bool, span: Span) =
            member this.typeName = typeName
            member this.caseName = caseName
            member this.fields = fields
            member this.hasRest = hasRest
            member this.span = span
            interface Pattern with
                member this.span = span
            interface HasSpan with
                member this.span = span

    module MatchArm =
        type Arm(pattern: Pattern, body: Expr, span: Span) =
            member this.pattern = pattern
            member this.body = body
            member this.span = span
            interface MatchArm with
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

        type Inferred(name: string, span: Span) =
            member this.name = name
            member this.span = span
            interface FnArg with
                member this.span = span
            interface HasSpan with
                member this.span = span

    module Decl =
        type Import(path: string list, isPublic: bool, span: Span) =
            member this.path = path
            member this.isPublic = isPublic
            member this.span = span
            interface Decl with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Data(name: string, typeParams: string list, items: DataItem list, span: Span) =
            member this.name = name
            /// 型パラメータ名のリスト（例: `data Pair A B` では `["A"; "B"]`）。非ジェネリックの場合は空リスト。
            member this.typeParams = typeParams
            member this.items = items
            member this.span = span
            interface Decl with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Enum(name: string, typeParams: string list, cases: EnumCase list, span: Span) =
            member this.name = name
            /// 型パラメータ名のリスト（例: `enum Opt T` では `["T"]`）。非ジェネリックの場合は空リスト。
            member this.typeParams = typeParams
            member this.cases = cases
            member this.span = span
            interface Decl with
                member this.span = span
            interface HasSpan with
                member this.span = span

        type Fn(name: string, args: FnArg list, ret: TypeExpr, body: Expr, isOverride: bool, isAsync: bool, span: Span) =
            member this.name = name
            member this.args = args
            member this.ret = ret
            member this.body = body
            /// `override` 修飾子の有無。`impl A as B` 内のメソッドでのみ意味があり、
            /// 他の文脈では Resolve フェーズでエラー扱いされる。
            member this.isOverride = isOverride
            /// `async` 修飾子の有無。本体内で `await` を使用できる。戻り値注釈は
            /// 「本体が返す内側の型」とみなし、Analyze フェーズで暗黙に Task で包む
            /// （Unit→Task, T→Task<T>）。
            member this.isAsync = isAsync
            member this.span = span
            interface Decl with
                member this.span = span
            interface HasSpan with
                member this.span = span

        /// `impl TypeName as DotNetClass { methods }` — .NET クラスを継承する形式。
        /// `asTypeName` が `Some` のとき `forTypeName` と `byFieldName` は常に `None`（パーサーが保証）。
        type Impl(typeName: string, typeParams: string list, asTypeName: string option, forTypeName: string option, byFieldName: string option, methods: Fn list, span: Span) =
            member this.typeName = typeName
            /// 型パラメータ名のリスト（例: `impl Opt T` では `["T"]`）。非ジェネリックの場合は空リスト。
            member this.typeParams = typeParams
            member this.asTypeName = asTypeName
            member this.forTypeName = forTypeName
            member this.byFieldName = byFieldName
            member this.methods = methods
            member this.span = span
            interface Decl with
                member this.span = span
            interface HasSpan with
                member this.span = span

        /// `role` 宣言の抽象メソッドシグネチャ（ボディなし）。
        type RoleFn(name: string, args: FnArg list, ret: TypeExpr, span: Span) =
            member this.name = name
            member this.args = args
            member this.ret = ret
            member this.span = span

        /// `role TypeName` — インターフェイス相当の役割型宣言。
        /// 内部的に .NET interface として CIL 出力される。
        type Role(name: string, methods: RoleFn list, span: Span) =
            member this.name = name
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
