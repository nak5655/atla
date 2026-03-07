namespace Atla.Compiler.Hir

open Atla.Compiler.Types

module Hir =
    // Expression HIR as a discriminated union
    type Expr =
        | Unit of span: Span
        | Int of value: int * span: Span
        | Float of value: float * span: Span
        | String of value: string * span: Span
        | Id of name: string * span: Span
        | Apply of func: Expr * args: Expr list * span: Span
        | MemberAccess of receiver: Expr * memberName: string * span: Span
        | Block of stmts: Stmt list * expr: Expr * span: Span
        | If of cond: Expr * thenBranch: Expr * elseBranch: Expr option * span: Span
        | Error of message: string * span: Span

    // Statements
    and Stmt =
        | Let of name: string * isMutable: bool * value: Expr * span: Span
        | Assign of name: string * value: Expr * span: Span
        | ExprStmt of expr: Expr * span: Span
        | ErrorStmt of message: string * span: Span

    // Type expressions
    type TypeExpr =
        | IdType of name: string * span: Span

    // Data items
    type DataItem =
        | Field of name: string * typeExpr: TypeExpr * span: Span

    type FnArg =
        | Unit of span: Span
        | Named of name: string * typeExpr: TypeExpr * span: Span

    // Declarations
    type Decl =
        | Import of path: string list * span: Span
        | Data of name: string * items: DataItem list * span: Span
        | Fn of name: string * args: FnArg list * body: Expr * span: Span
        | DeclError of message: string * span: Span

    type Module(decls: Decl list) =
        member this.decls = decls
