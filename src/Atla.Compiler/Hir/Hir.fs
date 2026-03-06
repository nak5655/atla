namespace Atla.Compiler.Hir

open Atla.Compiler.Types

module Hir =
    // Expression HIR as a discriminated union
    type Expr =
        | Unit of Span
        | Int of int * Span
        | Float of float * Span
        | String of string * Span
        | Id of string * Span
        | Apply of Expr * Expr list * Span
        | Block of Stmt list * Expr * Span
        | If of Expr * Expr * Expr option * Span
        | Error of string * Span

    // Statements
    and Stmt =
        | Let of string * bool * Expr * Span
        | Assign of string * Expr * Span
        | ExprStmt of Expr * Span
        | ErrorStmt of string * Span

    // Type expressions
    type TypeExpr =
        | IdType of string * Span

    // Data items
    type DataItem =
        | Field of string * TypeExpr * Span

    // Declarations
    type Decl =
        | Import of string list * Span
        | Data of string * DataItem list * Span
        | DeclError of string * Span

    type Module(decls: Decl list, stmts: Stmt list) =
        member this.decls = decls
        member this.stmts = stmts
