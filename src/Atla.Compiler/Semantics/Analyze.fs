namespace Atla.Compiler.Semantics

open Atla.Compiler.Syntax.Data
open Atla.Compiler.Semantics.Data

module Analyze =
    let analyzeModule (symbolTable: SymbolTable, typeSubst: TypeSubst, moduleName: string, moduleAst: Ast.Module) : Result<Hir.Module, Error list> =
        let resolvedModule = Resolve.resolveModule (symbolTable, moduleName, moduleAst)
        Infer.inferModule (symbolTable, typeSubst, resolvedModule)
