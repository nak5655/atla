namespace Atla.Compiler.Semantics

open System.Reflection
open Atla.Compiler.Syntax.Data
open Atla.Compiler.Semantics.Data

module Resolve =
    type ResolvedModule =
        { moduleName: string
          moduleScope: Scope
          fnDecls: Ast.Decl.Fn list }

    let private tryResolveSystemType (classPath: string) : System.Type option =
        match System.Type.GetType(classPath) with
        | null ->
            System.AppDomain.CurrentDomain.GetAssemblies()
            |> Seq.tryPick (fun asm ->
                match asm.GetType(classPath, false) with
                | null -> None
                | t -> Some t)
        | t -> Some t

    let private declareSystemType (symbolTable: SymbolTable) (scope: Scope) (classPath: string) : SymbolId =
        let sid = symbolTable.NextId()
        let name = Array.last (classPath.Split('.'))
        let resolvedType = tryResolveSystemType classPath |> Option.toObj
        let kind = SymbolKind.External(ExternalBinding.SystemTypeRef resolvedType)
        let symInfo = { name = name; typ = TypeId.Name sid; kind = kind }
        symbolTable.Add(sid, symInfo)
        scope.DeclareType(name, TypeId.Name sid)
        sid

    let private resolveImport (symbolTable: SymbolTable) (scope: Scope) (importDecl: Ast.Decl.Import) : unit =
        let classPath = String.concat "." importDecl.path
        // TODO: 今はSystem.Typeのみをサポートしているが、将来的にはユーザー定義型やモジュールもサポートする必要がある
        declareSystemType symbolTable scope classPath |> ignore

    let resolveModule (symbolTable: SymbolTable, moduleName: string, moduleAst: Ast.Module) : ResolvedModule =
        let moduleScope = Scope(None)
        moduleScope.DeclareType("Unit", TypeId.Unit)
        moduleScope.DeclareType("Bool", TypeId.Bool)
        moduleScope.DeclareType("Int", TypeId.Int)
        moduleScope.DeclareType("Float", TypeId.Float)
        moduleScope.DeclareType("String", TypeId.String)

        symbolTable.BuiltinOperators
        |> List.iter (fun (name, sid) -> moduleScope.DeclareVar(name, sid))

        let fnDecls = ResizeArray<Ast.Decl.Fn>()

        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Import as importDecl ->
                resolveImport symbolTable moduleScope importDecl
            | :? Ast.Decl.Fn as fnDecl ->
                fnDecls.Add(fnDecl)
            | _ -> failwith "Unsupported declaration type in module"

        { moduleName = moduleName
          moduleScope = moduleScope
          fnDecls = Seq.toList fnDecls }
