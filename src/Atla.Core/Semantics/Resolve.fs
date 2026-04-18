namespace Atla.Core.Semantics

open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open System.Runtime.Loader

module Resolve =
    type ResolvedModule =
        { moduleName: string
          moduleScope: Scope
          fnDecls: Ast.Decl.Fn list }

    let private tryResolveSystemType (classPath: string) : System.Type option =
        match System.Type.GetType(classPath) with
        | null ->
            let candidateAssemblies =
                seq {
                    yield! System.AppDomain.CurrentDomain.GetAssemblies()

                    for context in AssemblyLoadContext.All do
                        yield! context.Assemblies
                }
                |> Seq.distinct

            candidateAssemblies
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

        // import した .NET 型は値コンテキストで ctor 呼び出しできるよう、同名の変数シンボルとして ctor グループも公開する。
        if not (obj.ReferenceEquals(resolvedType, null)) then
            let ctorInfos =
                resolvedType.GetConstructors(System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Instance)
                |> Array.toList

            if not ctorInfos.IsEmpty then
                let ctorSid = symbolTable.NextId()
                let ctorType = TypeId.Fn([], TypeId.fromSystemType resolvedType)
                let ctorKind = SymbolKind.External(ExternalBinding.ConstructorGroup ctorInfos)
                symbolTable.Add(ctorSid, { name = name; typ = ctorType; kind = ctorKind })
                scope.DeclareVar(name, ctorSid)

        sid

    let private resolveImport (symbolTable: SymbolTable) (scope: Scope) (importDecl: Ast.Decl.Import) : unit =
        let classPath = String.concat "." importDecl.path
        let shortName = Array.last (classPath.Split('.'))
        // TODO: 今はSystem.Typeのみをサポートしているが、将来的にはユーザー定義型やモジュールもサポートする必要がある
        match scope.ResolveType(shortName) with
        | Some _ -> ()
        | None -> declareSystemType symbolTable scope classPath |> ignore

    let resolveModule (symbolTable: SymbolTable, moduleName: string, moduleAst: Ast.Module) : PhaseResult<ResolvedModule> =
        let moduleScope = Scope(None)
        moduleScope.DeclareType("Unit", TypeId.Unit)
        moduleScope.DeclareType("Bool", TypeId.Bool)
        moduleScope.DeclareType("Int", TypeId.Int)
        moduleScope.DeclareType("Float", TypeId.Float)
        moduleScope.DeclareType("String", TypeId.String)
        declareSystemType symbolTable moduleScope "System.Int32" |> ignore

        symbolTable.BuiltinOperators
        |> List.iter (fun (name, sid) -> moduleScope.DeclareVar(name, sid))

        let fnDecls = ResizeArray<Ast.Decl.Fn>()
        let diagnostics = ResizeArray<Diagnostic>()

        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Import as importDecl ->
                resolveImport symbolTable moduleScope importDecl
            | :? Ast.Decl.Fn as fnDecl ->
                fnDecls.Add(fnDecl)
            | _ -> diagnostics.Add(Diagnostic.Error("Unsupported declaration type in module", decl.span))

        if diagnostics.Count > 0 then
            PhaseResult.failed (Seq.toList diagnostics)
        else
            PhaseResult.succeeded
                { moduleName = moduleName
                  moduleScope = moduleScope
                  fnDecls = Seq.toList fnDecls }
                []
