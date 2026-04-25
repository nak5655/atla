namespace Atla.Core.Semantics

open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open System.Runtime.Loader

module Resolve =
    type ResolvedDataDecl =
        { typeSid: SymbolId
          decl: Ast.Decl.Data }

    type ResolvedModule =
        { moduleName: string
          moduleScope: Scope
          fnDecls: Ast.Decl.Fn list
          dataDecls: ResolvedDataDecl list }

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

    // import 宣言を処理し、型・変数スコープへ登録する。
    // 対象の .NET 型がロード済みアセンブリ内に見つからない場合は Warning 診断を返す。
    let private resolveImport (symbolTable: SymbolTable) (scope: Scope) (importDecl: Ast.Decl.Import) : Diagnostic list =
        let classPath = String.concat "." importDecl.path
        let shortName = Array.last (classPath.Split('.'))
        // TODO: 今はSystem.Typeのみをサポートしているが、将来的にはユーザー定義型やモジュールもサポートする必要がある
        match scope.ResolveType(shortName) with
        | Some _ -> []
        | None ->
            let sid = declareSystemType symbolTable scope classPath

            // declareSystemType が型を解決できたか確認する。
            // 解決できなかった場合（sysType が null）は依存関係の設定不備を示す Warning を返す。
            match symbolTable.Get(sid) with
            | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) } when isNull sysType ->
                [ Diagnostic.Warning(
                      sprintf
                          "import '%s': type could not be resolved from loaded assemblies. Ensure the dependency providing this type is listed in atla.yaml and has been restored."
                          classPath,
                      importDecl.span) ]
            | _ -> []

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
        let dataDecls = ResizeArray<ResolvedDataDecl>()
        let diagnostics = ResizeArray<Diagnostic>()

        // data 型名を先に登録し、同一モジュール内で相互参照可能にする。
        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Data as dataDecl ->
                match moduleScope.ResolveType(dataDecl.name) with
                | Some _ ->
                    diagnostics.Add(Diagnostic.Error(sprintf "Type '%s' is already defined" dataDecl.name, dataDecl.span))
                | None ->
                    let typeSid = symbolTable.NextId()
                    symbolTable.Add(typeSid, { name = dataDecl.name; typ = TypeId.Name typeSid; kind = SymbolKind.Local() })
                    moduleScope.DeclareType(dataDecl.name, TypeId.Name typeSid)
                    dataDecls.Add({ typeSid = typeSid; decl = dataDecl })
            | _ -> ()

        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Import as importDecl ->
                let importDiagnostics = resolveImport symbolTable moduleScope importDecl
                diagnostics.AddRange(importDiagnostics)
            | :? Ast.Decl.Data ->
                ()
            | :? Ast.Decl.Fn as fnDecl ->
                fnDecls.Add(fnDecl)
            | _ -> diagnostics.Add(Diagnostic.Error("Unsupported declaration type in module", decl.span))

        // Warning 診断があっても解析は続行する。Error 診断がある場合のみ失敗とする。
        let allDiagnostics = Seq.toList diagnostics
        if allDiagnostics |> List.exists (fun d -> d.isError) then
            PhaseResult.failed allDiagnostics
        else
            PhaseResult.succeeded
                { moduleName = moduleName
                  moduleScope = moduleScope
                  fnDecls = Seq.toList fnDecls
                  dataDecls = Seq.toList dataDecls }
                allDiagnostics
