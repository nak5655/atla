namespace Atla.Compiler.Semantics.Data

open System.Collections.Generic

// 名前解決用のスコープを表すクラス
type Scope(parent: Scope option) =
    let _vars : Dictionary<string, SymbolId> = Dictionary()
    let _types : Dictionary<string, TypeId> = Dictionary()

    member this.parent = parent
    member this.vars = _vars
    member this.types = _types

    member this.DeclareVar(name: string, symbol: SymbolId) =
        _vars.Add(name, symbol)

    member this.ResolveVar(name: string, _typ: TypeId) : SymbolId list =
        let mutable sym = Unchecked.defaultof<SymbolId>

        if _vars.TryGetValue(name, &sym) then
            [ sym ]
        else
            match parent with
            | Some parentScope -> parentScope.ResolveVar(name, _typ)
            | None -> []

    member this.DeclareType(name: string, typ: TypeId) =
        _types.Add(name, typ)

    member this.ResolveType(id: string) : TypeId option =
        let mutable typ = Unchecked.defaultof<TypeId>

        if _types.TryGetValue(id, &typ) then
            Some typ
        else
            match parent with
            | Some parentScope -> parentScope.ResolveType(id)
            | None -> None
