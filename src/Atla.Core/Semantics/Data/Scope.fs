namespace Atla.Core.Semantics.Data

open System.Collections.Generic

// 名前解決用のスコープを表すクラス
type Scope(parent: Scope option) =
    let _vars : Dictionary<string, SymbolId> = Dictionary()
    let _types : Dictionary<string, TypeId> = Dictionary()

    member this.parent = parent
    member this.vars = _vars
    member this.types = _types

    member this.DeclareVar(name: string, sid: SymbolId) =
        _vars.Add(name, sid)

    member this.ResolveVar(name: string, _tid: TypeId) : SymbolId list =
        let mutable sid = Unchecked.defaultof<SymbolId>

        if _vars.TryGetValue(name, &sid) then
            [ sid ]
        else
            match parent with
            | Some parentScope -> parentScope.ResolveVar(name, _tid)
            | None -> []

    member this.DeclareType(name: string, tid: TypeId) =
        _types.Add(name, tid)

    member this.ResolveType(id: string) : TypeId option =
        let mutable tid = Unchecked.defaultof<TypeId>

        if _types.TryGetValue(id, &tid) then
            Some tid
        else
            match parent with
            | Some parentScope -> parentScope.ResolveType(id)
            | None -> None
