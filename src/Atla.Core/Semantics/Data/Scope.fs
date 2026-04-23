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

    member this.ResolveVar(name: string) : SymbolId list =
        let mutable sid = Unchecked.defaultof<SymbolId>

        if _vars.TryGetValue(name, &sid) then
            [ sid ]
        else
            match parent with
            | Some parentScope -> parentScope.ResolveVar(name)
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

    /// スコープ内で参照可能な全変数を (name, SymbolId) のリストとして返す。
    /// 親スコープより内側のシンボルが優先される（同名は内側で上書き）。
    member this.allVisibleVars() : (string * SymbolId) list =
        let parentVars =
            match parent with
            | Some p -> p.allVisibleVars()
            | None -> []
        let localVars = _vars |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toList
        let localNames = localVars |> List.map fst |> Set.ofList
        let filteredParentVars = parentVars |> List.filter (fun (name, _) -> not (localNames.Contains name))
        localVars @ filteredParentVars
