namespace Atla.Core.Semantics.Data

open System.Collections.Generic

// 名前解決用のスコープを表すクラス
type Scope(parent: Scope option) =
    let _vars : Dictionary<string, SymbolId> = Dictionary()
    let _overloadedVars : Dictionary<string, ResizeArray<SymbolId>> = Dictionary()
    let _types : Dictionary<string, TypeId> = Dictionary()

    member this.parent = parent
    member this.vars = _vars
    member this.types = _types

    /// 同一スコープ内の同名変数に対し、候補を追加登録する。
    /// 先に登録された候補を優先順として保持し、ResolveVar で順序を維持して返す。
    member this.DeclareVar(name: string, sid: SymbolId) =
        let mutable existingSid = Unchecked.defaultof<SymbolId>
        if _vars.TryGetValue(name, &existingSid) then
            let mutable existingOverloads = Unchecked.defaultof<ResizeArray<SymbolId>>
            if _overloadedVars.TryGetValue(name, &existingOverloads) then
                existingOverloads.Add(sid)
            else
                _overloadedVars.Add(name, ResizeArray([ existingSid; sid ]))
        else
            _vars.Add(name, sid)

    /// 変数名から可視候補を解決する。
    /// 現在スコープに同名候補が存在する場合はそれのみを返し、親スコープへはフォールバックしない。
    member this.ResolveVar(name: string) : SymbolId list =
        let mutable sid = Unchecked.defaultof<SymbolId>
        if _vars.TryGetValue(name, &sid) then
            let mutable overloads = Unchecked.defaultof<ResizeArray<SymbolId>>
            if _overloadedVars.TryGetValue(name, &overloads) then
                overloads |> Seq.toList
            else
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
    /// 内側のスコープが外側の同名シンボルに優先される（各 name は最大1エントリのみ含まれる）。
    member this.allVisibleVars() : (string * SymbolId) list =
        let parentVars =
            match parent with
            | Some p -> p.allVisibleVars()
            | None -> []
        let localVars =
            _vars
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> Seq.toList
        let localNames = localVars |> List.map fst |> Set.ofList
        let filteredParentVars = parentVars |> List.filter (fun (name, _) -> not (localNames.Contains name))
        localVars @ filteredParentVars
