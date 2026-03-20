namespace Atla.Compiler.Hir

open System.Collections.Generic
open Atla.Compiler.Types

type Scope(parent: Scope option) =
    let mutable vars : Dictionary<string, TypeCray> = Dictionary()
    let mutable types : Dictionary<string, TypeCray> = Dictionary()
    let mutable scopes : Dictionary<string, Scope> = Dictionary()

    member this.parent = parent

    member this.DeclareVar(name: string, typ: TypeCray) =
        vars.[name] <- typ

    member this.ResolveVar(name: string) : TypeCray option =
        let mutable v = Unchecked.defaultof<TypeCray>
        if vars.TryGetValue(name, &v) then
            Some v
        else
            match parent with
            | Some parentScope -> parentScope.ResolveVar(name)
            | None -> None

    member this.HasVar(name: string) : bool =
        vars.ContainsKey(name)

    member this.DeclareType(name: string, typeItem: TypeCray) =
        types.[name] <- typeItem

    member this.ResolveType(name: string) : TypeCray option =
        let mutable t = Unchecked.defaultof<TypeCray>
        if types.TryGetValue(name, &t) then
            Some t
        else
            match parent with
            | Some parentScope -> parentScope.ResolveType(name)
            | None -> None

    member this.GetScope(name: string) : Scope option =
        let mutable s = Unchecked.defaultof<Scope>
        if scopes.TryGetValue(name, &s) then
            Some s
        else
            match parent with
            | Some parentEnv -> parentEnv.GetScope(name)
            | None -> None

    static member GlobalScope() : Scope =
        let globalScope = Scope(None)
        globalScope.DeclareType("Bool", TypeCray.System(typeof<bool>))
        globalScope.DeclareType("Int", TypeCray.System(typeof<int>))
        globalScope.DeclareType("Float", TypeCray.System(typeof<float>))
        globalScope.DeclareType("String", TypeCray.System(typeof<string>))
        globalScope.DeclareVar("+", TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)))
        globalScope.DeclareVar("*", TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)))
        globalScope