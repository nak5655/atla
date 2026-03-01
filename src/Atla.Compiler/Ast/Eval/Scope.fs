namespace Atla.Compiler.Ast.Eval

type Scope(parent: Scope option) =
    let mutable variables = Map.empty
    let mutable types = Map.empty
    let mutable scopes = Map.empty

    member this.parent = parent

    member this.SetVar(name: string, value: Value) : unit =
        variables <- variables.Add(name, value)

    member this.GetVar(name: string) : Value option =
        match variables.TryFind(name) with
        | Some value -> Some value
        | None ->
            match parent with
            | Some parentScope -> parentScope.GetVar(name)
            | None -> None

    member this.SetType(name: string, typeItem: Type) : unit =
        types <- types.Add(name, typeItem)

    member this.GetType(name: string) : Type option =
        match types.TryFind(name) with
        | Some typeItem -> Some typeItem
        | None ->
            match parent with
            | Some parentScope -> parentScope.GetType(name)
            | None -> None

    member this.GetScope(name: string) : Scope option =
        match scopes.TryFind(name) with
        | Some moduleEnv -> Some moduleEnv
        | None ->
            match parent with
            | Some parentEnv -> parentEnv.GetScope(name)
            | None -> None
