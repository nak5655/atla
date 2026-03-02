namespace Atla.Compiler.Ast.Eval

type Scope(parent: Scope option) =
    let mutable variables = Map.empty
    let mutable types = Map.empty
    let mutable scopes = Map.empty

    member this.parent = parent

    member this.SetVar(name: string, variable: Variable) : unit =
        variables <- variables.Add(name, variable)

    member this.GetVar(name: string) : Variable option =
        match variables.TryFind(name) with
        | Some variable -> Some variable
        | None ->
            match parent with
            | Some parentScope -> parentScope.GetVar(name)
            | None -> None

    member this.HasVar(name: string) : bool =
        variables.ContainsKey(name)

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
