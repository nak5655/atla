namespace Atla.Compiler.Hir

open System.Collections.Generic

type Scope(parent: Scope option) =
    let mutable variables : Dictionary<string, Variable> = Dictionary()
    let mutable types : Dictionary<string, Type> = Dictionary()
    let mutable scopes : Dictionary<string, Scope> = Dictionary()

    member this.parent = parent

    member this.SetVar(name: string, variable: Variable) : unit =
        variables.[name] <- variable

    member this.GetVar(name: string) : Variable option =
        let mutable v = Unchecked.defaultof<Variable>
        if variables.TryGetValue(name, &v) then
            Some v
        else
            match parent with
            | Some parentScope -> parentScope.GetVar(name)
            | None -> None

    member this.HasVar(name: string) : bool =
        variables.ContainsKey(name)

    member this.SetType(name: string, typeItem: Type) : unit =
        types.[name] <- typeItem

    member this.GetType(name: string) : Type option =
        let mutable t = Unchecked.defaultof<Type>
        if types.TryGetValue(name, &t) then
            Some t
        else
            match parent with
            | Some parentScope -> parentScope.GetType(name)
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
        globalScope.SetVar("+",
                           { value = Value.Function (function
                                                        | [Value.Int a; Value.Int b] -> Value.Int (a + b)
                                                        | _ -> failwith "Invalid arguments for + operator") ; isMutable = false })
        globalScope