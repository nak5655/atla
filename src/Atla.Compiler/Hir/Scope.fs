namespace Atla.Compiler.Hir

open System.Collections.Generic
open Atla.Compiler.Types
open Atla.Compiler.Mir

type Scope(parent: Scope option) =
    let mutable args : Dictionary<string, Symbol> = Dictionary()
    let mutable locals : Dictionary<string, Symbol> = Dictionary()
    let mutable methods: Dictionary<string, Symbol> = Dictionary()
    let mutable types : Dictionary<string, TypeCray> = Dictionary()

    member this.parent = parent
    
    member this.DeclareVar(id: string, symbol: Symbol) =
        match symbol with
        | Arg _ -> args.[id] <- symbol
        | Local _ -> locals.[id] <- symbol
        | Method _ -> methods.[id] <- symbol
        | NativeMethod _ -> methods.[id] <- symbol
        | BuildinMethod _ -> methods.[id] <- symbol
        | _ -> failwith "Only Arg, Local, and Method symbols can be declared as variables"

    member this.ResolveVar(name: string) : Symbol option =
        if locals.ContainsKey(name) then
            Some locals.[name]
        else if args.ContainsKey(name) then
            Some args.[name]
        else if methods.ContainsKey(name) then
            Some methods.[name]
        else
            match parent with
            | Some parentScope -> parentScope.ResolveVar(name)
            | None -> None

    member this.HasVar(id: string) : bool =
        this.ResolveVar(id).IsSome

    member this.DeclareType(id: string, typeItem: TypeCray) =
        types.[id] <- typeItem

    member this.ResolveType(id: string) : TypeCray option =
        let mutable t = Unchecked.defaultof<TypeCray>
        if types.TryGetValue(id, &t) then
            Some t
        else
            match parent with
            | Some parentScope -> parentScope.ResolveType(id)
            | None -> None

    static member GlobalScope() : Scope =
        let globalScope = Scope(None)
        globalScope.DeclareType("Bool", TypeCray.System(typeof<bool>))
        globalScope.DeclareType("Int", TypeCray.System(typeof<int>))
        globalScope.DeclareType("Float", TypeCray.System(typeof<float>))
        globalScope.DeclareType("String", TypeCray.System(typeof<string>))
        globalScope.DeclareVar("==", Symbol.BuildinMethod(
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<bool>)),
            fun (dest, lhs, rhs) -> [Mir.TAC(dest, lhs, Mir.OpCode.Eq, rhs)]
            ))
        globalScope.DeclareVar("+", Symbol.BuildinMethod(
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)),
            fun (dest, lhs, rhs) -> [Mir.TAC(dest, lhs, Mir.OpCode.Add, rhs)]
            ))
        globalScope.DeclareVar("-", Symbol.BuildinMethod(
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)),
            fun (dest, lhs, rhs) -> [Mir.TAC(dest, lhs, Mir.OpCode.Sub, rhs)]
            ))
        globalScope.DeclareVar("*", Symbol.BuildinMethod(
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)),
            fun (dest, lhs, rhs) -> [Mir.TAC(dest, lhs, Mir.OpCode.Mul, rhs)]
            ))
        globalScope.DeclareVar("/", Symbol.BuildinMethod(
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)),
            fun (dest, lhs, rhs) -> [Mir.TAC(dest, lhs, Mir.OpCode.Div, rhs)]
            ))
        globalScope