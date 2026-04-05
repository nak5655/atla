namespace Atla.Compiler.Semantics.Data

open System.Collections.Generic

// 名前解決用のスコープを表すクラス
type Scope(parent: Scope option) =
    let mutable _vars : Dictionary<string, SymbolId> = Dictionary()
    let mutable _types : Dictionary<string, SymbolId> = Dictionary()

    member this.parent = parent
    member this.vars = _vars
    member this.types = _types
    
    member this.DeclareVar(name: string, symbol: SymbolId) =
        _vars.Add(name, symbol)

    member this.ResolveVar(name: string, typ: TypeId) : SymbolId list =
        let localSyms = List.filter (fun (sym: SymbolId) -> sym.name = name && sym.typ.CanUnify(typ)) (List.ofSeq _locals)
        
        if localSyms.Length > 0 then
            localSyms
        else
            let argSyms = List.filter (fun (sym: Symbol) -> sym.name = name && sym.typ.CanUnify(typ)) (List.ofSeq _args)
            if argSyms.Length > 0 then
                argSyms
            else
                match parent with
                | Some parentScope -> parentScope.ResolveVar(name, typ)
                | None -> []

    member this.DeclareType(name: string, sym: SymbolId) =
        _types.Add(name, sym)

    member this.ResolveType(id: string) : TypeId option =
        let mutable t = Unchecked.defaultof<TypeId>
        if types.TryGetValue(id, &t) then
            Some t
        else
            match parent with
            | Some parentScope -> parentScope.ResolveType(id)
            | None -> None

    static member GlobalScope() : Scope =
        let globalScope = Scope(None)
        globalScope.DeclareType("Bool", TypeId.System(typeof<bool>))
        globalScope.DeclareType("Int", TypeId.System(typeof<int>))
        globalScope.DeclareType("Float", TypeId.System(typeof<float>))
        globalScope.DeclareType("String", TypeId.System(typeof<string>))

        globalScope.DeclareVar(Name.NativeMethod("==",
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<bool>)),
            fun (dest, xs) -> [Mir.TAC(dest, xs.[0], Mir.OpCode.Eq, xs.[1])]
            ))
        globalScope.DeclareVar(Name.NativeMethod("+",
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)),
            fun (dest, xs) -> [Mir.TAC(dest, xs.[0], Mir.OpCode.Add, xs.[1])]
            ))
        globalScope.DeclareVar(Name.NativeMethod("-",
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)),
            fun (dest, xs) -> [Mir.TAC(dest, xs.[0], Mir.OpCode.Sub, xs.[1])]
            ))
        globalScope.DeclareVar(Name.NativeMethod("*",
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)),
            fun (dest, xs) -> [Mir.TAC(dest, xs.[0], Mir.OpCode.Mul, xs.[1])]
            ))
        globalScope.DeclareVar(Name.NativeMethod("/",
            TypeCray.Function([TypeCray.System(typeof<int>); TypeCray.System(typeof<int>)], TypeCray.System(typeof<int>)),
            fun (dest, xs) -> [Mir.TAC(dest, xs.[0], Mir.OpCode.Div, xs.[1])]
            ))
        globalScope