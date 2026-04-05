namespace Atla.Compiler.Semantics.Data

open System.Reflection
open System.Collections.Generic

// 名前解決の結果を表す型
type SymbolKind =
    | Arg of unit
    | Local of unit
    | NativeMethod of MethodInfo list
    | Constructor of ConstructorInfo list
    | BuiltinOperator of Builtins.Operators
    | SystemType of System.Type

    override this.ToString (): string = 
        match this with
        | Arg () -> "Arg"
        | Local fi -> sprintf "Local(%A)" fi
        | NativeMethod mi -> sprintf "Method(%A)" mi
        | Constructor ci -> sprintf "Constructor(%A)" ci
        | BuiltinOperator bm -> sprintf "BuiltinOperator(%A)" bm

type SymbolInfo(name: string, typ: TypeId, kind: SymbolKind) =
    let mutable _typ = typ
    let mutable _kind = kind

    member this.name = name
    member this.typ
        with get() = _typ
        and set(v) = _typ <- v
    member this.kind
        with get() = _kind
        and set(v) = _kind <- v
        
    override this.ToString(): string =
        sprintf "SymbolInfo(name=%s, typ=%A, info=%A)" name typ kind
        
type SymbolTable() =
    let _table = Dictionary<SymbolId, SymbolInfo>()

    member this.NextId(): SymbolId =
        SymbolId(_table.Count)

    member this.Add(sid: SymbolId, info: SymbolInfo): unit =
        _table.Add(sid, info)

    member this.Get(sid: SymbolId): SymbolInfo option =
        match _table.TryGetValue(sid) with
        | true, info -> Some info
        | false, _ -> None