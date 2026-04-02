namespace Atla.Compiler.Semantics.Data

open System.Reflection
open System.Collections.Generic

module SymbolKind =
    type BuiltinMethod =
        | OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpEq

// 名前解決の結果を表す型
type SymbolKind =
    | Arg of unit
    | Local of unit
    | Method of MethodInfo
    | Constructor of ConstructorInfo
    | BuiltinMethod of SymbolKind.BuiltinMethod
    | SystemType of System.Type

    override this.ToString (): string = 
        match this with
        | Arg () -> "Arg"
        | Local fi -> sprintf "Local(%A)" fi
        | Method mi -> sprintf "Method(%A)" mi
        | Constructor ci -> sprintf "Constructor(%A)" ci
        | BuiltinMethod bm -> sprintf "BuiltinMethod(%A)" bm

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
        sprintf "SymbolInfo(name=%s, typ=%A, info=%A)" name typ info

type SymbolId(id: int) = 
    member this.id = id

type SymbolTable() =
    let _table = Dictionary<SymbolId, SymbolInfo>()

    member this.Add(info: SymbolInfo): SymbolId =
        let symbolId = SymbolId(_table.Count)
        _table.Add(symbolId, info)
        symbolId

    member this.Get(id: SymbolId): SymbolInfo =
        _table.[id]

    member this.TryGetValue(id: SymbolId): SymbolInfo option =
        match _table.TryGetValue(id) with
        | true, info -> Some info
        | false, _ -> None