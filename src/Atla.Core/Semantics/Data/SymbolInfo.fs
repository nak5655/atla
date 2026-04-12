namespace Atla.Core.Semantics.Data

open System.Reflection
open System.Collections.Generic

// 名前解決の結果を表す型
type ExternalBinding =
    | NativeMethodGroup of MethodInfo list
    | ConstructorGroup of ConstructorInfo list
    | SystemTypeRef of System.Type

type SymbolKind =
    | Arg of unit
    | Local of unit
    | BuiltinOperator of Builtins.Operators
    | External of ExternalBinding

    override this.ToString (): string = 
        match this with
        | Arg () -> "Arg"
        | Local fi -> sprintf "Local(%A)" fi
        | BuiltinOperator bm -> sprintf "BuiltinOperator(%A)" bm
        | External ext -> sprintf "External(%A)" ext

type SymbolInfo =
    { name: string
      typ: TypeId
      kind: SymbolKind }

    override this.ToString(): string =
        sprintf "SymbolInfo(name=%s, typ=%A, info=%A)" this.name this.typ this.kind
        
type SymbolTable() =
    let _table = Dictionary<SymbolId, SymbolInfo>()

    let addBuiltinOperator (name: string) (tid: TypeId) (op: Builtins.Operators) : SymbolId =
        let sid = SymbolId(_table.Count)
        _table.Add(sid, { name = name; typ = tid; kind = SymbolKind.BuiltinOperator op })
        sid

    let builtinOperators : (string * SymbolId) list =
        [ ("+", addBuiltinOperator "+" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpAdd)
          ("-", addBuiltinOperator "-" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpSub)
          ("*", addBuiltinOperator "*" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpMul)
          ("/", addBuiltinOperator "/" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpDiv)
          ("%", addBuiltinOperator "%" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpMod)
          ("==", addBuiltinOperator "==" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Bool)) Builtins.Operators.OpEq) ]

    member this.BuiltinOperators = builtinOperators

    member this.NextId(): SymbolId =
        SymbolId(_table.Count)

    member this.Add(sid: SymbolId, info: SymbolInfo): unit =
        _table.Add(sid, info)

    member this.Get(sid: SymbolId): SymbolInfo option =
        match _table.TryGetValue(sid) with
        | true, info -> Some info
        | false, _ -> None
