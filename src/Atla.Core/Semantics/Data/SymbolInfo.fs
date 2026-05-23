namespace Atla.Core.Semantics.Data

open System.Reflection
open System.Collections.Generic

// 名前解決の結果を表す型
type ExternalBinding =
    | NativeMethodGroup of MethodInfo list
    | ConstructorGroup of ConstructorInfo list
    | SystemTypeRef of System.Type
    | SystemFieldRef of FieldInfo

type SymbolKind =
    | Arg of unit
    | Local of unit
    | BuiltinOperator of Builtins.Operators
    | BuiltinFn of Builtins.BuiltinFunctions
    | External of ExternalBinding

    override this.ToString (): string =
        match this with
        | Arg () -> "Arg"
        | Local fi -> sprintf "Local(%A)" fi
        | BuiltinOperator bm -> sprintf "BuiltinOperator(%A)" bm
        | BuiltinFn fn -> sprintf "BuiltinFn(%A)" fn
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

    let addNativeMethodOp (name: string) (tid: TypeId) (methods: System.Reflection.MethodInfo list) : SymbolId =
        let sid = SymbolId(_table.Count)
        _table.Add(sid, { name = name; typ = tid; kind = SymbolKind.External(ExternalBinding.NativeMethodGroup methods) })
        sid

    let addBuiltinFn (name: string) (fn: Builtins.BuiltinFunctions) (typ: TypeId) : SymbolId =
        let sid = SymbolId(_table.Count)
        _table.Add(sid, { name = name; typ = typ; kind = SymbolKind.BuiltinFn fn })
        sid

    let builtinFunctions : (string * SymbolId) list =
        [ ("array",
           addBuiltinFn "array" Builtins.BuiltinFunctions.Array
               (TypeId.VarargFn(
                   [],
                   TypeId.TypeVar "T",
                   TypeId.App(TypeId.Native typeof<System.Array>, [ TypeId.TypeVar "T" ]))))
          // 空の List<T> を構築する組込関数。0 引数呼び出し（`List.`）で、
          // 戻り型 List<T> の T を呼び出し文脈の期待型から単一化で確定させる。
          ("List",
           addBuiltinFn "List" Builtins.BuiltinFunctions.List
               (TypeId.Fn(
                   [],
                   TypeId.App(TypeId.Native typedefof<System.Collections.Generic.List<_>>, [ TypeId.TypeVar "T" ]))))
          // 数値型変換組込関数。同名で複数のソース型を宣言し、引数型でオーバーロード解決する。
          ("toFloat", addBuiltinFn "toFloat" Builtins.BuiltinFunctions.ToFloat (TypeId.Fn([ TypeId.Double ], TypeId.Float)))
          ("toFloat", addBuiltinFn "toFloat" Builtins.BuiltinFunctions.ToFloat (TypeId.Fn([ TypeId.Int ], TypeId.Float)))
          ("toDouble", addBuiltinFn "toDouble" Builtins.BuiltinFunctions.ToDouble (TypeId.Fn([ TypeId.Float ], TypeId.Double)))
          ("toDouble", addBuiltinFn "toDouble" Builtins.BuiltinFunctions.ToDouble (TypeId.Fn([ TypeId.Int ], TypeId.Double)))
          ("toInt", addBuiltinFn "toInt" Builtins.BuiltinFunctions.ToInt (TypeId.Fn([ TypeId.Double ], TypeId.Int)))
          ("toInt", addBuiltinFn "toInt" Builtins.BuiltinFunctions.ToInt (TypeId.Fn([ TypeId.Float ], TypeId.Int))) ]

    let builtinOperators : (string * SymbolId) list =
        let strEq = typeof<string>.GetMethod("op_Equality", [| typeof<string>; typeof<string> |])
        let strNe = typeof<string>.GetMethod("op_Inequality", [| typeof<string>; typeof<string> |])
        let strConcat = typeof<string>.GetMethod("Concat", [| typeof<string>; typeof<string> |])
        [ ("+", addBuiltinOperator "+" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpAdd)
          ("+", addBuiltinOperator "+" (TypeId.Fn([ TypeId.Double; TypeId.Double ], TypeId.Double)) Builtins.Operators.OpAdd)
          ("+", addBuiltinOperator "+" (TypeId.Fn([ TypeId.Float; TypeId.Float ], TypeId.Float)) Builtins.Operators.OpAdd)
          ("-", addBuiltinOperator "-" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpSub)
          ("-", addBuiltinOperator "-" (TypeId.Fn([ TypeId.Double; TypeId.Double ], TypeId.Double)) Builtins.Operators.OpSub)
          ("-", addBuiltinOperator "-" (TypeId.Fn([ TypeId.Float; TypeId.Float ], TypeId.Float)) Builtins.Operators.OpSub)
          ("-", addBuiltinOperator "-" (TypeId.Fn([ TypeId.Int ], TypeId.Int)) Builtins.Operators.OpNeg)
          ("-", addBuiltinOperator "-" (TypeId.Fn([ TypeId.Double ], TypeId.Double)) Builtins.Operators.OpNeg)
          ("-", addBuiltinOperator "-" (TypeId.Fn([ TypeId.Float ], TypeId.Float)) Builtins.Operators.OpNeg)
          ("*", addBuiltinOperator "*" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpMul)
          ("*", addBuiltinOperator "*" (TypeId.Fn([ TypeId.Double; TypeId.Double ], TypeId.Double)) Builtins.Operators.OpMul)
          ("*", addBuiltinOperator "*" (TypeId.Fn([ TypeId.Float; TypeId.Float ], TypeId.Float)) Builtins.Operators.OpMul)
          ("/", addBuiltinOperator "/" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpDiv)
          ("/", addBuiltinOperator "/" (TypeId.Fn([ TypeId.Double; TypeId.Double ], TypeId.Double)) Builtins.Operators.OpDiv)
          ("/", addBuiltinOperator "/" (TypeId.Fn([ TypeId.Float; TypeId.Float ], TypeId.Float)) Builtins.Operators.OpDiv)
          ("%", addBuiltinOperator "%" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)) Builtins.Operators.OpMod)
          ("==", addBuiltinOperator "==" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Bool)) Builtins.Operators.OpEq)
          ("==", addBuiltinOperator "==" (TypeId.Fn([ TypeId.Double; TypeId.Double ], TypeId.Bool)) Builtins.Operators.OpEq)
          ("==", addBuiltinOperator "==" (TypeId.Fn([ TypeId.Float; TypeId.Float ], TypeId.Bool)) Builtins.Operators.OpEq)
          ("==", addNativeMethodOp "==" (TypeId.Fn([ TypeId.String; TypeId.String ], TypeId.Bool)) (if isNull strEq then [] else [ strEq ]))
          ("!=", addBuiltinOperator "!=" (TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Bool)) Builtins.Operators.OpNe)
          ("!=", addBuiltinOperator "!=" (TypeId.Fn([ TypeId.Double; TypeId.Double ], TypeId.Bool)) Builtins.Operators.OpNe)
          ("!=", addBuiltinOperator "!=" (TypeId.Fn([ TypeId.Float; TypeId.Float ], TypeId.Bool)) Builtins.Operators.OpNe)
          ("!=", addNativeMethodOp "!=" (TypeId.Fn([ TypeId.String; TypeId.String ], TypeId.Bool)) (if isNull strNe then [] else [ strNe ]))
          ("+", addNativeMethodOp "+" (TypeId.Fn([ TypeId.String; TypeId.String ], TypeId.String)) (if isNull strConcat then [] else [ strConcat ]))
          ("&&", addBuiltinOperator "&&" (TypeId.Fn([ TypeId.Bool; TypeId.Bool ], TypeId.Bool)) Builtins.Operators.OpAnd)
          ("||", addBuiltinOperator "||" (TypeId.Fn([ TypeId.Bool; TypeId.Bool ], TypeId.Bool)) Builtins.Operators.OpOr) ]

    member this.BuiltinOperators = builtinOperators
    member this.BuiltinFunctions = builtinFunctions

    member this.NextId(): SymbolId =
        SymbolId(_table.Count)

    member this.Add(sid: SymbolId, info: SymbolInfo): unit =
        _table.Add(sid, info)

    member this.Get(sid: SymbolId): SymbolInfo option =
        match _table.TryGetValue(sid) with
        | true, info -> Some info
        | false, _ -> None
