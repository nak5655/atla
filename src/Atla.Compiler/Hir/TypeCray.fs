namespace Atla.Compiler.Hir

type TypeCray =
    | Unknown
    | System of System.Type
    | Function of args: TypeCray list * ret: TypeCray
    | Var of TypeCray ref
    | Error of message: string

    // 経路圧縮
    member this.Compress() : TypeCray =
        match this with
        | Var v ->
            let compressed = v.Value.Compress()
            v.Value <- compressed
            compressed
        | _ -> this

    // 単一化
    member this.Unify(other: TypeCray) : TypeCray =
        match this, other with
        | Unknown, t | t, Unknown -> t
        | System t1, System t2 when t1 = t2 -> System t1
        | Function (args1, ret1), Function (args2, ret2) ->
            if List.length args1 = List.length args2 then
                let unifiedArgs = List.map2 (fun (a: TypeCray) b -> a.Unify(b)) args1 args2
                let unifiedRet = ret1.Unify(ret2)
                Function (unifiedArgs, unifiedRet)
            else
                Error "Function argument count mismatch"
        | Var v1, Var v2 when System.Object.ReferenceEquals(v1, v2) -> (Var v1).Compress()
        | Var v, t | t, Var v ->
            // v は 'TypeCray ref' なので中身を書き換えられる
            v.Value <- t
            t.Compress()
        | _ -> Error "Type mismatch"

    member this.ToSystemType() : System.Type =
        match this with
        | Unknown -> failwith "Cannot convert unknown type to System.Type"
        | System t -> t
        | Function (args, ret) ->
            let argTypes = args |> List.map (fun t -> t.ToSystemType()) |> List.toArray
            let retType = ret.ToSystemType()
            System.Linq.Expressions.Expression.GetFuncType(Array.append argTypes [|retType|])
        | Var t -> t.Value.ToSystemType()
        | Error msg -> failwithf "Cannot convert error type to System.Type: %s" msg

    member this.HasError() : bool =
        match this with
        | Error _ -> true
        | Var v -> v.Value.HasError()
        | Function (args, ret) -> List.exists (fun (t: TypeCray) -> t.HasError()) args || ret.HasError()
        | _ -> false
