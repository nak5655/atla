namespace Atla.Compiler.Types

type TypeCray =
    | Unknown
    | Unit
    | Bool
    | Int
    | Float
    | Char
    | String
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
        | Unit, Unit -> Unit
        | Bool, Bool -> Bool
        | Int, Int -> Int
        | Float, Float -> Float
        | Char, Char -> Char
        | String, String -> String
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