namespace Atla.Compiler.Semantics.Data

open System.Collections.Generic

type TypeMeta = TypeMeta of int

module TypeMeta =
    let mutable counter = 0
    let fresh () =
        let id = counter
        counter <- counter + 1
        TypeMeta id

type TypeId =
    | Unit
    | Bool
    | Int
    | Float
    | String
    | Data of id: string
    | System of classPath: string
    | Arrow of arg: TypeId * ret: TypeId
    | Meta of TypeMeta
    | Error of message: string

module TypeId =
    let freshMeta () = Meta (TypeMeta.fresh())

type TypeSubst = Dictionary<TypeMeta, TypeId>

module Type =
    let rec occurs (subst: TypeSubst) (m: TypeMeta) (t: TypeId) : bool =
        match t with
        | Meta m2 when m = m2 -> true
        | Meta m2 ->
            match subst.TryGetValue(m2) with
            | true, t' -> occurs subst m t'
            | false, _ -> false
        | Arrow (a, b) -> occurs subst m a || occurs subst m b
        | _ -> false


    let rec canUnify (subst: TypeSubst) (a: TypeId) (b: TypeId) : bool =
        match a, b with
        | Unit, Unit -> true
        | Bool, Bool -> true
        | Int, Int -> true
        | Float, Float -> true
        | String, String -> true
        | Data id1, Data id2 when id1 = id2 -> true
        | System p1, System p2 when p1 = p2 -> true
        | Arrow (arg1, ret1), Arrow (arg2, ret2) ->
            (canUnify subst arg1 arg2) && (canUnify subst ret1 ret2)
        | Meta a, b -> 
            match subst.TryGetValue(a) with
            | true, a' -> canUnify subst a' b
            | false, _ -> true // まだ型変数が具体的な型に置き換えられていない場合は、どの型とも単一化可能とする
        | a, Meta b -> canUnify subst (Meta b) a
        | _ -> false

    // 単一化
    let rec unify (subst: TypeSubst) (a: TypeId) (b: TypeId) : TypeId =
        match a, b with
        | Unit, Unit -> Unit
        | Bool, Bool -> Bool
        | Int, Int -> Int
        | Float, Float -> Float
        | String, String -> String
        | Data id1, Data id2 when id1 = id2 -> Data id1
        | System p1, System p2 when p1 = p2 -> System p1
        | Arrow (arg1, ret1), Arrow (arg2, ret2) ->
            let arg = unify subst arg1 arg2
            let ret = unify subst ret1 ret2
            Arrow (arg, ret)
        | Meta m, t
        | t, Meta m ->
            if occurs subst m t then
                failwith "Occurs check failed"
            subst.[m] <- t
            t

        | _ -> failwith "Cannot unify types"

    let rec hasError (subst: TypeSubst) (t: TypeId) : bool =
        match t with
        | Error _ -> true
        | Arrow (arg, ret) -> hasError subst arg || hasError subst ret
        | Meta m ->
            match subst.TryGetValue(m) with
            | true, t' -> hasError subst t'
            | false, _ -> false
        | _ -> false
