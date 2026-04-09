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
    | Name of sid: SymbolId // TODO: App of SymbolId * TypeId list // 型コンストラクタ適用
    | Fn of args: TypeId list * ret: TypeId
    | Meta of TypeMeta
    | Native of System.Type
    | Error of message: string

module TypeId =
    let freshMeta () = Meta (TypeMeta.fresh())
    
    let fromSystemType (t: System.Type) : TypeId =
        if t = typeof<unit> then Unit
        elif t = typeof<bool> then Bool
        elif t = typeof<int> then Int
        elif t = typeof<float> then Float
        elif t = typeof<string> then String
        else Native t

type TypeSubst = Dictionary<TypeMeta, TypeId>

module Type =
    let rec occurs (subst: TypeSubst) (m: TypeMeta) (tid: TypeId) : bool =
        match tid with
        | Meta m2 when m = m2 -> true
        | Meta m2 ->
            match subst.TryGetValue(m2) with
            | true, t' -> occurs subst m t'
            | false, _ -> false
        | Fn (args, ret) -> List.exists (occurs subst m) args || occurs subst m ret
        | _ -> false

    let rec canUnify (subst: TypeSubst) (tid1: TypeId) (tid2: TypeId) : bool =
        match tid1, tid2 with
        | Unit, Unit -> true
        | Bool, Bool -> true
        | Int, Int -> true
        | Float, Float -> true
        | String, String -> true
        | Name id1, Name id2 when id1 = id2 -> true
        | Fn (args1, ret1), Fn (args2, ret2) ->
            if List.length args1 <> List.length args2 then
                false
             else
                (List.zip args1 args2) |> List.forall (fun (a1, a2) -> canUnify subst a1 a2) && (canUnify subst ret1 ret2)
        | Meta m, tid ->
            match subst.TryGetValue(m) with
            | true, resolvedTid -> canUnify subst resolvedTid tid
            | false, _ -> true // まだ型変数が具体的な型に置き換えられていない場合は、どの型とも単一化可能とする
        | tid, Meta m -> canUnify subst (Meta m) tid
        | _ -> false

    let rec resolve (subst: TypeSubst) (tid: TypeId) : TypeId =
        match tid with
        | Meta m ->
            match subst.TryGetValue(m) with
            | true, t' -> resolve subst t'
            | false, _ -> tid
        | Fn (args, ret) -> Fn (List.map (resolve subst) args, resolve subst ret)
        | _ -> tid

    // 単一化
    let rec unify (subst: TypeSubst) (tid1: TypeId) (tid2: TypeId) : TypeId =
        match tid1, tid2 with
        | Unit, Unit -> Unit
        | Bool, Bool -> Bool
        | Int, Int -> Int
        | Float, Float -> Float
        | String, String -> String
        | Name id1, Name id2 when id1 = id2 -> Name id1
        | Fn (args1, ret1), Fn (args2, ret2) ->
            if List.length args1 <> List.length args2 then
                failwith "Cannot unify function types with different number of arguments"
            else
                let args = List.zip args1 args2 |> List.map (fun (a1, a2) -> unify subst a1 a2)
                let ret = unify subst ret1 ret2
                Fn (args, ret)
        | Meta m, tid
        | tid, Meta m ->
            let resolvedTid = resolve subst tid
            if occurs subst m resolvedTid then
                failwith "Occurs check failed"
            subst.[m] <- resolvedTid
            resolvedTid

        | _ -> failwith "Cannot unify types"

    let rec hasError (subst: TypeSubst) (tid: TypeId) : bool =
        match tid with
        | Error _ -> true
        | Fn (args, ret) -> List.exists (hasError subst) args || hasError subst ret
        | Meta m ->
            match subst.TryGetValue(m) with
            | true, t' -> hasError subst t'
            | false, _ -> false
        | _ -> false
