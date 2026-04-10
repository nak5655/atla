namespace Atla.Compiler.Semantics.Data

open System.Collections.Generic

type TypeMeta = TypeMeta of int

type TypeMetaFactory(start: int) =
    let mutable nextId = start

    new () = TypeMetaFactory(0)

    member this.Fresh() =
        let id = nextId
        nextId <- nextId + 1
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
    let fromSystemType (t: System.Type) : TypeId =
        if t = typeof<unit> then Unit
        elif t = typeof<bool> then Bool
        elif t = typeof<int> then Int
        elif t = typeof<float> then Float
        elif t = typeof<string> then String
        else Native t

    let tryToRuntimeSystemType (tid: TypeId) : System.Type option =
        match tid with
        | Unit -> Some typeof<unit>
        | Bool -> Some typeof<bool>
        | Int -> Some typeof<int>
        | Float -> Some typeof<float>
        | String -> Some typeof<string>
        | Native t -> Some t
        | _ -> None

    let tryResolveToSystemType (resolveName: SymbolId -> System.Type option) (tid: TypeId) : System.Type option =
        match tid with
        | Name sid -> resolveName sid
        | _ -> tryToRuntimeSystemType tid

type TypeSubst = Dictionary<TypeMeta, TypeId>

type UnifyError =
    | DifferentFunctionArity of leftArity: int * rightArity: int
    | OccursCheckFailed of meta: TypeMeta * actual: TypeId
    | CannotUnify of left: TypeId * right: TypeId

module UnifyError =
    let toMessage (err: UnifyError) : string =
        match err with
        | DifferentFunctionArity (leftArity, rightArity) ->
            sprintf "Cannot unify function types with different number of arguments: %d vs %d" leftArity rightArity
        | OccursCheckFailed (meta, actual) ->
            sprintf "Occurs check failed: %A occurs in %A" meta actual
        | CannotUnify (left, right) ->
            sprintf "Cannot unify types: %A and %A" left right

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
    let rec unify (subst: TypeSubst) (tid1: TypeId) (tid2: TypeId) : Result<TypeId, UnifyError> =
        match tid1, tid2 with
        | Unit, Unit -> Result.Ok Unit
        | Bool, Bool -> Result.Ok Bool
        | Int, Int -> Result.Ok Int
        | Float, Float -> Result.Ok Float
        | String, String -> Result.Ok String
        | Name id1, Name id2 when id1 = id2 -> Result.Ok(Name id1)
        | Fn (args1, ret1), Fn (args2, ret2) ->
            if List.length args1 <> List.length args2 then
                Result.Error(DifferentFunctionArity(List.length args1, List.length args2))
            else
                let zippedArgs = List.zip args1 args2
                let rec unifyArgs pairs acc =
                    match pairs with
                    | [] -> Result.Ok(List.rev acc)
                    | (a1, a2) :: rest ->
                        match unify subst a1 a2 with
                        | Result.Ok arg -> unifyArgs rest (arg :: acc)
                        | Result.Error err -> Result.Error err

                match unifyArgs zippedArgs [] with
                | Result.Error err -> Result.Error err
                | Result.Ok args ->
                    match unify subst ret1 ret2 with
                    | Result.Ok ret -> Result.Ok(Fn(args, ret))
                    | Result.Error err -> Result.Error err
        | Meta m, tid
        | tid, Meta m ->
            let resolvedTid = resolve subst tid
            if occurs subst m resolvedTid then
                Result.Error(OccursCheckFailed(m, resolvedTid))
            else
                subst.[m] <- resolvedTid
                Result.Ok resolvedTid

        | _ -> Result.Error(CannotUnify(tid1, tid2))

    let rec hasError (subst: TypeSubst) (tid: TypeId) : bool =
        match tid with
        | Error _ -> true
        | Fn (args, ret) -> List.exists (hasError subst) args || hasError subst ret
        | Meta m ->
            match subst.TryGetValue(m) with
            | true, t' -> hasError subst t'
            | false, _ -> false
        | _ -> false
