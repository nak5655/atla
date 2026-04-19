namespace Atla.Core.Semantics.Data

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
    | App of head: TypeId * args: TypeId list
    | Name of sid: SymbolId
    | Fn of args: TypeId list * ret: TypeId
    | Meta of TypeMeta
    | Native of System.Type
    | Error of message: string

module TypeId =
    let rec fromSystemType (t: System.Type) : TypeId =
        if t = typeof<unit> then Unit
        elif t = typeof<System.Void> then Native typeof<System.Void>
        elif t = typeof<bool> then Bool
        elif t = typeof<int> then Int
        elif t = typeof<float> then Float
        elif t = typeof<string> then String
        elif t.IsArray then
            if t.GetArrayRank() = 1 then
                let elemType = t.GetElementType()
                if obj.ReferenceEquals(elemType, null) then Native t else App(Native typeof<System.Array>, [ fromSystemType elemType ])
            else
                Native t
        else Native t

    // デリゲート型かどうかを判定する。
    let isDelegateType (t: System.Type) : bool =
        not (obj.ReferenceEquals(t, null))
        && typeof<System.Delegate>.IsAssignableFrom(t)
        && t <> typeof<System.Delegate>
        && t <> typeof<System.MulticastDelegate>

    // デリゲート型の Invoke メソッドを取得する。
    let tryGetDelegateInvoke (t: System.Type) : System.Reflection.MethodInfo option =
        if isDelegateType t then t.GetMethod("Invoke") |> Option.ofObj
        else None

    // TypeId.Fn を .NET デリゲート型へ変換する。
    // Fn([A, B], Unit) → Action<A, B>
    // Fn([A], R) → Func<A, R>
    let rec tryFnToDelegateSystemType (args: TypeId list) (ret: TypeId) : System.Type option =
        let resolvedArgs = args |> List.map tryToRuntimeSystemType
        let resolvedRet = tryToRuntimeSystemType ret
        if resolvedArgs |> List.exists Option.isNone then None
        else
            let argTypes = resolvedArgs |> List.choose id |> List.toArray
            match resolvedRet with
            | None -> None
            | Some retType ->
                let isVoid = retType = typeof<unit> || retType = typeof<System.Void>
                if isVoid then
                    match argTypes.Length with
                    | 0 -> Some typeof<System.Action>
                    | 1 -> Some (typedefof<System.Action<_>>.MakeGenericType(argTypes))
                    | 2 -> Some (typedefof<System.Action<_, _>>.MakeGenericType(argTypes))
                    | 3 -> Some (typedefof<System.Action<_, _, _>>.MakeGenericType(argTypes))
                    | _ -> None
                else
                    let funcArgTypes = Array.append argTypes [| retType |]
                    match argTypes.Length with
                    | 0 -> Some (typedefof<System.Func<_>>.MakeGenericType(funcArgTypes))
                    | 1 -> Some (typedefof<System.Func<_, _>>.MakeGenericType(funcArgTypes))
                    | 2 -> Some (typedefof<System.Func<_, _, _>>.MakeGenericType(funcArgTypes))
                    | 3 -> Some (typedefof<System.Func<_, _, _, _>>.MakeGenericType(funcArgTypes))
                    | _ -> None

    and tryToRuntimeSystemType (tid: TypeId) : System.Type option =
        match tid with
        | Unit -> Some typeof<unit>
        | Bool -> Some typeof<bool>
        | Int -> Some typeof<int>
        | Float -> Some typeof<float>
        | String -> Some typeof<string>
        | App (Native t, [ elem ]) when t = typeof<System.Array> ->
            tryToRuntimeSystemType elem
            |> Option.map (fun elementType -> elementType.MakeArrayType())
        | App _ -> None
        | Native t -> Some t
        // TypeId.Fn は対応するデリゲート型（Func<>/Action<>）へ変換する。
        | Fn (args, ret) -> tryFnToDelegateSystemType args ret
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


module Type =
    let rec occurs (subst: TypeSubst) (m: TypeMeta) (tid: TypeId) : bool =
        match tid with
        | Meta m2 when m = m2 -> true
        | Meta m2 ->
            match subst.TryGetValue(m2) with
            | true, t' -> occurs subst m t'
            | false, _ -> false
        | App (head, args) -> occurs subst m head || (args |> List.exists (occurs subst m))
        | Fn (args, ret) -> List.exists (occurs subst m) args || occurs subst m ret
        | _ -> false

    let rec canUnify (subst: TypeSubst) (tid1: TypeId) (tid2: TypeId) : bool =
        match tid1, tid2 with
        | Unit, Unit -> true
        | Unit, Native t
        | Native t, Unit when t = typeof<System.Void> -> true
        | Bool, Bool -> true
        | Int, Int -> true
        | Float, Float -> true
        | String, String -> true
        | App (leftHead, leftArgs), App (rightHead, rightArgs) ->
            List.length leftArgs = List.length rightArgs
            && canUnify subst leftHead rightHead
            && (List.zip leftArgs rightArgs |> List.forall (fun (leftArg, rightArg) -> canUnify subst leftArg rightArg))
        | Native t1, Native t2 when t1 = t2 -> true
        | Name id1, Name id2 when id1 = id2 -> true
        // Name（インポート型名）と Native（リフレクション由来の型）は互換とみなす。
        | Name _, Native _
        | Native _, Name _ -> true
        | Fn (args1, ret1), Fn (args2, ret2) ->
            if List.length args1 <> List.length args2 then
                false
             else
                (List.zip args1 args2) |> List.forall (fun (a1, a2) -> canUnify subst a1 a2) && (canUnify subst ret1 ret2)
        // TypeId.Fn と .NET デリゲート型（Native）の互換チェック。
        | Fn (fnArgs, fnRet), Native t when TypeId.isDelegateType t ->
            match TypeId.tryGetDelegateInvoke t with
            | None -> false
            | Some invoke ->
                let dArgs = invoke.GetParameters() |> Array.toList |> List.map (fun p -> TypeId.fromSystemType p.ParameterType)
                let dRet = TypeId.fromSystemType invoke.ReturnType
                List.length fnArgs = List.length dArgs
                && (List.zip fnArgs dArgs |> List.forall (fun (a, da) -> canUnify subst a da))
                && canUnify subst fnRet dRet
        | Native t, Fn _ when TypeId.isDelegateType t -> canUnify subst tid2 tid1
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
        | App (head, args) -> App (resolve subst head, args |> List.map (resolve subst))
        | Fn (args, ret) -> Fn (List.map (resolve subst) args, resolve subst ret)
        | _ -> tid

    // 単一化
    let rec unify (subst: TypeSubst) (tid1: TypeId) (tid2: TypeId) : Result<TypeId, UnifyError> =
        match tid1, tid2 with
        | Unit, Unit -> Result.Ok Unit
        | Unit, Native t
        | Native t, Unit when t = typeof<System.Void> -> Result.Ok Unit
        | Bool, Bool -> Result.Ok Bool
        | Int, Int -> Result.Ok Int
        | Float, Float -> Result.Ok Float
        | String, String -> Result.Ok String
        | App (leftHead, leftArgs), App (rightHead, rightArgs) ->
            if List.length leftArgs <> List.length rightArgs then
                Result.Error(CannotUnify(tid1, tid2))
            else
                match unify subst leftHead rightHead with
                | Result.Error err -> Result.Error err
                | Result.Ok unifiedHead ->
                    let zippedArgs = List.zip leftArgs rightArgs
                    let rec unifyAppArgs pairs acc =
                        match pairs with
                        | [] -> Result.Ok(List.rev acc)
                        | (leftArg, rightArg) :: rest ->
                            match unify subst leftArg rightArg with
                            | Result.Ok unifiedArg -> unifyAppArgs rest (unifiedArg :: acc)
                            | Result.Error err -> Result.Error err

                    match unifyAppArgs zippedArgs [] with
                    | Result.Ok unifiedArgs -> Result.Ok(App(unifiedHead, unifiedArgs))
                    | Result.Error err -> Result.Error err
        | Native t1, Native t2 when t1 = t2 -> Result.Ok (Native t1)
        | Name id1, Name id2 when id1 = id2 -> Result.Ok(Name id1)
        // Name（インポート型名）と Native（リフレクション由来の型）は
        // どちらも .NET 型を表すため相互に互換とみなして Native を採用する。
        // Name は import 宣言で登録されたシンボル参照であり、
        // Native は System.Reflection から取得した System.Type を直接保持する。
        | Name _, Native t -> Result.Ok (Native t)
        | Native t, Name _ -> Result.Ok (Native t)
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
        // TypeId.Fn と .NET デリゲート型（Native）の単一化。
        // 関数型とデリゲート型は互換とみなし、引数・戻り値を逐一単一化したうえで Native を採用する。
        | Fn (fnArgs, fnRet), Native t when TypeId.isDelegateType t ->
            match TypeId.tryGetDelegateInvoke t with
            | None -> Result.Error(CannotUnify(tid1, tid2))
            | Some invoke ->
                let dArgs = invoke.GetParameters() |> Array.toList |> List.map (fun p -> TypeId.fromSystemType p.ParameterType)
                let dRet = TypeId.fromSystemType invoke.ReturnType
                if List.length fnArgs <> List.length dArgs then
                    Result.Error(DifferentFunctionArity(List.length fnArgs, List.length dArgs))
                else
                    let argUnifyResults = List.zip fnArgs dArgs |> List.map (fun (a, da) -> unify subst a da)
                    let retUnifyResult = unify subst fnRet dRet
                    match argUnifyResults |> List.tryFind Result.isError, retUnifyResult with
                    | None, Result.Ok _ -> Result.Ok (Native t)
                    | None, Result.Error e -> Result.Error e
                    | Some (Result.Error e), _ -> Result.Error e
                    | Some (Result.Ok _), _ -> Result.Error(CannotUnify(tid1, tid2))
        | Native t, Fn _ when TypeId.isDelegateType t ->
            // 対称性のため引数を入れ替えて再帰する。
            match unify subst tid2 tid1 with
            | Result.Ok _ -> Result.Ok (Native t)
            | Result.Error e -> Result.Error e
        | Meta m, tid
        | tid, Meta m ->
            let resolvedTid = resolve subst tid
            if resolvedTid = Meta m then
                Result.Ok resolvedTid
            elif occurs subst m resolvedTid then
                Result.Error(OccursCheckFailed(m, resolvedTid))
            else
                subst.[m] <- resolvedTid
                Result.Ok resolvedTid

        | _ -> Result.Error(CannotUnify(tid1, tid2))

    let rec hasError (subst: TypeSubst) (tid: TypeId) : bool =
        match tid with
        | Error _ -> true
        | App (head, args) -> hasError subst head || (args |> List.exists (hasError subst))
        | Fn (args, ret) -> List.exists (hasError subst) args || hasError subst ret
        | Meta m ->
            match subst.TryGetValue(m) with
            | true, t' -> hasError subst t'
            | false, _ -> false
        | _ -> false
