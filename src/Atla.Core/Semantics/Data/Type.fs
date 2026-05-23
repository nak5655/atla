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
    /// 単精度浮動小数点（System.Single / .NET float32）。ユーザー言語の型名は `Float`、リテラルは `1.0f`。
    | Float
    /// 倍精度浮動小数点（System.Double / .NET float）。ユーザー言語の型名は `Double`、リテラルは `1.0`。
    | Double
    | String
    | App of head: TypeId * args: TypeId list
    | Name of sid: SymbolId
    | Fn of args: TypeId list * ret: TypeId
    | Meta of TypeMeta
    | Native of System.Type
    | Error of message: string
    /// ジェネリック型定義のスコープ内で使われる型パラメータ（例: `enum Opt T` の `T`）。
    /// この TypeId は型検査・HIR/MIR を経て Gen.fs で GenericTypeParameterBuilder へ解決される。
    | TypeVar of name: string
    /// 可変長引数関数型: a1 -> ... -> an -> b... -> r
    /// fixedArgs: 先頭の固定引数型リスト, elemType: 可変長部分の要素型, ret: 戻り値型
    | VarargFn of fixedArgs: TypeId list * elemType: TypeId * ret: TypeId
    /// マネージドポインタ（CIL の `T&`）。state machine の `ref this.<>u__N` や
    /// `ref stateMachine` 等、address-of 値・ref パラメータの型として用いる。
    /// ユーザー言語からは直接生成されず、AsyncRewrite 等の lowering で導入する。
    | ByRef of inner: TypeId

module TypeId =
    /// `async fn` の戻り値型を暗黙に Task で包む。`Unit -> Task`, `T -> Task<T>`。
    /// `App(Native typeof<Task>, [t])` は `Task t` をユーザーが書いたときと構造的に同一で、
    /// tryUnwrapTaskType / AsyncRewrite.tryClassifyTaskType・codegen がそのまま処理できる。
    let wrapInTask (inner: TypeId) : TypeId =
        match inner with
        | Unit -> Native typeof<System.Threading.Tasks.Task>
        | t -> App(Native typeof<System.Threading.Tasks.Task>, [ t ])

    let rec fromSystemType (t: System.Type) : TypeId =
        if t = typeof<unit> then Unit
        elif t = typeof<System.Void> then Native typeof<System.Void>
        elif t = typeof<bool> then Bool
        elif t = typeof<int> then Int
        elif t = typeof<float> then Double
        elif t = typeof<float32> then Float
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
        | Double -> Some typeof<float>
        | Float -> Some typeof<float32>
        | String -> Some typeof<string>
        | App (Native t, [ elem ]) when t = typeof<System.Array> ->
            tryToRuntimeSystemType elem
            |> Option.map (fun elementType -> elementType.MakeArrayType())
        | App _ -> None
        | Native t -> Some t
        // TypeId.Fn は対応するデリゲート型（Func<>/Action<>）へ変換する。
        | Fn (args, ret) -> tryFnToDelegateSystemType args ret
        | VarargFn _ -> None
        // `ByRef T` は CIL の `T&`。CIL シグネチャに直接載せられる。
        | ByRef inner ->
            tryToRuntimeSystemType inner
            |> Option.map (fun t -> t.MakeByRefType())
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
        | VarargFn(fixedArgs, e, r) ->
            (fixedArgs |> List.exists (occurs subst m)) || occurs subst m e || occurs subst m r
        | ByRef inner -> occurs subst m inner
        | _ -> false

    let rec canUnify (subst: TypeSubst) (tid1: TypeId) (tid2: TypeId) : bool =
        match tid1, tid2 with
        | Unit, Unit -> true
        | Unit, Native t
        | Native t, Unit when t = typeof<System.Void> -> true
        | Bool, Bool -> true
        | Int, Int -> true
        | Float, Float -> true
        | Double, Double -> true
        | String, String -> true
        | App (leftHead, leftArgs), App (rightHead, rightArgs) ->
            List.length leftArgs = List.length rightArgs
            && canUnify subst leftHead rightHead
            && (List.zip leftArgs rightArgs |> List.forall (fun (leftArg, rightArg) -> canUnify subst leftArg rightArg))
        // .NET の継承・インターフェース実装を考慮したサブタイプ互換チェック。
        // t2.IsAssignableFrom(t1) は t1 = t2 の場合も true を返すため、等値チェックを包含する。
        | Native t1, Native t2 -> t2.IsAssignableFrom(t1)
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
        // 同名の型パラメータは互換とみなす。
        | TypeVar n1, TypeVar n2 -> n1 = n2
        // ByRef は内側の型が互換であれば互換。
        | ByRef a, ByRef b -> canUnify subst a b
        | VarargFn(fixed1, e1, r1), VarargFn(fixed2, e2, r2)
            when List.length fixed1 = List.length fixed2 ->
            (List.zip fixed1 fixed2 |> List.forall (fun (a, b) -> canUnify subst a b))
            && canUnify subst e1 e2
            && canUnify subst r1 r2
        | VarargFn(fixedArgs, elemType, ret), Fn(allArgs, retType)
            when allArgs.Length >= fixedArgs.Length ->
            let leadingArgs, variadicArgs = List.splitAt fixedArgs.Length allArgs
            (List.zip fixedArgs leadingArgs |> List.forall (fun (a, b) -> canUnify subst a b))
            && (variadicArgs |> List.forall (canUnify subst elemType))
            && canUnify subst ret retType
        | Fn _, VarargFn _ -> canUnify subst tid2 tid1
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
        | VarargFn(fixedArgs, e, r) ->
            VarargFn(fixedArgs |> List.map (resolve subst), resolve subst e, resolve subst r)
        | ByRef inner -> ByRef (resolve subst inner)
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
        | Double, Double -> Result.Ok Double
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
        // 同名の型パラメータは同一型として単一化する。
        | TypeVar n1, TypeVar n2 when n1 = n2 -> Result.Ok (TypeVar n1)
        | ByRef a, ByRef b ->
            unify subst a b |> Result.map ByRef
        | VarargFn(fixed1, e1, r1), VarargFn(fixed2, e2, r2)
            when List.length fixed1 = List.length fixed2 ->
            let fixedResults = List.zip fixed1 fixed2 |> List.map (fun (a, b) -> unify subst a b)
            let firstFixedErr = fixedResults |> List.tryPick (function Result.Error e -> Some e | _ -> None)
            match firstFixedErr with
            | Some err -> Result.Error err
            | None ->
                match unify subst e1 e2 with
                | Result.Error err -> Result.Error err
                | Result.Ok _ ->
                    unify subst r1 r2 |> Result.map (fun _ -> VarargFn(fixed1, e1, r1))
        | VarargFn(fixedArgs, elemType, ret), Fn(allArgs, retType)
            when allArgs.Length >= fixedArgs.Length ->
            let leadingArgs, variadicArgs = List.splitAt fixedArgs.Length allArgs
            let fixedResults = List.zip fixedArgs leadingArgs |> List.map (fun (a, b) -> unify subst a b)
            let firstFixedErr = fixedResults |> List.tryPick (function Result.Error e -> Some e | _ -> None)
            match firstFixedErr with
            | Some err -> Result.Error err
            | None ->
                let variadicResults = variadicArgs |> List.map (fun arg -> unify subst arg elemType)
                let firstVarErr = variadicResults |> List.tryPick (function Result.Error e -> Some e | _ -> None)
                match firstVarErr with
                | Some err -> Result.Error err
                | None ->
                    unify subst ret retType |> Result.map (fun _ -> VarargFn(fixedArgs, elemType, ret))
        | Fn _, VarargFn _ ->
            match unify subst tid2 tid1 with
            | Result.Ok _ -> Result.Ok tid1
            | Result.Error err -> Result.Error err
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
        | VarargFn(fixedArgs, e, r) ->
            (fixedArgs |> List.exists (hasError subst)) || hasError subst e || hasError subst r
        | ByRef inner -> hasError subst inner
        | Meta m ->
            match subst.TryGetValue(m) with
            | true, t' -> hasError subst t'
            | false, _ -> false
        | _ -> false
