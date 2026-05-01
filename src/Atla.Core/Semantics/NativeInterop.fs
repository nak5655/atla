namespace Atla.Core.Semantics

open System.Reflection
open System.Collections.Generic
open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open Atla.Core.Semantics.Data.AnalyzeEnv

module NativeInterop =
    let private extensionAttributeType = typeof<System.Runtime.CompilerServices.ExtensionAttribute>

    // TypeId から実行時 System.Type を解決する。Name 参照はシンボル表を経由して辿る。
    let resolveRuntimeSystemType (nameEnv: NameEnv) (typeEnv: TypeEnv) (tid: TypeId) : System.Type option =
        let resolveNameType (sid: SymbolId) : System.Type option =
            match nameEnv.resolveSym sid with
            | Some symInfo ->
                match symInfo.kind with
                | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) -> Some sysType
                | _ -> None
            | None -> None

        typeEnv.resolveType tid
        |> TypeId.tryResolveToSystemType resolveNameType

    /// 受け手型の public instance メンバー候補を、実装型 + 実装インターフェースから決定的順序で収集する。
    /// 明示的インターフェース実装により実装型側で直接見えないメンバー（例: ICollection<T>.Add）も候補化する。
    let getPublicInstanceMembersIncludingInterfaces (systemType: System.Type) : MemberInfo list =
        let directMembers =
            systemType.GetMembers(BindingFlags.Public ||| BindingFlags.Instance)
            |> Array.toList

        let interfaceMembers =
            systemType.GetInterfaces()
            |> Array.sortBy (fun iface -> iface.FullName)
            |> Array.toList
            |> List.collect (fun iface ->
                iface.GetMembers(BindingFlags.Public ||| BindingFlags.Instance)
                |> Array.toList)

        let allMembers = directMembers @ interfaceMembers
        allMembers
        |> List.distinctBy (fun memberInfo ->
            let declaringTypeName =
                if obj.ReferenceEquals(memberInfo.DeclaringType, null) then
                    ""
                else
                    memberInfo.DeclaringType.AssemblyQualifiedName

            // メソッドはオーバーロードを区別するため、パラメータ型列もキーに含める。
            // フィールド・プロパティはオーバーロードがないので名前だけで識別できる。
            let paramKey =
                match memberInfo with
                | :? MethodInfo as mi ->
                    mi.GetParameters()
                    |> Array.map (fun p -> p.ParameterType.AssemblyQualifiedName)
                    |> String.concat ","
                | _ -> ""

            memberInfo.MemberType, memberInfo.Name, declaringTypeName, paramKey)

    /// 複数のプロパティ・フィールド候補が残った場合に最も派生したクラス型の宣言を優先して絞り込む。
    /// インターフェース由来のメンバーはクラス由来より低優先で扱う。これにより、クラスとそれが実装する
    /// インターフェースの両方に同名プロパティが存在するケース（例: ContentControl.Content と
    /// IContentControl.Content）で誤って "Ambiguous member" が報告されるのを防ぐ。
    let private narrowNonMethodCandidates (results: (MemberInfo * TypeId) list) : (MemberInfo * TypeId) list =
        // 全員がメソッドなら対象外（メソッドオーバーロードの解決は別ロジックで扱う）。
        let allMethods = results |> List.forall (fun (mi, _) -> mi :? MethodInfo)
        if allMethods || results.Length <= 1 then
            results
        else
            // クラス（非インターフェース）由来のメンバーを優先する。
            let classMembers =
                results
                |> List.filter (fun (mi, _) ->
                    not (obj.ReferenceEquals(mi.DeclaringType, null)) && not mi.DeclaringType.IsInterface)
            let candidates = if classMembers.IsEmpty then results else classMembers
            if candidates.Length <= 1 then
                candidates
            else
                // クラスメンバー間では宣言型が最も派生したものを選ぶ。
                // m が「最も派生」= 他のすべての候補の宣言型が m の宣言型の祖先（または同一）。
                let mostDerived =
                    candidates
                    |> List.filter (fun (mi, _) ->
                        candidates
                        |> List.forall (fun (other, _) ->
                            obj.ReferenceEquals(mi, other)
                            || obj.ReferenceEquals(mi.DeclaringType, null)
                            || obj.ReferenceEquals(other.DeclaringType, null)
                            || other.DeclaringType.IsAssignableFrom(mi.DeclaringType)))
                match mostDerived with
                | [ _ ] -> mostDerived
                | _ -> candidates

    let resolveNativeMember (typeEnv: TypeEnv) (memberInfos: MemberInfo list) (tid: TypeId) : (MemberInfo * TypeId) list =
        let resolvedExpectedType = typeEnv.resolveType tid
        let isUnconstrainedExpectedType =
            match resolvedExpectedType with
            | TypeId.Meta _ -> true
            | _ -> false

        let exactResult = List<MemberInfo * TypeId>()
        let optionalResult = List<MemberInfo * TypeId>()
        for memberInfo in memberInfos do
            match memberInfo with
            | :? MethodInfo as methodInfo ->
                let parameterTypes = [ for p in methodInfo.GetParameters() -> TypeId.fromSystemType p.ParameterType ]
                let returnType = TypeId.fromSystemType methodInfo.ReturnType
                let methodType = TypeId.Fn(parameterTypes, returnType)
                match resolvedExpectedType with
                | TypeId.Fn(expectedArgs, expectedRet) ->
                    let parameters = methodInfo.GetParameters() |> Array.toList
                    let requiredParamCount = parameters |> List.filter (fun p -> not p.IsOptional) |> List.length
                    if
                        expectedArgs.Length >= requiredParamCount
                        && expectedArgs.Length <= parameters.Length
                        && List.forall2 (fun expectedArg (parameterInfo: ParameterInfo) -> typeEnv.canUnify expectedArg (TypeId.fromSystemType parameterInfo.ParameterType)) expectedArgs (parameters |> List.take expectedArgs.Length)
                        && typeEnv.canUnify expectedRet returnType then
                        if expectedArgs.Length = parameters.Length then
                            exactResult.Add(memberInfo, TypeId.Fn(expectedArgs, returnType))
                        else
                            optionalResult.Add(memberInfo, TypeId.Fn(expectedArgs, returnType))
                | _ ->
                    if isUnconstrainedExpectedType || typeEnv.canUnify methodType resolvedExpectedType then
                        exactResult.Add(memberInfo, methodType)
            | :? FieldInfo as fieldInfo ->
                let fieldType = TypeId.fromSystemType fieldInfo.FieldType
                if isUnconstrainedExpectedType || typeEnv.canUnify fieldType resolvedExpectedType then
                    exactResult.Add(memberInfo, fieldType)
            | :? PropertyInfo as propertyInfo ->
                let propertyType = TypeId.fromSystemType propertyInfo.PropertyType
                if isUnconstrainedExpectedType || typeEnv.canUnify propertyType resolvedExpectedType then
                    exactResult.Add(memberInfo, propertyType)
            | _ -> ()
        // プロパティ・フィールドで複数候補が残った場合はクラス優先・最派生型優先で絞り込む。
        if exactResult.Count > 0 then
            narrowNonMethodCandidates (Seq.toList exactResult)
        else
            narrowNonMethodCandidates (Seq.toList optionalResult)

    type EnumeratorMembers =
        { iteratorType: System.Type
          moveNext: MethodInfo
          current: PropertyInfo }

    let tryResolveEnumeratorMembers (iterableSystemType: System.Type) : EnumeratorMembers option =
        let allTypes = iterableSystemType :: (iterableSystemType.GetInterfaces() |> Array.toList)
        let moveNextMethod =
            allTypes
            |> List.tryPick (fun t ->
                t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
                |> Array.tryFind (fun methodInfo -> methodInfo.Name = "MoveNext" && methodInfo.GetParameters().Length = 0))
        let currentProperty =
            allTypes
            |> List.collect (fun t ->
                t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
                |> Array.filter (fun propertyInfo -> propertyInfo.Name = "Current")
                |> Array.toList)
            |> List.tryFind (fun propertyInfo -> propertyInfo.PropertyType <> typeof<obj>)
            |> Option.orElseWith (fun () ->
                allTypes
                |> List.tryPick (fun t ->
                    t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
                    |> Array.tryFind (fun propertyInfo -> propertyInfo.Name = "Current")))

        match moveNextMethod, currentProperty with
        | Some moveNext, Some current ->
            Some { iteratorType = iterableSystemType; moveNext = moveNext; current = current }
        | _ -> None

    let tryGetEnumerator (iterableSystemType: System.Type) : MethodInfo option =
        (iterableSystemType :: (iterableSystemType.GetInterfaces() |> Array.toList))
        |> List.tryPick (fun t ->
            t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
            |> Array.tryFind (fun methodInfo -> methodInfo.Name = "GetEnumerator" && methodInfo.GetParameters().Length = 0))

    let tryDefaultArgExpr (parameterInfo: ParameterInfo) (span: Atla.Core.Data.Span) : Hir.Expr option =
        if not parameterInfo.IsOptional then
            None
        elif not parameterInfo.HasDefaultValue then
            None
        else
            let parameterType = parameterInfo.ParameterType
            let defaultValue = parameterInfo.DefaultValue
            if parameterType.IsEnum then
                Some(Hir.Expr.Int(System.Convert.ToInt32(defaultValue), span))
            elif parameterType = typeof<int> then
                Some(Hir.Expr.Int(unbox<int> defaultValue, span))
            elif parameterType = typeof<bool> then
                Some(Hir.Expr.Int((if unbox<bool> defaultValue then 1 else 0), span))
            elif parameterType = typeof<float> then
                Some(Hir.Expr.Float(unbox<float> defaultValue, span))
            elif parameterType = typeof<string> then
                Some(Hir.Expr.String((if obj.ReferenceEquals(defaultValue, null) then "" else unbox<string> defaultValue), span))
            elif not parameterType.IsValueType && obj.ReferenceEquals(defaultValue, null) then
                // null デフォルト値を持つ参照型パラメータ（Action などのデリゲート型が該当）は
                // HIR の Null リテラルとして表現し、CIL 生成時に ldnull を発行する。
                Some(Hir.Expr.Null(TypeId.fromSystemType parameterType, span))
            else
                None

    let tryResolveIndexerMethod (receiverType: System.Type) : MethodInfo option =
        let tryResolveSingleDimArrayIndexer () =
            if receiverType.IsArray && receiverType.GetArrayRank() = 1 then
                receiverType.GetMethod("GetValue", BindingFlags.Public ||| BindingFlags.Instance, null, [| typeof<int> |], null)
                |> Option.ofObj
            else
                None

        let allTypes = receiverType :: (receiverType.GetInterfaces() |> Array.toList)
        let candidates =
            allTypes
            |> List.collect (fun t ->
                t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
                |> Array.filter (fun methodInfo ->
                    let ps = methodInfo.GetParameters()
                    ps.Length = 1 && ps.[0].ParameterType = typeof<int>)
                |> Array.toList)
        let pickByName name =
            candidates |> List.tryFind (fun methodInfo -> methodInfo.Name = name)

        tryResolveSingleDimArrayIndexer ()
        |> Option.orElseWith (fun () -> pickByName "get_Item")
        |> Option.orElseWith (fun () -> pickByName "get_Chars")

    // 拡張メソッド判定を一箇所に集約する。
    let private isExtensionMethod (methodInfo: MethodInfo) : bool =
        methodInfo.IsDefined(extensionAttributeType, false)

    // 拡張メソッドを含む MemberAccess の見かけ上の関数型を計算する。
    let memberMethodType (instanceOpt: Hir.Expr option) (methodInfo: MethodInfo) : TypeId =
        let allParamTypes = methodInfo.GetParameters() |> Array.toList |> List.map (fun p -> TypeId.fromSystemType p.ParameterType)
        let logicalParamTypes =
            match instanceOpt with
            | Some _ when methodInfo.IsStatic && isExtensionMethod methodInfo && allParamTypes.Length > 0 -> allParamTypes.Tail
            | _ -> allParamTypes
        TypeId.Fn(logicalParamTypes, TypeId.fromSystemType methodInfo.ReturnType)

    // 受け手型とメンバー名に一致する拡張メソッド候補を列挙する（順序は決定的に維持）。
    let findExtensionMethodCandidates (receiverType: System.Type) (memberName: string) : MethodInfo list =
        System.AppDomain.CurrentDomain.GetAssemblies()
        |> Seq.sortBy (fun asm -> asm.FullName)
        |> Seq.collect (fun asm ->
            let exportedTypes =
                try asm.GetTypes() with _ -> [||]
            exportedTypes
            |> Seq.collect (fun typ ->
                typ.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                |> Seq.filter (fun methodInfo ->
                    let parameters = methodInfo.GetParameters()
                    methodInfo.Name = memberName
                    && isExtensionMethod methodInfo
                    && parameters.Length > 0
                    && parameters.[0].ParameterType.IsAssignableFrom(receiverType))))
        |> Seq.sortBy (fun methodInfo ->
            let parameterCount = methodInfo.GetParameters().Length
            let assemblyName = methodInfo.DeclaringType.Assembly.FullName
            let declaringTypeName = methodInfo.DeclaringType.FullName
            assemblyName, declaringTypeName, methodInfo.Name, parameterCount)
        |> Seq.toList

    // 拡張メソッドの型照合を、受け手を除いた論理引数で行う。
    let resolveExtensionMember (typeEnv: TypeEnv) (methodInfos: MethodInfo list) (tid: TypeId) : (MethodInfo * TypeId) list =
        let exactResult = List<MethodInfo * TypeId>()
        let optionalResult = List<MethodInfo * TypeId>()

        for methodInfo in methodInfos do
            let parameters = methodInfo.GetParameters() |> Array.toList
            if not parameters.IsEmpty then
                let logicalParameters = parameters.Tail
                let logicalMethodType = memberMethodType (Some(Hir.Expr.Unit(Atla.Core.Data.Span.Empty))) methodInfo
                match tid with
                | TypeId.Fn(expectedArgs, expectedRet) ->
                    let requiredParamCount = logicalParameters |> List.filter (fun p -> not p.IsOptional) |> List.length
                    if
                        expectedArgs.Length >= requiredParamCount
                        && expectedArgs.Length <= logicalParameters.Length
                        && List.forall2 (fun expectedArg (parameterInfo: ParameterInfo) -> typeEnv.canUnify expectedArg (TypeId.fromSystemType parameterInfo.ParameterType)) expectedArgs (logicalParameters |> List.take expectedArgs.Length)
                        && match logicalMethodType with
                           | TypeId.Fn(_, methodRet) -> typeEnv.canUnify expectedRet methodRet
                           | _ -> false
                    then
                        if expectedArgs.Length = logicalParameters.Length then
                            exactResult.Add(methodInfo, TypeId.Fn(expectedArgs, expectedRet))
                        else
                            optionalResult.Add(methodInfo, TypeId.Fn(expectedArgs, expectedRet))
                | _ ->
                    if typeEnv.canUnify logicalMethodType tid then
                        exactResult.Add(methodInfo, logicalMethodType)

        if exactResult.Count > 0 then Seq.toList exactResult
        elif optionalResult.Count > 0 then Seq.toList optionalResult
        else []

    // GenericApply で指定された型引数を runtime 型へ解決する。
    let resolveGenericTypeArgs (nameEnv: NameEnv) (genericApplyExpr: Ast.Expr.GenericApply) : Result<System.Type array, string> =
        let resolvedTypeArgs = genericApplyExpr.typeArgs |> List.map nameEnv.resolveTypeExpr
        let resolveTypeArgToRuntimeType (tid: TypeId) : System.Type option =
            let resolveNameType (sid: SymbolId) : System.Type option =
                match nameEnv.resolveSym sid with
                | Some symInfo ->
                    match symInfo.kind with
                    | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) -> Some sysType
                    | _ -> None
                | None -> None

            TypeId.tryResolveToSystemType resolveNameType tid

        match resolvedTypeArgs |> List.tryFind (function | TypeId.Error _ -> true | _ -> false) with
        | Some (TypeId.Error message) -> Result.Error message
        | _ ->
            resolvedTypeArgs
            |> List.mapi (fun idx tid ->
                match resolveTypeArgToRuntimeType tid with
                | Some runtimeType -> Result.Ok runtimeType
                | None -> Result.Error(sprintf "Generic type argument #%d is not a runtime type" (idx + 1)))
            |> List.fold (fun acc current ->
                match acc, current with
                | Result.Ok items, Result.Ok item -> Result.Ok(items @ [ item ])
                | Result.Error err, _ -> Result.Error err
                | _, Result.Error err -> Result.Error err) (Result.Ok [])
            |> Result.map List.toArray

    // Generic method definition を型引数で閉じたメソッドへ変換する。
    let closeGenericMethod (genericArgTypes: System.Type array) (methodInfo: MethodInfo) : MethodInfo option =
        if methodInfo.IsGenericMethodDefinition && methodInfo.GetGenericArguments().Length = genericArgTypes.Length then
            try Some(methodInfo.MakeGenericMethod(genericArgTypes)) with _ -> None
        elif methodInfo.IsGenericMethod && methodInfo.GetGenericArguments().Length = genericArgTypes.Length then
            Some methodInfo
        else
            None
