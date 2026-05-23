namespace Atla.Core.Lowering

open System
open System.Reflection
open System.Reflection.Emit
open Atla.Core.Lowering.Data
open Atla.Core.Semantics.Data
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Reflection.Metadata.Ecma335
open System.IO
open System.Runtime.Versioning
open System.Collections.Generic

module Gen =
    /// リテラル値を CIL 即値ロード命令として IL スタックへ積む。
    let private emitLiteralConstant (gen: ILGenerator) (fieldType: Type) (rawValue: obj) =
        // enum リテラルは基底型へ正規化してから即値命令へ落とす。
        let normalizedType, normalizedValue =
            if fieldType.IsEnum then
                let underlyingType = Enum.GetUnderlyingType(fieldType)
                let converted =
                    if obj.ReferenceEquals(rawValue, null) then
                        null
                    else
                        Convert.ChangeType(rawValue, underlyingType)
                underlyingType, converted
            else
                fieldType, rawValue

        match normalizedValue with
        | null -> gen.Emit(OpCodes.Ldnull)
        | _ when normalizedType = typeof<bool> ->
            if unbox<bool> normalizedValue then gen.Emit(OpCodes.Ldc_I4_1) else gen.Emit(OpCodes.Ldc_I4_0)
        | _ when normalizedType = typeof<byte> -> gen.Emit(OpCodes.Ldc_I4, int (unbox<byte> normalizedValue))
        | _ when normalizedType = typeof<sbyte> -> gen.Emit(OpCodes.Ldc_I4, int (unbox<sbyte> normalizedValue))
        | _ when normalizedType = typeof<int16> -> gen.Emit(OpCodes.Ldc_I4, int (unbox<int16> normalizedValue))
        | _ when normalizedType = typeof<uint16> -> gen.Emit(OpCodes.Ldc_I4, int (unbox<uint16> normalizedValue))
        | _ when normalizedType = typeof<int> -> gen.Emit(OpCodes.Ldc_I4, unbox<int> normalizedValue)
        | _ when normalizedType = typeof<uint32> -> gen.Emit(OpCodes.Ldc_I4, int (unbox<uint32> normalizedValue))
        | _ when normalizedType = typeof<int64> -> gen.Emit(OpCodes.Ldc_I8, unbox<int64> normalizedValue)
        | _ when normalizedType = typeof<uint64> -> gen.Emit(OpCodes.Ldc_I8, int64 (unbox<uint64> normalizedValue))
        | _ when normalizedType = typeof<char> -> gen.Emit(OpCodes.Ldc_I4, int (unbox<char> normalizedValue))
        | _ when normalizedType = typeof<float32> -> gen.Emit(OpCodes.Ldc_R4, unbox<float32> normalizedValue)
        | _ when normalizedType = typeof<float> -> gen.Emit(OpCodes.Ldc_R8, unbox<float> normalizedValue)
        | _ when normalizedType = typeof<string> -> gen.Emit(OpCodes.Ldstr, unbox<string> normalizedValue)
        | _ ->
            failwithf "Unsupported literal field type for CIL generation: %s" normalizedType.FullName

    // Genモジュール内で共有する生成コンテキスト
    type Env =
        { typeBuilders: Dictionary<SymbolId, TypeBuilder>
          // env-class / 外部型のデフォルトコンストラクタ（typeSid -> ConstructorInfo）。
          typeCtors: Dictionary<SymbolId, ConstructorInfo>
          // env-class / 外部型のフィールド情報（fieldSid -> FieldInfo）。
          fieldBuilders: Dictionary<SymbolId, FieldInfo>
          methodBuilders: Dictionary<SymbolId, MethodInfo>
          // インポート型（TypeId.Name sid）を System.Type へ解決するためのシンボルテーブル。
          symbolTable: SymbolTable
          // 現在処理中のジェネリック型の型パラメータビルダー（型パラメータ名 -> System.Type）。
          // 型メンバー宣言・本体生成フェーズで TypeId.TypeVar を解決するために使用する。
          typeParamBuilders: Dictionary<string, Type> }

    // MIRのTypeIdをCIL生成用のSystem.Typeへ解決する
    let rec private resolveType (env: Env) (tid: TypeId) : Type =
        let resolveName (sid: SymbolId) : Type option =
            match env.typeBuilders.TryGetValue(sid) with
            | true, builder -> Some (builder :> Type)
            | false, _ ->
                // ユーザー定義型に見つからない場合、インポートされた外部型として SymbolTable を参照する。
                match env.symbolTable.Get(sid) with
                | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) } when not (isNull sysType) ->
                    Some sysType
                | _ -> None

        // TypeId.TypeVar は現在処理中のジェネリック型パラメータへ解決する。
        match tid with
        | TypeId.TypeVar name ->
            match env.typeParamBuilders.TryGetValue(name) with
            | true, builder -> builder
            | false, _ -> typeof<obj> // 型消去：ジェネリクスコンテキスト外では obj にフォールバック
        // `ByRef T` は CIL の `T&`。内側型を resolveType で再帰解決し、MakeByRefType を適用する。
        // `TypeId.Name` 等を含むケースに対応するため `tryToRuntimeSystemType` ではなく resolveType を経由する。
        | TypeId.ByRef inner ->
            (resolveType env inner).MakeByRefType()
        | _ ->

        // 先に通常の解決を試す（Native/App/配列/関数など）。
        match TypeId.tryResolveToSystemType resolveName tid with
        | Some resolvedType -> resolvedType
        | None ->
            // Atla の data/enum ジェネリックは現状 CIL 側では実体を非ジェネリック型として出力する。
            // そのため App(Name sid, args) が来ても、ヘッド型が非ジェネリックとして解決できる場合は
            // 型引数を消去してヘッド型へフォールバックする。
            let erasedAppType =
                match tid with
                | TypeId.App(TypeId.Name sid, _) ->
                    match resolveName sid with
                    | Some headType when not headType.IsGenericTypeDefinition -> Some headType
                    | _ -> None
                | _ -> None

            match erasedAppType with
            | Some erasedType -> erasedType
            | None ->
            match tid with
            // CILメンバーシグネチャに載せられない型は明示的に失敗
            | TypeId.Name sid -> failwithf "Unknown type symbol: %A" sid
            // TypeId.Fn はデリゲート型（Func<>/Action<>）へ変換する。
            | TypeId.Fn _ ->
                match TypeId.tryToRuntimeSystemType tid with
                | Some delegateType -> delegateType
                | None -> failwithf "Cannot map function type to delegate: %A" tid
            | TypeId.Meta _ -> failwithf "Unresolved meta type is not supported in Gen: %A" tid
            | TypeId.Error message -> failwithf "Cannot generate CIL for error type: %s" message
            // ここに到達する App は、現状の CIL 生成で具体型へ解決できない高レベル型引数付き表現。
            // ランタイム表現は型引数を保持しない（消去的）ため、最終フォールバックとして object を使う。
            | TypeId.App _ -> typeof<obj>
            | _ -> failwithf "Unsupported type for CIL generation: %A" tid

    /// 外部メソッドグループから引数個数に一致する候補を選択する。
    let private tryResolveExternalMethod (sid: SymbolId) (argCount: int) (symbolTable: SymbolTable) : MethodInfo option =
        match symbolTable.Get(sid) with
        | Some { kind = SymbolKind.External(ExternalBinding.NativeMethodGroup methods) } ->
            methods
            |> List.tryFind (fun methodInfo ->
                let expectedCount =
                    if methodInfo.IsStatic then
                        methodInfo.GetParameters().Length
                    else
                        methodInfo.GetParameters().Length + 1
                expectedCount = argCount)
            // NativeMethodGroup は意味解析で候補が絞られている前提のため、
            // 引数個数が一致しない場合は最後の保険として先頭候補を使う。
            |> Option.orElse (methods |> List.tryHead)
        | _ ->
            None

    /// シンボル表から CIL メソッド名を決定する。
    let private getMethodClrName (symbolTable: SymbolTable) (sid: SymbolId) (defaultName: string) (preserveQualifiedName: bool) : string =
        match symbolTable.Get(sid) with
        | Some symbolInfo when preserveQualifiedName -> symbolInfo.name
        | Some symbolInfo ->
            let lastDot = symbolInfo.name.LastIndexOf('.')
            if lastDot < 0 then symbolInfo.name else symbolInfo.name.Substring(lastDot + 1)
        | None -> defaultName

    // MIRの値をILスタックへ積む
    let rec private genValue (env: Env) (gen: ILGenerator) (value: Mir.Value) =
        match value with
        // 即値のロード
        | Mir.Value.ImmVal imm ->
            match imm with
            | Mir.Imm.Bool b -> if b then gen.Emit(OpCodes.Ldc_I4_1) else gen.Emit(OpCodes.Ldc_I4_0)
            | Mir.Imm.Int i -> gen.Emit(OpCodes.Ldc_I4, i)
            | Mir.Imm.Double f -> gen.Emit(OpCodes.Ldc_R8, f)
            | Mir.Imm.Float f -> gen.Emit(OpCodes.Ldc_R4, f)
            | Mir.Imm.String s -> gen.Emit(OpCodes.Ldstr, s)
            // null リテラル: 参照型のオプショナル引数デフォルト値として CIL の ldnull を発行する
            | Mir.Imm.Null -> gen.Emit(OpCodes.Ldnull)
            // Nullable<T> デフォルト値: ローカル変数を zero-initialize して値をロードする
            | Mir.Imm.NullableDefault t ->
                let local = gen.DeclareLocal(t)
                gen.Emit(OpCodes.Ldloca_S, local)
                gen.Emit(OpCodes.Initobj, t)
                gen.Emit(OpCodes.Ldloc, local)
        // レジスタ値のロード
        | Mir.Value.RegVal reg ->
            match reg with
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Ldloc, index) // TODO Ldloc_0, Ldloc_1, Ldloc_2, Ldloc_3 を使う
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Ldarg, index) // TODO Ldarg_0, Ldarg_1, Ldarg_2, Ldarg_3 を使う
        // this経由でフィールドをロード
        | Mir.Value.FieldVal field ->
            if field.IsStatic && field.IsLiteral then
                // static literal field（例: enum メンバー）は member 参照ではなく定数即値として発行する。
                // PersistedAssemblyBuilder での ldsfld 互換性問題を避けるため、GetRawConstantValue を直接 IL 即値へ変換する。
                emitLiteralConstant gen field.FieldType (field.GetRawConstantValue())
            elif field.IsStatic then
                gen.Emit(OpCodes.Ldsfld, field)
            else
                genValue env gen (Mir.Value.RegVal(Mir.Reg.Arg 0)) // Assuming 'this' is at Arg 0
                gen.Emit(OpCodes.Ldfld, field)
        | Mir.Value.MethodVal methodInfo ->
            failwithf "Method value cannot be loaded directly: %A" methodInfo
        // 関数シンボルをデリゲートに変換してスタックへ積む。
        // targetReg=None:   ldnull;          ldftn <method>; newobj <delegateType>::.ctor(object, native int)
        // targetReg=Some r: ldarg/ldloc <r>; ldftn <method>; newobj <delegateType>::.ctor(object, native int)
        | Mir.Value.FnDelegate (sid, delegateType, targetReg) ->
            match env.methodBuilders.TryGetValue(sid) with
            | true, methodInfo ->
                let ctor = delegateType.GetConstructor([| typeof<obj>; typeof<nativeint> |])
                if obj.ReferenceEquals(ctor, null) then
                    failwithf "Delegate type '%s' has no (object, native int) constructor" delegateType.FullName
                match targetReg with
                | Some(Mir.Reg.Arg index) -> gen.Emit(OpCodes.Ldarg, index)
                | Some(Mir.Reg.Loc index) -> gen.Emit(OpCodes.Ldloc, index)
                | None -> gen.Emit(OpCodes.Ldnull)
                gen.Emit(OpCodes.Ldftn, methodInfo)
                gen.Emit(OpCodes.Newobj, ctor)
            | false, _ ->
                failwithf "Unknown method symbol for delegate creation: %A" sid
        // レジスタのアドレスをロード（マネージドポインタ）。
        | Mir.Value.RegAddr reg ->
            match reg with
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Ldloca, index)
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Ldarga, index)
        // インスタンスレジスタのフィールドアドレスをロード。
        | Mir.Value.FieldAddr (inst, field) ->
            genValue env gen (Mir.Value.RegVal inst)
            gen.Emit(OpCodes.Ldflda, field)

    /// レジスタの CIL 型を返す。locs/args を逆引きして locTypes/argTypes を参照し、resolveType で解決する。
    let private getRegCilType (env: Env) (frame: Mir.Frame) (reg: Mir.Reg) : Type option =
        let findSid (regMap: Map<SymbolId, Mir.Reg>) =
            regMap |> Map.tryFindKey (fun _ r -> r = reg)
        let sidOpt = findSid frame.locs |> Option.orElseWith (fun () -> findSid frame.args)
        sidOpt
        |> Option.bind (fun sid ->
            frame.locTypes |> Map.tryFind sid
            |> Option.orElseWith (fun () -> frame.argTypes |> Map.tryFind sid))
        |> Option.bind (fun tid ->
            match tid with
            | TypeId.Meta _ -> None  // Meta 型は解決不可能；呼び出し元は None をアンボックス不要として扱う
            | _ -> Some (resolveType env tid))

    /// MIR 値が genValue によって CIL スタックへ積まれるときの .NET 型を返す。
    /// RegVal はフレームの argTypes / locTypes から逆引きする。型を確定できない場合は None を返す。
    /// フレームのサイズは関数のローカル変数数に比例し、実用的に小さいため線形探索の逆引きは許容される。
    /// フレームが Reg → TypeId の直接マップを持つように改善した場合はその構造を使うこと。
    let private getValueStackType (frame: Mir.Frame) (value: Mir.Value) : Type option =
        match value with
        | Mir.Value.ImmVal imm ->
            match imm with
            | Mir.Imm.Bool _   -> Some typeof<bool>
            | Mir.Imm.Int _    -> Some typeof<int32>
            | Mir.Imm.Double _ -> Some typeof<float>
            | Mir.Imm.Float _  -> Some typeof<float32>
            | Mir.Imm.String _ -> Some typeof<string>
            | Mir.Imm.Null           -> None
            | Mir.Imm.NullableDefault t -> Some t
        | Mir.Value.RegVal reg ->
            // SymbolId → Reg のマップを Reg → SymbolId へ逆引きしてから型を取得する。
            let findSid (regMap: Map<SymbolId, Mir.Reg>) =
                regMap |> Map.tryFindKey (fun _ r -> r = reg)
            let sidOpt =
                findSid frame.args
                |> Option.orElseWith (fun () -> findSid frame.locs)
            match sidOpt with
            | None -> None
            | Some sid ->
                let tidOpt =
                    frame.argTypes |> Map.tryFind sid
                    |> Option.orElseWith (fun () -> frame.locTypes |> Map.tryFind sid)
                tidOpt |> Option.bind TypeId.tryToRuntimeSystemType
        | Mir.Value.FieldVal fi -> Some fi.FieldType
        // アドレス値は `T&`（MakeByRefType）。インナー型を逆引きしてから `&` を付ける。
        | Mir.Value.RegAddr reg ->
            let findSid (regMap: Map<SymbolId, Mir.Reg>) =
                regMap |> Map.tryFindKey (fun _ r -> r = reg)
            let sidOpt =
                findSid frame.args
                |> Option.orElseWith (fun () -> findSid frame.locs)
            match sidOpt with
            | None -> None
            | Some sid ->
                frame.argTypes |> Map.tryFind sid
                |> Option.orElseWith (fun () -> frame.locTypes |> Map.tryFind sid)
                |> Option.bind TypeId.tryToRuntimeSystemType
                |> Option.map (fun t -> t.MakeByRefType())
        | Mir.Value.FieldAddr (_, field) -> Some (field.FieldType.MakeByRefType())
        | _ -> None

    /// ネイティブメソッド・コンストラクター呼び出し時に必要な数値型変換命令を発行する。
    /// CIL の型安全性規則により、スタック上の型とパラメーター期待型が一致しない数値型の
    /// 組み合わせは invalid program となるため、以下の変換を自動挿入する:
    ///   int32  → float64 (conv.r8)  widening
    ///   int32  → float32 (conv.r4)  widening
    ///   int32  → int64   (conv.i8)  widening
    ///   int32  → uint64  (conv.u8)  widening
    ///   int64  → float64 (conv.r8)  widening
    ///   float32 → float64 (conv.r8) widening
    ///   float64 → float32 (conv.r4) narrowing（Atla の Double は float64 だが、
    ///                                           ネイティブ API が float32 を要求する場合に必要）
    /// 型が一致するか非数値型の場合は何も発行しない。
    let private emitNumericCoercionIfNeeded (gen: ILGenerator) (expectedType: Type) (actualTypeOpt: Type option) =
        match actualTypeOpt with
        | None -> ()
        | Some actualType when actualType = expectedType -> ()
        | Some t ->
            match t, expectedType with
            | t, e when t = typeof<int32>   && e = typeof<float>   -> gen.Emit(OpCodes.Conv_R8)
            | t, e when t = typeof<int32>   && e = typeof<float32> -> gen.Emit(OpCodes.Conv_R4)
            | t, e when t = typeof<int32>   && e = typeof<int64>   -> gen.Emit(OpCodes.Conv_I8)
            | t, e when t = typeof<int32>   && e = typeof<uint64>  -> gen.Emit(OpCodes.Conv_U8)
            | t, e when t = typeof<int64>   && e = typeof<float>   -> gen.Emit(OpCodes.Conv_R8)
            | t, e when t = typeof<float32> && e = typeof<float>   -> gen.Emit(OpCodes.Conv_R8)
            // float64 → float32: Atla の Double は float64 だが、ネイティブ API が float32 を
            // 要求する場合は精度を落として変換する（CIL の型検証を通すために必要）。
            | t, e when t = typeof<float>   && e = typeof<float32> -> gen.Emit(OpCodes.Conv_R4)
            | _ ->
                // .NET の暗黙的変換演算子（op_Implicit）を探して発行する。
                // fromType か toType のどちらかに op_Implicit が定義されている場合に対応する。
                let implicitOp =
                    t.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                    |> Array.tryFind (fun m ->
                        m.Name = "op_Implicit" && m.ReturnType = expectedType
                        && m.GetParameters().Length = 1 && m.GetParameters().[0].ParameterType = t)
                    |> Option.orElseWith (fun () ->
                        expectedType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
                        |> Array.tryFind (fun m ->
                            m.Name = "op_Implicit" && m.ReturnType = expectedType
                            && m.GetParameters().Length = 1 && m.GetParameters().[0].ParameterType = t))
                match implicitOp with
                | Some opMethod -> gen.Emit(OpCodes.Call, opMethod)
                | None -> ()

    // MIR命令をIL命令列へ変換する。
    // ilLabels/ilOffsets はメソッドごとに作成したラベル解決状態を受け取る。
    // frame はレジスタ型の解決および数値型強制変換に使用する。
    // try/catch のネストした命令列を再帰処理するため rec。
    let rec private genIns (env: Env) (frame: Mir.Frame) (gen: ILGenerator) (ilLabels: Dictionary<Mir.LabelId, Label>) (ilOffsets: Dictionary<Mir.LabelId, int>) (ins: Mir.Ins) =
        let emitMethodCall (methodInfo: MethodInfo) =
            if methodInfo.IsStatic then
                gen.Emit(OpCodes.Call, methodInfo)
            elif methodInfo.DeclaringType.IsValueType then
                gen.Emit(OpCodes.Call, methodInfo)
            elif methodInfo.IsVirtual || methodInfo.DeclaringType.IsInterface then
                gen.Emit(OpCodes.Callvirt, methodInfo)
            else
                gen.Emit(OpCodes.Call, methodInfo)

        // 各実引数を CIL スタックへ積み、パラメーター型との数値型不一致があれば変換命令を発行する。
        // 構造体インスタンスメソッドの receiver は ldloca/ldarga でアドレスロードし、変換は行わない。
        let emitMethodArgs (methodInfo: MethodInfo) (args: Mir.Value list) =
            let parameters = methodInfo.GetParameters()
            match args with
            | receiver :: rest when not methodInfo.IsStatic && methodInfo.DeclaringType.IsValueType ->
                // 値型インスタンスメソッドのレシーバーはアドレスを必要とする。
                // レシーバーが既にマネージドポインタ（ByRef: RegAddr / FieldAddr / ByRef 型の Reg）
                // の場合はそのアドレスをそのまま積む。そうでなければ Ldloca/Ldarga でアドレスを取る。
                let receiverIsByRef =
                    match getValueStackType frame receiver with
                    | Some t -> t.IsByRef
                    | None -> false
                match receiver with
                | _ when receiverIsByRef -> genValue env gen receiver
                | Mir.Value.RegVal (Mir.Reg.Loc index) -> gen.Emit(OpCodes.Ldloca, index)
                | Mir.Value.RegVal (Mir.Reg.Arg index) -> gen.Emit(OpCodes.Ldarga, index)
                | _ -> genValue env gen receiver

                // rest[i] は parameters[i] に対応する。
                rest |> List.iteri (fun i arg ->
                    genValue env gen arg
                    if i < parameters.Length then
                        emitNumericCoercionIfNeeded gen parameters.[i].ParameterType (getValueStackType frame arg))
            | _ ->
                // 静的メソッド: args[i] → parameters[i]
                // インスタンスメソッド（非値型）: args[0] はレシーバー（parameters なし）、args[i] (i≥1) → parameters[i-1]
                let isInstance = not methodInfo.IsStatic
                args |> List.iteri (fun i arg ->
                    genValue env gen arg
                    let paramIdx = if isInstance then i - 1 else i
                    if paramIdx >= 0 && paramIdx < parameters.Length then
                        emitNumericCoercionIfNeeded gen parameters.[paramIdx].ParameterType (getValueStackType frame arg))

        match ins with
        // 単純代入
        | Mir.Ins.Assign (reg, value) ->
            genValue env gen value
            match reg with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // 二項演算（TAC）
        | Mir.Ins.TAC (dest, arg1, op, arg2) ->
            genValue env gen arg1
            genValue env gen arg2
            match op with
            | Mir.OpCode.Ne ->
                // != は ceq の結果を論理反転する（ceq; ldc.i4.0; ceq）。
                gen.Emit(OpCodes.Ceq)
                gen.Emit(OpCodes.Ldc_I4_0)
                gen.Emit(OpCodes.Ceq)
            | Mir.OpCode.Add -> gen.Emit(OpCodes.Add)
            | Mir.OpCode.Sub -> gen.Emit(OpCodes.Sub)
            | Mir.OpCode.Mul -> gen.Emit(OpCodes.Mul)
            | Mir.OpCode.Div -> gen.Emit(OpCodes.Div)
            | Mir.OpCode.Mod -> gen.Emit(OpCodes.Rem)
            | Mir.OpCode.Or -> gen.Emit(OpCodes.Or)
            | Mir.OpCode.And -> gen.Emit(OpCodes.And)
            | Mir.OpCode.Eq -> gen.Emit(OpCodes.Ceq)
            match dest with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // 戻り値付きreturn
        | Mir.Ins.RetValue value ->
            genValue env gen value
            gen.Emit(OpCodes.Ret)
        // 戻り値なしreturn
        | Mir.Ins.Ret ->
            gen.Emit(OpCodes.Ret)
        // 関数/コンストラクタ呼び出し
        | Mir.Ins.Call (method, args) ->
            match method with
            | Choice1Of2 mi ->
                emitMethodArgs mi args
                emitMethodCall mi
            | Choice2Of2 ci ->
                // コンストラクタ呼び出し（call ctor 形式）: パラメーター型に合わせて数値変換を行う。
                let ctorParams = ci.GetParameters()
                args |> List.iteri (fun i arg ->
                    genValue env gen arg
                    if i < ctorParams.Length then
                        emitNumericCoercionIfNeeded gen ctorParams.[i].ParameterType (getValueStackType frame arg))
                gen.Emit(OpCodes.Call, ci)
        | Mir.Ins.CallSym (sid, args) ->
            for arg in args do
                genValue env gen arg
            match env.methodBuilders.TryGetValue(sid) with
            | true, methodInfo -> gen.Emit(OpCodes.Call, methodInfo)
            | false, _ ->
                match tryResolveExternalMethod sid args.Length env.symbolTable with
                | Some methodInfo -> emitMethodCall methodInfo
                | None -> failwithf "Unknown method symbol: %A" sid
        | Mir.Ins.CallAssign (dst, method, args) ->
            emitMethodArgs method args
            emitMethodCall method
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // ジェネリックメソッド定義を Gen で解決した型引数で閉じて呼ぶ。
        // 引数（レシーバーのアドレス含む）は呼び出し側が構築済みのため、評価順に積むだけでよい。
        // 型引数に生成型（TypeBuilder）が含まれると GetParameters() が例外を投げ得るため、
        // 数値型強制は行わず（async builder の引数は全てマネージドポインタで強制不要）そのまま積む。
        | Mir.Ins.CallGenericNative (methodDef, typeArgs, args) ->
            let closed = methodDef.MakeGenericMethod(typeArgs |> List.map (resolveType env) |> List.toArray)
            for arg in args do genValue env gen arg
            emitMethodCall closed
        | Mir.Ins.CallGenericNativeAssign (dst, methodDef, typeArgs, args) ->
            let closed = methodDef.MakeGenericMethod(typeArgs |> List.map (resolveType env) |> List.toArray)
            for arg in args do genValue env gen arg
            emitMethodCall closed
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // `base'X` 由来の非仮想呼び出し。virtual メソッドであっても `OpCodes.Call` で発行し、
        // 親クラス実装を直接呼ぶ（callvirt だとオーバーライド側に再ディスパッチし無限再帰になる）。
        | Mir.Ins.CallBase (method, args) ->
            emitMethodArgs method args
            gen.Emit(OpCodes.Call, method)
        | Mir.Ins.CallAssignBase (dst, method, args) ->
            emitMethodArgs method args
            gen.Emit(OpCodes.Call, method)
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        | Mir.Ins.CallAssignSym (dst, sid, args) ->
            for arg in args do
                genValue env gen arg
            match env.methodBuilders.TryGetValue(sid) with
            | true, methodInfo -> emitMethodCall methodInfo
            | false, _ ->
                match tryResolveExternalMethod sid args.Length env.symbolTable with
                | Some methodInfo -> emitMethodCall methodInfo
                | None -> failwithf "Unknown method symbol: %A" sid
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        | Mir.Ins.New (dst, ctor, args) ->
            // コンストラクタ引数をスタックへ積み、パラメーター型との数値型不一致があれば変換命令を発行する。
            let ctorParams = ctor.GetParameters()
            args |> List.iteri (fun i arg ->
                genValue env gen arg
                if i < ctorParams.Length then
                    emitNumericCoercionIfNeeded gen ctorParams.[i].ParameterType (getValueStackType frame arg))
            gen.Emit(OpCodes.Newobj, ctor)
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        | Mir.Ins.NewArr (dst, elemType, values) ->
            gen.Emit(OpCodes.Ldc_I4, values.Length)
            gen.Emit(OpCodes.Newarr, elemType)
            values |> List.iteri (fun i value ->
                gen.Emit(OpCodes.Dup)
                gen.Emit(OpCodes.Ldc_I4, i)
                genValue env gen value
                gen.Emit(OpCodes.Stelem, elemType))
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // 数値型変換: ソース値をスタックへ積み、変換先に応じた conv 命令を発行する。
        | Mir.Ins.Convert (dst, src, target) ->
            genValue env gen src
            if target = typeof<float32> then gen.Emit(OpCodes.Conv_R4)
            elif target = typeof<float> then gen.Emit(OpCodes.Conv_R8)
            elif target = typeof<int> then gen.Emit(OpCodes.Conv_I4)
            else ()
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // ネイティブフィールドへの書き込み: receiver（参照 or アドレス）と value を積み、
        // 数値型の不一致があれば変換してから stfld を発行する。
        | Mir.Ins.StoreNativeField (receiver, field, value) ->
            genValue env gen receiver
            genValue env gen value
            emitNumericCoercionIfNeeded gen field.FieldType (getValueStackType frame value)
            gen.Emit(OpCodes.Stfld, field)
        // ネイティブインスタンスフィールドの読み出し: instance（参照・ポインタ・値型）を積み、ldfld して dst へ格納する。
        | Mir.Ins.LoadNativeField (dst, instance, field) ->
            genValue env gen instance
            gen.Emit(OpCodes.Ldfld, field)
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // ラベル定義とジャンプ
        | Mir.Ins.MarkLabel labelId ->
            // labelId に対応する ILLabel を取得または新規定義し、現在の ILOffset を記録する。
            let ilLabel =
                match ilLabels.TryGetValue(labelId) with
                | true, l -> l
                | false, _ ->
                    let l = gen.DefineLabel()
                    ilLabels.[labelId] <- l
                    l
            ilOffsets.[labelId] <- gen.ILOffset
            gen.MarkLabel(ilLabel)
        | Mir.Ins.Jump labelId ->
            let ilLabel =
                match ilLabels.TryGetValue(labelId) with
                | true, l -> l
                | false, _ ->
                    let l = gen.DefineLabel()
                    ilLabels.[labelId] <- l
                    l
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let ilOffset = match ilOffsets.TryGetValue(labelId) with | true, o -> o | false, _ -> -1
            let offset = ilOffset - gen.ILOffset
            let op = if (0 < ilOffset && -120 < offset && offset < 120) then OpCodes.Br_S else OpCodes.Br
            gen.Emit(op, ilLabel)
        | Mir.Ins.JumpTrue (cond, labelId) ->
            genValue env gen cond
            let ilLabel =
                match ilLabels.TryGetValue(labelId) with
                | true, l -> l
                | false, _ ->
                    let l = gen.DefineLabel()
                    ilLabels.[labelId] <- l
                    l
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let ilOffset = match ilOffsets.TryGetValue(labelId) with | true, o -> o | false, _ -> -1
            let offset = ilOffset - gen.ILOffset
            let op = if (0 < ilOffset && -120 < offset && offset < 120) then OpCodes.Brtrue_S else OpCodes.Brtrue
            gen.Emit(op, ilLabel)
        | Mir.Ins.JumpFalse (cond, labelId) ->
            genValue env gen cond
            let ilLabel =
                match ilLabels.TryGetValue(labelId) with
                | true, l -> l
                | false, _ ->
                    let l = gen.DefineLabel()
                    ilLabels.[labelId] <- l
                    l
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let ilOffset = match ilOffsets.TryGetValue(labelId) with | true, o -> o | false, _ -> -1
            let offset = ilOffset - gen.ILOffset
            let op = if (0 < ilOffset && -120 < offset && offset < 120) then OpCodes.Brfalse_S else OpCodes.Brfalse
            gen.Emit(op, ilLabel)
        // 未対応命令は明示的に失敗
        | Mir.Ins.NewEnv (dst, typeSid) ->
            // env-class の新規インスタンスを生成する（デフォルトコンストラクタ使用）。
            match env.typeCtors.TryGetValue(typeSid) with
            | true, ctorInfo ->
                gen.Emit(OpCodes.Newobj, ctorInfo)
                match dst with
                | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
                | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
            | false, _ ->
                match env.symbolTable.Get(typeSid) with
                | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) } when not (isNull sysType) ->
                    match sysType.GetConstructor([||]) with
                    | null -> failwithf "No default constructor registered for env type: %A" typeSid
                    | ctorInfo ->
                        gen.Emit(OpCodes.Newobj, ctorInfo)
                        match dst with
                        | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
                        | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
                | _ -> failwithf "No default constructor registered for env type: %A" typeSid
        | Mir.Ins.StoreEnvField (inst, _, fieldSid, value) ->
            // env インスタンスをスタックへ積み、値をスタックへ積んでから stfld を発行する。
            genValue env gen (Mir.Value.RegVal inst)
            genValue env gen value
            // TypeVar フィールド（型消去により obj）に値型を格納する場合はボックス化する。
            let storeFieldTypId = env.symbolTable.Get(fieldSid) |> Option.map (fun s -> s.typ)
            match storeFieldTypId with
            | Some (TypeId.TypeVar _) ->
                match getValueStackType frame value with
                | Some t when t.IsValueType -> gen.Emit(OpCodes.Box, t)
                | _ -> ()
            | _ -> ()
            match env.fieldBuilders.TryGetValue(fieldSid) with
            | true, fieldInfo -> gen.Emit(OpCodes.Stfld, fieldInfo)
            | false, _ ->
                match env.symbolTable.Get(fieldSid) with
                | Some { kind = SymbolKind.External(ExternalBinding.SystemFieldRef fieldInfo) } ->
                    gen.Emit(OpCodes.Stfld, fieldInfo)
                | _ -> failwithf "Unknown env field symbol for StoreEnvField: %A" fieldSid
        | Mir.Ins.LoadEnvFieldAddr (dst, inst, _, fieldSid) ->
            // env インスタンスをスタックへ積み、ldflda でフィールドアドレスを取得して dst へ格納する。
            genValue env gen (Mir.Value.RegVal inst)
            match env.fieldBuilders.TryGetValue(fieldSid) with
            | true, fieldInfo -> gen.Emit(OpCodes.Ldflda, fieldInfo)
            | false, _ ->
                match env.symbolTable.Get(fieldSid) with
                | Some { kind = SymbolKind.External(ExternalBinding.SystemFieldRef fieldInfo) } ->
                    gen.Emit(OpCodes.Ldflda, fieldInfo)
                | _ -> failwithf "Unknown env field symbol for LoadEnvFieldAddr: %A" fieldSid
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        | Mir.Ins.LoadEnvField (dst, inst, _, fieldSid) ->
            // env インスタンスをスタックへ積んでから ldfld を発行し、結果をレジスタへ格納する。
            genValue env gen (Mir.Value.RegVal inst)
            match env.fieldBuilders.TryGetValue(fieldSid) with
            | true, fieldInfo -> gen.Emit(OpCodes.Ldfld, fieldInfo)
            | false, _ ->
                match env.symbolTable.Get(fieldSid) with
                | Some { kind = SymbolKind.External(ExternalBinding.SystemFieldRef fieldInfo) } ->
                    gen.Emit(OpCodes.Ldfld, fieldInfo)
                | _ -> failwithf "Unknown env field symbol for LoadEnvField: %A" fieldSid
            // TypeVar フィールド（型消去により obj として宣言）をロードした場合、
            // 格納先レジスタの期待型が obj 以外であればキャスト／アンボックスを挿入する。
            let fieldTypId = env.symbolTable.Get(fieldSid) |> Option.map (fun s -> s.typ)
            match fieldTypId with
            | Some (TypeId.TypeVar _) ->
                match getRegCilType env frame dst with
                | Some expectedCilType when expectedCilType <> typeof<obj> ->
                    gen.Emit(OpCodes.Unbox_Any, expectedCilType)
                | _ -> ()
            | _ -> ()
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // 保護領域外ラベルへの脱出（CIL `leave`）。
        | Mir.Ins.Leave labelId ->
            let ilLabel =
                match ilLabels.TryGetValue(labelId) with
                | true, l -> l
                | false, _ ->
                    let l = gen.DefineLabel()
                    ilLabels.[labelId] <- l
                    l
            gen.Emit(OpCodes.Leave, ilLabel)
        // try/catch（単一 catch 型）。
        | Mir.Ins.TryCatch (tryBody, catchType, catchVar, catchBody) ->
            gen.BeginExceptionBlock() |> ignore
            for tryIns in tryBody do
                genIns env frame gen ilLabels ilOffsets tryIns
            gen.BeginCatchBlock(catchType)
            // catch 進入時、例外オブジェクトが評価スタックに積まれているので catchVar へ退避する。
            match catchVar with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
            for catchIns in catchBody do
                genIns env frame gen ilLabels ilOffsets catchIns
            gen.EndExceptionBlock()
        | _ -> failwithf "Unsupported instruction: %A" ins

    // コンストラクタ本体を生成する
    let private genConstructor (_env: Env) (ctor: Mir.Constructor) =
        let gen = ctor.builder.GetILGenerator()
        // メソッドごとのラベル解決状態を初期化する。
        let ilLabels = Dictionary<Mir.LabelId, Label>()
        let ilOffsets = Dictionary<Mir.LabelId, int>()

        // ローカル変数スロットを Loc インデックス昇順に確保する。
        for (sid, reg) in ctor.frame.locs |> Map.toSeq |> Seq.sortBy (fun (_, r) -> match r with | Mir.Reg.Loc i -> i | Mir.Reg.Arg _ -> failwith "Unexpected Arg register in frame.locs") do
            match reg with
            | Mir.Reg.Loc _ ->
                let localType =
                    match ctor.frame.locTypes |> Map.tryFind sid with
                    | Some tid ->
                        match tid with
                        | TypeId.Meta _
                        | TypeId.Error _ -> typeof<obj>
                        // TypeId.Fn はデリゲート型へ変換する。変換できない場合は obj にフォールバック。
                        | TypeId.Fn _ ->
                            TypeId.tryToRuntimeSystemType tid |> Option.defaultValue typeof<obj>
                        | _ -> resolveType _env tid
                    | None -> typeof<obj>
                gen.DeclareLocal(localType) |> ignore
            | Mir.Reg.Arg _ -> ()

        // 本体命令を順に生成
        for ins in ctor.body do
            genIns _env ctor.frame gen ilLabels ilOffsets ins

    // メソッド本体を生成する
    let private genMethod (_env: Env) (method: Mir.Method) =
        let gen = method.builder.GetILGenerator()
        // メソッドごとのラベル解決状態を初期化する。
        let ilLabels = Dictionary<Mir.LabelId, Label>()
        let ilOffsets = Dictionary<Mir.LabelId, int>()

        // ローカル変数スロットを Loc インデックス昇順に確保する。
        for (sid, reg) in method.frame.locs |> Map.toSeq |> Seq.sortBy (fun (_, r) -> match r with | Mir.Reg.Loc i -> i | Mir.Reg.Arg _ -> failwith "Unexpected Arg register in frame.locs") do
            match reg with
            | Mir.Reg.Loc _ ->
                let localType =
                    match method.frame.locTypes |> Map.tryFind sid with
                    | Some tid ->
                        match tid with
                        | TypeId.Meta _
                        | TypeId.Error _ -> typeof<obj>
                        // TypeId.Fn はデリゲート型へ変換する。変換できない場合は obj にフォールバック。
                        | TypeId.Fn _ ->
                            TypeId.tryToRuntimeSystemType tid |> Option.defaultValue typeof<obj>
                        | _ -> resolveType _env tid
                    | None -> typeof<obj>
                gen.DeclareLocal(localType) |> ignore
            | Mir.Reg.Arg _ -> ()

        // 本体命令を順に生成
        for ins in method.body do
            try
                genIns _env method.frame gen ilLabels ilOffsets ins
            with ex ->
                failwithf "Error in method '%s' (sym=%A) at instruction %A: %s" method.name method.sym ins ex.Message

    /// 型のフィールド・コンストラクタ・クロージャー invoke メソッドを宣言し、
    /// MethodBuilder を env.methodBuilders へ登録する。本体（IL）はまだ生成しない。
    /// 全型の宣言が完了してからメソッド本体を生成することで、invoke メソッドが
    /// モジュールレベル関数（module.methods）を CallSym/FnDelegate で参照できるようになる。
    let private declareTypeMembers (env: Env) (typ: Mir.Type) : unit =
        let resolveMethodReturnType (tid: TypeId) : Type =
            match tid with
            | TypeId.Unit -> typeof<Void>
            | _ -> resolveType env tid

        if typ.isInterface then
            // インターフェイス型（role）: フィールドもコンストラクタも不要。
            // 各メソッドを abstract virtual として宣言する。
            for method in typ.methods do
                let explicitArgTypes = method.args |> List.map (resolveType env) |> List.toArray
                let methodRetType = resolveMethodReturnType method.ret
                method.builder <-
                    typ.builder.DefineMethod(
                        getMethodClrName env.symbolTable method.sym method.name false,
                        MethodAttributes.Public ||| MethodAttributes.Abstract ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig,
                        methodRetType,
                        explicitArgTypes)
                env.methodBuilders.[method.sym] <- (method.builder :> MethodInfo)
        else
            // フィールド定義
            for field in typ.fields do
                let fieldType = resolveType env field.typ
                let fieldName =
                    match env.symbolTable.Get(field.sym) with
                    | Some symbolInfo ->
                        let lastDot = symbolInfo.name.LastIndexOf('.')
                        if lastDot < 0 then symbolInfo.name else symbolInfo.name.Substring(lastDot + 1)
                    | None ->
                        sprintf "field_%d" field.sym.id
                field.builder <- typ.builder.DefineField(fieldName, fieldType, FieldAttributes.Public)
                // env-class フィールドを fieldBuilders へ登録する（GenIns での LoadEnvField/StoreEnvField 解決に使用）。
                env.fieldBuilders.[field.sym] <- field.builder

            // 明示コンストラクタ定義（MethodBuilder のみ、本体生成は genTypeBodies で行う）。
            for ctor in typ.ctors do
                let ctorArgTypes = ctor.args |> List.map (resolveType env) |> List.toArray
                ctor.builder <- typ.builder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, ctorArgTypes)

            // 明示コンストラクタが無い場合はデフォルトコンストラクタを宣言し、本体も即時生成する。
            // デフォルトコンストラクタは単純（ldarg.0; call base..ctor; ret）で外部参照を持たないため、
            // 宣言と同時に生成しても問題ない。
            if typ.ctors.IsEmpty then
                let defaultCtor = typ.builder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||])
                let ctorIL = defaultCtor.GetILGenerator()
                ctorIL.Emit(OpCodes.Ldarg_0)
                // 基底型が指定されている場合はその型のパラメータなしコンストラクタを呼ぶ。
                // 基底型がインターフェイス、または指定なし/パラメータなしコンストラクタが存在しない場合は Object のコンストラクタを呼ぶ。
                let baseCtorInfo =
                    match typ.baseType with
                    | Some baseTid ->
                        let resolvedBase = resolveType env baseTid
                        if isNull resolvedBase || resolvedBase.IsInterface then
                            typeof<obj>.GetConstructor([||])
                        else
                            let flags =
                                System.Reflection.BindingFlags.Public |||
                                System.Reflection.BindingFlags.NonPublic |||
                                System.Reflection.BindingFlags.Instance
                            let baseCtor = resolvedBase.GetConstructor(flags, null, [||], null)
                            if isNull baseCtor then typeof<obj>.GetConstructor([||])
                            else baseCtor
                    | None -> typeof<obj>.GetConstructor([||])
                ctorIL.Emit(OpCodes.Call, baseCtorInfo)
                ctorIL.Emit(OpCodes.Ret)
                env.typeCtors.[typ.sym] <- defaultCtor

            // インスタンスメソッドの MethodBuilder を宣言・登録する。
            // layoutDataTypeMethod がすでに 'this' を method.args から除去しているため、
            // method.args をそのまま CIL 明示引数シグネチャとして使用する。
            // 本体生成（IL emit）は genTypeBodies で行う。
            // override 付きメソッド（overrideTarget が Some）は親クラスの virtual slot を再利用するため
            // `Virtual | HideBySig` で emit し、`DefineMethodOverride` で明示紐付けする。
            for method in typ.methods do
                let explicitArgTypes = method.args |> List.map (resolveType env) |> List.toArray
                let methodRetType = resolveMethodReturnType method.ret
                let attrs =
                    match method.overrideTarget with
                    | Some target ->
                        let visibility =
                            if target.IsPublic then MethodAttributes.Public
                            elif target.IsFamilyOrAssembly then MethodAttributes.FamORAssem
                            else MethodAttributes.Family
                        // 親 slot を再利用するため NewSlot は付けない。
                        visibility ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig
                    | None ->
                        MethodAttributes.Public
                method.builder <-
                    typ.builder.DefineMethod(getMethodClrName env.symbolTable method.sym method.name false, attrs, methodRetType, explicitArgTypes)
                env.methodBuilders.[method.sym] <- (method.builder :> MethodInfo)
                // 親メソッドの slot へ明示的に紐付ける（generic 等での名前解決のロバスト性向上）。
                match method.overrideTarget with
                | Some target -> typ.builder.DefineMethodOverride(method.builder, target)
                | None -> ()

    /// 型の明示コンストラクタ本体と invoke メソッド本体を生成し、型を確定（CreateType）する。
    /// declareTypeMembers で全 MethodBuilder が登録済みであることが前提。
    /// main メソッドが型内に存在する場合はその MethodInfo を返す。
    let private genTypeBodies (env: Env) (typ: Mir.Type) : MethodInfo option =
        if typ.isInterface then
            // インターフェイス型（role）は abstract メソッドのみを持つため、本体生成は不要。
            typ.builder.CreateType() |> ignore
            None
        else
            // 明示コンストラクタ本体を生成する。
            for ctor in typ.ctors do
                genConstructor env ctor

            // invoke メソッド本体を生成する。
            for method in typ.methods do
                genMethod env method

            // 型確定
            typ.builder.CreateType() |> ignore

            // 型内main探索
            typ.builder.DeclaredMethods
            |> Seq.tryFind (fun method -> method.Name = "main")

    // モジュール単位で型とグローバル関数を生成し、mainメソッドを返す
    let private genModule (moduleBuilder: ModuleBuilder) (modul: Mir.Module) (symbolTable: SymbolTable) : MethodInfo option =
        let resolveMethodReturnType (env: Env) (tid: TypeId) : Type =
            match tid with
            | TypeId.Unit -> typeof<Void>
            | _ -> resolveType env tid

        // モジュール内型解決テーブルを初期化
        let env =
            { typeBuilders = Dictionary<SymbolId, TypeBuilder>()
              typeCtors = Dictionary<SymbolId, ConstructorInfo>()
              fieldBuilders = Dictionary<SymbolId, FieldInfo>()
              methodBuilders = Dictionary<SymbolId, MethodInfo>()
              symbolTable = symbolTable
              // 型パラメータビルダーは型処理フェーズで逐次設定・クリアする可変テーブル。
              typeParamBuilders = Dictionary<string, Type>() }

        // 型ごとの型パラメータビルダーを保持するテーブル（Phase 1a で構築、Phase 2/4 で参照）。
        let genericParamBuildersByType = Dictionary<SymbolId, Dictionary<string, Type>>()

        // フェーズ 1a: 全型の TypeBuilder を先行宣言する。
        // インターフェイス型（role）は TypeAttributes.Interface | Abstract で定義し、
        // 具象型（data）は通常の class として定義する。
        // TypeId.Name による役割型参照の解決は Phase 1b で行う（この時点では未登録の可能性がある）。
        // ジェネリック型の場合は DefineGenericParameters を呼び出して型パラメータを登録する。
        for typ in modul.types do
            let typeBuilder =
                if typ.isInterface then
                    // role 型は CIL インターフェイスとして定義する。
                    moduleBuilder.DefineType(
                        typ.name,
                        TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)
                else
                    let resolvedBaseType =
                        match typ.baseType with
                        | Some (TypeId.Native sysType) when not (obj.ReferenceEquals(sysType, null)) && not sysType.IsInterface ->
                            // .NET 継承クラス（impl A as DotNetClass）: DefineType で基底クラスを指定する。
                            sysType
                        | _ ->
                            // TypeId.Name（role）や None の場合は Phase 1b で AddInterfaceImplementation を使用する。
                            typeof<obj>
                    moduleBuilder.DefineType(typ.name, TypeAttributes.Public, resolvedBaseType)
            // Atla の data/enum 型はジェネリクスを型消去でコンパイルする。
            // DefineGenericParameters を呼ばず、TypeVar フィールドは obj として CIL に出力する。
            // （genericParamBuildersByType は使用しない）
            typ.builder <- typeBuilder
            env.typeBuilders.Add(typ.sym, typeBuilder)

        // フェーズ 1b: 具象型に .NET ネイティブ基底インターフェイスの実装を追加する。
        // role（TypeId.Name）経由のインターフェイス実装は impl メソッドが CIL インスタンスメソッドとして
        // 生成されるが、AddInterfaceImplementation は行わない（型安全性はセマンティクス層で保証）。
        // `impl A as B` 内の override 付きメソッドは、declareTypeMembers 内で
        // `Virtual | HideBySig` + `DefineMethodOverride` で親クラスの slot を上書きする。
        // TypeId.Native で指定された .NET インターフェイスのみ AddInterfaceImplementation を追加する。
        for typ in modul.types do
            if not typ.isInterface then
                match typ.baseType with
                | Some (TypeId.Native sysType) when not (obj.ReferenceEquals(sysType, null)) && sysType.IsInterface ->
                    typ.builder.AddInterfaceImplementation(sysType)
                | _ -> ()

        // フェーズ 2: 全型のフィールド・コンストラクタ・invoke メソッドを宣言し、
        // MethodBuilder を methodBuilders へ登録する（本体 IL はまだ生成しない）。
        // ジェネリック型の場合は先に型パラメータビルダーを env へ設定する。
        for typ in modul.types do
            env.typeParamBuilders.Clear()
            match genericParamBuildersByType.TryGetValue(typ.sym) with
            | true, paramMap ->
                for kvp in paramMap do
                    env.typeParamBuilders.[kvp.Key] <- kvp.Value
            | false, _ -> ()
            declareTypeMembers env typ

        // フェーズ 3: モジュール直下メソッド（静的グローバル）を宣言し、
        // MethodBuilder を methodBuilders へ登録する（本体 IL はまだ生成しない）。
        // 全 MethodBuilder を先行登録することで、invoke メソッド本体が
        // モジュールレベル関数を CallSym/FnDelegate で参照できるようになる。
        // グローバル関数は型パラメータを持たないため typeParamBuilders をクリアしておく。
        env.typeParamBuilders.Clear()
        let globalsTypeBuilder =
            moduleBuilder.DefineType(
                $"{modul.name}.Globals",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        for method in modul.methods do
            let methodArgTypes = method.args |> List.map (resolveType env) |> List.toArray
            let methodRetType = resolveMethodReturnType env method.ret
            method.builder <-
                globalsTypeBuilder.DefineMethod(
                    getMethodClrName env.symbolTable method.sym method.name true,
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    methodRetType,
                    methodArgTypes)
            env.methodBuilders.[method.sym] <- (method.builder :> MethodInfo)

        // フェーズ 4: 全型の本体 IL を生成して型を確定（CreateType）する。
        // この時点で全 MethodBuilder（型 invoke + モジュールグローバル）が登録済みのため、
        // 相互参照（クロージャー → グローバル関数 など）が正しく解決される。
        // ジェネリック型の場合は型パラメータビルダーを再設定してから本体を生成する。
        let mainInTypes =
            modul.types
            |> List.tryPick (fun typ ->
                env.typeParamBuilders.Clear()
                match genericParamBuildersByType.TryGetValue(typ.sym) with
                | true, paramMap ->
                    for kvp in paramMap do
                        env.typeParamBuilders.[kvp.Key] <- kvp.Value
                | false, _ -> ()
                genTypeBodies env typ)

        // フェーズ 5: モジュールレベルメソッドの本体 IL を生成する。
        env.typeParamBuilders.Clear()
        for method in modul.methods do
            genMethod env method

        let globalsType = globalsTypeBuilder.CreateType()
        let mainInGlobals = globalsType.GetMethod("main", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)

        // モジュールメソッドmainを優先し、なければ型内mainを返す
        match mainInGlobals with
        | null -> mainInTypes
        | methodInfo -> Some methodInfo

    // MIRアセンブリをPEファイルとして出力する
    let rec genAssembly (assembly: Mir.Assembly, filePath: string, symbolTable: SymbolTable) : PhaseResult<unit> =
        try
            // アセンブリビルダー初期化
            assembly.builder <- PersistedAssemblyBuilder(AssemblyName(assembly.name), typeof<obj>.Assembly)

            // 各モジュールを生成して最初のmainメソッドを採用
            let mainMethod =
                assembly.modules
                |> List.fold (fun foundMain modul ->
                    let moduleBuilder = assembly.builder.DefineDynamicModule(modul.name)
                    let moduleMain = genModule moduleBuilder modul symbolTable

                    match foundMain with
                    | Some _ -> foundMain
                    | None -> moduleMain) None

            // 実行時ターゲット情報を付与
            let tfaCtor = typeof<TargetFrameworkAttribute>.GetConstructor([| typeof<string> |])
            let tfa = CustomAttributeBuilder(tfaCtor, [| ".NETCoreApp,Version=v10.0" |])
            assembly.builder.SetCustomAttribute(tfa)

            // メタデータ/ILを生成
            let mutable ilStream = BlobBuilder()
            let mutable fieldData = BlobBuilder()
            let metadataBuilder = assembly.builder.GenerateMetadata(&ilStream, &fieldData)

            // エントリポイント付き/なしでPEビルダーを構築
            let peBuilder =
                match mainMethod with
                | Some methodInfo ->
                    ManagedPEBuilder(
                        header = PEHeaderBuilder.CreateExecutableHeader(),
                        metadataRootBuilder = MetadataRootBuilder(metadataBuilder),
                        ilStream = ilStream,
                        mappedFieldData = fieldData,
                        entryPoint = MetadataTokens.MethodDefinitionHandle(methodInfo.MetadataToken &&& 0x00FFFFFF))
                | None ->
                    ManagedPEBuilder(
                        header = PEHeaderBuilder.CreateExecutableHeader(),
                        metadataRootBuilder = MetadataRootBuilder(metadataBuilder),
                        ilStream = ilStream,
                        mappedFieldData = fieldData)

            // PEをシリアライズして書き出し
            let peBlob = BlobBuilder()
            peBuilder.Serialize(peBlob) |> ignore

            use fs = new FileStream(filePath, FileMode.Create, FileAccess.Write)
            peBlob.WriteContentTo(fs)

            // 実行用runtimeconfigを書き出し
            genRuntimeConfig filePath
            PhaseResult.succeeded () []
        with ex ->
            PhaseResult.failed [ Diagnostic.Error($"CIL generation failed: {ex.Message}", Atla.Core.Data.Span.Empty) ]

    // dotnet実行に必要なruntimeconfigを生成する
    and genRuntimeConfig (assemblyPath: string) =
        let runtimeConfig = """{
  "runtimeOptions": {
    "tfm": "net10.0",
    "framework": {
      "name": "Microsoft.NETCore.App",
      "version": "10.0.0"
    },
    "configProperties": {
      "System.Runtime.Serialization.EnableUnsafeBinaryFormatterSerialization": false
    }
  }
}
        """
        let configPath = Path.ChangeExtension(assemblyPath, ".runtimeconfig.json")
        File.WriteAllText(configPath, runtimeConfig)
