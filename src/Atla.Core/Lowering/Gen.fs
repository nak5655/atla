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
    // Genモジュール内で共有する生成コンテキスト
    type Env =
        { typeBuilders: Dictionary<SymbolId, TypeBuilder>
          // env-class のデフォルトコンストラクタ（typeSid -> ConstructorBuilder）。
          typeCtors: Dictionary<SymbolId, ConstructorBuilder>
          // env-class フィールドビルダー（fieldSid -> FieldBuilder）。
          fieldBuilders: Dictionary<SymbolId, FieldBuilder>
          methodBuilders: Dictionary<SymbolId, MethodInfo>
          // インポート型（TypeId.Name sid）を System.Type へ解決するためのシンボルテーブル。
          symbolTable: SymbolTable }

    // MIRのTypeIdをCIL生成用のSystem.Typeへ解決する
    let private resolveType (env: Env) (tid: TypeId) : Type =
        let resolveName (sid: SymbolId) : Type option =
            match env.typeBuilders.TryGetValue(sid) with
            | true, builder -> Some (builder :> Type)
            | false, _ ->
                // ユーザー定義型に見つからない場合、インポートされた外部型として SymbolTable を参照する。
                match env.symbolTable.Get(sid) with
                | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) } when not (obj.ReferenceEquals(sysType, null)) ->
                    Some sysType
                | _ -> None

        match TypeId.tryResolveToSystemType resolveName tid with
        | Some resolvedType -> resolvedType
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
            | _ -> failwithf "Unsupported type for CIL generation: %A" tid

    // MIRの値をILスタックへ積む
    let rec private genValue (env: Env) (gen: ILGenerator) (value: Mir.Value) =
        match value with
        // 即値のロード
        | Mir.Value.ImmVal imm ->
            match imm with
            | Mir.Imm.Bool b -> if b then gen.Emit(OpCodes.Ldc_I4_1) else gen.Emit(OpCodes.Ldc_I4_0)
            | Mir.Imm.Int i -> gen.Emit(OpCodes.Ldc_I4, i)
            | Mir.Imm.Float f -> gen.Emit(OpCodes.Ldc_R8, f)
            | Mir.Imm.String s -> gen.Emit(OpCodes.Ldstr, s)
            // null リテラル: 参照型のオプショナル引数デフォルト値として CIL の ldnull を発行する
            | Mir.Imm.Null -> gen.Emit(OpCodes.Ldnull)
        // レジスタ値のロード
        | Mir.Value.RegVal reg ->
            match reg with
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Ldloc, index) // TODO Ldloc_0, Ldloc_1, Ldloc_2, Ldloc_3 を使う
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Ldarg, index) // TODO Ldarg_0, Ldarg_1, Ldarg_2, Ldarg_3 を使う
        // this経由でフィールドをロード
        | Mir.Value.FieldVal field ->
            if field.IsStatic then
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

    // MIR命令をIL命令列へ変換する
    let private genIns (env: Env) (gen: ILGenerator) (ins: Mir.Ins) =
        let emitMethodCall (methodInfo: MethodInfo) =
            if methodInfo.IsStatic then
                gen.Emit(OpCodes.Call, methodInfo)
            elif methodInfo.DeclaringType.IsValueType then
                gen.Emit(OpCodes.Call, methodInfo)
            elif methodInfo.IsVirtual || methodInfo.DeclaringType.IsInterface then
                gen.Emit(OpCodes.Callvirt, methodInfo)
            else
                gen.Emit(OpCodes.Call, methodInfo)

        let emitMethodArgs (methodInfo: MethodInfo) (args: Mir.Value list) =
            match args with
            | receiver :: rest when not methodInfo.IsStatic && methodInfo.DeclaringType.IsValueType ->
                match receiver with
                | Mir.Value.RegVal (Mir.Reg.Loc index) -> gen.Emit(OpCodes.Ldloca, index)
                | Mir.Value.RegVal (Mir.Reg.Arg index) -> gen.Emit(OpCodes.Ldarga, index)
                | _ -> genValue env gen receiver

                for arg in rest do
                    genValue env gen arg
            | _ ->
                for arg in args do
                    genValue env gen arg

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
            let opcode =
                match op with
                | Mir.OpCode.Add -> OpCodes.Add
                | Mir.OpCode.Sub -> OpCodes.Sub
                | Mir.OpCode.Mul -> OpCodes.Mul
                | Mir.OpCode.Div -> OpCodes.Div
                | Mir.OpCode.Mod -> OpCodes.Rem
                | Mir.OpCode.Or -> OpCodes.Or
                | Mir.OpCode.And -> OpCodes.And
                | Mir.OpCode.Eq -> OpCodes.Ceq
            gen.Emit(opcode)
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
                for arg in args do
                    genValue env gen arg
                gen.Emit(OpCodes.Call, ci)
        | Mir.Ins.CallSym (sid, args) ->
            for arg in args do
                genValue env gen arg
            match env.methodBuilders.TryGetValue(sid) with
            | true, methodInfo -> gen.Emit(OpCodes.Call, methodInfo)
            | false, _ -> failwithf "Unknown method symbol: %A" sid
        | Mir.Ins.CallAssign (dst, method, args) ->
            emitMethodArgs method args
            emitMethodCall method
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        | Mir.Ins.CallAssignSym (dst, sid, args) ->
            for arg in args do
                genValue env gen arg
            match env.methodBuilders.TryGetValue(sid) with
            | true, methodInfo -> emitMethodCall methodInfo
            | false, _ -> failwithf "Unknown method symbol: %A" sid
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        | Mir.Ins.New (dst, ctor, args) ->
            for arg in args do
                genValue env gen arg
            gen.Emit(OpCodes.Newobj, ctor)
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // ラベル定義とジャンプ
        | Mir.Ins.MarkLabel label ->
            // ジャンプ距離を計算するためにオフセットを保持しておく
            label.ilOffset <- gen.ILOffset
            gen.MarkLabel(label.get(gen))
        | Mir.Ins.Jump label ->
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Br_S else OpCodes.Br
            gen.Emit(op, label.get(gen))
        | Mir.Ins.JumpTrue (cond, label) ->
            genValue env gen cond
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brtrue_S else OpCodes.Brtrue
            gen.Emit(op, label.get(gen))
        | Mir.Ins.JumpFalse (cond, label) ->
            genValue env gen cond
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brfalse_S else OpCodes.Brfalse
            gen.Emit(op, label.get(gen))
        // 未対応命令は明示的に失敗
        | Mir.Ins.NewEnv (dst, typeSid) ->
            // env-class の新規インスタンスを生成する（デフォルトコンストラクタ使用）。
            match env.typeCtors.TryGetValue(typeSid) with
            | true, ctorBuilder ->
                gen.Emit(OpCodes.Newobj, ctorBuilder :> ConstructorInfo)
                match dst with
                | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
                | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
            | false, _ -> failwithf "No default constructor registered for env type: %A" typeSid
        | Mir.Ins.StoreEnvField (inst, _, fieldSid, value) ->
            // env インスタンスをスタックへ積み、値をスタックへ積んでから stfld を発行する。
            genValue env gen (Mir.Value.RegVal inst)
            genValue env gen value
            match env.fieldBuilders.TryGetValue(fieldSid) with
            | true, fb -> gen.Emit(OpCodes.Stfld, fb :> FieldInfo)
            | false, _ -> failwithf "Unknown env field symbol for StoreEnvField: %A" fieldSid
        | Mir.Ins.LoadEnvField (dst, inst, _, fieldSid) ->
            // env インスタンスをスタックへ積んでから ldfld を発行し、結果をレジスタへ格納する。
            genValue env gen (Mir.Value.RegVal inst)
            match env.fieldBuilders.TryGetValue(fieldSid) with
            | true, fb -> gen.Emit(OpCodes.Ldfld, fb :> FieldInfo)
            | false, _ -> failwithf "Unknown env field symbol for LoadEnvField: %A" fieldSid
            match dst with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        | _ -> failwithf "Unsupported instruction: %A" ins

    // コンストラクタ本体を生成する
    let private genConstructor (_env: Env) (ctor: Mir.Constructor) =
        let gen = ctor.builder.GetILGenerator()

        // ローカル変数スロットを確保
        for KeyValue(sid, reg) in ctor.frame.locs do
            match reg with
            | Mir.Reg.Loc _ ->
                let localType =
                    match ctor.frame.locTypes.TryGetValue(sid) with
                    | true, tid ->
                        match tid with
                        | TypeId.Meta _
                        | TypeId.Error _ -> typeof<obj>
                        // TypeId.Fn はデリゲート型へ変換する。変換できない場合は obj にフォールバック。
                        | TypeId.Fn _ ->
                            TypeId.tryToRuntimeSystemType tid |> Option.defaultValue typeof<obj>
                        | _ -> resolveType _env tid
                    | false, _ -> typeof<obj>
                gen.DeclareLocal(localType) |> ignore
            | Mir.Reg.Arg _ -> ()

        // 本体命令を順に生成
        for ins in ctor.body do
            genIns _env gen ins

    // メソッド本体を生成する
    let private genMethod (_env: Env) (method: Mir.Method) =
        let gen = method.builder.GetILGenerator()

        // ローカル変数スロットを確保
        for KeyValue(sid, reg) in method.frame.locs do
            match reg with
            | Mir.Reg.Loc _ ->
                let localType =
                    match method.frame.locTypes.TryGetValue(sid) with
                    | true, tid ->
                        match tid with
                        | TypeId.Meta _
                        | TypeId.Error _ -> typeof<obj>
                        // TypeId.Fn はデリゲート型へ変換する。変換できない場合は obj にフォールバック。
                        | TypeId.Fn _ ->
                            TypeId.tryToRuntimeSystemType tid |> Option.defaultValue typeof<obj>
                        | _ -> resolveType _env tid
                    | false, _ -> typeof<obj>
                gen.DeclareLocal(localType) |> ignore
            | Mir.Reg.Arg _ -> ()

        // 本体命令を順に生成
        for ins in method.body do
            genIns _env gen ins

    // 型メンバー（フィールド/コンストラクタ/メソッド）を生成し、mainメソッドがあれば返す
    let private genType (env: Env) (typ: Mir.Type) : MethodInfo option =
        let resolveMethodReturnType (tid: TypeId) : Type =
            match tid with
            | TypeId.Unit -> typeof<Void>
            | _ -> resolveType env tid

        // フィールド定義
        for field in typ.fields do
            let fieldType = resolveType env field.typ
            field.builder <- typ.builder.DefineField(sprintf "field_%d" field.sym.id, fieldType, FieldAttributes.Public)
            // env-class フィールドを fieldBuilders へ登録する（GenIns での LoadEnvField/StoreEnvField 解決に使用）。
            env.fieldBuilders.[field.sym] <- field.builder

        // コンストラクタ定義
        for ctor in typ.ctors do
            let ctorArgTypes = ctor.args |> List.map (resolveType env) |> List.toArray
            ctor.builder <- typ.builder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, ctorArgTypes)
            genConstructor env ctor

        // フィールド定義後、明示的コンストラクタが無い場合はデフォルトコンストラクタを自動生成する。
        // これにより NewEnv 命令が new_env(typeSid) でインスタンスを生成できる。
        if typ.ctors.IsEmpty then
            let defaultCtor = typ.builder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||])
            let ctorIL = defaultCtor.GetILGenerator()
            ctorIL.Emit(OpCodes.Ldarg_0)
            ctorIL.Emit(OpCodes.Call, typeof<obj>.GetConstructor([||]))
            ctorIL.Emit(OpCodes.Ret)
            env.typeCtors.[typ.sym] <- defaultCtor

        // インスタンスメソッド定義（クロージャー invoke メソッド）。
        // typ.methods に含まれるメソッドはすべてインスタンスメソッドとして生成する。
        // args の先頭要素は 'this'（env インスタンス）なので CIL シグネチャからは除外する。
        for method in typ.methods do
            let explicitArgTypes = method.args |> List.tail |> List.map (resolveType env) |> List.toArray
            let methodRetType = resolveMethodReturnType method.ret
            method.builder <- typ.builder.DefineMethod(method.name, MethodAttributes.Public, methodRetType, explicitArgTypes)
            // invoke メソッドの SymbolId を methodBuilders へ登録する（FnDelegate 値生成で使用）。
            env.methodBuilders.[method.sym] <- (method.builder :> MethodInfo)
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
              typeCtors = Dictionary<SymbolId, ConstructorBuilder>()
              fieldBuilders = Dictionary<SymbolId, FieldBuilder>()
              methodBuilders = Dictionary<SymbolId, MethodInfo>()
              symbolTable = symbolTable }

        // 型を先に宣言してTypeId.Name解決を可能にする
        for typ in modul.types do
            typ.builder <- moduleBuilder.DefineType(typ.name, TypeAttributes.Public)
            env.typeBuilders.Add(typ.sym, typ.builder)

        // 型内main探索
        let mainInTypes =
            modul.types
            |> List.tryPick (fun typ -> genType env typ)

        // モジュール直下メソッドは静的ヘルパー型へ生成する
        let globalsTypeBuilder =
            moduleBuilder.DefineType(
                $"{modul.name}.Globals",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        for method in modul.methods do
            let methodArgTypes = method.args |> List.map (resolveType env) |> List.toArray
            let methodRetType = resolveMethodReturnType env method.ret
            method.builder <- globalsTypeBuilder.DefineMethod(method.name, MethodAttributes.Public ||| MethodAttributes.Static, methodRetType, methodArgTypes)
            env.methodBuilders.[method.sym] <- (method.builder :> MethodInfo)

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
