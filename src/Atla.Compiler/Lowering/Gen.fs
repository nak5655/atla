namespace Atla.Compiler.Lowering

open System
open System.Reflection
open System.Reflection.Emit
open Atla.Compiler.Lowering.Data
open Atla.Compiler.Semantics.Data
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Reflection.Metadata.Ecma335
open System.IO
open System.Runtime.Versioning
open System.Collections.Generic

module Gen =
    // Genモジュール内で共有する生成コンテキスト
    type Env =
        { typeBuilders: Dictionary<SymbolId, TypeBuilder> }

    // MIRのTypeIdをCIL生成用のSystem.Typeへ解決する
    let private resolveType (env: Env) (tid: TypeId) : Type =
        match tid with
        // プリミティブ型
        | TypeId.Unit -> typeof<Void>
        | TypeId.Bool -> typeof<bool>
        | TypeId.Int -> typeof<int>
        | TypeId.Float -> typeof<float>
        | TypeId.String -> typeof<string>
        | TypeId.Native t -> t
        // モジュール内で事前定義したTypeBuilderをSystem.Typeとして扱う
        | TypeId.Name sid ->
            match env.typeBuilders.TryGetValue(sid) with
            | true, builder -> builder :> Type
            | false, _ -> failwithf "Unknown type symbol: %A" sid
        // CILメンバーシグネチャに載せられない型は明示的に失敗
        | TypeId.Fn _ -> failwithf "Function type is not supported for CIL member signatures: %A" tid
        | TypeId.Meta _ -> failwithf "Unresolved meta type is not supported in Gen: %A" tid
        | TypeId.Error message -> failwithf "Cannot generate CIL for error type: %s" message

    // MIRの値をILスタックへ積む
    let rec private genValue (gen: ILGenerator) (value: Mir.Value) =
        match value with
        // 即値のロード
        | Mir.Value.ImmVal imm ->
            match imm with
            | Mir.Imm.Bool b -> if b then gen.Emit(OpCodes.Ldc_I4_1) else gen.Emit(OpCodes.Ldc_I4_0)
            | Mir.Imm.Int i -> gen.Emit(OpCodes.Ldc_I4, i)
            | Mir.Imm.Float f -> gen.Emit(OpCodes.Ldc_R8, f)
            | Mir.Imm.String s -> gen.Emit(OpCodes.Ldstr, s)
        // レジスタ値のロード
        | Mir.Value.RegVal reg ->
            match reg with
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Ldloc, index) // TODO Ldloc_0, Ldloc_1, Ldloc_2, Ldloc_3 を使う
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Ldarg, index) // TODO Ldarg_0, Ldarg_1, Ldarg_2, Ldarg_3 を使う
        // this経由でフィールドをロード
        | Mir.Value.FieldVal field ->
            genValue gen (Mir.Value.RegVal(Mir.Reg.Arg 0)) // Assuming 'this' is at Arg 0
            gen.Emit(OpCodes.Ldfld, field)

    // MIR命令をIL命令列へ変換する
    let private genIns (gen: ILGenerator) (ins: Mir.Ins) =
        match ins with
        // 単純代入
        | Mir.Ins.Assign (reg, value) ->
            genValue gen value
            match reg with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
        // 二項演算（TAC）
        | Mir.Ins.TAC (dest, arg1, op, arg2) ->
            genValue gen arg1
            genValue gen arg2
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
            genValue gen value
            gen.Emit(OpCodes.Ret)
        // 戻り値なしreturn
        | Mir.Ins.Ret ->
            gen.Emit(OpCodes.Ret)
        // 関数/コンストラクタ呼び出し
        | Mir.Ins.Call (method, args) ->
            for arg in args do
                genValue gen arg
            match method with
            | Choice1Of2 mi -> gen.Emit(OpCodes.Call, mi)
            | Choice2Of2 ci -> gen.Emit(OpCodes.Call, ci)
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
            genValue gen cond
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brtrue_S else OpCodes.Brtrue
            gen.Emit(op, label.get(gen))
        | Mir.Ins.JumpFalse (cond, label) ->
            genValue gen cond
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brfalse_S else OpCodes.Brfalse
            gen.Emit(op, label.get(gen))
        // 未対応命令は明示的に失敗
        | _ -> failwithf "Unsupported instruction: %A" ins

    // コンストラクタ本体を生成する
    let private genConstructor (_env: Env) (ctor: Mir.Constructor) =
        let gen = ctor.builder.GetILGenerator()

        // ローカル変数スロットを確保
        for KeyValue(_, reg) in ctor.frame.locs do
            match reg with
            | Mir.Reg.Loc _ -> gen.DeclareLocal(typeof<obj>) |> ignore
            | Mir.Reg.Arg _ -> ()

        // 本体命令を順に生成
        for ins in ctor.body do
            genIns gen ins

    // メソッド本体を生成する
    let private genMethod (_env: Env) (method: Mir.Method) =
        let gen = method.builder.GetILGenerator()

        // ローカル変数スロットを確保
        for KeyValue(_, reg) in method.frame.locs do
            match reg with
            | Mir.Reg.Loc _ -> gen.DeclareLocal(typeof<obj>) |> ignore
            | Mir.Reg.Arg _ -> ()

        // 本体命令を順に生成
        for ins in method.body do
            genIns gen ins

    // 型メンバー（フィールド/コンストラクタ/メソッド）を生成し、mainメソッドがあれば返す
    let private genType (env: Env) (typ: Mir.Type) : MethodInfo option =
        // フィールド定義
        for field in typ.fields do
            let fieldType = resolveType env field.typ
            field.builder <- typ.builder.DefineField(sprintf "field_%d" field.sym.id, fieldType, FieldAttributes.Public)

        // コンストラクタ定義
        for ctor in typ.ctors do
            let ctorArgTypes = ctor.args |> List.map (resolveType env) |> List.toArray
            ctor.builder <- typ.builder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, ctorArgTypes)
            genConstructor env ctor

        // メソッド定義
        for method in typ.methods do
            let methodArgTypes = method.args |> List.map (resolveType env) |> List.toArray
            let methodRetType = resolveType env method.ret
            method.builder <- typ.builder.DefineMethod(method.name, MethodAttributes.Public ||| MethodAttributes.Static, methodRetType, methodArgTypes)
            genMethod env method

        // 型確定
        typ.builder.CreateType() |> ignore

        // 型内main探索
        typ.builder.DeclaredMethods
        |> Seq.tryFind (fun method -> method.Name = "main")

    // モジュール単位で型とグローバル関数を生成し、mainメソッドを返す
    let private genModule (moduleBuilder: ModuleBuilder) (modul: Mir.Module) : MethodInfo option =
        // モジュール内型解決テーブルを初期化
        let env = { typeBuilders = Dictionary<SymbolId, TypeBuilder>() }

        // 型を先に宣言してTypeId.Name解決を可能にする
        for typ in modul.types do
            typ.builder <- moduleBuilder.DefineType(typ.name, TypeAttributes.Public)
            env.typeBuilders.Add(typ.sym, typ.builder)

        // 型内main探索
        let mainInTypes =
            modul.types
            |> List.tryPick (fun typ -> genType env typ)

        // グローバルメソッド生成
        for method in modul.methods do
            let methodArgTypes = method.args |> List.map (resolveType env) |> List.toArray
            let methodRetType = resolveType env method.ret
            method.builder <- moduleBuilder.DefineGlobalMethod(method.name, MethodAttributes.Public ||| MethodAttributes.Static, methodRetType, methodArgTypes)
            genMethod env method

        // グローバル関数確定
        moduleBuilder.CreateGlobalFunctions() |> ignore

        // <Module>.main を優先し、なければ型内mainを返す
        match moduleBuilder.GetMethod("main") with
        | null -> mainInTypes
        | methodInfo -> Some methodInfo

    // MIRアセンブリをPEファイルとして出力する
    let rec genAssembly (assembly: Mir.Assembly, filePath: string) =
        // アセンブリビルダー初期化
        assembly.builder <- PersistedAssemblyBuilder(AssemblyName(assembly.name), typeof<obj>.Assembly)

        // 各モジュールを生成して最初のmainメソッドを採用
        let mainMethod =
            assembly.modules
            |> List.fold (fun foundMain modul ->
                let moduleBuilder = assembly.builder.DefineDynamicModule(modul.name)
                let moduleMain = genModule moduleBuilder modul

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
                    entryPoint = MetadataTokens.MethodDefinitionHandle(methodInfo.MetadataToken))
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
