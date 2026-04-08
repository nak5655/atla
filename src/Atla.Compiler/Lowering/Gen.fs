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

type Gen() =
    let mutable mainMethod: MethodInfo option = None
    
    let resolveType (typeBuilders: Dictionary<SymbolId, TypeBuilder>) (typ: TypeId) : Type =
        match typ with
        | TypeId.Unit -> typeof<Void>
        | TypeId.Bool -> typeof<bool>
        | TypeId.Int -> typeof<int>
        | TypeId.Float -> typeof<float>
        | TypeId.String -> typeof<string>
        | TypeId.Native t -> t
        | TypeId.Name sym ->
            match typeBuilders.TryGetValue(sym) with
            | true, builder -> builder :> Type
            | false, _ -> failwithf "Unknown type symbol: %A" sym
        | TypeId.Fn _ -> failwithf "Function type is not supported for CIL member signatures: %A" typ
        | TypeId.Meta _ -> failwithf "Unresolved meta type is not supported in Gen: %A" typ
        | TypeId.Error message -> failwithf "Cannot generate CIL for error type: %s" message

    let rec genValue (gen: ILGenerator) (value: Mir.Value) =
        match value with
        | Mir.Value.ImmVal imm ->
            match imm with
            | Mir.Imm.Bool b -> if b then gen.Emit(OpCodes.Ldc_I4_1) else gen.Emit(OpCodes.Ldc_I4_0)
            | Mir.Imm.Int i -> gen.Emit(OpCodes.Ldc_I4, i)
            | Mir.Imm.Float f -> gen.Emit(OpCodes.Ldc_R8, f)
            | Mir.Imm.String s -> gen.Emit(OpCodes.Ldstr, s)
        | Mir.Value.RegVal reg ->
            match reg with
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Ldloc, index) // TODO Ldloc_0, Ldloc_1, Ldloc_2, Ldloc_3 を使う
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Ldarg, index) // TODO Ldarg_0, Ldarg_1, Ldarg_2, Ldarg_3 を使う
        | Mir.Value.FieldVal (field) ->
            genValue gen (Mir.Value.RegVal (Mir.Reg.Arg 0)) // Assuming 'this' is at Arg 0
            gen.Emit(OpCodes.Ldfld, field)

    let genIns (gen: ILGenerator) (ins: Mir.Ins) =
        match ins with
        | Mir.Ins.Assign (reg, value) ->
            genValue gen value
            match reg with
            | Mir.Reg.Arg index ->
                gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index ->
                gen.Emit(OpCodes.Stloc, index)
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
            | Mir.Reg.Arg index ->
                gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index ->
                gen.Emit(OpCodes.Stloc, index)
        | Mir.Ins.RetValue value ->
            genValue gen value
            gen.Emit(OpCodes.Ret)
        | Mir.Ins.Ret ->
            gen.Emit(OpCodes.Ret)
        | Mir.Ins.Call (method, args) ->
            for arg in args do
                genValue gen arg
            match method with
            | Choice1Of2 mi -> gen.Emit(OpCodes.Call, mi)
            | Choice2Of2 ci -> gen.Emit(OpCodes.Call, ci)
        | Mir.Ins.MarkLabel label ->
            label.ilOffset <- gen.ILOffset // ジャンプ距離を計算するためにオフセットを保持しておく
            gen.MarkLabel(label.get(gen))
        | Mir.Ins.Jump label ->
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Br_S else OpCodes.Br;
            gen.Emit(op, label.get(gen))
        | Mir.Ins.JumpTrue (cond, label) ->
            genValue gen cond
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brtrue_S else OpCodes.Brtrue;
            gen.Emit(op, label.get(gen))
        | Mir.Ins.JumpFalse (cond, label) ->
            genValue gen cond
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brfalse_S else OpCodes.Brfalse;
            gen.Emit(op, label.get(gen))
        | _ -> failwithf "Unsupported instruction: %A" ins

    let genConstructor (_: TypeId -> Type) (ctor: Mir.Constructor) =
        let gen = ctor.builder.GetILGenerator()
        let frame = ctor.frame

        for KeyValue(_, reg) in frame.locs do
            match reg with
            | Mir.Reg.Loc _ -> gen.DeclareLocal(typeof<obj>) |> ignore
            | Mir.Reg.Arg _ -> ()

        for ins in ctor.body do
            genIns gen ins

    let genMethod (_: TypeId -> Type) (method: Mir.Method) =
        let gen = method.builder.GetILGenerator()
        let frame = method.frame

        for KeyValue(_, reg) in frame.locs do
            match reg with
            | Mir.Reg.Loc _ -> gen.DeclareLocal(typeof<obj>) |> ignore
            | Mir.Reg.Arg _ -> ()

        for ins in method.body do
            genIns gen ins
            
    let genType (resolveMirType: TypeId -> Type) (typ: Mir.Type) =
        for field in typ.fields do
            let fieldType = resolveMirType field.typ
            field.builder <- typ.builder.DefineField(sprintf "field_%d" field.sym.id, fieldType, FieldAttributes.Public)

        for ctor in typ.ctors do
            let ctorArgTypes = ctor.args |> List.map resolveMirType |> List.toArray
            ctor.builder <- typ.builder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, ctorArgTypes)
            genConstructor resolveMirType ctor

        for method in typ.methods do
            let methodArgTypes = method.args |> List.map resolveMirType |> List.toArray
            let methodRetType = resolveMirType method.ret
            method.builder <- typ.builder.DefineMethod(method.name, MethodAttributes.Public ||| MethodAttributes.Static, methodRetType, methodArgTypes)
            genMethod resolveMirType method
            
        typ.builder.CreateType() |> ignore
        
        // main関数を見つけたら、エントリポイントに指定するために保存しておく
        for method in typ.builder.DeclaredMethods do
            if method.Name = "main" then
                mainMethod <- Some method

    let genModule (builder: ModuleBuilder) (modul: Mir.Module) =
        let typeBuilders = Dictionary<SymbolId, TypeBuilder>()

        for typ in modul.types do
            typ.builder <- modul.builder.DefineType(typ.name, TypeAttributes.Public)
            typeBuilders.Add(typ.sym, typ.builder)

        let resolveMirTypeInModule = resolveType typeBuilders

        for typ in modul.types do
            genType resolveMirTypeInModule typ
        
        for method in modul.methods do
            let methodArgTypes = method.args |> List.map resolveMirTypeInModule |> List.toArray
            let methodRetType = resolveMirTypeInModule method.ret
            method.builder <- builder.DefineGlobalMethod(method.name, MethodAttributes.Public ||| MethodAttributes.Static, methodRetType, methodArgTypes)
            genMethod resolveMirTypeInModule method

        builder.CreateGlobalFunctions() |> ignore
        
    member this.GenAssembly (assembly: Mir.Assembly, filePath: string)  =
        assembly.builder <- System.Reflection.Emit.PersistedAssemblyBuilder(AssemblyName(assembly.name), typeof<obj>.Assembly)
        for modul in assembly.modules do
            let moduleBuilder = assembly.builder.DefineDynamicModule(modul.name)
            genModule moduleBuilder modul

            // main関数を見つけたら、エントリポイントに指定するために保存しておく
            match moduleBuilder.GetMethod("main") with
            | null -> ()
            | mm -> mainMethod <- Some mm

        // ターゲットフレームワークを指定するために、TargetFrameworkAttributeをアセンブリに追加する
        let tfaCtor = typeof<TargetFrameworkAttribute>.GetConstructor([| typeof<string> |])
        let tfa = CustomAttributeBuilder(tfaCtor, [| ".NETCoreApp,Version=v10.0" |])
        assembly.builder.SetCustomAttribute(tfa)

        // EntryPointを指定してビルドするために、ILとフィールドデータを手動で生成してPEファイルを作成する
        let mutable ilStream = BlobBuilder()
        let mutable fieldData = BlobBuilder()
        let metadataBuilder = assembly.builder.GenerateMetadata(&ilStream, &fieldData)

        let peBuilder =
            match mainMethod with 
            | Some m -> ManagedPEBuilder(
                header = PEHeaderBuilder.CreateExecutableHeader(),
                metadataRootBuilder = MetadataRootBuilder(metadataBuilder),
                ilStream = ilStream,
                mappedFieldData = fieldData,
                entryPoint = MetadataTokens.MethodDefinitionHandle(m.MetadataToken))
            | _ -> ManagedPEBuilder(
                header = PEHeaderBuilder.CreateExecutableHeader(),
                metadataRootBuilder = MetadataRootBuilder(metadataBuilder),
                ilStream = ilStream,
                mappedFieldData = fieldData)

        let peBlob = BlobBuilder()
        peBuilder.Serialize(peBlob) |> ignore

        use fs = new FileStream(filePath, FileMode.Create, FileAccess.Write)

        peBlob.WriteContentTo(fs)

        // dotnetコマンドで実行するためのランタイム構成ファイルを生成する
        this.GenRuntimeConfig(filePath)

    member this.GenRuntimeConfig (assemblyPath: string) =
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
