namespace Atla.Compiler.Lowering

open System.Reflection
open System.Reflection.Emit
open Atla.Compiler.Cir
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Reflection.Metadata.Ecma335
open System.IO

type Gen() =
    let mutable mainMethodBuilder: MethodBuilder option = None

    let genIns (gen: ILGenerator) (ins: Cir.Ins) =
        match ins with
        | Cir.Ins.LdLoc index -> gen.Emit(OpCodes.Ldloc, index) // TODO Ldloc_0, Ldloc_1, Ldloc_2, Ldloc_3 を使う
        | Cir.Ins.StLoc index -> gen.Emit(OpCodes.Stloc, index) // TODO Stloc_0, Stloc_1, Stloc_2, Stloc_3 を使う
        | Cir.Ins.LdArg index -> gen.Emit(OpCodes.Ldarg, index) // TODO Ldarg_0, Ldarg_1, Ldarg_2, Ldarg_3 を使う
        | Cir.Ins.StArg index -> gen.Emit(OpCodes.Starg, index)
        | Cir.Ins.LdLocA index -> gen.Emit(OpCodes.Ldloca, index)
        | Cir.Ins.LdArgA index -> gen.Emit(OpCodes.Ldarga, index)
        | Cir.Ins.LdI32 value -> gen.Emit(OpCodes.Ldc_I4, value) // TODO Ldc_I4_0, ... Ldc_I4_8 を使う
        | Cir.Ins.LdF64 value -> gen.Emit(OpCodes.Ldc_R8, value)
        | Cir.Ins.LdStr str -> gen.Emit(OpCodes.Ldstr, str)
        | Cir.Ins.StFld fieldInfo -> gen.Emit(OpCodes.Stfld, fieldInfo)
        | Cir.Ins.LdFld fieldInfo -> gen.Emit(OpCodes.Ldfld, fieldInfo)
        | Cir.Ins.Add -> gen.Emit(OpCodes.Add)
        | Cir.Ins.Sub -> gen.Emit(OpCodes.Sub)
        | Cir.Ins.Mul -> gen.Emit(OpCodes.Mul)
        | Cir.Ins.Div -> gen.Emit(OpCodes.Div)
        | Cir.Ins.Rem -> gen.Emit(OpCodes.Rem)
        | Cir.Ins.Or -> gen.Emit(OpCodes.Or)
        | Cir.Ins.And -> gen.Emit(OpCodes.And)
        | Cir.Ins.Eq -> gen.Emit(OpCodes.Ceq)
        | Cir.Ins.Call method ->
            match method with
            | Choice1Of2 mi -> gen.Emit(OpCodes.Call, mi)
            | Choice2Of2 ci -> gen.Emit(OpCodes.Call, ci)
        | Cir.Ins.CallVirt mi -> gen.Emit(OpCodes.Callvirt, mi)
        | Cir.Ins.NewObj ctor -> gen.Emit(OpCodes.Newobj, ctor)
        | Cir.Ins.Ret -> gen.Emit(OpCodes.Ret)
        | Cir.Ins.BeginExceptionBlock -> gen.BeginExceptionBlock() |> ignore
        | Cir.Ins.BeginFinallyBlock -> gen.BeginFinallyBlock() |> ignore
        | Cir.Ins.EndExceptionBlock -> gen.EndExceptionBlock() |> ignore
        | Cir.Ins.Nop -> gen.Emit(OpCodes.Nop)
        | Cir.Ins.MarkLabel label ->
            label.ilOffset <- gen.ILOffset // ジャンプ距離を計算するためにオフセットを保持しておく
            gen.MarkLabel(label.get(gen))
        | Cir.Ins.Br label ->
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Br_S else OpCodes.Br;
            gen.Emit(op, label.get(gen))
        | Cir.Ins.BrTrue label ->
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brtrue_S else OpCodes.Brtrue;
            gen.Emit(op, label.get(gen))
        | Cir.Ins.BrFalse label ->
            // ジャンプ距離が十分短いときは省略形が使える(1byteまで) labelのILOffsetが未確定(負数)の場合に注意
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brfalse_S else OpCodes.Brfalse;
            gen.Emit(op, label.get(gen))

    let genConstructor (ctorBuilder: ConstructorBuilder) (ctor: Cir.Constructor) =
        let gen = ctorBuilder.GetILGenerator()

        for sym in ctor.frame.locs do
            gen.DeclareLocal(sym.typ) |> ignore

        for ins in ctor.body do
            genIns gen ins

    let genMethod (methodBuilder: MethodBuilder) (method: Cir.Method) =
        let gen = methodBuilder.GetILGenerator()

        for sym in method.frame.locs do
            gen.DeclareLocal(sym.typ) |> ignore

        for ins in method.body do
            genIns gen ins
            
    let genType (builder: TypeBuilder) (typ: Cir.Type) =
        for ctor in typ.ctors do
            let ctorBuilder = builder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, List.toArray ctor.args)
            genConstructor ctorBuilder ctor

        for method in typ.methods do
            let methodBuilder = builder.DefineMethod(method.name, MethodAttributes.Public ||| MethodAttributes.Static, method.ret, List.toArray method.args)
            genMethod methodBuilder method

    let genModule (builder: ModuleBuilder) (modul: Cir.Module) =
        for typ in modul.types do
            let typeBuilder = builder.DefineType(typ.name, TypeAttributes.Public)
            genType typeBuilder typ
        
        for method in modul.methods do
            let methodBuilder = builder.DefineGlobalMethod(method.name, MethodAttributes.Public ||| MethodAttributes.Static, method.ret, List.toArray method.args)
            genMethod methodBuilder method

            // main関数を見つけたら、後でエントリポイントに指定するために保存しておく
            if method.name = "main" then
                mainMethodBuilder <- Some methodBuilder

    let genAssembly (assembly: Cir.Assembly) (filePath: string)  =
        let builder = PersistedAssemblyBuilder(AssemblyName(assembly.name), typeof<obj>.Assembly)
        for modul in assembly.modules do
            let moduleBuilder = builder.DefineDynamicModule(modul.name)
            genModule moduleBuilder modul

        match mainMethodBuilder with
        | Some m ->
            // EntryPointを指定してビルドするために、ILとフィールドデータを手動で生成してPEファイルを作成する
            let mutable ilStream = BlobBuilder()
            let mutable fieldData = BlobBuilder()
            let metadataBuilder = builder.GenerateMetadata(ref ilStream, ref fieldData)

            let peBuilder = ManagedPEBuilder(PEHeaderBuilder.CreateExecutableHeader(), MetadataRootBuilder(metadataBuilder), ilStream, fieldData, entryPoint = MetadataTokens.MethodDefinitionHandle(m.MetadataToken))

            let peBlob = BlobBuilder()
            peBuilder.Serialize(peBlob) |> ignore

            use fs = new FileStream(filePath, FileMode.Create, FileAccess.Write)

            peBlob.WriteContentTo(fs)

        | _ -> builder.Save(filePath)