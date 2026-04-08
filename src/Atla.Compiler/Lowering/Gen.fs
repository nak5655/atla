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
    type Env =
        { typeBuilders: Dictionary<SymbolId, TypeBuilder> }

    let private resolveType (env: Env) (typ: TypeId) : Type =
        match typ with
        | TypeId.Unit -> typeof<Void>
        | TypeId.Bool -> typeof<bool>
        | TypeId.Int -> typeof<int>
        | TypeId.Float -> typeof<float>
        | TypeId.String -> typeof<string>
        | TypeId.Native t -> t
        | TypeId.Name sym ->
            match env.typeBuilders.TryGetValue(sym) with
            | true, builder -> builder :> Type
            | false, _ -> failwithf "Unknown type symbol: %A" sym
        | TypeId.Fn _ -> failwithf "Function type is not supported for CIL member signatures: %A" typ
        | TypeId.Meta _ -> failwithf "Unresolved meta type is not supported in Gen: %A" typ
        | TypeId.Error message -> failwithf "Cannot generate CIL for error type: %s" message

    let rec private genValue (gen: ILGenerator) (value: Mir.Value) =
        match value with
        | Mir.Value.ImmVal imm ->
            match imm with
            | Mir.Imm.Bool b -> if b then gen.Emit(OpCodes.Ldc_I4_1) else gen.Emit(OpCodes.Ldc_I4_0)
            | Mir.Imm.Int i -> gen.Emit(OpCodes.Ldc_I4, i)
            | Mir.Imm.Float f -> gen.Emit(OpCodes.Ldc_R8, f)
            | Mir.Imm.String s -> gen.Emit(OpCodes.Ldstr, s)
        | Mir.Value.RegVal reg ->
            match reg with
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Ldloc, index)
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Ldarg, index)
        | Mir.Value.FieldVal field ->
            genValue gen (Mir.Value.RegVal(Mir.Reg.Arg 0))
            gen.Emit(OpCodes.Ldfld, field)

    let private genIns (gen: ILGenerator) (ins: Mir.Ins) =
        match ins with
        | Mir.Ins.Assign (reg, value) ->
            genValue gen value
            match reg with
            | Mir.Reg.Arg index -> gen.Emit(OpCodes.Starg, index)
            | Mir.Reg.Loc index -> gen.Emit(OpCodes.Stloc, index)
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
            label.ilOffset <- gen.ILOffset
            gen.MarkLabel(label.get(gen))
        | Mir.Ins.Jump label ->
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Br_S else OpCodes.Br
            gen.Emit(op, label.get(gen))
        | Mir.Ins.JumpTrue (cond, label) ->
            genValue gen cond
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brtrue_S else OpCodes.Brtrue
            gen.Emit(op, label.get(gen))
        | Mir.Ins.JumpFalse (cond, label) ->
            genValue gen cond
            let offset = label.ilOffset - gen.ILOffset
            let op = if (0 < label.ilOffset && -120 < offset && offset < 120) then OpCodes.Brfalse_S else OpCodes.Brfalse
            gen.Emit(op, label.get(gen))
        | _ -> failwithf "Unsupported instruction: %A" ins

    let private genConstructor (_env: Env) (ctor: Mir.Constructor) =
        let gen = ctor.builder.GetILGenerator()

        for KeyValue(_, reg) in ctor.frame.locs do
            match reg with
            | Mir.Reg.Loc _ -> gen.DeclareLocal(typeof<obj>) |> ignore
            | Mir.Reg.Arg _ -> ()

        for ins in ctor.body do
            genIns gen ins

    let private genMethod (_env: Env) (method: Mir.Method) =
        let gen = method.builder.GetILGenerator()

        for KeyValue(_, reg) in method.frame.locs do
            match reg with
            | Mir.Reg.Loc _ -> gen.DeclareLocal(typeof<obj>) |> ignore
            | Mir.Reg.Arg _ -> ()

        for ins in method.body do
            genIns gen ins

    let private genType (env: Env) (typ: Mir.Type) : MethodInfo option =
        for field in typ.fields do
            let fieldType = resolveType env field.typ
            field.builder <- typ.builder.DefineField(sprintf "field_%d" field.sym.id, fieldType, FieldAttributes.Public)

        for ctor in typ.ctors do
            let ctorArgTypes = ctor.args |> List.map (resolveType env) |> List.toArray
            ctor.builder <- typ.builder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, ctorArgTypes)
            genConstructor env ctor

        for method in typ.methods do
            let methodArgTypes = method.args |> List.map (resolveType env) |> List.toArray
            let methodRetType = resolveType env method.ret
            method.builder <- typ.builder.DefineMethod(method.name, MethodAttributes.Public ||| MethodAttributes.Static, methodRetType, methodArgTypes)
            genMethod env method

        typ.builder.CreateType() |> ignore

        typ.builder.DeclaredMethods
        |> Seq.tryFind (fun method -> method.Name = "main")

    let private genModule (moduleBuilder: ModuleBuilder) (modul: Mir.Module) : MethodInfo option =
        let env = { typeBuilders = Dictionary<SymbolId, TypeBuilder>() }

        for typ in modul.types do
            typ.builder <- moduleBuilder.DefineType(typ.name, TypeAttributes.Public)
            env.typeBuilders.Add(typ.sym, typ.builder)

        let mainInTypes =
            modul.types
            |> List.tryPick (fun typ -> genType env typ)

        for method in modul.methods do
            let methodArgTypes = method.args |> List.map (resolveType env) |> List.toArray
            let methodRetType = resolveType env method.ret
            method.builder <- moduleBuilder.DefineGlobalMethod(method.name, MethodAttributes.Public ||| MethodAttributes.Static, methodRetType, methodArgTypes)
            genMethod env method

        moduleBuilder.CreateGlobalFunctions() |> ignore

        match moduleBuilder.GetMethod("main") with
        | null -> mainInTypes
        | methodInfo -> Some methodInfo

    let rec genAssembly (assembly: Mir.Assembly, filePath: string) =
        assembly.builder <- PersistedAssemblyBuilder(AssemblyName(assembly.name), typeof<obj>.Assembly)

        let mainMethod =
            assembly.modules
            |> List.fold (fun foundMain modul ->
                let moduleBuilder = assembly.builder.DefineDynamicModule(modul.name)
                let moduleMain = genModule moduleBuilder modul

                match foundMain with
                | Some _ -> foundMain
                | None -> moduleMain) None

        let tfaCtor = typeof<TargetFrameworkAttribute>.GetConstructor([| typeof<string> |])
        let tfa = CustomAttributeBuilder(tfaCtor, [| ".NETCoreApp,Version=v10.0" |])
        assembly.builder.SetCustomAttribute(tfa)

        let mutable ilStream = BlobBuilder()
        let mutable fieldData = BlobBuilder()
        let metadataBuilder = assembly.builder.GenerateMetadata(&ilStream, &fieldData)

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

        let peBlob = BlobBuilder()
        peBuilder.Serialize(peBlob) |> ignore

        use fs = new FileStream(filePath, FileMode.Create, FileAccess.Write)
        peBlob.WriteContentTo(fs)

        genRuntimeConfig filePath

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
