namespace Atla.Compiler.Tests.Lowering.Gen

open System
open Xunit
open Atla.Compiler.Mir
open Atla.Compiler.Lowering
open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Reflection.Metadata.Ecma335

module GenTests =
    [<Fact>]
    let ``helloCIL`` () =
        let cir = Mir.Assembly("HelloCIL", [
            Mir.Module("MainModule", [], [
                Mir.Method("main", [], typeof<Void>, [
                    Mir.Ins.Call(Choice1Of2 (typeof<Console>.GetMethod("WriteLine", [| typeof<string> |])), [Mir.Value.ImmVal(Mir.Imm.String("Hello, World!"))])
                    Mir.Ins.Ret
                ], Frame() :> obj)
            ])
        ])

        if Directory.Exists("files") <> true then
            Directory.CreateDirectory("files") |> ignore

        let asmPath = Path.Join("files", "helloCIL.dll");
        let gen = Gen()
        gen.GenAssembly(cir, asmPath)

        let asm = Assembly.LoadFile(Path.GetFullPath(asmPath))
        Assert.True(asm.EntryPoint.IsStatic)
        Assert.Equal(typeof<Void>, asm.EntryPoint.ReturnType)
