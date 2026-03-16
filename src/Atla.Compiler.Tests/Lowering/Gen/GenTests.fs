namespace Atla.Compiler.Tests.Lowering.Gen

open System
open Xunit
open Atla.Compiler.Cir
open Atla.Compiler.Types
open Atla.Compiler.Parsing
open Atla.Compiler.Hir
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
        let cir = Cir.Assembly("HelloCIL", [
            Cir.Module("MainModule", [], [
                Cir.Method("main", [], typeof<Void>, Frame(), [
                    Cir.Ins.LdStr "Hello, World!"
                    Cir.Ins.Call(Choice1Of2 (typeof<Console>.GetMethod("WriteLine", [| typeof<string> |])))
                    Cir.Ins.Ret
                ])
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
