namespace Atla.Compiler.Tests.Lowering.Gen

open System
open System.IO
open System.Reflection
open Xunit
open Atla.Compiler.Lowering
open Atla.Compiler.Lowering.Data
open Atla.Compiler.Semantics.Data

module GenTests =
    [<Fact>]
    let ``GenAssembly resolves TypeId.Name from module type table`` () =
        let typeSym = SymbolId 100
        let mainSym = SymbolId 101
        let useFooSym = SymbolId 102

        let mirType = Mir.Type("Foo", typeSym, [], [], [])

        let mainMethod =
            Mir.Method(
                "main",
                mainSym,
                [],
                TypeId.Unit,
                [ Mir.Ins.Ret ],
                Mir.Frame())

        let useFooMethod =
            Mir.Method(
                "useFoo",
                useFooSym,
                [ TypeId.Name typeSym ],
                TypeId.Unit,
                [ Mir.Ins.Ret ],
                Mir.Frame())

        let assembly =
            Mir.Assembly(
                "GenTypeResolution",
                [ Mir.Module("MainModule", [ mirType ], [ mainMethod; useFooMethod ]) ])

        let outputDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outputDir) |> ignore

        let asmPath = Path.Join(outputDir, "gen-type-resolution.dll")
        Gen.genAssembly(assembly, asmPath)

        let loaded = Assembly.LoadFile(Path.GetFullPath(asmPath))
        let useFoo = loaded.ManifestModule.GetMethods() |> Array.find (fun m -> m.Name = "useFoo")

        Assert.NotNull(useFoo)
        Assert.Equal(1, useFoo.GetParameters().Length)
        Assert.Equal("Foo", useFoo.GetParameters().[0].ParameterType.Name)
