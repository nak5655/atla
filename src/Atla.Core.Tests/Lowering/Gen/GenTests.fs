namespace Atla.Core.Tests.Lowering.Gen

open System
open System.IO
open System.Reflection
open Xunit
open Atla.Core.Lowering
open Atla.Core.Lowering.Data
open Atla.Core.Semantics.Data

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
        match Gen.genAssembly(assembly, asmPath) with
        | { succeeded = true } -> ()
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            failwith $"Gen.genAssembly failed: {message}"

        let loaded = Assembly.LoadFile(Path.GetFullPath(asmPath))
        let useFoo =
            loaded.GetTypes()
            |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance))
            |> Array.find (fun m -> m.Name = "useFoo")

        Assert.NotNull(useFoo)
        Assert.Equal(1, useFoo.GetParameters().Length)
        Assert.Equal("Foo", useFoo.GetParameters().[0].ParameterType.Name)

    [<Fact>]
    let ``GenAssembly maps Unit return to CLR Void and preserves Int return`` () =
        let mainSym = SymbolId 201
        let intSym = SymbolId 202

        let unitMainMethod =
            Mir.Method(
                "main",
                mainSym,
                [],
                TypeId.Unit,
                [ Mir.Ins.Ret ],
                Mir.Frame())

        let intMethod =
            Mir.Method(
                "answer",
                intSym,
                [],
                TypeId.Int,
                [ Mir.Ins.RetValue(Mir.Value.ImmVal(Mir.Imm.Int 42)) ],
                Mir.Frame())

        let assembly =
            Mir.Assembly(
                "GenUnitVoidReturn",
                [ Mir.Module("MainModule", [], [ unitMainMethod; intMethod ]) ])

        let outputDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outputDir) |> ignore

        let asmPath = Path.Join(outputDir, "gen-unit-void-return.dll")
        match Gen.genAssembly(assembly, asmPath) with
        | { succeeded = true } -> ()
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            failwith $"Gen.genAssembly failed: {message}"

        let loaded = Assembly.LoadFile(Path.GetFullPath(asmPath))
        let globalsType = loaded.GetType("MainModule.Globals")

        Assert.NotNull(globalsType)

        let main = globalsType.GetMethod("main", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
        let answer = globalsType.GetMethod("answer", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)

        Assert.NotNull(main)
        Assert.NotNull(answer)
        Assert.Equal(typeof<Void>, main.ReturnType)
        Assert.Equal(typeof<int>, answer.ReturnType)

    [<Fact>]
    let ``GenAssembly maps App(ArrayCtor, String) argument to CLR string array`` () =
        let mainSym = SymbolId 301
        let firstSym = SymbolId 302

        let mainMethod =
            Mir.Method(
                "main",
                mainSym,
                [],
                TypeId.Unit,
                [ Mir.Ins.Ret ],
                Mir.Frame())

        let firstMethod =
            Mir.Method(
                "first",
                firstSym,
                [ TypeId.App(TypeId.Native typeof<System.Array>, [ TypeId.String ]) ],
                TypeId.String,
                [ Mir.Ins.RetValue(Mir.Value.ImmVal(Mir.Imm.String "ok")) ],
                Mir.Frame())

        let assembly =
            Mir.Assembly(
                "GenArrayStringArg",
                [ Mir.Module("MainModule", [], [ mainMethod; firstMethod ]) ])

        let outputDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outputDir) |> ignore

        let asmPath = Path.Join(outputDir, "gen-array-string-arg.dll")
        match Gen.genAssembly(assembly, asmPath) with
        | { succeeded = true } -> ()
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            failwith $"Gen.genAssembly failed: {message}"

        let loaded = Assembly.LoadFile(Path.GetFullPath(asmPath))
        let globalsType = loaded.GetType("MainModule.Globals")
        let first = globalsType.GetMethod("first", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)

        Assert.NotNull(first)
        Assert.Equal(1, first.GetParameters().Length)
        Assert.Equal(typeof<string[]>, first.GetParameters().[0].ParameterType)
        Assert.Equal(typeof<string>, first.ReturnType)
