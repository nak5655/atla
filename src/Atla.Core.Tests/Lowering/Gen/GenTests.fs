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

        let mirType = Mir.Type("Foo", typeSym, None, [], [], [])

        let mainMethod =
            Mir.Method(
                "main",
                mainSym,
                [],
                TypeId.Unit,
                [ Mir.Ins.Ret ],
                Mir.Frame.empty)

        let useFooMethod =
            Mir.Method(
                "useFoo",
                useFooSym,
                [ TypeId.Name typeSym ],
                TypeId.Unit,
                [ Mir.Ins.Ret ],
                Mir.Frame.empty)

        let assembly =
            Mir.Assembly(
                "GenTypeResolution",
                [ Mir.Module("MainModule", [ mirType ], [ mainMethod; useFooMethod ]) ])

        let outputDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outputDir) |> ignore

        let asmPath = Path.Join(outputDir, "gen-type-resolution.dll")
        match Gen.genAssembly(assembly, asmPath, SymbolTable()) with
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
                Mir.Frame.empty)

        let intMethod =
            Mir.Method(
                "answer",
                intSym,
                [],
                TypeId.Int,
                [ Mir.Ins.RetValue(Mir.Value.ImmVal(Mir.Imm.Int 42)) ],
                Mir.Frame.empty)

        let assembly =
            Mir.Assembly(
                "GenUnitVoidReturn",
                [ Mir.Module("MainModule", [], [ unitMainMethod; intMethod ]) ])

        let outputDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outputDir) |> ignore

        let asmPath = Path.Join(outputDir, "gen-unit-void-return.dll")
        match Gen.genAssembly(assembly, asmPath, SymbolTable()) with
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
                Mir.Frame.empty)

        let firstMethod =
            Mir.Method(
                "first",
                firstSym,
                [ TypeId.App(TypeId.Native typeof<System.Array>, [ TypeId.String ]) ],
                TypeId.String,
                [ Mir.Ins.RetValue(Mir.Value.ImmVal(Mir.Imm.String "ok")) ],
                Mir.Frame.empty)

        let assembly =
            Mir.Assembly(
                "GenArrayStringArg",
                [ Mir.Module("MainModule", [], [ mainMethod; firstMethod ]) ])

        let outputDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outputDir) |> ignore

        let asmPath = Path.Join(outputDir, "gen-array-string-arg.dll")
        match Gen.genAssembly(assembly, asmPath, SymbolTable()) with
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

    /// static literal field（enum メンバー）を ldsfld せず即値として emit できることを検証する。
    [<Fact>]
    let ``GenAssembly emits enum literal static field as constant`` () =
        let mainSym = SymbolId 350
        let enumField = typeof<DayOfWeek>.GetField("Monday", BindingFlags.Public ||| BindingFlags.Static)

        Assert.NotNull(enumField)

        let mainMethod =
            Mir.Method(
                "main",
                mainSym,
                [],
                TypeId.Native typeof<DayOfWeek>,
                [ Mir.Ins.RetValue(Mir.Value.FieldVal enumField) ],
                Mir.Frame.empty)

        let assembly =
            Mir.Assembly(
                "GenEnumLiteralField",
                [ Mir.Module("MainModule", [], [ mainMethod ]) ])

        let outputDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outputDir) |> ignore

        let asmPath = Path.Join(outputDir, "gen-enum-literal-field.dll")
        match Gen.genAssembly(assembly, asmPath, SymbolTable()) with
        | { succeeded = true } -> ()
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            failwith $"Gen.genAssembly failed: {message}"

        let loaded = Assembly.LoadFile(Path.GetFullPath(asmPath))
        let globalsType = loaded.GetType("MainModule.Globals")
        Assert.NotNull(globalsType)

        let main = globalsType.GetMethod("main", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
        Assert.NotNull(main)
        Assert.Equal(typeof<DayOfWeek>, main.ReturnType)

        let result = main.Invoke(null, [||]) :?> DayOfWeek
        Assert.Equal(DayOfWeek.Monday, result)

    /// 高階関数の CIL 生成が正しくデリゲート経由の呼び出しを発行することを検証する。
    /// `apply (f: Int -> Int) (x: Int): Int = f x` を CIL に変換し、
    /// Func<int,int> 型の引数を受け取って正しい結果を返すことを確認する。
    [<Fact>]
    let ``GenAssembly emits correct delegate-call CIL for higher-order function parameter`` () =
        let applySym = SymbolId 401
        let fSid = SymbolId 402
        let xSid = SymbolId 403

        // apply の frame: f=Arg(0): Func<int,int>, x=Arg(1): int, retVal=Loc(0): int
        let _, applyFrame0 = Mir.Frame.addArg fSid (TypeId.Fn([ TypeId.Int ], TypeId.Int)) Mir.Frame.empty
        let _, applyFrame1 = Mir.Frame.addArg xSid TypeId.Int applyFrame0
        // 戻り値を保持するローカル変数を登録する。
        let retLocSid = SymbolId 404
        let _, applyFrame = Mir.Frame.addLoc retLocSid TypeId.Int applyFrame1

        // Func<int,int>::Invoke(int):int を取得する。
        let funcIntInt = typeof<System.Func<int, int>>
        let invokeMethod = funcIntInt.GetMethod("Invoke")

        // apply の本体: f(x) → callvirt Func<int,int>::Invoke(Arg(1)) に Arg(0) をレシーバとして渡す。
        let applyRetReg = Mir.Reg.Loc 0
        let applyMethod =
            Mir.Method(
                "apply",
                applySym,
                [ TypeId.Fn([ TypeId.Int ], TypeId.Int); TypeId.Int ],
                TypeId.Int,
                [ Mir.Ins.CallAssign(applyRetReg, invokeMethod, [ Mir.Value.RegVal(Mir.Reg.Arg 0); Mir.Value.RegVal(Mir.Reg.Arg 1) ])
                  Mir.Ins.RetValue(Mir.Value.RegVal applyRetReg) ],
                applyFrame)

        let assembly =
            Mir.Assembly(
                "GenHigherOrder",
                [ Mir.Module("MainModule", [], [ applyMethod ]) ])

        let outputDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outputDir) |> ignore

        let asmPath = Path.Join(outputDir, "gen-higher-order.dll")
        match Gen.genAssembly(assembly, asmPath, SymbolTable()) with
        | { succeeded = true } -> ()
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            failwith $"Gen.genAssembly failed: {message}"

        let loaded = Assembly.LoadFile(Path.GetFullPath(asmPath))
        let globalsType = loaded.GetType("MainModule.Globals")
        Assert.NotNull(globalsType)

        // `apply` は Func<int,int> × int -> int のシグネチャを持つ。
        let applyMi = globalsType.GetMethod("apply", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
        Assert.NotNull(applyMi)
        Assert.Equal(2, applyMi.GetParameters().Length)
        Assert.Equal(typeof<System.Func<int, int>>, applyMi.GetParameters().[0].ParameterType)
        Assert.Equal(typeof<int>, applyMi.GetParameters().[1].ParameterType)
        Assert.Equal(typeof<int>, applyMi.ReturnType)

        // Func<int,int> デリゲートを経由して正しい計算結果が得られることを確認する (5 * 2 = 10)。
        let delegateTwice = System.Func<int, int>(fun x -> x + x)
        let result = applyMi.Invoke(null, [| delegateTwice; 5 |])
        Assert.Equal(10, result :?> int)

    /// target 付き FnDelegate が ldarg/ldloc を使って正しく生成されることを検証する。
    [<Fact>]
    let ``GenAssembly creates delegate with bound target for static lifted method`` () =
        let liftedSym = SymbolId 501
        let mainSym = SymbolId 502
        let envSid = SymbolId 503

        // lifted(env: string, x: int): int = x
        // target に env を束縛した Func<int,int> を生成できれば、引数1つで呼び出せる。
        let _, liftedFrame0 = Mir.Frame.addArg envSid TypeId.String Mir.Frame.empty
        let xSid = SymbolId 504
        let _, liftedFrame = Mir.Frame.addArg xSid TypeId.Int liftedFrame0
        let liftedMethod =
            Mir.Method(
                "lifted",
                liftedSym,
                [ TypeId.String; TypeId.Int ],
                TypeId.Int,
                [ Mir.Ins.RetValue(Mir.Value.RegVal(Mir.Reg.Arg 1)) ],
                liftedFrame)

        let _, mainFrame = Mir.Frame.addArg envSid TypeId.String Mir.Frame.empty
        let mainMethod =
            Mir.Method(
                "main",
                mainSym,
                [ TypeId.String ],
                TypeId.Fn([ TypeId.Int ], TypeId.Int),
                [ Mir.Ins.RetValue(Mir.Value.FnDelegate(liftedSym, typeof<System.Func<int, int>>, Some(Mir.Reg.Arg 0))) ],
                mainFrame)

        let assembly =
            Mir.Assembly(
                "GenBoundTargetDelegate",
                [ Mir.Module("MainModule", [], [ liftedMethod; mainMethod ]) ])

        let outputDir = Path.Join(Path.GetTempPath(), "atla-tests")
        Directory.CreateDirectory(outputDir) |> ignore

        let asmPath = Path.Join(outputDir, "gen-bound-target-delegate.dll")
        match Gen.genAssembly(assembly, asmPath, SymbolTable()) with
        | { succeeded = true } -> ()
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            failwith $"Gen.genAssembly failed: {message}"

        let loaded = Assembly.LoadFile(Path.GetFullPath(asmPath))
        let globalsType = loaded.GetType("MainModule.Globals")
        Assert.NotNull(globalsType)

        let mainMi = globalsType.GetMethod("main", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
        Assert.NotNull(mainMi)
        let delObj = mainMi.Invoke(null, [| "captured-env" |]) :?> System.Func<int, int>
        let result = delObj.Invoke(7)
        Assert.Equal(7, result)
