namespace Atla.Core.Lowering

open Atla.Core.Semantics.Data
open Atla.Core.Lowering.Data

module Layout =
    type private KNormal = {
        ins: Mir.Ins list
        res: Mir.Value option
    }

    let private declareTemp (frame: Mir.Frame) (tid: TypeId) : Mir.Reg =
        let rec freshTempSid (candidate: int) : SymbolId =
            let sid = SymbolId candidate
            match frame.get sid with
            | Some _ -> freshTempSid (candidate - 1)
            | None -> sid
        let sid = freshTempSid -1
        frame.addLoc(sid, tid)

    let rec private layoutExpr (frame: Mir.Frame) (expr: Hir.Expr) : KNormal =
        match expr with
        | Hir.Expr.Unit _ -> { ins = []; res = None }
        | Hir.Expr.Int (value, _) -> { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Int value)) }
        | Hir.Expr.Float (value, _) -> { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Float value)) }
        | Hir.Expr.String (value, _) -> { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.String value)) }
        // null リテラル: 参照型のオプショナル引数デフォルト値として使用する
        | Hir.Expr.Null _ -> { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Null)) }
        | Hir.Expr.Id (sid, tid, _) ->
            match frame.get sid with
            | Some reg -> { ins = []; res = Some(Mir.Value.RegVal reg) }
            | None ->
                // フレームに存在しない sid は「グローバル関数参照」として扱う。
                // TypeId.Fn の場合はデリゲートに変換し、それ以外はメソッド引数として追加する。
                match tid with
                | TypeId.Fn _ ->
                    match TypeId.tryToRuntimeSystemType tid with
                    | Some delegateType ->
                        // グローバル関数を対応するデリゲート（Func<>/Action<>）に包む。
                        { ins = []; res = Some(Mir.Value.FnDelegate(sid, delegateType, None)) }
                    | None ->
                        let argReg = frame.addArg(sid, tid)
                        { ins = []; res = Some(Mir.Value.RegVal argReg) }
                // .NET デリゲート型（Native）で注釈された関数参照はその型のまま使用する。
                | TypeId.Native t when TypeId.isDelegateType t ->
                    { ins = []; res = Some(Mir.Value.FnDelegate(sid, t, None)) }
                | _ ->
                    let argReg = frame.addArg(sid, tid)
                    { ins = []; res = Some(Mir.Value.RegVal argReg) }
        | Hir.Expr.MemberAccess (mem, instance, tid, span) ->
            match mem with
            | Hir.Member.NativeField fi -> { ins = []; res = Some(Mir.Value.FieldVal fi) }
            | Hir.Member.NativeMethod mi -> { ins = []; res = Some(Mir.Value.MethodVal mi) }
            | Hir.Member.NativeProperty pi ->
                match pi.GetMethod with
                | null -> failwithf "Property has no getter: %s" pi.Name
                | getter ->
                    let instanceKn =
                        match instance with
                        | Some expr -> Some(layoutExpr frame expr)
                        | None -> None

                    let instanceIns =
                        instanceKn
                        |> Option.map (fun kn -> kn.ins)
                        |> Option.defaultValue []

                    let instanceArg =
                        instanceKn
                        |> Option.bind (fun kn -> kn.res)
                        |> Option.map List.singleton
                        |> Option.defaultValue []

                    if tid = TypeId.Unit || getter.ReturnType = typeof<System.Void> then
                        { ins = instanceIns @ [ Mir.Ins.Call(Choice1Of2 getter, instanceArg) ]; res = None }
                    else
                        let dst = declareTemp frame tid
                        { ins = instanceIns @ [ Mir.Ins.CallAssign(dst, getter, instanceArg) ]; res = Some(Mir.Value.RegVal dst) }
        | Hir.Expr.Call (func, _, args, tid, _) ->
            let argKn = args |> List.map (layoutExpr frame)
            let argIns = argKn |> List.collect (fun k -> k.ins)
            let argValues =
                argKn
                |> List.map (fun k ->
                    match k.res with
                    | Some value -> value
                    | None -> failwith "Argument expression did not produce a value")

            let emitCall (mi: System.Reflection.MethodInfo) =
                let returnsUnit = (mi.ReturnType = typeof<System.Void>)
                match tid, returnsUnit with
                | _, true
                | TypeId.Unit, _ ->
                    { ins = argIns @ [ Mir.Ins.Call(Choice1Of2 mi, argValues) ]; res = None }
                | _ ->
                    let dst = declareTemp frame tid
                    { ins = argIns @ [ Mir.Ins.CallAssign(dst, mi, argValues) ]; res = Some(Mir.Value.RegVal dst) }

            match func with
            | Hir.Callable.NativeMethod mi -> emitCall mi
            | Hir.Callable.NativeMethodGroup methods ->
                match methods |> List.tryFind (fun m -> m.GetParameters().Length = argValues.Length) with
                | Some mi -> emitCall mi
                | None -> failwithf "No overload matched argument count %d" argValues.Length
            | Hir.Callable.NativeConstructor ctor ->
                let dst = declareTemp frame tid
                { ins = argIns @ [ Mir.Ins.New(dst, ctor, argValues) ]; res = Some(Mir.Value.RegVal dst) }
            | Hir.Callable.NativeConstructorGroup ctors ->
                match ctors |> List.tryFind (fun c -> c.GetParameters().Length = argValues.Length) with
                | Some ctor ->
                    let dst = declareTemp frame tid
                    { ins = argIns @ [ Mir.Ins.New(dst, ctor, argValues) ]; res = Some(Mir.Value.RegVal dst) }
                | None -> failwithf "No constructor matched argument count %d" argValues.Length
            | Hir.Callable.BuiltinOperator op ->
                let dst = declareTemp frame tid
                let lhs = argValues |> List.tryItem 0 |> Option.defaultWith (fun () -> failwith "Missing lhs")
                let rhs = argValues |> List.tryItem 1 |> Option.defaultWith (fun () -> failwith "Missing rhs")
                let opcode =
                    match op with
                    | Builtins.OpAdd -> Mir.OpCode.Add
                    | Builtins.OpSub -> Mir.OpCode.Sub
                    | Builtins.OpMul -> Mir.OpCode.Mul
                    | Builtins.OpDiv -> Mir.OpCode.Div
                    | Builtins.OpMod -> Mir.OpCode.Mod
                    | Builtins.OpEq -> Mir.OpCode.Eq
                { ins = argIns @ [ Mir.Ins.TAC(dst, lhs, opcode, rhs) ]; res = Some(Mir.Value.RegVal dst) }
            | Hir.Callable.Fn sid ->
                match frame.get sid with
                | Some delegateReg ->
                    // sid がフレームに存在する → 引数または let 束縛されたデリゲートを介した呼び出し。
                    // フレームから型を取得してデリゲート型の Invoke メソッドを得る。
                    let fnType =
                        match frame.argTypes.TryGetValue(sid) with
                        | true, t -> t
                        | _ ->
                            match frame.locTypes.TryGetValue(sid) with
                            | true, t -> t
                            | _ -> failwithf "Unknown type for function-typed register: %A" sid
                    match TypeId.tryToRuntimeSystemType fnType with
                    | Some delegateType ->
                        let invokeMethod = delegateType.GetMethod("Invoke")
                        if obj.ReferenceEquals(invokeMethod, null) then
                            failwithf "Delegate type '%s' has no Invoke method" delegateType.FullName
                        // レシーバー（デリゲート自身）を先頭引数として渡す。
                        let callArgs = (Mir.Value.RegVal delegateReg) :: argValues
                        let returnsVoid = invokeMethod.ReturnType = typeof<System.Void>
                        match tid, returnsVoid with
                        | _, true
                        | TypeId.Unit, _ ->
                            { ins = argIns @ [ Mir.Ins.Call(Choice1Of2 invokeMethod, callArgs) ]; res = None }
                        | _ ->
                            let dst = declareTemp frame tid
                            { ins = argIns @ [ Mir.Ins.CallAssign(dst, invokeMethod, callArgs) ]; res = Some(Mir.Value.RegVal dst) }
                    | None ->
                        failwithf "Cannot resolve delegate type for function-typed symbol: %A" sid
                | None ->
                    // sid がフレームに存在しない → グローバル関数への直接呼び出し。
                    match tid with
                    | TypeId.Unit ->
                        { ins = argIns @ [ Mir.Ins.CallSym(sid, argValues) ]; res = None }
                    | _ ->
                        let dst = declareTemp frame tid
                        { ins = argIns @ [ Mir.Ins.CallAssignSym(dst, sid, argValues) ]; res = Some(Mir.Value.RegVal dst) }
        | Hir.Expr.Block (stmts, expr, _, _) ->
            let stmtIns = stmts |> List.collect (layoutStmt frame)
            let exprKn = layoutExpr frame expr
            { ins = stmtIns @ exprKn.ins; res = exprKn.res }
        | Hir.Expr.If (cond, thenBranch, elseBranch, tid, _) ->
            let condKn = layoutExpr frame cond
            let thenKn = layoutExpr frame thenBranch
            let elseKn = layoutExpr frame elseBranch
            let thenLabel = Mir.Label()
            let elseLabel = Mir.Label()
            let endLabel = Mir.Label()

            match tid with
            | TypeId.Unit ->
                {
                    ins =
                        condKn.ins
                        @ [ Mir.Ins.JumpTrue(condKn.res.Value, thenLabel); Mir.Ins.JumpFalse(condKn.res.Value, elseLabel) ]
                        @ [ Mir.Ins.MarkLabel thenLabel ] @ thenKn.ins @ [ Mir.Ins.Jump endLabel ]
                        @ [ Mir.Ins.MarkLabel elseLabel ] @ elseKn.ins @ [ Mir.Ins.Jump endLabel ]
                        @ [ Mir.Ins.MarkLabel endLabel ]
                    res = None
                }
            | _ ->
                let dst = declareTemp frame tid
                {
                    ins =
                        condKn.ins
                        @ [ Mir.Ins.JumpTrue(condKn.res.Value, thenLabel); Mir.Ins.JumpFalse(condKn.res.Value, elseLabel) ]
                        @ [ Mir.Ins.MarkLabel thenLabel ]
                        @ thenKn.ins
                        @ [ Mir.Ins.Assign(dst, thenKn.res.Value); Mir.Ins.Jump endLabel ]
                        @ [ Mir.Ins.MarkLabel elseLabel ]
                        @ elseKn.ins
                        @ [ Mir.Ins.Assign(dst, elseKn.res.Value); Mir.Ins.Jump endLabel ]
                        @ [ Mir.Ins.MarkLabel endLabel ]
                    res = Some(Mir.Value.RegVal dst)
                }
        | Hir.Expr.ExprError (message, _, _) ->
            failwithf "Cannot lower erroneous expression: %s" message
        | Hir.Expr.Lambda _ ->
            failwith "Lambda lowering is not implemented"
        // env-class クロージャーフィールド参照: env インスタンス引数からフィールドを読み込む。
        | Hir.Expr.EnvFieldLoad (envArgSid, capturedSid, tid, _) ->
            let envReg =
                match frame.get envArgSid with
                | Some reg -> reg
                | None -> failwithf "Env arg %A not found in frame for EnvFieldLoad" envArgSid
            let envTypeSid =
                match frame.argTypes.TryGetValue(envArgSid) with
                | true, TypeId.Name sid -> sid
                | true, t -> failwithf "Env arg %A has unexpected type %A; expected TypeId.Name" envArgSid t
                | false, _ -> failwithf "Env arg type not found in frame for %A" envArgSid
            let dst = declareTemp frame tid
            { ins = [ Mir.Ins.LoadEnvField(dst, envReg, envTypeSid, capturedSid) ]; res = Some(Mir.Value.RegVal dst) }
        // env-class クロージャー生成式: env インスタンスを生成し、捕捉変数を格納し、bound delegate を返す。
        | Hir.Expr.ClosureCreate (envTypeSid, methodSid, captured, tid, _) ->
            // 1. env インスタンス用レジスタを確保し、NewEnv 命令を生成する。
            let envTyp = TypeId.Name envTypeSid
            let envReg = declareTemp frame envTyp
            let newEnvIns = Mir.Ins.NewEnv(envReg, envTypeSid)
            // 2. 各捕捉変数を env フィールドへ格納する命令を生成する。
            let storeIns =
                captured
                |> List.choose (fun (capturedSid, _, _) ->
                    match frame.get capturedSid with
                    | Some capturedReg ->
                        Some(Mir.Ins.StoreEnvField(envReg, envTypeSid, capturedSid, Mir.Value.RegVal capturedReg))
                    | None -> None)
            // 3. env インスタンスにバインドしたデリゲートを生成する値を返す。
            match TypeId.tryToRuntimeSystemType tid with
            | Some delegateType ->
                { ins = [ newEnvIns ] @ storeIns; res = Some(Mir.Value.FnDelegate(methodSid, delegateType, Some envReg)) }
            | None ->
                failwithf "Cannot resolve delegate type for ClosureCreate: %A" tid

    and private layoutStmt (frame: Mir.Frame) (stmt: Hir.Stmt) : Mir.Ins list =
        match stmt with
        | Hir.Stmt.Let (sid, _, value, _) ->
            let valueKn = layoutExpr frame value
            let dst = frame.addLoc(sid, value.typ)
            match valueKn.res with
            | Some v -> valueKn.ins @ [ Mir.Ins.Assign(dst, v) ]
            | None -> valueKn.ins
        | Hir.Stmt.Assign (sid, value, _) ->
            let valueKn = layoutExpr frame value
            match frame.get sid, valueKn.res with
            | Some dst, Some v -> valueKn.ins @ [ Mir.Ins.Assign(dst, v) ]
            | Some _, None -> valueKn.ins
            | None, _ -> failwithf "Undefined variable in assignment: %A" sid
        | Hir.Stmt.ExprStmt (expr, _) ->
            (layoutExpr frame expr).ins
        | Hir.Stmt.For (sid, tid, iterable, body, span) ->
            let iterableKn = layoutExpr frame iterable
            let iterReg = declareTemp frame iterable.typ
            let iterAssignIns =
                match iterableKn.res with
                | Some value -> [ Mir.Ins.Assign(iterReg, value) ]
                | None -> failwithf "For iterable expression did not produce a value at %A" span

            let iterType =
                match TypeId.tryToRuntimeSystemType iterable.typ with
                | Some t -> t
                | None -> failwithf "For iterable type is not a runtime type: %A at %A" iterable.typ span

            let iterCandidateTypes = iterType :: (iterType.GetInterfaces() |> Array.toList)
            let moveNextMethod =
                iterCandidateTypes
                |> List.tryPick (fun t ->
                    t.GetMethods(System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Instance)
                    |> Array.tryFind (fun methodInfo -> methodInfo.Name = "MoveNext" && methodInfo.GetParameters().Length = 0))
                |> Option.toObj
            let currentProperty =
                iterCandidateTypes
                |> List.collect (fun t ->
                    t.GetProperties(System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Instance)
                    |> Array.filter (fun propertyInfo -> propertyInfo.Name = "Current")
                    |> Array.toList)
                |> List.tryFind (fun propertyInfo -> propertyInfo.PropertyType <> typeof<obj>)
                |> Option.orElseWith (fun () ->
                    iterCandidateTypes
                    |> List.tryPick (fun t ->
                        t.GetProperties(System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Instance)
                        |> Array.tryFind (fun propertyInfo -> propertyInfo.Name = "Current")))
                |> Option.toObj
            let currentGetter =
                match currentProperty with
                | null -> null
                | propertyInfo -> propertyInfo.GetMethod

            if obj.ReferenceEquals(moveNextMethod, null) then
                failwithf "For iterable type '%A' does not define MoveNext() at %A" iterType span
            elif obj.ReferenceEquals(currentGetter, null) then
                failwithf "For iterable type '%A' does not define Current getter at %A" iterType span
            else
                let loopVarReg = frame.addLoc(sid, tid)
                let condReg = declareTemp frame TypeId.Bool
                let currentReg = declareTemp frame tid
                let loopStart = Mir.Label()
                let loopBody = Mir.Label()
                let loopEnd = Mir.Label()
                let bodyIns = body |> List.collect (layoutStmt frame)

                iterableKn.ins
                @ iterAssignIns
                @ [ Mir.Ins.MarkLabel loopStart
                    Mir.Ins.CallAssign(condReg, moveNextMethod, [ Mir.Value.RegVal iterReg ])
                    Mir.Ins.JumpTrue(Mir.Value.RegVal condReg, loopBody)
                    Mir.Ins.JumpFalse(Mir.Value.RegVal condReg, loopEnd)
                    Mir.Ins.MarkLabel loopBody
                    Mir.Ins.CallAssign(currentReg, currentGetter, [ Mir.Value.RegVal iterReg ])
                    Mir.Ins.Assign(loopVarReg, Mir.Value.RegVal currentReg) ]
                @ bodyIns
                @ [ Mir.Ins.Jump loopStart
                    Mir.Ins.MarkLabel loopEnd ]
        | Hir.Stmt.ErrorStmt (message, _) ->
            failwithf "Cannot lower erroneous statement: %s" message

    let private layoutMethod (methodName: string) (hirMethod: Hir.Method) : Mir.Method =
        let frame = Mir.Frame()
        // メソッド引数を宣言順にフレームへ事前登録する。
        // これにより、ボディ内で引数を参照したときに正しい Arg インデックスが割り当てられる。
        for (argSid, argType) in hirMethod.args do
            frame.addArg(argSid, argType) |> ignore
        let bodyKn = layoutExpr frame hirMethod.body
        let body =
            match hirMethod.typ with
            | TypeId.Fn (_, TypeId.Unit) -> bodyKn.ins @ [ Mir.Ins.Ret ]
            | TypeId.Fn (_, _) ->
                match bodyKn.res with
                | Some v -> bodyKn.ins @ [ Mir.Ins.RetValue v ]
                | None -> bodyKn.ins @ [ Mir.Ins.Ret ]
            | _ -> failwithf "Expected function type for method: %A" hirMethod.typ

        match hirMethod.typ with
        | TypeId.Fn (args, ret) -> Mir.Method(methodName, hirMethod.sym, args, ret, body, frame)
        | _ -> failwithf "Expected function type for method: %A" hirMethod.typ

    let private layoutType (typeName: string) (hirType: Hir.Type) : Mir.Type =
        let fields = hirType.fields |> List.map (fun field -> Mir.Field(field.sym, field.typ))
        Mir.Type(typeName, hirType.sym, fields, [], [])

    /// env-class の invoke メソッドとして使用するインスタンスメソッドを生成する。
    /// 通常の layoutMethod との違い: 第一引数（env インスタンス）は TypeId.Name の場合のみ除去する。
    let private layoutInvokeMethod (methodName: string) (hirMethod: Hir.Method) : Mir.Method =
        let frame = Mir.Frame()
        // invoke method の全引数をフレームへ事前登録する（第一引数 = env インスタンスを含む）。
        // インスタンスメソッドでは Arg(0) が 'this' に対応するため、第一引数 sid を Arg(0) に割り当てる。
        for (argSid, argType) in hirMethod.args do
            frame.addArg(argSid, argType) |> ignore
        let bodyKn = layoutExpr frame hirMethod.body
        let body =
            match hirMethod.typ with
            | TypeId.Fn (_, TypeId.Unit) -> bodyKn.ins @ [ Mir.Ins.Ret ]
            | TypeId.Fn (_, _) ->
                match bodyKn.res with
                | Some v -> bodyKn.ins @ [ Mir.Ins.RetValue v ]
                | None -> bodyKn.ins @ [ Mir.Ins.Ret ]
            | _ -> failwithf "Expected function type for invoke method: %A" hirMethod.typ

        // CIL インスタンスメソッドの arg 型リストは 'this' を除いた明示引数のみ。
        // hirMethod.args の先頭要素 (envArgSid, TypeId.Name envTypeSid) を除外する。
        let explicitArgs =
            match hirMethod.args with
            | _ :: rest -> rest |> List.map snd
            | [] -> []
        let ret =
            match hirMethod.typ with
            | TypeId.Fn (_, r) -> r
            | _ -> failwithf "Expected function type for invoke method: %A" hirMethod.typ
        Mir.Method(methodName, hirMethod.sym, explicitArgs, ret, body, frame)

    let private layoutModule (hirModule: Hir.Module) : Mir.Module =
        let resolveTypeName (sid: SymbolId) =
            hirModule.scope.types
            |> Seq.tryPick (fun (KeyValue(name, tid)) ->
                match tid with
                | TypeId.Name tid when tid.id = sid.id -> Some name
                | _ -> None)
            |> Option.defaultValue (sprintf "type_%d" sid.id)

        let resolveMethodName (sid: SymbolId) =
            hirModule.scope.vars
            |> Seq.tryPick (fun (KeyValue(name, symSid)) ->
                if symSid.id = sid.id then Some name else None)
            |> Option.defaultValue (sprintf "fn_%d" sid.id)

        let closureInvokeMap = hirModule.closureInvokeMethods

        // 型を生成する。クロージャー invoke メソッドがあれば env-class の Mir.Type.methods に追加する。
        let types =
            hirModule.types
            |> List.map (fun hirType ->
                let baseType = layoutType (resolveTypeName hirType.sym) hirType
                // この型に属するクロージャー invoke メソッドを収集し、インスタンスメソッドとして生成する。
                let invokeMethods =
                    hirModule.methods
                    |> List.choose (fun hirMethod ->
                        match closureInvokeMap |> Map.tryFind hirMethod.sym.id with
                        | Some envTypeSid when envTypeSid = hirType.sym.id ->
                            Some(layoutInvokeMethod (resolveMethodName hirMethod.sym) hirMethod)
                        | _ -> None)
                Mir.Type(baseType.name, baseType.sym, baseType.fields, baseType.ctors, invokeMethods))

        // クロージャー invoke メソッドはすでに型内に配置したため、モジュールレベルからは除外する。
        let methods =
            hirModule.methods
            |> List.filter (fun hirMethod -> not (closureInvokeMap |> Map.containsKey hirMethod.sym.id))
            |> List.map (fun hirMethod -> layoutMethod (resolveMethodName hirMethod.sym) hirMethod)

        Mir.Module(hirModule.name, types, methods)

    /// 式木に Lambda ノードが残っているかを判定する。
    let rec private hasLambdaExpr (expr: Hir.Expr) : bool =
        match expr with
        | Hir.Expr.Lambda _ -> true
        | Hir.Expr.Block (stmts, body, _, _) ->
            (stmts |> List.exists hasLambdaStmt) || hasLambdaExpr body
        | Hir.Expr.Call (_, instance, args, _, _) ->
            let instanceHasLambda =
                instance
                |> Option.map hasLambdaExpr
                |> Option.defaultValue false
            instanceHasLambda || (args |> List.exists hasLambdaExpr)
        | Hir.Expr.MemberAccess (_, instance, _, _) ->
            instance
            |> Option.map hasLambdaExpr
            |> Option.defaultValue false
        | Hir.Expr.If (cond, thenBranch, elseBranch, _, _) ->
            hasLambdaExpr cond || hasLambdaExpr thenBranch || hasLambdaExpr elseBranch
        // EnvFieldLoad と ClosureCreate は変換済みノードなので Lambda ではない。
        | Hir.Expr.EnvFieldLoad _ -> false
        | Hir.Expr.ClosureCreate _ -> false
        | _ -> false

    /// 文木に Lambda ノードが残っているかを判定する。
    and private hasLambdaStmt (stmt: Hir.Stmt) : bool =
        match stmt with
        | Hir.Stmt.Let (_, _, value, _)
        | Hir.Stmt.Assign (_, value, _)
        | Hir.Stmt.ExprStmt (value, _) -> hasLambdaExpr value
        | Hir.Stmt.For (_, _, iterable, body, _) ->
            hasLambdaExpr iterable || (body |> List.exists hasLambdaStmt)
        | Hir.Stmt.ErrorStmt _ -> false

    let layoutAssembly (asmName: string, asm: Hir.Assembly) : PhaseResult<Mir.Assembly> =
        match ClosureConversion.preprocessAssembly asm with
        | { succeeded = false; diagnostics = diagnostics } -> PhaseResult.failed diagnostics
        | { value = Some preprocessedAsm } ->
            let residualLambdaDiagnostics =
                preprocessedAsm.modules
                |> List.collect (fun hirModule ->
                    hirModule.methods
                    |> List.choose (fun hirMethod ->
                        if hasLambdaExpr hirMethod.body then
                            Some(Diagnostic.Error($"Residual lambda remains after closure conversion. methodSid={hirMethod.sym.id}", hirMethod.span))
                        else
                            None))

            match residualLambdaDiagnostics with
            | residual when not residual.IsEmpty -> PhaseResult.failed residual
            | _ ->
                try
                    let modules = preprocessedAsm.modules |> List.map layoutModule
                    PhaseResult.succeeded (Mir.Assembly(asmName, modules)) []
                with ex ->
                    PhaseResult.failed [ Diagnostic.Error($"Lowering failed: {ex.Message}", Atla.Core.Data.Span.Empty) ]
        | _ ->
            PhaseResult.failed [ Diagnostic.Error("Closure conversion preprocessing failed with unknown state", Atla.Core.Data.Span.Empty) ]