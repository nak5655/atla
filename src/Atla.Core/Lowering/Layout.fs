namespace Atla.Core.Lowering

open Atla.Core.Semantics.Data
open Atla.Core.Lowering.Data

module Layout =
    type private KNormal = {
        ins: Mir.Ins list
        res: Mir.Value option
    }

    /// Layout フェーズ内でフレームとラベルカウンタを一緒に引き回す不変状態レコード。
    type private LayoutState = {
        frame: Mir.Frame
        nextLabel: int
    }

    /// 初期レイアウト状態（空フレーム、ラベルカウンタ 0）を返す。
    let private emptyState: LayoutState = { frame = Mir.Frame.empty; nextLabel = 0 }

    /// 一意なラベル ID を払い出し、更新後の状態と共に返す。
    let private freshLabel (state: LayoutState) : Mir.LabelId * LayoutState =
        Mir.LabelId state.nextLabel, { state with nextLabel = state.nextLabel + 1 }

    /// 一時変数用の新規ローカルレジスタを確保し、更新後の状態と共に返す。
    /// 負の SymbolId を候補として使い、既存エントリと衝突しないものを選ぶ。
    let private declareTemp (state: LayoutState) (tid: TypeId) : Mir.Reg * LayoutState =
        let rec freshTempSid (candidate: int) : SymbolId =
            let sid = SymbolId candidate
            match Mir.Frame.get sid state.frame with
            | Some _ -> freshTempSid (candidate - 1)
            | None -> sid
        let sid = freshTempSid -1
        let reg, newFrame = Mir.Frame.addLoc sid tid state.frame
        reg, { state with frame = newFrame }

    let rec private layoutExpr (state: LayoutState) (expr: ClosedHir.Expr) : LayoutState * KNormal =
        match expr with
        | ClosedHir.Expr.Unit _ -> state, { ins = []; res = None }
        | ClosedHir.Expr.Int (value, _) -> state, { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Int value)) }
        | ClosedHir.Expr.Float (value, _) -> state, { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Float value)) }
        | ClosedHir.Expr.String (value, _) -> state, { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.String value)) }
        // null リテラル: 参照型のオプショナル引数デフォルト値として使用する
        | ClosedHir.Expr.Null _ -> state, { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Null)) }
        | ClosedHir.Expr.Id (sid, tid, _) ->
            match Mir.Frame.get sid state.frame with
            | Some reg -> state, { ins = []; res = Some(Mir.Value.RegVal reg) }
            | None ->
                // フレームに存在しない sid は「グローバル関数参照」として扱う。
                // TypeId.Fn の場合はデリゲートに変換し、それ以外はメソッド引数として追加する。
                match tid with
                | TypeId.Fn _ ->
                    match TypeId.tryToRuntimeSystemType tid with
                    | Some delegateType ->
                        // グローバル関数を対応するデリゲート（Func<>/Action<>）に包む。
                        state, { ins = []; res = Some(Mir.Value.FnDelegate(sid, delegateType, None)) }
                    | None ->
                        let argReg, newFrame = Mir.Frame.addArg sid tid state.frame
                        { state with frame = newFrame }, { ins = []; res = Some(Mir.Value.RegVal argReg) }
                // .NET デリゲート型（Native）で注釈された関数参照はその型のまま使用する。
                | TypeId.Native t when TypeId.isDelegateType t ->
                    state, { ins = []; res = Some(Mir.Value.FnDelegate(sid, t, None)) }
                | _ ->
                    let argReg, newFrame = Mir.Frame.addArg sid tid state.frame
                    { state with frame = newFrame }, { ins = []; res = Some(Mir.Value.RegVal argReg) }
        | ClosedHir.Expr.MemberAccess (mem, instance, tid, span) ->
            match mem with
            | Hir.Member.NativeField fi -> state, { ins = []; res = Some(Mir.Value.FieldVal fi) }
            | Hir.Member.NativeMethod mi -> state, { ins = []; res = Some(Mir.Value.MethodVal mi) }
            | Hir.Member.NativeProperty pi ->
                match pi.GetMethod with
                | null -> failwithf "Property has no getter: %s" pi.Name
                | getter ->
                    let state1, instanceKnOpt =
                        match instance with
                        | Some expr ->
                            let s1, kn = layoutExpr state expr
                            s1, Some kn
                        | None -> state, None

                    let instanceIns =
                        instanceKnOpt
                        |> Option.map (fun kn -> kn.ins)
                        |> Option.defaultValue []

                    let instanceArg =
                        instanceKnOpt
                        |> Option.bind (fun kn -> kn.res)
                        |> Option.map List.singleton
                        |> Option.defaultValue []

                    if tid = TypeId.Unit || getter.ReturnType = typeof<System.Void> then
                        state1, { ins = instanceIns @ [ Mir.Ins.Call(Choice1Of2 getter, instanceArg) ]; res = None }
                    else
                        let dst, state2 = declareTemp state1 tid
                        state2, { ins = instanceIns @ [ Mir.Ins.CallAssign(dst, getter, instanceArg) ]; res = Some(Mir.Value.RegVal dst) }
        | ClosedHir.Expr.Call (func, _, args, tid, _) ->
            let argKnList, state1 =
                args |> List.mapFold (fun st arg -> let s, kn = layoutExpr st arg in kn, s) state
            let argIns = argKnList |> List.collect (fun k -> k.ins)
            let argValues =
                argKnList
                |> List.map (fun k ->
                    match k.res with
                    | Some value -> value
                    | None -> failwith "Argument expression did not produce a value")

            // MethodInfo を使った呼び出しを発行するヘルパー。
            let emitCall (st: LayoutState) (mi: System.Reflection.MethodInfo) =
                let returnsUnit = (mi.ReturnType = typeof<System.Void>)
                match tid, returnsUnit with
                | _, true
                | TypeId.Unit, _ ->
                    st, { ins = argIns @ [ Mir.Ins.Call(Choice1Of2 mi, argValues) ]; res = None }
                | _ ->
                    let dst, st' = declareTemp st tid
                    st', { ins = argIns @ [ Mir.Ins.CallAssign(dst, mi, argValues) ]; res = Some(Mir.Value.RegVal dst) }

            match func with
            | Hir.Callable.NativeMethod mi -> emitCall state1 mi
            | Hir.Callable.NativeMethodGroup methods ->
                match methods |> List.tryFind (fun m -> m.GetParameters().Length = argValues.Length) with
                | Some mi -> emitCall state1 mi
                | None -> failwithf "No overload matched argument count %d" argValues.Length
            | Hir.Callable.NativeConstructor ctor ->
                let dst, state2 = declareTemp state1 tid
                state2, { ins = argIns @ [ Mir.Ins.New(dst, ctor, argValues) ]; res = Some(Mir.Value.RegVal dst) }
            | Hir.Callable.NativeConstructorGroup ctors ->
                match ctors |> List.tryFind (fun c -> c.GetParameters().Length = argValues.Length) with
                | Some ctor ->
                    let dst, state2 = declareTemp state1 tid
                    state2, { ins = argIns @ [ Mir.Ins.New(dst, ctor, argValues) ]; res = Some(Mir.Value.RegVal dst) }
                | None -> failwithf "No constructor matched argument count %d" argValues.Length
            | Hir.Callable.BuiltinOperator op ->
                let dst, state2 = declareTemp state1 tid
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
                state2, { ins = argIns @ [ Mir.Ins.TAC(dst, lhs, opcode, rhs) ]; res = Some(Mir.Value.RegVal dst) }
            | Hir.Callable.Fn sid ->
                match Mir.Frame.get sid state1.frame with
                | Some delegateReg ->
                    // sid がフレームに存在する → 引数または let 束縛されたデリゲートを介した呼び出し。
                    // フレームから型を取得してデリゲート型の Invoke メソッドを得る。
                    let fnType =
                        match state1.frame.argTypes |> Map.tryFind sid with
                        | Some t -> t
                        | None ->
                            match state1.frame.locTypes |> Map.tryFind sid with
                            | Some t -> t
                            | None -> failwithf "Unknown type for function-typed register: %A" sid
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
                            state1, { ins = argIns @ [ Mir.Ins.Call(Choice1Of2 invokeMethod, callArgs) ]; res = None }
                        | _ ->
                            let dst, state2 = declareTemp state1 tid
                            state2, { ins = argIns @ [ Mir.Ins.CallAssign(dst, invokeMethod, callArgs) ]; res = Some(Mir.Value.RegVal dst) }
                    | None ->
                        failwithf "Cannot resolve delegate type for function-typed symbol: %A" sid
                | None ->
                    // sid がフレームに存在しない → グローバル関数への直接呼び出し。
                    match tid with
                    | TypeId.Unit ->
                        state1, { ins = argIns @ [ Mir.Ins.CallSym(sid, argValues) ]; res = None }
                    | _ ->
                        let dst, state2 = declareTemp state1 tid
                        state2, { ins = argIns @ [ Mir.Ins.CallAssignSym(dst, sid, argValues) ]; res = Some(Mir.Value.RegVal dst) }
        | ClosedHir.Expr.Block (stmts, expr, _, _) ->
            let stmtInsList, state1 =
                stmts |> List.mapFold (fun st stmt -> let s, ins = layoutStmt st stmt in ins, s) state
            let stmtIns = List.concat stmtInsList
            let state2, exprKn = layoutExpr state1 expr
            state2, { ins = stmtIns @ exprKn.ins; res = exprKn.res }
        | ClosedHir.Expr.If (cond, thenBranch, elseBranch, tid, _) ->
            let state1, condKn = layoutExpr state cond
            let state2, thenKn = layoutExpr state1 thenBranch
            let state3, elseKn = layoutExpr state2 elseBranch
            let thenLabelId, state4 = freshLabel state3
            let elseLabelId, state5 = freshLabel state4
            let endLabelId, state6 = freshLabel state5

            match tid with
            | TypeId.Unit ->
                state6, {
                    ins =
                        condKn.ins
                        @ [ Mir.Ins.JumpTrue(condKn.res.Value, thenLabelId); Mir.Ins.JumpFalse(condKn.res.Value, elseLabelId) ]
                        @ [ Mir.Ins.MarkLabel thenLabelId ] @ thenKn.ins @ [ Mir.Ins.Jump endLabelId ]
                        @ [ Mir.Ins.MarkLabel elseLabelId ] @ elseKn.ins @ [ Mir.Ins.Jump endLabelId ]
                        @ [ Mir.Ins.MarkLabel endLabelId ]
                    res = None
                }
            | _ ->
                let dst, state7 = declareTemp state6 tid
                state7, {
                    ins =
                        condKn.ins
                        @ [ Mir.Ins.JumpTrue(condKn.res.Value, thenLabelId); Mir.Ins.JumpFalse(condKn.res.Value, elseLabelId) ]
                        @ [ Mir.Ins.MarkLabel thenLabelId ]
                        @ thenKn.ins
                        @ [ Mir.Ins.Assign(dst, thenKn.res.Value); Mir.Ins.Jump endLabelId ]
                        @ [ Mir.Ins.MarkLabel elseLabelId ]
                        @ elseKn.ins
                        @ [ Mir.Ins.Assign(dst, elseKn.res.Value); Mir.Ins.Jump endLabelId ]
                        @ [ Mir.Ins.MarkLabel endLabelId ]
                    res = Some(Mir.Value.RegVal dst)
                }
        | ClosedHir.Expr.ExprError (message, _, _) ->
            failwithf "Cannot lower erroneous expression: %s" message
        | ClosedHir.Expr.Lambda _ ->
            failwith "Lambda lowering is not implemented"
        // env-class クロージャーフィールド参照: env インスタンス引数からフィールドを読み込む。
        | ClosedHir.Expr.EnvFieldLoad (envArgSid, capturedSid, tid, _) ->
            let envReg =
                match Mir.Frame.get envArgSid state.frame with
                | Some reg -> reg
                | None -> failwithf "Env arg %A not found in frame for EnvFieldLoad" envArgSid
            let envTypeSid =
                match state.frame.argTypes |> Map.tryFind envArgSid with
                | Some (TypeId.Name sid) -> sid
                | Some t -> failwithf "Env arg %A has unexpected type %A; expected TypeId.Name" envArgSid t
                | None -> failwithf "Env arg type not found in frame for %A" envArgSid
            let dst, state1 = declareTemp state tid
            state1, { ins = [ Mir.Ins.LoadEnvField(dst, envReg, envTypeSid, capturedSid) ]; res = Some(Mir.Value.RegVal dst) }
        // env-class クロージャー生成式: env インスタンスを生成し、捕捉変数を格納し、bound delegate を返す。
        | ClosedHir.Expr.ClosureCreate (envTypeSid, methodSid, captured, tid, _) ->
            // 1. env インスタンス用レジスタを確保し、NewEnv 命令を生成する。
            let envTyp = TypeId.Name envTypeSid
            let envReg, state1 = declareTemp state envTyp
            let newEnvIns = Mir.Ins.NewEnv(envReg, envTypeSid)
            // 2. 各捕捉変数を env フィールドへ格納する命令を生成する。
            let storeIns =
                captured
                |> List.choose (fun (capturedSid, _, _) ->
                    match Mir.Frame.get capturedSid state1.frame with
                    | Some capturedReg ->
                        Some(Mir.Ins.StoreEnvField(envReg, envTypeSid, capturedSid, Mir.Value.RegVal capturedReg))
                    | None -> None)
            // 3. env インスタンスにバインドしたデリゲートを生成する値を返す。
            match TypeId.tryToRuntimeSystemType tid with
            | Some delegateType ->
                state1, { ins = [ newEnvIns ] @ storeIns; res = Some(Mir.Value.FnDelegate(methodSid, delegateType, Some envReg)) }
            | None ->
                failwithf "Cannot resolve delegate type for ClosureCreate: %A" tid

    and private layoutStmt (state: LayoutState) (stmt: ClosedHir.Stmt) : LayoutState * Mir.Ins list =
        match stmt with
        | ClosedHir.Stmt.Let (sid, _, value, _) ->
            let state1, valueKn = layoutExpr state value
            let dst, newFrame = Mir.Frame.addLoc sid value.typ state1.frame
            let state2 = { state1 with frame = newFrame }
            match valueKn.res with
            | Some v -> state2, valueKn.ins @ [ Mir.Ins.Assign(dst, v) ]
            | None -> state2, valueKn.ins
        | ClosedHir.Stmt.Assign (sid, value, _) ->
            let state1, valueKn = layoutExpr state value
            match Mir.Frame.get sid state1.frame, valueKn.res with
            | Some dst, Some v -> state1, valueKn.ins @ [ Mir.Ins.Assign(dst, v) ]
            | Some _, None -> state1, valueKn.ins
            | None, _ -> failwithf "Undefined variable in assignment: %A" sid
        | ClosedHir.Stmt.ExprStmt (expr, _) ->
            let state1, kn = layoutExpr state expr
            state1, kn.ins
        | ClosedHir.Stmt.For (sid, tid, iterable, body, span) ->
            let state1, iterableKn = layoutExpr state iterable
            let iterReg, state2 = declareTemp state1 iterable.typ
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
                let loopVarReg, loopVarFrame = Mir.Frame.addLoc sid tid state2.frame
                let state3 = { state2 with frame = loopVarFrame }
                let condReg, state4 = declareTemp state3 TypeId.Bool
                let currentReg, state5 = declareTemp state4 tid
                let loopStartId, state6 = freshLabel state5
                let loopBodyId, state7 = freshLabel state6
                let loopEndId, state8 = freshLabel state7
                let bodyInsList, state9 =
                    body |> List.mapFold (fun st stmt -> let s, ins = layoutStmt st stmt in ins, s) state8
                let bodyIns = List.concat bodyInsList

                state9, (
                    iterableKn.ins
                    @ iterAssignIns
                    @ [ Mir.Ins.MarkLabel loopStartId
                        Mir.Ins.CallAssign(condReg, moveNextMethod, [ Mir.Value.RegVal iterReg ])
                        Mir.Ins.JumpTrue(Mir.Value.RegVal condReg, loopBodyId)
                        Mir.Ins.JumpFalse(Mir.Value.RegVal condReg, loopEndId)
                        Mir.Ins.MarkLabel loopBodyId
                        Mir.Ins.CallAssign(currentReg, currentGetter, [ Mir.Value.RegVal iterReg ])
                        Mir.Ins.Assign(loopVarReg, Mir.Value.RegVal currentReg) ]
                    @ bodyIns
                    @ [ Mir.Ins.Jump loopStartId
                        Mir.Ins.MarkLabel loopEndId ]
                )
        | ClosedHir.Stmt.ErrorStmt (message, _) ->
            failwithf "Cannot lower erroneous statement: %s" message

    let private layoutMethod (methodName: string) (hirMethod: ClosedHir.Method) : Mir.Method =
        // メソッド引数を宣言順にフレームへ事前登録する。
        // これにより、ボディ内で引数を参照したときに正しい Arg インデックスが割り当てられる。
        let initFrame =
            hirMethod.args |> List.fold (fun frame (argSid, argType) ->
                let _, newFrame = Mir.Frame.addArg argSid argType frame
                newFrame) Mir.Frame.empty
        let initState = { emptyState with frame = initFrame }
        let finalState, bodyKn = layoutExpr initState hirMethod.body
        let body =
            match hirMethod.typ with
            | TypeId.Fn (_, TypeId.Unit) -> bodyKn.ins @ [ Mir.Ins.Ret ]
            | TypeId.Fn (_, _) ->
                match bodyKn.res with
                | Some v -> bodyKn.ins @ [ Mir.Ins.RetValue v ]
                | None -> bodyKn.ins @ [ Mir.Ins.Ret ]
            | _ -> failwithf "Expected function type for method: %A" hirMethod.typ

        match hirMethod.typ with
        | TypeId.Fn (args, ret) -> Mir.Method(methodName, hirMethod.sym, args, ret, body, finalState.frame)
        | _ -> failwithf "Expected function type for method: %A" hirMethod.typ

    let private layoutType (typeName: string) (hirType: ClosedHir.Type) : Mir.Type =
        let fields = hirType.fields |> List.map (fun field -> Mir.Field(field.sym, field.typ))
        Mir.Type(typeName, hirType.sym, fields, [], [])

    /// env-class の invoke メソッドとして使用するインスタンスメソッドを生成する。
    /// 通常の layoutMethod との違い: 第一引数（env インスタンス）は TypeId.Name の場合のみ除去する。
    let private layoutInvokeMethod (methodName: string) (hirMethod: ClosedHir.Method) : Mir.Method =
        // invoke method の全引数をフレームへ事前登録する（第一引数 = env インスタンスを含む）。
        // インスタンスメソッドでは Arg(0) が 'this' に対応するため、第一引数 sid を Arg(0) に割り当てる。
        let initFrame =
            hirMethod.args |> List.fold (fun frame (argSid, argType) ->
                let _, newFrame = Mir.Frame.addArg argSid argType frame
                newFrame) Mir.Frame.empty
        let initState = { emptyState with frame = initFrame }
        let finalState, bodyKn = layoutExpr initState hirMethod.body
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
        Mir.Method(methodName, hirMethod.sym, explicitArgs, ret, body, finalState.frame)

    let private layoutModule (hirModule: ClosedHir.Module) : Mir.Module =
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
    let rec private hasLambdaExpr (expr: ClosedHir.Expr) : bool =
        match expr with
        | ClosedHir.Expr.Lambda _ -> true
        | ClosedHir.Expr.Block (stmts, body, _, _) ->
            (stmts |> List.exists hasLambdaStmt) || hasLambdaExpr body
        | ClosedHir.Expr.Call (_, instance, args, _, _) ->
            let instanceHasLambda =
                instance
                |> Option.map hasLambdaExpr
                |> Option.defaultValue false
            instanceHasLambda || (args |> List.exists hasLambdaExpr)
        | ClosedHir.Expr.MemberAccess (_, instance, _, _) ->
            instance
            |> Option.map hasLambdaExpr
            |> Option.defaultValue false
        | ClosedHir.Expr.If (cond, thenBranch, elseBranch, _, _) ->
            hasLambdaExpr cond || hasLambdaExpr thenBranch || hasLambdaExpr elseBranch
        // EnvFieldLoad と ClosureCreate は変換済みノードなので Lambda ではない。
        | ClosedHir.Expr.EnvFieldLoad _ -> false
        | ClosedHir.Expr.ClosureCreate _ -> false
        | _ -> false

    /// 文木に Lambda ノードが残っているかを判定する。
    and private hasLambdaStmt (stmt: ClosedHir.Stmt) : bool =
        match stmt with
        | ClosedHir.Stmt.Let (_, _, value, _)
        | ClosedHir.Stmt.Assign (_, value, _)
        | ClosedHir.Stmt.ExprStmt (value, _) -> hasLambdaExpr value
        | ClosedHir.Stmt.For (_, _, iterable, body, _) ->
            hasLambdaExpr iterable || (body |> List.exists hasLambdaStmt)
        | ClosedHir.Stmt.ErrorStmt _ -> false

    /// クロージャー変換済み `ClosedHir.Assembly` を受け取り、MIR へ変換する。
    /// 入力は `ClosureConversion.preprocessAssembly` の出力であることが前提であり、
    /// 残留 Lambda ノードが存在する場合は診断エラーを返す。
    let layoutAssembly (asmName: string, asm: ClosedHir.Assembly) : PhaseResult<Mir.Assembly> =
        let residualLambdaDiagnostics =
            asm.modules
            |> List.collect (fun closedModule ->
                closedModule.methods
                |> List.choose (fun closedMethod ->
                    if hasLambdaExpr closedMethod.body then
                        Some(Diagnostic.Error($"Residual lambda remains after closure conversion. methodSid={closedMethod.sym.id}", closedMethod.span))
                    else
                        None))

        match residualLambdaDiagnostics with
        | residual when not residual.IsEmpty -> PhaseResult.failed residual
        | _ ->
            try
                let modules = asm.modules |> List.map layoutModule
                PhaseResult.succeeded (Mir.Assembly(asmName, modules)) []
            with ex ->
                PhaseResult.failed [ Diagnostic.Error($"Lowering failed: {ex.Message}", Atla.Core.Data.Span.Empty) ]