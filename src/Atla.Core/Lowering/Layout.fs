namespace Atla.Core.Lowering

open Atla.Core.Data
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
        /// ClosedHir の Label/Goto の labelId（メソッド内一意な int）から Mir.LabelId への対応表。
        /// 状態機械 MoveNext の resume ラベル等、前方参照されるラベルを安定して同一 Mir.LabelId へ解決する。
        gotoLabels: Map<int, Mir.LabelId>
    }

    /// 初期レイアウト状態（空フレーム、ラベルカウンタ 0）を返す。
    let private emptyState: LayoutState = { frame = Mir.Frame.empty; nextLabel = 0; gotoLabels = Map.empty }

    /// 一意なラベル ID を払い出し、更新後の状態と共に返す。
    let private freshLabel (state: LayoutState) : Mir.LabelId * LayoutState =
        Mir.LabelId state.nextLabel, { state with nextLabel = state.nextLabel + 1 }

    /// ClosedHir のラベル ID に対応する Mir.LabelId を取得（未割当なら新規払い出し）する。
    let private gotoLabel (state: LayoutState) (clId: int) : Mir.LabelId * LayoutState =
        match state.gotoLabels |> Map.tryFind clId with
        | Some lbl -> lbl, state
        | None ->
            let lbl, state1 = freshLabel state
            lbl, { state1 with gotoLabels = state1.gotoLabels |> Map.add clId lbl }

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

    /// Result を考慮した mapFold: エラーが発生したら最初のエラーを即座に返す。
    /// 各要素を状態と共に変換し、全成功時は (最終状態, 結果リスト) を返す。
    /// 結果リストは宣言順に保持される（内部では逆順に蓄積し最後に反転）。
    let private mapFoldResult
        (f: 'S -> 'T -> Result<'S * 'U, Diagnostic>)
        (state: 'S)
        (items: 'T list)
        : Result<'S * 'U list, Diagnostic> =
        items
        |> List.fold (fun acc item ->
            match acc with
            | Result.Error e -> Result.Error e
            | Ok (st, results) ->
                match f st item with
                | Result.Error e -> Result.Error e
                | Ok (st', result) -> Ok (st', result :: results)) (Ok (state, []))
        |> Result.map (fun (st, results) -> st, List.rev results)

    /// Result リストから全 Ok 値を収集する。最初の Error が見つかった時点で Error を返す。
    /// 結果リストは入力順に保持される（内部では逆順に蓄積し最後に反転）。
    let private collectResults (results: Result<'T, Diagnostic> list) : Result<'T list, Diagnostic> =
        results
        |> List.fold (fun acc r ->
            match acc, r with
            | Result.Error e, _ -> Result.Error e
            | _, Result.Error e -> Result.Error e
            | Ok vs, Ok v -> Ok (v :: vs)) (Ok [])
        |> Result.map List.rev

    let rec private layoutExpr (state: LayoutState) (expr: ClosedHir.Expr) : Result<LayoutState * KNormal, Diagnostic> =
        match expr with
        | ClosedHir.Expr.Unit _ -> Ok (state, { ins = []; res = None })
        | ClosedHir.Expr.Bool (value, _) -> Ok (state, { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Bool value)) })
        | ClosedHir.Expr.Int (value, _) -> Ok (state, { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Int value)) })
        | ClosedHir.Expr.Float (value, _) -> Ok (state, { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Float value)) })
        | ClosedHir.Expr.String (value, _) -> Ok (state, { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.String value)) })
        // null リテラル: 参照型は ldnull、Nullable<T> 値型は initobj+ldloc シーケンスで発行する。
        | ClosedHir.Expr.Null (tid, _) ->
            let imm =
                match tid with
                | TypeId.Native t when t.IsValueType -> Mir.Imm.NullableDefault t
                | _ -> Mir.Imm.Null
            Ok (state, { ins = []; res = Some(Mir.Value.ImmVal(imm)) })
        | ClosedHir.Expr.Id (sid, tid, _) ->
            match Mir.Frame.get sid state.frame with
            | Some reg -> Ok (state, { ins = []; res = Some(Mir.Value.RegVal reg) })
            | None ->
                // フレームに存在しない sid は「グローバル関数参照」として扱う。
                // TypeId.Fn の場合はデリゲートに変換し、それ以外はメソッド引数として追加する。
                match tid with
                | TypeId.Fn _ ->
                    match TypeId.tryToRuntimeSystemType tid with
                    | Some delegateType ->
                        // グローバル関数を対応するデリゲート（Func<>/Action<>）に包む。
                        Ok (state, { ins = []; res = Some(Mir.Value.FnDelegate(sid, delegateType, None)) })
                    | None ->
                        let argReg, newFrame = Mir.Frame.addArg sid tid state.frame
                        Ok ({ state with frame = newFrame }, { ins = []; res = Some(Mir.Value.RegVal argReg) })
                // .NET デリゲート型（Native）で注釈された関数参照はその型のまま使用する。
                | TypeId.Native t when TypeId.isDelegateType t ->
                    Ok (state, { ins = []; res = Some(Mir.Value.FnDelegate(sid, t, None)) })
                | _ ->
                    let argReg, newFrame = Mir.Frame.addArg sid tid state.frame
                    Ok ({ state with frame = newFrame }, { ins = []; res = Some(Mir.Value.RegVal argReg) })
        | ClosedHir.Expr.MemberAccess (mem, instance, tid, span) ->
            match mem with
            | Hir.Member.NativeField fi ->
                if fi.IsStatic then
                    // 静的フィールド（例: Color'White）は FieldVal（ldsfld）で読む。
                    Ok (state, { ins = []; res = Some(Mir.Value.FieldVal fi) })
                else
                    // インスタンスフィールドはレシーバーを評価して ldfld で読む。
                    match instance with
                    | None -> Result.Error (Diagnostic.Error(sprintf "Instance field '%s' access requires a receiver" fi.Name, span))
                    | Some instanceExpr ->
                        match layoutExpr state instanceExpr with
                        | Result.Error e -> Result.Error e
                        | Ok (state1, instanceKn) ->
                            match instanceKn.res with
                            | Some instVal ->
                                let dst, state2 = declareTemp state1 tid
                                Ok (state2, { ins = instanceKn.ins @ [ Mir.Ins.LoadNativeField(dst, instVal, fi) ]; res = Some(Mir.Value.RegVal dst) })
                            | None ->
                                Result.Error (Diagnostic.Error(sprintf "Instance field '%s' receiver did not produce a value" fi.Name, span))
            | Hir.Member.NativeMethod mi -> Ok (state, { ins = []; res = Some(Mir.Value.MethodVal mi) })
            | Hir.Member.NativeMethodGroup _ ->
                Result.Error (Diagnostic.Error("First-class native method group value is not supported", span))
            | Hir.Member.DataMethod _ ->
                Result.Error (Diagnostic.Error("First-class data method value is not supported", span))
            | Hir.Member.DataField (typeSid, fieldSid) ->
                match instance with
                | None -> Result.Error (Diagnostic.Error("Data field access requires an instance receiver", span))
                | Some instanceExpr ->
                    match layoutExpr state instanceExpr with
                    | Result.Error e -> Result.Error e
                    | Ok (state1, instanceKn) ->
                        match instanceKn.res with
                        | Some(Mir.Value.RegVal instReg) ->
                            let dst, state2 = declareTemp state1 tid
                            Ok (state2, { ins = instanceKn.ins @ [ Mir.Ins.LoadEnvField(dst, instReg, typeSid, fieldSid) ]; res = Some(Mir.Value.RegVal dst) })
                        | Some _ ->
                            Result.Error (Diagnostic.Error("Data field receiver must be a register value", span))
                        | None ->
                            Result.Error (Diagnostic.Error("Data field receiver did not produce a value", span))
            | Hir.Member.NativeProperty pi ->
                layoutPropertyGetter state pi instance tid span false
            | Hir.Member.NativeBaseProperty pi ->
                // base'X 由来のプロパティ getter は非仮想呼び出しで発行する。
                layoutPropertyGetter state pi instance tid span true
            | Hir.Member.NativeBaseMethod _ ->
                Result.Error (Diagnostic.Error("First-class native base method value is not supported", span))
            | Hir.Member.NativeBaseMethodGroup _ ->
                Result.Error (Diagnostic.Error("First-class native base method group value is not supported", span))
        | ClosedHir.Expr.Call (func, instanceOpt, args, tid, callSpan) ->
            // instance call の receiver を必ず先頭に載せ、Gen 側の call/callvirt 規約（receiver :: args）を満たす。
            let instanceLayoutResult =
                match instanceOpt with
                | Some instanceExpr -> layoutExpr state instanceExpr |> Result.map (fun (s1, kn) -> s1, Some kn)
                | None -> Ok (state, None)

            match instanceLayoutResult with
            | Result.Error e -> Result.Error e
            | Ok (state1, instanceKnOpt) ->
                match mapFoldResult layoutExpr state1 args with
                | Result.Error e -> Result.Error e
                | Ok (state2, argKnList) ->
                    // 命令列は receiver 評価 → 実引数評価の順で連結し、副作用順序を安定化する。
                    let instanceIns =
                        instanceKnOpt
                        |> Option.map (fun kn -> kn.ins)
                        |> Option.defaultValue []

                    let argIns = argKnList |> List.collect (fun k -> k.ins)

                    let instanceArgResult =
                        instanceKnOpt
                        |> Option.map (fun kn ->
                            match kn.res with
                            | Some value -> Ok [ value ]
                            | None -> Result.Error(Diagnostic.Error("Instance expression did not produce a value", callSpan)))
                        |> Option.defaultValue (Ok [])

                    let argValuesResult =
                        argKnList
                        |> List.map (fun k ->
                            match k.res with
                            | Some value -> Ok value
                            | None -> Result.Error (Diagnostic.Error("Argument expression did not produce a value", callSpan)))
                        |> collectResults
                    match argValuesResult with
                    | Result.Error e -> Result.Error e
                    | Ok argValues ->
                        match instanceArgResult with
                        | Result.Error e -> Result.Error e
                        | Ok instanceArgs ->
                            let callArgs = instanceArgs @ argValues
                            // MethodInfo を使った呼び出しを発行するヘルパー。
                            // nonVirtual=true の場合は `base'X` 由来として `CallBase`/`CallAssignBase` を発行する。
                            let emitCallWith (nonVirtual: bool) (st: LayoutState) (mi: System.Reflection.MethodInfo) =
                                let returnsUnit = (mi.ReturnType = typeof<System.Void>)
                                match tid, returnsUnit with
                                | _, true
                                | TypeId.Unit, _ ->
                                    let callIns =
                                        if nonVirtual then Mir.Ins.CallBase(mi, callArgs)
                                        else Mir.Ins.Call(Choice1Of2 mi, callArgs)
                                    Ok (st, { ins = instanceIns @ argIns @ [ callIns ]; res = None })
                                | _ ->
                                    let dst, st' = declareTemp st tid
                                    let callIns =
                                        if nonVirtual then Mir.Ins.CallAssignBase(dst, mi, callArgs)
                                        else Mir.Ins.CallAssign(dst, mi, callArgs)
                                    Ok (st', { ins = instanceIns @ argIns @ [ callIns ]; res = Some(Mir.Value.RegVal dst) })
                            let emitCall = emitCallWith false

                            match func with
                            | Hir.Callable.NativeMethod mi -> emitCall state2 mi
                            | Hir.Callable.NativeGenericMethod (methodDef, typeArgs) ->
                                // 型引数は Gen で resolveType→MakeGenericMethod される。args は instance を含めた全実引数。
                                let returnsVoid = (methodDef.ReturnType = typeof<System.Void>)
                                match tid, returnsVoid with
                                | _, true
                                | TypeId.Unit, _ ->
                                    Ok (state2, { ins = instanceIns @ argIns @ [ Mir.Ins.CallGenericNative(methodDef, typeArgs, callArgs) ]; res = None })
                                | _ ->
                                    let dst, state3 = declareTemp state2 tid
                                    Ok (state3, { ins = instanceIns @ argIns @ [ Mir.Ins.CallGenericNativeAssign(dst, methodDef, typeArgs, callArgs) ]; res = Some(Mir.Value.RegVal dst) })
                            | Hir.Callable.NativeMethodGroup methods ->
                                match methods |> List.tryFind (fun m -> m.GetParameters().Length = argValues.Length) with
                                | Some mi -> emitCall state2 mi
                                | None -> Result.Error (Diagnostic.Error($"No overload matched argument count {argValues.Length}", callSpan))
                            | Hir.Callable.NativeBaseMethod mi -> emitCallWith true state2 mi
                            | Hir.Callable.NativeBaseMethodGroup methods ->
                                match methods |> List.tryFind (fun m -> m.GetParameters().Length = argValues.Length) with
                                | Some mi -> emitCallWith true state2 mi
                                | None -> Result.Error (Diagnostic.Error($"No overload matched argument count {argValues.Length}", callSpan))
                            | Hir.Callable.NativeConstructor ctor ->
                                let dst, state3 = declareTemp state2 tid
                                Ok (state3, { ins = instanceIns @ argIns @ [ Mir.Ins.New(dst, ctor, callArgs) ]; res = Some(Mir.Value.RegVal dst) })
                            | Hir.Callable.NativeConstructorGroup ctors ->
                                match ctors |> List.tryFind (fun c -> c.GetParameters().Length = argValues.Length) with
                                | Some ctor ->
                                    let dst, state3 = declareTemp state2 tid
                                    Ok (state3, { ins = instanceIns @ argIns @ [ Mir.Ins.New(dst, ctor, callArgs) ]; res = Some(Mir.Value.RegVal dst) })
                                | None -> Result.Error (Diagnostic.Error($"No constructor matched argument count {argValues.Length}", callSpan))
                            | Hir.Callable.DataConstructor (typeSid, fieldSids) ->
                                let dst, state3 = declareTemp state2 tid
                                let newIns = Mir.Ins.NewEnv(dst, typeSid)
                                let storeIns =
                                    List.zip fieldSids callArgs
                                    |> List.map (fun (fieldSid, fieldValue) -> Mir.Ins.StoreEnvField(dst, typeSid, fieldSid, fieldValue))
                                Ok (state3, { ins = instanceIns @ argIns @ [ newIns ] @ storeIns; res = Some(Mir.Value.RegVal dst) })
                            | Hir.Callable.BuiltinOperator op ->
                                let dst, state3 = declareTemp state2 tid
                                match op with
                                | Builtins.OpNeg ->
                                    // 単項マイナス: `0 - operand` を MIR TAC として生成する。
                                    // 結果型（tid）に応じてゼロ即値を選択し、float/int の型整合を保つ。
                                    match argValues |> List.tryItem 0 with
                                    | Some operand ->
                                        let zeroImm =
                                            if tid = TypeId.Float then Mir.Value.ImmVal(Mir.Imm.Float 0.0)
                                            elif tid = TypeId.Single then Mir.Value.ImmVal(Mir.Imm.Single 0.0f)
                                            else Mir.Value.ImmVal(Mir.Imm.Int 0)
                                        Ok (state3, { ins = instanceIns @ argIns @ [ Mir.Ins.TAC(dst, zeroImm, Mir.OpCode.Sub, operand) ]; res = Some(Mir.Value.RegVal dst) })
                                    | None -> Result.Error (Diagnostic.Error("Missing operand for unary negation", callSpan))
                                | _ ->
                                    match argValues |> List.tryItem 0, argValues |> List.tryItem 1 with
                                    | Some lhs, Some rhs ->
                                        let opcode =
                                            match op with
                                            | Builtins.OpAdd -> Mir.OpCode.Add
                                            | Builtins.OpSub -> Mir.OpCode.Sub
                                            | Builtins.OpMul -> Mir.OpCode.Mul
                                            | Builtins.OpDiv -> Mir.OpCode.Div
                                            | Builtins.OpMod -> Mir.OpCode.Mod
                                            | Builtins.OpEq -> Mir.OpCode.Eq
                                            | Builtins.OpNe -> Mir.OpCode.Ne
                                            | Builtins.OpAnd -> Mir.OpCode.And
                                            | Builtins.OpOr -> Mir.OpCode.Or
                                            | Builtins.OpNeg -> failwith "OpNeg must be handled as unary before this match"
                                        Ok (state3, { ins = instanceIns @ argIns @ [ Mir.Ins.TAC(dst, lhs, opcode, rhs) ]; res = Some(Mir.Value.RegVal dst) })
                                    | None, _ -> Result.Error (Diagnostic.Error("Missing lhs operand for builtin operator", callSpan))
                                    | _, None -> Result.Error (Diagnostic.Error("Missing rhs operand for builtin operator", callSpan))
                            | Hir.Callable.BuiltinArray ->
                                let elemSysTypeOpt =
                                    match tid with
                                    | TypeId.App(TypeId.Native arrT, [elemTid]) when arrT = typeof<System.Array> ->
                                        TypeId.tryToRuntimeSystemType elemTid
                                    | _ -> None
                                match elemSysTypeOpt with
                                | None ->
                                    Result.Error(Diagnostic.Error("Cannot resolve element type for 'array' builtin", callSpan))
                                | Some elemSysType ->
                                    let dst, state3 = declareTemp state2 tid
                                    Ok (state3, { ins = argIns @ [ Mir.Ins.NewArr(dst, elemSysType, argValues) ]; res = Some(Mir.Value.RegVal dst) })
                            | Hir.Callable.BuiltinConvert targetTid ->
                                // 数値変換組込関数（toSingle/toFloat/toInt）を Convert 命令へ下す。
                                match TypeId.tryToRuntimeSystemType targetTid, argValues with
                                | Some targetSysType, [ srcVal ] ->
                                    let dst, state3 = declareTemp state2 tid
                                    Ok (state3, { ins = argIns @ [ Mir.Ins.Convert(dst, srcVal, targetSysType) ]; res = Some(Mir.Value.RegVal dst) })
                                | None, _ -> Result.Error (Diagnostic.Error("Cannot resolve target type for numeric conversion", callSpan))
                                | _, _ -> Result.Error (Diagnostic.Error("Numeric conversion expects exactly one argument", callSpan))
                            | Hir.Callable.Fn sid ->
                                match Mir.Frame.get sid state2.frame with
                                | Some delegateReg ->
                                    // sid がフレームに存在する → 引数または let 束縛されたデリゲートを介した呼び出し。
                                    // フレームから型を取得してデリゲート型の Invoke メソッドを得る。
                                    let fnTypeResult =
                                        match state2.frame.argTypes |> Map.tryFind sid with
                                        | Some t -> Ok t
                                        | None ->
                                            match state2.frame.locTypes |> Map.tryFind sid with
                                            | Some t -> Ok t
                                            | None -> Result.Error (Diagnostic.Error($"Unknown type for function-typed register: {sid}", callSpan))
                                    match fnTypeResult with
                                    | Result.Error e -> Result.Error e
                                    | Ok fnType ->
                                        match TypeId.tryToRuntimeSystemType fnType with
                                        | Some delegateType ->
                                            let invokeMethod = delegateType.GetMethod("Invoke")
                                            if obj.ReferenceEquals(invokeMethod, null) then
                                                Result.Error (Diagnostic.Error($"Delegate type '{delegateType.FullName}' has no Invoke method", callSpan))
                                            else
                                                // レシーバー（デリゲート自身）を先頭引数として渡す。
                                                let delegateCallArgs = (Mir.Value.RegVal delegateReg) :: argValues
                                                let returnsVoid = invokeMethod.ReturnType = typeof<System.Void>
                                                match tid, returnsVoid with
                                                | _, true
                                                | TypeId.Unit, _ ->
                                                    Ok (state2, { ins = instanceIns @ argIns @ [ Mir.Ins.Call(Choice1Of2 invokeMethod, delegateCallArgs) ]; res = None })
                                                | _ ->
                                                    let dst, state3 = declareTemp state2 tid
                                                    Ok (state3, { ins = instanceIns @ argIns @ [ Mir.Ins.CallAssign(dst, invokeMethod, delegateCallArgs) ]; res = Some(Mir.Value.RegVal dst) })
                                        | None ->
                                            Result.Error (Diagnostic.Error($"Cannot resolve delegate type for function-typed symbol: {sid}", callSpan))
                                | None ->
                                    // sid がフレームに存在しない → グローバル関数への直接呼び出し。
                                    match tid with
                                    | TypeId.Unit ->
                                        Ok (state2, { ins = instanceIns @ argIns @ [ Mir.Ins.CallSym(sid, argValues) ]; res = None })
                                    | _ ->
                                        let dst, state3 = declareTemp state2 tid
                                        Ok (state3, { ins = instanceIns @ argIns @ [ Mir.Ins.CallAssignSym(dst, sid, argValues) ]; res = Some(Mir.Value.RegVal dst) })
        | ClosedHir.Expr.Block (stmts, expr, _, _) ->
            match mapFoldResult layoutStmt state stmts with
            | Result.Error e -> Result.Error e
            | Ok (state1, stmtInsList) ->
                let stmtIns = List.concat stmtInsList
                match layoutExpr state1 expr with
                | Result.Error e -> Result.Error e
                | Ok (state2, exprKn) ->
                    Ok (state2, { ins = stmtIns @ exprKn.ins; res = exprKn.res })
        | ClosedHir.Expr.If (cond, thenBranch, elseBranch, tid, _) ->
            match layoutExpr state cond with
            | Result.Error e -> Result.Error e
            | Ok (state1, condKn) ->
                match layoutExpr state1 thenBranch with
                | Result.Error e -> Result.Error e
                | Ok (state2, thenKn) ->
                    match layoutExpr state2 elseBranch with
                    | Result.Error e -> Result.Error e
                    | Ok (state3, elseKn) ->
                        let thenLabelId, state4 = freshLabel state3
                        let elseLabelId, state5 = freshLabel state4
                        let endLabelId, state6 = freshLabel state5
                        match tid with
                        | TypeId.Unit ->
                            Ok (state6, {
                                ins =
                                    condKn.ins
                                    @ [ Mir.Ins.JumpTrue(condKn.res.Value, thenLabelId); Mir.Ins.JumpFalse(condKn.res.Value, elseLabelId) ]
                                    @ [ Mir.Ins.MarkLabel thenLabelId ] @ thenKn.ins @ [ Mir.Ins.Jump endLabelId ]
                                    @ [ Mir.Ins.MarkLabel elseLabelId ] @ elseKn.ins @ [ Mir.Ins.Jump endLabelId ]
                                    @ [ Mir.Ins.MarkLabel endLabelId ]
                                res = None
                            })
                        | _ ->
                            let dst, state7 = declareTemp state6 tid
                            Ok (state7, {
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
                            })
        | ClosedHir.Expr.ExprError (message, _, span) ->
            Result.Error (Diagnostic.Error($"Cannot lower erroneous expression: {message}", span))
        | ClosedHir.Expr.Lambda (_, _, _, _, span) ->
            Result.Error (Diagnostic.Error("Lambda lowering is not implemented", span))
        | ClosedHir.Expr.Await (_, _, span) ->
            // PR-3 で AsyncRewrite が状態機械化する前提のため、ここに来ることは想定していない。
            Result.Error (Diagnostic.Error("await expressions are not yet lowered to CIL (state machine generation pending)", span))
        // `&target` をマネージドポインタ値へ下す。target は Id / DataField / NativeField のいずれか。
        // AsyncRewrite 等の lowering が導入するノードであり、ユーザー言語からは生成されない。
        | ClosedHir.Expr.AddrOf (target, tid, span) ->
            match target with
            | ClosedHir.Expr.Id (sid, _, _) ->
                match Mir.Frame.get sid state.frame with
                | Some reg -> Ok (state, { ins = []; res = Some(Mir.Value.RegAddr reg) })
                | None -> Result.Error (Diagnostic.Error($"AddrOf: id {sid} not found in frame", span))
            | ClosedHir.Expr.MemberAccess (mem, instanceOpt, _, _) ->
                match mem, instanceOpt with
                | Hir.Member.DataField (typeSid, fieldSid), Some instExpr ->
                    match layoutExpr state instExpr with
                    | Result.Error e -> Result.Error e
                    | Ok (state1, instKn) ->
                        match instKn.res with
                        | Some (Mir.Value.RegVal instReg) ->
                            let dst, state2 = declareTemp state1 tid
                            Ok (state2, { ins = instKn.ins @ [ Mir.Ins.LoadEnvFieldAddr(dst, instReg, typeSid, fieldSid) ]; res = Some(Mir.Value.RegVal dst) })
                        | _ ->
                            Result.Error (Diagnostic.Error("AddrOf: data field instance must lower to a register", span))
                | Hir.Member.NativeField fi, Some instExpr ->
                    match layoutExpr state instExpr with
                    | Result.Error e -> Result.Error e
                    | Ok (state1, instKn) ->
                        match instKn.res with
                        | Some (Mir.Value.RegVal instReg) ->
                            Ok (state1, { ins = instKn.ins; res = Some(Mir.Value.FieldAddr(instReg, fi)) })
                        | _ ->
                            Result.Error (Diagnostic.Error("AddrOf: native field instance must lower to a register", span))
                | _ ->
                    Result.Error (Diagnostic.Error("AddrOf: only data/native field member access is supported", span))
            | _ ->
                Result.Error (Diagnostic.Error("AddrOf target must be Id or field MemberAccess", span))
        // env-class クロージャーフィールド参照: env インスタンス引数からフィールドを読み込む。
        | ClosedHir.Expr.EnvFieldLoad (envArgSid, capturedSid, tid, span) ->
            match Mir.Frame.get envArgSid state.frame with
            | None -> Result.Error (Diagnostic.Error($"Env arg {envArgSid} not found in frame for EnvFieldLoad", span))
            | Some envReg ->
                match state.frame.argTypes |> Map.tryFind envArgSid with
                | Some (TypeId.Name envTypeSid) ->
                    let dst, state1 = declareTemp state tid
                    Ok (state1, { ins = [ Mir.Ins.LoadEnvField(dst, envReg, envTypeSid, capturedSid) ]; res = Some(Mir.Value.RegVal dst) })
                | Some t ->
                    Result.Error (Diagnostic.Error($"Env arg {envArgSid} has unexpected type {t}; expected TypeId.Name", span))
                | None ->
                    Result.Error (Diagnostic.Error($"Env arg type not found in frame for {envArgSid}", span))
        // env-class クロージャー生成式: env インスタンスを生成し、捕捉変数を格納し、bound delegate を返す。
        | ClosedHir.Expr.ClosureCreate (envTypeSid, methodSid, captured, tid, span) ->
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
                Ok (state1, { ins = [ newEnvIns ] @ storeIns; res = Some(Mir.Value.FnDelegate(methodSid, delegateType, Some envReg)) })
            | None ->
                Result.Error (Diagnostic.Error($"Cannot resolve delegate type for ClosureCreate: {tid}", span))

    /// Property getter 呼び出しを MIR 命令列に変換する共通ヘルパー。
    /// `nonVirtual=true` の場合（`base'X` 由来）は `CallBase`/`CallAssignBase` を発行し、
    /// `false` の場合は通常の `Call`/`CallAssign` を発行する。
    and private layoutPropertyGetter
        (state: LayoutState)
        (pi: System.Reflection.PropertyInfo)
        (instance: ClosedHir.Expr option)
        (tid: TypeId)
        (span: Span)
        (nonVirtual: bool)
        : Result<LayoutState * KNormal, Diagnostic> =
        match pi.GetMethod with
        | null -> Result.Error (Diagnostic.Error($"Property has no getter: {pi.Name}", span))
        | getter ->
            let instanceResult =
                match instance with
                | Some expr -> layoutExpr state expr |> Result.map (fun (s1, kn) -> s1, Some kn)
                | None -> Ok (state, None)
            match instanceResult with
            | Result.Error e -> Result.Error e
            | Ok (state1, instanceKnOpt) ->
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
                    let callIns =
                        if nonVirtual then Mir.Ins.CallBase(getter, instanceArg)
                        else Mir.Ins.Call(Choice1Of2 getter, instanceArg)
                    Ok (state1, { ins = instanceIns @ [ callIns ]; res = None })
                else
                    let dst, state2 = declareTemp state1 tid
                    let callIns =
                        if nonVirtual then Mir.Ins.CallAssignBase(dst, getter, instanceArg)
                        else Mir.Ins.CallAssign(dst, getter, instanceArg)
                    Ok (state2, { ins = instanceIns @ [ callIns ]; res = Some(Mir.Value.RegVal dst) })

    and private layoutStmt (state: LayoutState) (stmt: ClosedHir.Stmt) : Result<LayoutState * Mir.Ins list, Diagnostic> =
        match stmt with
        | ClosedHir.Stmt.Let (sid, _, value, _) ->
            match layoutExpr state value with
            | Result.Error e -> Result.Error e
            | Ok (state1, valueKn) ->
                let dst, newFrame = Mir.Frame.addLoc sid value.typ state1.frame
                let state2 = { state1 with frame = newFrame }
                match valueKn.res with
                | Some v -> Ok (state2, valueKn.ins @ [ Mir.Ins.Assign(dst, v) ])
                | None -> Ok (state2, valueKn.ins)
        | ClosedHir.Stmt.Assign (sid, value, span) ->
            match layoutExpr state value with
            | Result.Error e -> Result.Error e
            | Ok (state1, valueKn) ->
                match Mir.Frame.get sid state1.frame, valueKn.res with
                | Some dst, Some v -> Ok (state1, valueKn.ins @ [ Mir.Ins.Assign(dst, v) ])
                | Some _, None -> Ok (state1, valueKn.ins)
                | None, _ -> Result.Error (Diagnostic.Error($"Undefined variable in assignment: {sid}", span))
        | ClosedHir.Stmt.ExprStmt (expr, _) ->
            match layoutExpr state expr with
            | Result.Error e -> Result.Error e
            | Ok (state1, kn) -> Ok (state1, kn.ins)
        | ClosedHir.Stmt.StoreField (instanceExpr, typeSid, fieldSid, value, span) ->
            // インスタンス式をレイアウトし、値式をレイアウトしてから StoreEnvField を発行する。
            match layoutExpr state instanceExpr with
            | Result.Error e -> Result.Error e
            | Ok (state1, instKn) ->
                match layoutExpr state1 value with
                | Result.Error e -> Result.Error e
                | Ok (state2, valueKn) ->
                    match instKn.res, valueKn.res with
                    | None, _ -> Result.Error (Diagnostic.Error("StoreField: instance expression produced no value", span))
                    | _, None -> Result.Error (Diagnostic.Error("StoreField: value expression produced no value", span))
                    | Some instVal, Some valueVal ->
                        // StoreEnvField は Reg を要求するため、インスタンス値を一時レジスタへ格納する。
                        let instReg, state3 = declareTemp state2 instanceExpr.typ
                        Ok (state3, instKn.ins @ [ Mir.Ins.Assign(instReg, instVal) ] @ valueKn.ins @ [ Mir.Ins.StoreEnvField(instReg, typeSid, fieldSid, valueVal) ])
        | ClosedHir.Stmt.StoreNativeField (receiverExpr, field, value, span) ->
            // 値型（struct）レシーバーはアドレス経由で in-place 書き込みする（AddrOf 経路を再利用）。
            // 参照型（class）レシーバーはオブジェクト参照値をそのまま stfld のレシーバーに使う。
            let receiverLayout =
                if field.DeclaringType.IsValueType then
                    layoutExpr state (ClosedHir.Expr.AddrOf(receiverExpr, TypeId.ByRef receiverExpr.typ, span))
                else
                    layoutExpr state receiverExpr
            match receiverLayout with
            | Result.Error e -> Result.Error e
            | Ok (state1, recvKn) ->
                match layoutExpr state1 value with
                | Result.Error e -> Result.Error e
                | Ok (state2, valueKn) ->
                    match recvKn.res, valueKn.res with
                    | None, _ -> Result.Error (Diagnostic.Error("StoreNativeField: receiver expression produced no value", span))
                    | _, None -> Result.Error (Diagnostic.Error("StoreNativeField: value expression produced no value", span))
                    | Some recvVal, Some valueVal ->
                        Ok (state2, recvKn.ins @ valueKn.ins @ [ Mir.Ins.StoreNativeField(recvVal, field, valueVal) ])
        | ClosedHir.Stmt.For (sid, tid, iterable, body, span) ->
            match layoutExpr state iterable with
            | Result.Error e -> Result.Error e
            | Ok (state1, iterableKn) ->
                let iterReg, state2 = declareTemp state1 iterable.typ
                match iterableKn.res with
                | None -> Result.Error (Diagnostic.Error($"For iterable expression did not produce a value", span))
                | Some iterValue ->
                    let iterAssignIns = [ Mir.Ins.Assign(iterReg, iterValue) ]
                    match TypeId.tryToRuntimeSystemType iterable.typ with
                    | None -> Result.Error (Diagnostic.Error($"For iterable type is not a runtime type: {iterable.typ}", span))
                    | Some iterType ->
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
                            Result.Error (Diagnostic.Error($"For iterable type '{iterType}' does not define MoveNext()", span))
                        elif obj.ReferenceEquals(currentGetter, null) then
                            Result.Error (Diagnostic.Error($"For iterable type '{iterType}' does not define Current getter", span))
                        else
                            let loopVarReg, loopVarFrame = Mir.Frame.addLoc sid tid state2.frame
                            let state3 = { state2 with frame = loopVarFrame }
                            let condReg, state4 = declareTemp state3 TypeId.Bool
                            let currentReg, state5 = declareTemp state4 tid
                            let loopStartId, state6 = freshLabel state5
                            let loopBodyId, state7 = freshLabel state6
                            let loopEndId, state8 = freshLabel state7
                            match mapFoldResult layoutStmt state8 body with
                            | Result.Error e -> Result.Error e
                            | Ok (state9, bodyInsList) ->
                                let bodyIns = List.concat bodyInsList
                                Ok (state9, (
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
                                ))
        | ClosedHir.Stmt.If (cond, thenBody, elseBody, _) ->
            match layoutExpr state cond with
            | Result.Error e -> Result.Error e
            | Ok (state1, condKn) ->
                match mapFoldResult layoutStmt state1 thenBody with
                | Result.Error e -> Result.Error e
                | Ok (state2, thenInsList) ->
                    match mapFoldResult layoutStmt state2 elseBody with
                    | Result.Error e -> Result.Error e
                    | Ok (state3, elseInsList) ->
                        let thenLabelId, state4 = freshLabel state3
                        let elseLabelId, state5 = freshLabel state4
                        let endLabelId, state6 = freshLabel state5
                        Ok (state6,
                            condKn.ins
                            @ [ Mir.Ins.JumpTrue(condKn.res.Value, thenLabelId)
                                Mir.Ins.JumpFalse(condKn.res.Value, elseLabelId) ]
                            @ [ Mir.Ins.MarkLabel thenLabelId ] @ List.concat thenInsList @ [ Mir.Ins.Jump endLabelId ]
                            @ [ Mir.Ins.MarkLabel elseLabelId ] @ List.concat elseInsList @ [ Mir.Ins.Jump endLabelId ]
                            @ [ Mir.Ins.MarkLabel endLabelId ])
        | ClosedHir.Stmt.Label (clId, _) ->
            let lbl, state1 = gotoLabel state clId
            Ok (state1, [ Mir.Ins.MarkLabel lbl ])
        | ClosedHir.Stmt.Goto (clId, _) ->
            let lbl, state1 = gotoLabel state clId
            Ok (state1, [ Mir.Ins.Jump lbl ])
        | ClosedHir.Stmt.Return _ ->
            Ok (state, [ Mir.Ins.Ret ])
        | ClosedHir.Stmt.Leave (clId, _) ->
            let lbl, state1 = gotoLabel state clId
            Ok (state1, [ Mir.Ins.Leave lbl ])
        | ClosedHir.Stmt.TryCatch (tryBody, catchType, catchVarSid, catchBody, _) ->
            match mapFoldResult layoutStmt state tryBody with
            | Result.Error e -> Result.Error e
            | Ok (state1, tryInsList) ->
                // catch 変数（例外）を Loc として frame に登録してから catchBody を layout する。
                let _, frame2 = Mir.Frame.addLoc catchVarSid (TypeId.Native catchType) state1.frame
                let state2 = { state1 with frame = frame2 }
                let catchVarReg =
                    match Mir.Frame.get catchVarSid state2.frame with
                    | Some reg -> reg
                    | None -> failwith "TryCatch: catch variable register not found after registration"
                match mapFoldResult layoutStmt state2 catchBody with
                | Result.Error e -> Result.Error e
                | Ok (state3, catchInsList) ->
                    Ok (state3, [ Mir.Ins.TryCatch(List.concat tryInsList, catchType, catchVarReg, List.concat catchInsList) ])
        | ClosedHir.Stmt.ErrorStmt (message, span) ->
            Result.Error (Diagnostic.Error($"Cannot lower erroneous statement: {message}", span))

    let private layoutMethod (methodName: string) (hirMethod: ClosedHir.Method) : Result<Mir.Method, Diagnostic> =
        // メソッド引数を宣言順にフレームへ事前登録する。
        // これにより、ボディ内で引数を参照したときに正しい Arg インデックスが割り当てられる。
        let initFrame =
            hirMethod.args |> List.fold (fun frame (argSid, argType) ->
                let _, newFrame = Mir.Frame.addArg argSid argType frame
                newFrame) Mir.Frame.empty
        let initState = { emptyState with frame = initFrame }
        match hirMethod.typ with
        | TypeId.Fn (args, ret) ->
            match layoutExpr initState hirMethod.body with
            | Result.Error e -> Result.Error e
            | Ok (finalState, bodyKn) ->
                let body =
                    if ret = TypeId.Unit then bodyKn.ins @ [ Mir.Ins.Ret ]
                    else
                        match bodyKn.res with
                        | Some v -> bodyKn.ins @ [ Mir.Ins.RetValue v ]
                        | None -> bodyKn.ins @ [ Mir.Ins.Ret ]
                Ok (Mir.Method(methodName, hirMethod.sym, args, ret, body, None, finalState.frame))
        | _ -> Result.Error (Diagnostic.Error($"Expected function type for method: {hirMethod.typ}", hirMethod.span))

    /// データ型または role 型のインスタンスメソッドを生成する。
    /// layoutMethod との違い: 第一引数（this）を CIL の明示引数リストから除去する。
    /// CIL インスタンスメソッドでは 'this' は暗黙の Arg(0) であり、DefineMethod のパラメーターに含めない。
    let private layoutDataTypeMethod (methodName: string) (hirMethod: ClosedHir.Method) : Result<Mir.Method, Diagnostic> =
        // 全引数（this を含む）をフレームへ事前登録する。
        // CIL では this が Arg(0) のため、宣言順に登録することで正しいインデックスが得られる。
        let initFrame =
            hirMethod.args |> List.fold (fun frame (argSid, argType) ->
                let _, newFrame = Mir.Frame.addArg argSid argType frame
                newFrame) Mir.Frame.empty
        let initState = { emptyState with frame = initFrame }
        match hirMethod.typ with
        | TypeId.Fn (_, ret) ->
            match layoutExpr initState hirMethod.body with
            | Result.Error e -> Result.Error e
            | Ok (finalState, bodyKn) ->
                let body =
                    if ret = TypeId.Unit then bodyKn.ins @ [ Mir.Ins.Ret ]
                    else
                        match bodyKn.res with
                        | Some v -> bodyKn.ins @ [ Mir.Ins.RetValue v ]
                        | None -> bodyKn.ins @ [ Mir.Ins.Ret ]
                // CIL インスタンスメソッドの引数リストは 'this' を除いた明示引数のみ。
                let explicitArgs =
                    match hirMethod.args with
                    | _ :: rest -> rest |> List.map snd
                    | [] -> []
                Ok (Mir.Method(methodName, hirMethod.sym, explicitArgs, ret, body, hirMethod.overrideTarget, finalState.frame))
        | _ -> Result.Error (Diagnostic.Error($"Expected function type for data type method: {hirMethod.typ}", hirMethod.span))

    let private layoutType (typeName: string) (resolveMethodName: SymbolId -> string) (hirType: ClosedHir.Type) : Result<Mir.Type, Diagnostic> =
        let fields = hirType.fields |> List.map (fun field -> Mir.Field(field.sym, field.typ))
        let methodResults =
            hirType.methods
            |> List.map (fun hirMethod -> layoutDataTypeMethod (resolveMethodName hirMethod.sym) hirMethod)
        let methodErrors, methodSuccesses =
            methodResults
            |> List.fold (fun (errs, oks) r ->
                match r with
                | Result.Error e -> (e :: errs, oks)
                | Ok m -> (errs, m :: oks)) ([], [])
        if List.isEmpty methodErrors then
            Result.Ok(Mir.Type(typeName, hirType.sym, hirType.isInterface, hirType.baseType, hirType.typeParams, fields, [], List.rev methodSuccesses))
        else
            Result.Error(List.rev methodErrors |> List.head)

    /// env-class の invoke メソッドとして使用するインスタンスメソッドを生成する。
    /// 通常の layoutMethod との違い: 第一引数（env インスタンス）を CIL arg リストから除去する。
    let private layoutInvokeMethod (methodName: string) (hirMethod: ClosedHir.Method) : Result<Mir.Method, Diagnostic> =
        // invoke method の全引数をフレームへ事前登録する（第一引数 = env インスタンスを含む）。
        // インスタンスメソッドでは Arg(0) が 'this' に対応するため、第一引数 sid を Arg(0) に割り当てる。
        let initFrame =
            hirMethod.args |> List.fold (fun frame (argSid, argType) ->
                let _, newFrame = Mir.Frame.addArg argSid argType frame
                newFrame) Mir.Frame.empty
        let initState = { emptyState with frame = initFrame }
        match hirMethod.typ with
        | TypeId.Fn (_, ret) ->
            match layoutExpr initState hirMethod.body with
            | Result.Error e -> Result.Error e
            | Ok (finalState, bodyKn) ->
                let body =
                    if ret = TypeId.Unit then bodyKn.ins @ [ Mir.Ins.Ret ]
                    else
                        match bodyKn.res with
                        | Some v -> bodyKn.ins @ [ Mir.Ins.RetValue v ]
                        | None -> bodyKn.ins @ [ Mir.Ins.Ret ]
                // CIL インスタンスメソッドの arg 型リストは 'this' を除いた明示引数のみ。
                // hirMethod.args の先頭要素 (envArgSid, TypeId.Name envTypeSid) を除外する。
                let explicitArgs =
                    match hirMethod.args with
                    | _ :: rest -> rest |> List.map snd
                    | [] -> []
                Ok (Mir.Method(methodName, hirMethod.sym, explicitArgs, ret, body, None, finalState.frame))
        | _ -> Result.Error (Diagnostic.Error($"Expected function type for invoke method: {hirMethod.typ}", hirMethod.span))

    let private layoutModule (hirModule: ClosedHir.Module) : Result<Mir.Module, Diagnostic list> =
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
        // 各型と invoke メソッドの Result を収集し、失敗があれば診断リストを返す。
        let typeResults =
            hirModule.types
            |> List.map (fun hirType ->
                let baseTypeResult = layoutType (resolveTypeName hirType.sym) resolveMethodName hirType
                // この型に属するクロージャー invoke メソッドを収集し、インスタンスメソッドとして生成する。
                let invokeMethodResults =
                    hirModule.methods
                    |> List.choose (fun hirMethod ->
                        match closureInvokeMap |> Map.tryFind hirMethod.sym.id with
                        | Some envTypeSid when envTypeSid = hirType.sym.id ->
                            Some(layoutInvokeMethod (resolveMethodName hirMethod.sym) hirMethod)
                        | _ -> None)
                // invoke メソッドの成功・失敗を分類する（単一走査）。
                let invokeErrors, invokeSuccesses =
                    invokeMethodResults
                    |> List.fold (fun (errs, oks) r ->
                        match r with
                        | Result.Error e -> (e :: errs, oks)
                        | Ok m -> (errs, m :: oks)) ([], [])
                match baseTypeResult with
                | Result.Error e -> Result.Error [ e ]
                | Result.Ok baseType when List.isEmpty invokeErrors ->
                    Ok (Mir.Type(baseType.name, baseType.sym, baseType.isInterface, baseType.baseType, baseType.typeParams, baseType.fields, baseType.ctors, baseType.methods @ (List.rev invokeSuccesses)))
                | Result.Ok _ ->
                    Result.Error (List.rev invokeErrors))

        // クロージャー invoke メソッドはすでに型内に配置したため、モジュールレベルからは除外する。
        let methodResults =
            hirModule.methods
            |> List.filter (fun hirMethod -> not (closureInvokeMap |> Map.containsKey hirMethod.sym.id))
            |> List.map (fun hirMethod -> layoutMethod (resolveMethodName hirMethod.sym) hirMethod)

        // 型とメソッドの全エラーを収集し、エラーがなければ成功を返す（単一走査）。
        let typeErrors, typeSuccesses =
            typeResults
            |> List.fold (fun (errs, oks) r ->
                match r with
                | Result.Error es -> (errs @ es, oks)
                | Ok t -> (errs, t :: oks)) ([], [])
        let methodErrors, methodSuccesses =
            methodResults
            |> List.fold (fun (errs, oks) r ->
                match r with
                | Result.Error e -> (e :: errs, oks)
                | Ok m -> (errs, m :: oks)) ([], [])
        let allErrors = typeErrors @ methodErrors
        if List.isEmpty allErrors then
            Ok (Mir.Module(hirModule.name, List.rev typeSuccesses, List.rev methodSuccesses))
        else
            Result.Error allErrors

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
        | ClosedHir.Stmt.StoreField (instanceExpr, _, _, value, _) ->
            hasLambdaExpr instanceExpr || hasLambdaExpr value
        | ClosedHir.Stmt.StoreNativeField (receiver, _, value, _) ->
            hasLambdaExpr receiver || hasLambdaExpr value
        | ClosedHir.Stmt.For (_, _, iterable, body, _) ->
            hasLambdaExpr iterable || (body |> List.exists hasLambdaStmt)
        | ClosedHir.Stmt.If (cond, thenBody, elseBody, _) ->
            hasLambdaExpr cond
            || (thenBody |> List.exists hasLambdaStmt)
            || (elseBody |> List.exists hasLambdaStmt)
        | ClosedHir.Stmt.TryCatch (tryBody, _, _, catchBody, _) ->
            (tryBody |> List.exists hasLambdaStmt) || (catchBody |> List.exists hasLambdaStmt)
        | ClosedHir.Stmt.Label _ | ClosedHir.Stmt.Goto _ | ClosedHir.Stmt.Return _ | ClosedHir.Stmt.Leave _ -> false
        | ClosedHir.Stmt.ErrorStmt _ -> false

    /// クロージャー変換済み `ClosedHir.Assembly` を受け取り、MIR へ変換する。
    /// 入力は `ClosureConversion.preprocessAssembly` の出力であることが前提であり、
    /// 残留 Lambda ノードが存在する場合は診断エラーを返す。
    let layoutAssembly (asmName: string, asm: ClosedHir.Assembly) : PhaseResult<Mir.Assembly> =
        let residualLambdaDiagnostics =
            asm.modules
            |> List.collect (fun closedModule ->
                // モジュールレベルメソッドの残留 Lambda を検出する。
                let moduleMethodDiags =
                    closedModule.methods
                    |> List.choose (fun closedMethod ->
                        if hasLambdaExpr closedMethod.body then
                            Some(Diagnostic.Error($"Residual lambda remains after closure conversion. methodSid={closedMethod.sym.id}", closedMethod.span))
                        else
                            None)
                // 型インスタンスメソッドの残留 Lambda を検出する。
                let typeMethodDiags =
                    closedModule.types
                    |> List.filter (fun t -> not t.isInterface)
                    |> List.collect (fun t ->
                        t.methods
                        |> List.choose (fun closedMethod ->
                            if hasLambdaExpr closedMethod.body then
                                Some(Diagnostic.Error($"Residual lambda remains after closure conversion in instance method. methodSid={closedMethod.sym.id}", closedMethod.span))
                            else
                                None))
                moduleMethodDiags @ typeMethodDiags)

        match residualLambdaDiagnostics with
        | residual when not residual.IsEmpty -> PhaseResult.failed residual
        | _ ->
            let moduleResults = asm.modules |> List.map layoutModule
            // 全モジュールの成功・失敗を単一走査で分類する。
            let allErrors, moduleSuccesses =
                moduleResults
                |> List.fold (fun (errs, oks) r ->
                    match r with
                    | Result.Error es -> (errs @ es, oks)
                    | Ok m -> (errs, m :: oks)) ([], [])
            if List.isEmpty allErrors then
                PhaseResult.succeeded (Mir.Assembly(asmName, List.rev moduleSuccesses)) []
            else
                PhaseResult.failed allErrors
