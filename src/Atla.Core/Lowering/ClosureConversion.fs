namespace Atla.Core.Lowering

open Atla.Core.Semantics.Data

module ClosureConversion =
    /// 捕捉変数1件分のメタデータ。
    type private CapturedVarMetadata = {
        sid: int
        isMutable: bool
        typ: TypeId
    }

    /// 1つのラムダ式に対する捕捉情報メタデータ。
    type private LambdaCaptureMetadata = {
        ownerMethodSid: int
        span: Atla.Core.Data.Span
        captured: CapturedVarMetadata list
    }

    /// モジュール内で一意な SymbolId 採番を行う状態。
    type private ConversionState = {
        nextSymbolId: int
        generatedMethods: ClosedHir.Method list
        generatedTypes: ClosedHir.Type list
        // クロージャー invoke メソッドの (liftedMethodSid -> envTypeSid) マッピング。
        closureInvokeMethods: Map<int, int>
        diagnostics: Diagnostic list
        captureMetadata: LambdaCaptureMetadata list
    }

    /// 変換状態へ新しい SymbolId を採番して返す。
    let private allocateSymbolId (state: ConversionState) : SymbolId * ConversionState =
        let sid = SymbolId state.nextSymbolId
        sid, { state with nextSymbolId = state.nextSymbolId + 1 }

    /// `Hir.Expr` を構造的に `ClosedHir.Expr` へ変換する（クロージャー変換なし）。
    /// ソース HIR の型フィールド初期化式やモジュールフィールドなど、
    /// クロージャー変換が適用されないパスで HIR→ClosedHir の型変換のみを行う。
    /// Lambda ノードが現れた場合もクロージャー変換は行わず構造的に変換する。
    let rec private convertHirExpr (expr: Hir.Expr) : ClosedHir.Expr =
        match expr with
        | Hir.Expr.Unit span -> ClosedHir.Expr.Unit span
        | Hir.Expr.Int (v, span) -> ClosedHir.Expr.Int(v, span)
        | Hir.Expr.Float (v, span) -> ClosedHir.Expr.Float(v, span)
        | Hir.Expr.String (v, span) -> ClosedHir.Expr.String(v, span)
        | Hir.Expr.Null (tid, span) -> ClosedHir.Expr.Null(tid, span)
        | Hir.Expr.Id (sid, tid, span) -> ClosedHir.Expr.Id(sid, tid, span)
        | Hir.Expr.Call (func, instance, args, tid, span) ->
            ClosedHir.Expr.Call(func, instance |> Option.map convertHirExpr, args |> List.map convertHirExpr, tid, span)
        | Hir.Expr.Lambda (args, ret, body, tid, span) ->
            ClosedHir.Expr.Lambda(args, ret, convertHirExpr body, tid, span)
        | Hir.Expr.MemberAccess (mem, instance, tid, span) ->
            ClosedHir.Expr.MemberAccess(mem, instance |> Option.map convertHirExpr, tid, span)
        | Hir.Expr.Block (stmts, body, tid, span) ->
            ClosedHir.Expr.Block(stmts |> List.map convertHirStmt, convertHirExpr body, tid, span)
        | Hir.Expr.If (cond, thenBranch, elseBranch, tid, span) ->
            ClosedHir.Expr.If(convertHirExpr cond, convertHirExpr thenBranch, convertHirExpr elseBranch, tid, span)
        | Hir.Expr.ExprError (msg, tid, span) -> ClosedHir.Expr.ExprError(msg, tid, span)

    /// `Hir.Stmt` を構造的に `ClosedHir.Stmt` へ変換する（クロージャー変換なし）。
    and private convertHirStmt (stmt: Hir.Stmt) : ClosedHir.Stmt =
        match stmt with
        | Hir.Stmt.Let (sid, isMutable, value, span) -> ClosedHir.Stmt.Let(sid, isMutable, convertHirExpr value, span)
        | Hir.Stmt.Assign (sid, value, span) -> ClosedHir.Stmt.Assign(sid, convertHirExpr value, span)
        | Hir.Stmt.ExprStmt (expr, span) -> ClosedHir.Stmt.ExprStmt(convertHirExpr expr, span)
        | Hir.Stmt.For (sid, tid, iterable, body, span) ->
            ClosedHir.Stmt.For(sid, tid, convertHirExpr iterable, body |> List.map convertHirStmt, span)
        | Hir.Stmt.ErrorStmt (msg, span) -> ClosedHir.Stmt.ErrorStmt(msg, span)

    /// 式内の自由変数集合を収集する（globalSymbols は捕捉対象から除外）。
    /// `rewriteExpr` で `Hir.Expr` から変換済みの `ClosedHir.Expr` に対して呼び出す。
    /// Lambda ノードは `rewriteExpr` 内でネストしたラムダの捕捉判定に使用する。
    let rec private collectFreeVarsExpr (bound: Set<int>) (globalSymbols: Set<int>) (expr: ClosedHir.Expr) : Set<int> =
        match expr with
        | ClosedHir.Expr.Unit _
        | ClosedHir.Expr.Int _
        | ClosedHir.Expr.Float _
        | ClosedHir.Expr.String _
        | ClosedHir.Expr.Null _
        | ClosedHir.Expr.ExprError _ -> Set.empty
        | ClosedHir.Expr.Id (sid, _, _) ->
            if bound.Contains sid.id || globalSymbols.Contains sid.id then Set.empty else Set.singleton sid.id
        | ClosedHir.Expr.MemberAccess (_, instance, _, _) ->
            instance
            |> Option.map (collectFreeVarsExpr bound globalSymbols)
            |> Option.defaultValue Set.empty
        | ClosedHir.Expr.Call (_, instance, args, _, _) ->
            let instanceVars =
                instance
                |> Option.map (collectFreeVarsExpr bound globalSymbols)
                |> Option.defaultValue Set.empty
            let argVars = args |> List.map (collectFreeVarsExpr bound globalSymbols) |> List.fold Set.union Set.empty
            Set.union instanceVars argVars
        | ClosedHir.Expr.Block (stmts, body, _, _) ->
            let freeInStmts, boundAfterStmts = collectFreeVarsStmts bound globalSymbols stmts
            let freeInBody = collectFreeVarsExpr boundAfterStmts globalSymbols body
            Set.union freeInStmts freeInBody
        | ClosedHir.Expr.If (cond, thenBranch, elseBranch, _, _) ->
            [ collectFreeVarsExpr bound globalSymbols cond
              collectFreeVarsExpr bound globalSymbols thenBranch
              collectFreeVarsExpr bound globalSymbols elseBranch ]
            |> List.fold Set.union Set.empty
        | ClosedHir.Expr.Lambda (args, _, body, _, _) ->
            // ラムダの自由変数判定では、外側スコープの束縛は「自由変数候補」として扱う。
            // ここではラムダ自身の引数のみを束縛集合へ入れる。
            let lambdaBound = args |> List.fold (fun (acc: Set<int>) arg -> acc.Add arg.sid.id) Set.empty
            collectFreeVarsExpr lambdaBound globalSymbols body
        // EnvFieldLoad は env インスタンス引数へのアクセスで、外側スコープの自由変数ではない。
        | ClosedHir.Expr.EnvFieldLoad _ -> Set.empty
        // ClosureCreate が捕捉している変数 sid は外側スコープで参照が必要。
        | ClosedHir.Expr.ClosureCreate (_, _, captured, _, _) ->
            captured |> List.map (fun (sid, _, _) -> sid.id) |> Set.ofList

    /// 文列を宣言順に走査し、自由変数集合と更新後 bound を返す。
    and private collectFreeVarsStmts (bound: Set<int>) (globalSymbols: Set<int>) (stmts: ClosedHir.Stmt list) : Set<int> * Set<int> =
        stmts
        |> List.fold (fun (freeVars, currentBound) stmt ->
            let stmtFree, nextBound = collectFreeVarsStmt currentBound globalSymbols stmt
            Set.union freeVars stmtFree, nextBound) (Set.empty, bound)

    /// 単一文の自由変数集合と更新後 bound を返す。
    and private collectFreeVarsStmt (bound: Set<int>) (globalSymbols: Set<int>) (stmt: ClosedHir.Stmt) : Set<int> * Set<int> =
        match stmt with
        | ClosedHir.Stmt.Let (sid, _, value, _) ->
            collectFreeVarsExpr bound globalSymbols value, bound.Add sid.id
        | ClosedHir.Stmt.Assign (_, value, _)
        | ClosedHir.Stmt.ExprStmt (value, _) ->
            collectFreeVarsExpr bound globalSymbols value, bound
        | ClosedHir.Stmt.For (sid, _, iterable, body, _) ->
            let iterableVars = collectFreeVarsExpr bound globalSymbols iterable
            // for 反復変数 sid はボディ内で束縛済みとして扱う。
            // ラムダがボディ内でこの変数を参照した場合は「捕捉」とみなされる（env-class 方式で処理される）。
            // これは C#互換の「反復ごと新規束縛」セマンティクスに対応している。
            let bodyVars, _ = collectFreeVarsStmts (bound.Add sid.id) globalSymbols body
            Set.union iterableVars bodyVars, bound
        | ClosedHir.Stmt.ErrorStmt _ -> Set.empty, bound

    /// lifted invoke method 本体内の捕捉変数参照（ClosedHir.Expr.Id）を EnvFieldLoad へ書き換える。
    /// `ClosedHir.mapExpr` を用いて構造的再帰をインフラに委譲する。
    let private rewriteCapturedRefs (envArgSid: SymbolId) (capturedSids: Set<int>) (expr: ClosedHir.Expr) : ClosedHir.Expr =
        ClosedHir.mapExpr (fun e ->
            match e with
            | ClosedHir.Expr.Id (sid, tid, span) when capturedSids.Contains sid.id ->
                // 捕捉変数参照を env インスタンスのフィールドアクセスへ変換する。
                ClosedHir.Expr.EnvFieldLoad(envArgSid, sid, tid, span)
            | _ -> e) expr

    /// 捕捉変数参照の書き換えを文に適用する。
    let private rewriteCapturedRefsStmt (envArgSid: SymbolId) (capturedSids: Set<int>) (stmt: ClosedHir.Stmt) : ClosedHir.Stmt =
        ClosedHir.mapStmt (fun e ->
            match e with
            | ClosedHir.Expr.Id (sid, tid, span) when capturedSids.Contains sid.id ->
                ClosedHir.Expr.EnvFieldLoad(envArgSid, sid, tid, span)
            | _ -> e) stmt

    /// lambda lifting のために式を再帰変換し（入力: `Hir.Expr`、出力: `ClosedHir.Expr`）、
    /// 必要な生成メソッドを state に蓄積する。
    let rec private rewriteExpr (ownerMethod: Hir.Method) (bound: Set<int>) (bindings: Map<int, bool * TypeId>) (globalSymbols: Set<int>) (expr: Hir.Expr) (state: ConversionState) : ClosedHir.Expr * ConversionState =
        match expr with
        | Hir.Expr.Unit span -> ClosedHir.Expr.Unit span, state
        | Hir.Expr.Int (v, span) -> ClosedHir.Expr.Int(v, span), state
        | Hir.Expr.Float (v, span) -> ClosedHir.Expr.Float(v, span), state
        | Hir.Expr.String (v, span) -> ClosedHir.Expr.String(v, span), state
        | Hir.Expr.Null (tid, span) -> ClosedHir.Expr.Null(tid, span), state
        | Hir.Expr.Id (sid, tid, span) -> ClosedHir.Expr.Id(sid, tid, span), state
        | Hir.Expr.ExprError (msg, tid, span) -> ClosedHir.Expr.ExprError(msg, tid, span), state
        | Hir.Expr.MemberAccess (mem, instance, tid, span) ->
            let rewrittenInstance, nextState =
                match instance with
                | Some instExpr ->
                    let rewrittenExpr, rewrittenState = rewriteExpr ownerMethod bound bindings globalSymbols instExpr state
                    Some rewrittenExpr, rewrittenState
                | None -> None, state
            ClosedHir.Expr.MemberAccess(mem, rewrittenInstance, tid, span), nextState
        | Hir.Expr.Call (func, instance, args, tid, span) ->
            let rewrittenInstance, instanceState =
                match instance with
                | Some instExpr ->
                    let rewrittenExpr, rewrittenState = rewriteExpr ownerMethod bound bindings globalSymbols instExpr state
                    Some rewrittenExpr, rewrittenState
                | None -> None, state
            let rewrittenArgs, finalState =
                args
                |> List.fold (fun (acc, st) arg ->
                    let rewrittenArg, nextState = rewriteExpr ownerMethod bound bindings globalSymbols arg st
                    acc @ [ rewrittenArg ], nextState) ([], instanceState)
            ClosedHir.Expr.Call(func, rewrittenInstance, rewrittenArgs, tid, span), finalState
        | Hir.Expr.Block (stmts, body, tid, span) ->
            let rewrittenStmts, boundAfterStmts, bindingsAfterStmts, stateAfterStmts = rewriteStmts ownerMethod bound bindings globalSymbols stmts state
            let rewrittenBody, finalState = rewriteExpr ownerMethod boundAfterStmts bindingsAfterStmts globalSymbols body stateAfterStmts
            ClosedHir.Expr.Block(rewrittenStmts, rewrittenBody, tid, span), finalState
        | Hir.Expr.If (cond, thenBranch, elseBranch, tid, span) ->
            let rewrittenCond, condState = rewriteExpr ownerMethod bound bindings globalSymbols cond state
            let rewrittenThen, thenState = rewriteExpr ownerMethod bound bindings globalSymbols thenBranch condState
            let rewrittenElse, finalState = rewriteExpr ownerMethod bound bindings globalSymbols elseBranch thenState
            ClosedHir.Expr.If(rewrittenCond, rewrittenThen, rewrittenElse, tid, span), finalState
        | Hir.Expr.Lambda (args, ret, body, tid, span) ->
            // 捕捉判定は「ラムダ自身の引数」を束縛として扱う（外側束縛は捕捉対象）。
            let lambdaBound = args |> List.fold (fun (acc: Set<int>) arg -> acc.Add arg.sid.id) Set.empty
            let lambdaBindings =
                args
                |> List.fold (fun (acc: Map<int, bool * TypeId>) arg -> acc.Add(arg.sid.id, (false, arg.typ))) bindings
            let rewrittenBody, bodyState = rewriteExpr ownerMethod lambdaBound lambdaBindings globalSymbols body state
            let captured =
                collectFreeVarsExpr lambdaBound globalSymbols rewrittenBody
                |> Set.toList
                |> List.sort

            match captured with
            | [] ->
                // 非捕捉ラムダ: static delegate として lambda lifting する。
                let liftedSid, sidAllocatedState = allocateSymbolId bodyState
                let methodArgs = args |> List.map (fun arg -> arg.sid, arg.typ)
                let liftedMethodType = TypeId.Fn(methodArgs |> List.map snd, ret)
                let liftedMethod = ClosedHir.Method(liftedSid, methodArgs, rewrittenBody, liftedMethodType, span)
                let updatedState = { sidAllocatedState with generatedMethods = sidAllocatedState.generatedMethods @ [ liftedMethod ] }
                ClosedHir.Expr.Id(liftedSid, tid, span), updatedState
            | _ ->
                // 捕捉ラムダ: env-class 方式でクロージャーを生成する。
                let capturedWithBindings =
                    captured
                    |> List.map (fun sid ->
                        match bindings.TryFind sid with
                        | Some(isMutable, typ) -> Some { sid = sid; isMutable = isMutable; typ = typ }, None
                        | None -> None, Some sid)
                let unknownSids = capturedWithBindings |> List.choose snd
                let capturedMetadata = capturedWithBindings |> List.choose fst

                match unknownSids with
                | _ :: _ ->
                    // 型情報を持たない捕捉変数が存在する（不正な HIR）。
                    let unknownText = unknownSids |> List.map string |> String.concat ", "
                    let diagnostic =
                        Diagnostic.Error(
                            $"Closure conversion failed: captured variable(s) have no type information. sids=[{unknownText}], ownerMethodSid={ownerMethod.sym.id}",
                            ownerMethod.span)
                    { bodyState with diagnostics = bodyState.diagnostics @ [ diagnostic ] }
                    |> fun s -> ClosedHir.Expr.Lambda(args, ret, rewrittenBody, tid, span), s
                | [] ->
                    // env クラスの SymbolId を採番する。
                    let envTypeSid, state1 = allocateSymbolId bodyState
                    // lifted invoke メソッドの SymbolId を採番する。
                    let liftedMethodSid, state2 = allocateSymbolId state1
                    // lifted method 内で env インスタンス引数として使う SymbolId を採番する。
                    let envArgSid, state3 = allocateSymbolId state2

                    // env クラスの ClosedHir.Field エントリを生成（各捕捉変数に対して 1 フィールド）。
                    let envFields =
                        capturedMetadata
                        |> List.map (fun cm ->
                    // フィールドの body は Unit で代用する（実際の値は外側スコープから StoreEnvField で格納される）。
                            ClosedHir.Field(SymbolId cm.sid, cm.typ, ClosedHir.Expr.Unit span, span))

                    // ラムダ本体内の捕捉変数参照を EnvFieldLoad へ書き換える。
                    let capturedSidSet = capturedMetadata |> List.map (fun cm -> cm.sid) |> Set.ofList
                    let rewrittenBodyWithEnv = rewriteCapturedRefs envArgSid capturedSidSet rewrittenBody

                    // lifted invoke method を生成する。
                    // 第一引数は env インスタンス（TypeId.Name envTypeSid）、残りはラムダ引数。
                    let methodArgs = (envArgSid, TypeId.Name envTypeSid) :: (args |> List.map (fun arg -> arg.sid, arg.typ))
                    let liftedMethodType = TypeId.Fn(methodArgs |> List.map snd, ret)
                    let liftedMethod = ClosedHir.Method(liftedMethodSid, methodArgs, rewrittenBodyWithEnv, liftedMethodType, span)

                    // env-class の ClosedHir.Type を生成する。
                    let envType = ClosedHir.Type(envTypeSid, envFields)

                    // ClosureCreate 式を生成する。
                    let capturedForCreate = capturedMetadata |> List.map (fun cm -> SymbolId cm.sid, cm.typ, cm.isMutable)
                    let closureExpr = ClosedHir.Expr.ClosureCreate(envTypeSid, liftedMethodSid, capturedForCreate, tid, span)

                    let captureInfo = { ownerMethodSid = ownerMethod.sym.id; span = ownerMethod.span; captured = capturedMetadata }
                    let updatedState =
                        { state3 with
                            generatedMethods = state3.generatedMethods @ [ liftedMethod ]
                            generatedTypes = state3.generatedTypes @ [ envType ]
                            closureInvokeMethods = state3.closureInvokeMethods |> Map.add liftedMethodSid.id envTypeSid.id
                            captureMetadata = state3.captureMetadata @ [ captureInfo ] }

                    closureExpr, updatedState

    /// lambda lifting のために文を再帰変換し（入力: `Hir.Stmt`、出力: `ClosedHir.Stmt`）、
    /// Let の束縛を逐次反映する。
    and private rewriteStmt (ownerMethod: Hir.Method) (bound: Set<int>) (bindings: Map<int, bool * TypeId>) (globalSymbols: Set<int>) (stmt: Hir.Stmt) (state: ConversionState) : ClosedHir.Stmt * Set<int> * Map<int, bool * TypeId> * ConversionState =
        match stmt with
        | Hir.Stmt.Let (sid, isMutable, value, span) ->
            let rewrittenValue, nextState = rewriteExpr ownerMethod bound bindings globalSymbols value state
            let nextBindings = bindings.Add(sid.id, (isMutable, value.typ))
            ClosedHir.Stmt.Let(sid, isMutable, rewrittenValue, span), bound.Add sid.id, nextBindings, nextState
        | Hir.Stmt.Assign (sid, value, span) ->
            let rewrittenValue, nextState = rewriteExpr ownerMethod bound bindings globalSymbols value state
            ClosedHir.Stmt.Assign(sid, rewrittenValue, span), bound, bindings, nextState
        | Hir.Stmt.ExprStmt (value, span) ->
            let rewrittenValue, nextState = rewriteExpr ownerMethod bound bindings globalSymbols value state
            ClosedHir.Stmt.ExprStmt(rewrittenValue, span), bound, bindings, nextState
        | Hir.Stmt.For (sid, tid, iterable, body, span) ->
            let rewrittenIterable, iterableState = rewriteExpr ownerMethod bound bindings globalSymbols iterable state
            // for 反復変数 sid はボディ内で束縛済みとして扱い、ラムダからの捕捉候補となる（C#互換: 反復ごと新規束縛）。
            let bodyBindings = bindings.Add(sid.id, (false, tid))
            let rewrittenBody, _, _, bodyState = rewriteStmts ownerMethod (bound.Add sid.id) bodyBindings globalSymbols body iterableState
            ClosedHir.Stmt.For(sid, tid, rewrittenIterable, rewrittenBody, span), bound, bindings, bodyState
        | Hir.Stmt.ErrorStmt (msg, span) -> ClosedHir.Stmt.ErrorStmt(msg, span), bound, bindings, state

    /// 文列を逐次変換し、最終 bound と state を返す。
    and private rewriteStmts (ownerMethod: Hir.Method) (bound: Set<int>) (bindings: Map<int, bool * TypeId>) (globalSymbols: Set<int>) (stmts: Hir.Stmt list) (state: ConversionState) : ClosedHir.Stmt list * Set<int> * Map<int, bool * TypeId> * ConversionState =
        stmts
        |> List.fold (fun (rewrittenStmts, currentBound, currentBindings, currentState) stmt ->
            let rewrittenStmt, nextBound, nextBindings, nextState = rewriteStmt ownerMethod currentBound currentBindings globalSymbols stmt currentState
            rewrittenStmts @ [ rewrittenStmt ], nextBound, nextBindings, nextState) ([], bound, bindings, state)

    /// モジュール内の既存 SymbolId の最大値を収集する（採番開始位置に使用）。
    /// `Hir.foldExpr` を用いて構造的再帰をインフラに委譲する。
    /// Lambda の引数 SymbolId も対象に含める。
    let private collectMaxSymbolIdExpr (expr: Hir.Expr) : int =
        Hir.foldExpr (fun acc e ->
            match e with
            | Hir.Expr.Id (sid, _, _) -> max acc sid.id
            | Hir.Expr.Lambda (args, _, _, _, _) ->
                args |> List.fold (fun a arg -> max a arg.sid.id) acc
            | _ -> acc) -1 expr

    /// モジュール内の文に含まれる SymbolId の最大値を収集する。
    /// Let / Assign / For の束縛サイト sid も対象に含める。
    let private collectMaxSymbolIdStmt (stmt: Hir.Stmt) : int =
        let bindSid =
            match stmt with
            | Hir.Stmt.Let (sid, _, _, _)
            | Hir.Stmt.Assign (sid, _, _)
            | Hir.Stmt.For (sid, _, _, _, _) -> sid.id
            | _ -> -1
        max bindSid (Hir.foldStmt (fun acc e ->
            match e with
            | Hir.Expr.Id (sid, _, _) -> max acc sid.id
            | Hir.Expr.Lambda (args, _, _, _, _) ->
                args |> List.fold (fun a arg -> max a arg.sid.id) acc
            | _ -> acc) -1 stmt)

    /// モジュール内 method を順序保持で変換し、生成メソッドと型を末尾追加した一覧を返す。
    let private rewriteMethods (hirModule: Hir.Module) : ClosedHir.Method list * ClosedHir.Type list * Map<int, int> * Diagnostic list =
        let globalSymbols = hirModule.methods |> List.map (fun methodInfo -> methodInfo.sym.id) |> Set.ofList
        let maxInModule =
            let maxMethodSid = hirModule.methods |> List.map (fun methodInfo -> methodInfo.sym.id) |> List.fold max -1
            let maxExprSid = hirModule.methods |> List.map (fun methodInfo -> collectMaxSymbolIdExpr methodInfo.body) |> List.fold max -1
            max maxMethodSid maxExprSid
        let initialState =
            { nextSymbolId = maxInModule + 1
              generatedMethods = []
              generatedTypes = []
              closureInvokeMethods = Map.empty
              diagnostics = []
              captureMetadata = [] }

        let rewrittenMethods, finalState =
            hirModule.methods
            |> List.fold (fun (accMethods, state) methodInfo ->
                let methodBound = methodInfo.args |> List.map fst |> List.fold (fun (acc: Set<int>) sid -> acc.Add sid.id) Set.empty
                let methodBindings =
                    methodInfo.args
                    |> List.fold (fun (acc: Map<int, bool * TypeId>) (sid, tid) -> acc.Add(sid.id, (false, tid))) Map.empty
                let rewrittenBody, bodyState = rewriteExpr methodInfo methodBound methodBindings globalSymbols methodInfo.body state
                let rewrittenMethod = ClosedHir.Method(methodInfo.sym, methodInfo.args, rewrittenBody, methodInfo.typ, methodInfo.span)
                accMethods @ [ rewrittenMethod ], bodyState) ([], initialState)

        rewrittenMethods @ finalState.generatedMethods,
        finalState.generatedTypes,
        finalState.closureInvokeMethods,
        finalState.diagnostics

    /// クロージャー変換前処理を行い、ラムダを lambda lifting / env-class 方式で lower する。
    /// 入力: `Hir.Assembly`（ソース意味論 IR）、出力: `ClosedHir.Assembly`（変換後 IR）。
    let preprocessAssembly (asm: Hir.Assembly) : PhaseResult<ClosedHir.Assembly> =
        let rewrittenModules, diagnostics =
            asm.modules
            |> List.fold (fun (modules, allDiagnostics) hirModule ->
                let rewrittenMethods, generatedTypes, closureInvokeMethods, moduleDiagnostics = rewriteMethods hirModule
                // 元の HIR 型定義を ClosedHir.Type へ構造的に変換する。
                let convertedTypes =
                    hirModule.types
                    |> List.map (fun hirType ->
                        ClosedHir.Type(
                            hirType.sym,
                            hirType.fields |> List.map (fun f -> ClosedHir.Field(f.sym, f.typ, convertHirExpr f.body, f.span))))
                let allTypes = convertedTypes @ generatedTypes
                // 元の HIR モジュールフィールドを ClosedHir.Field へ構造的に変換する。
                let convertedFields =
                    hirModule.fields
                    |> List.map (fun f -> ClosedHir.Field(f.sym, f.typ, convertHirExpr f.body, f.span))
                let rewrittenModule =
                    ClosedHir.Module(
                        hirModule.name,
                        allTypes,
                        convertedFields,
                        rewrittenMethods,
                        hirModule.scope,
                        closureInvokeMethods)
                modules @ [ rewrittenModule ], allDiagnostics @ moduleDiagnostics) ([], [])

        match diagnostics with
        | [] -> PhaseResult.succeeded (ClosedHir.Assembly(asm.name, rewrittenModules)) []
        | _ -> PhaseResult.failed diagnostics
