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

    /// モジュール内の変換状態（生成メソッド・型・診断の蓄積）。
    type private ConversionState = {
        generatedMethods: ClosedHir.Method list
        generatedTypes: ClosedHir.Type list
        // クロージャー invoke メソッドの (liftedMethodSid -> envTypeSid) マッピング。
        closureInvokeMethods: Map<int, int>
        diagnostics: Diagnostic list
        captureMetadata: LambdaCaptureMetadata list
    }

    /// `SymbolTable` を使って新しい SymbolId を採番し、コンパイラ生成シンボル情報を登録して返す。
    /// 採番はコンパイル全体で一元管理されるため、フェーズ間の ID 衝突が型レベルで防止される。
    /// `SymbolKind.Local()` は Gen.fs が `typeBuilders`/`methodBuilders` で型・メソッドを管理するため、
    /// SymbolTable のキャラクター情報（kind）はコード生成に影響しない。将来的に専用 kind 変種を導入可。
    let private allocateSymbolId (symbolTable: SymbolTable) (typ: TypeId) : SymbolId =
        let sid = symbolTable.NextId()
        symbolTable.Add(sid, { name = "<compiler_generated>"; typ = typ; kind = SymbolKind.Local() })
        sid

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

    // ─────────────────────────────────────────────
    // 自由変数収集インフラ
    // ─────────────────────────────────────────────

    /// 自由変数収集に用いる束縛変数集合を保持する文脈レコード。
    type private FreeVarCtx = { bound: Set<int> }

    /// HIR 式内の自由変数集合を収集する（globalSymbols は捕捉対象から除外）。
    /// `Hir.foldExprWithCtx` を用いてトラバーサルをインフラに委譲する。
    /// Lambda 境界では bound をリセットし、Block 内の Let は逐次追加する。
    /// Phase 1 の buildCaptureMap で各ラムダ本体の自由変数計算に使用する。
    let private collectFreeVarsHirExpr (bound: Set<int>) (globalSymbols: Set<int>) (expr: Hir.Expr) : Set<int> =
        Hir.foldExprWithCtx
            (fun ctx e ->
                match e with
                | Hir.Expr.Lambda (args, _, _, _, _) ->
                    // Lambda 境界: bound をリセットしてラムダ自身の引数のみを束縛とする。
                    { ctx with bound = args |> List.fold (fun acc arg -> acc.Add arg.sid.id) Set.empty }
                | _ -> ctx)
            (fun ctx s ->
                match s with
                | Hir.Stmt.Let (sid, _, _, _) -> { ctx with bound = ctx.bound.Add sid.id }
                | _ -> ctx)
            (fun ctx e ->
                match e with
                | Hir.Expr.Id (sid, _, _) ->
                    if ctx.bound.Contains sid.id || globalSymbols.Contains sid.id then Set.empty
                    else Set.singleton sid.id
                | _ -> Set.empty)
            Set.union
            Set.empty
            { bound = bound }
            expr

    /// ClosedHir 式内の自由変数集合を収集する（globalSymbols は捕捉対象から除外）。
    /// `ClosedHir.foldExprWithCtx` を用いてトラバーサルをインフラに委譲する。
    /// Lambda 境界では bound をリセットし、Block 内の Let は逐次追加する。
    let private collectFreeVarsExpr (bound: Set<int>) (globalSymbols: Set<int>) (expr: ClosedHir.Expr) : Set<int> =
        ClosedHir.foldExprWithCtx
            (fun ctx e ->
                match e with
                | ClosedHir.Expr.Lambda (args, _, _, _, _) ->
                    // Lambda 境界: bound をリセットしてラムダ自身の引数のみを束縛とする。
                    { ctx with bound = args |> List.fold (fun acc arg -> acc.Add arg.sid.id) Set.empty }
                | _ -> ctx)
            (fun ctx s ->
                match s with
                | ClosedHir.Stmt.Let (sid, _, _, _) -> { ctx with bound = ctx.bound.Add sid.id }
                | _ -> ctx)
            (fun ctx e ->
                match e with
                | ClosedHir.Expr.Id (sid, _, _) ->
                    if ctx.bound.Contains sid.id || globalSymbols.Contains sid.id then Set.empty
                    else Set.singleton sid.id
                // EnvFieldLoad は env インスタンス引数へのアクセスで、外側スコープの自由変数ではない。
                | ClosedHir.Expr.EnvFieldLoad _ -> Set.empty
                // ClosureCreate が捕捉している変数 sid は外側スコープで参照が必要。
                | ClosedHir.Expr.ClosureCreate (_, _, captured, _, _) ->
                    captured |> List.map (fun (sid, _, _) -> sid.id) |> Set.ofList
                | _ -> Set.empty)
            Set.union
            Set.empty
            { bound = bound }
            expr

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

    // ─────────────────────────────────────────────
    // Phase 1: キャプチャー解析
    // HIR を走査して各 Lambda の捕捉変数メタデータを CaptureMap として構築する。
    // bound / bindings のスレッドはこのフェーズに閉じ込め、Phase 2 から除去する。
    // ─────────────────────────────────────────────

    /// Phase 1 の解析文脈: 束縛変数集合と型情報マップを保持する。
    type private AnalysisCtx = {
        bound: Set<int>
        bindings: Map<int, bool * TypeId>
    }

    /// Phase 1: メソッド本体を走査し、各 Lambda の捕捉変数メタデータを CaptureMap として構築する。
    /// `collectFreeVarsHirExpr`（`Hir.foldExprWithCtx` ベース）で各ラムダ本体の自由変数を算出し、
    /// 外側スコープの bindings と照合して CapturedVarMetadata リストに変換する。
    /// CaptureMap のキーは Lambda ノードの Span（ソース中で一意）。
    /// 戻り値の int list は型情報が解決できなかった捕捉変数 sid（エラー報告用）。
    let private buildCaptureMap
        (globalSymbols: Set<int>)
        (method: Hir.Method) : Map<Atla.Core.Data.Span, CapturedVarMetadata list * int list> =

        // 外側スコープの bindings から捕捉変数の型情報を解決し、CaptureMap エントリを構築する。
        let buildCaptureEntries (outerBindings: Map<int, bool * TypeId>) (freeVars: Set<int>) : CapturedVarMetadata list * int list =
            let pairs =
                freeVars |> Set.toList |> List.sort
                |> List.map (fun sid ->
                    match outerBindings.TryFind sid with
                    | Some(isMut, typ) -> Some { sid = sid; isMutable = isMut; typ = typ }, None
                    | None -> None, Some sid)
            List.choose fst pairs, List.choose snd pairs

        // HIR を再帰走査して各 Lambda の CaptureMap エントリを構築する。
        // bound / bindings は Let・For 宣言に従い逐次更新する。
        let rec traverseExpr
            (ctx: AnalysisCtx)
            (captureMap: Map<Atla.Core.Data.Span, CapturedVarMetadata list * int list>)
            (expr: Hir.Expr) : Map<Atla.Core.Data.Span, CapturedVarMetadata list * int list> =
            match expr with
            | Hir.Expr.Lambda (args, _, body, _, span) ->
                let lambdaBound = args |> List.fold (fun (acc: Set<int>) arg -> acc.Add arg.sid.id) Set.empty
                let lambdaBindings =
                    args |> List.fold (fun (acc: Map<int, bool * TypeId>) arg -> acc.Add(arg.sid.id, (false, arg.typ))) ctx.bindings
                // 自由変数をラムダ引数のみを束縛として計算し、外側 bindings で型情報を解決する。
                let freeVars = collectFreeVarsHirExpr lambdaBound globalSymbols body
                let known, unknown = buildCaptureEntries ctx.bindings freeVars
                let map' = captureMap |> Map.add span (known, unknown)
                // ラムダ本体を再帰走査してネストしたラムダも登録する。
                traverseExpr { bound = lambdaBound; bindings = lambdaBindings } map' body
            | Hir.Expr.Block (stmts, body, _, _) ->
                let map', ctx' =
                    stmts
                    |> List.fold
                        (fun (m, c) stmt -> traverseStmt c m stmt, afterStmtCtx c stmt)
                        (captureMap, ctx)
                traverseExpr ctx' map' body
            | Hir.Expr.Call (_, instance, args, _, _) ->
                let m' = instance |> Option.fold (traverseExpr ctx) captureMap
                args |> List.fold (traverseExpr ctx) m'
            | Hir.Expr.If (cond, thenBranch, elseBranch, _, _) ->
                [ cond; thenBranch; elseBranch ] |> List.fold (traverseExpr ctx) captureMap
            | Hir.Expr.MemberAccess (_, instance, _, _) ->
                instance |> Option.fold (traverseExpr ctx) captureMap
            | _ -> captureMap

        and traverseStmt
            (ctx: AnalysisCtx)
            (captureMap: Map<Atla.Core.Data.Span, CapturedVarMetadata list * int list>)
            (stmt: Hir.Stmt) : Map<Atla.Core.Data.Span, CapturedVarMetadata list * int list> =
            match stmt with
            | Hir.Stmt.Let (_, _, value, _) | Hir.Stmt.Assign (_, value, _) | Hir.Stmt.ExprStmt (value, _) ->
                traverseExpr ctx captureMap value
            | Hir.Stmt.For (sid, tid, iterable, body, _) ->
                let m' = traverseExpr ctx captureMap iterable
                let innerCtx =
                    { ctx with
                        bound = ctx.bound.Add sid.id
                        bindings = ctx.bindings.Add(sid.id, (false, tid)) }
                body
                |> List.fold (fun (m, c) s -> traverseStmt c m s, afterStmtCtx c s) (m', innerCtx)
                |> fst
            | Hir.Stmt.ErrorStmt _ -> captureMap

        and afterStmtCtx (ctx: AnalysisCtx) (stmt: Hir.Stmt) : AnalysisCtx =
            match stmt with
            | Hir.Stmt.Let (sid, isMut, value, _) ->
                { ctx with bound = ctx.bound.Add sid.id; bindings = ctx.bindings.Add(sid.id, (isMut, value.typ)) }
            | _ -> ctx

        let methodBound = method.args |> List.map fst |> List.fold (fun (acc: Set<int>) sid -> acc.Add sid.id) Set.empty
        let methodBindings =
            method.args |> List.fold (fun (acc: Map<int, bool * TypeId>) (sid, tid) -> acc.Add(sid.id, (false, tid))) Map.empty
        traverseExpr { bound = methodBound; bindings = methodBindings } Map.empty method.body

    // ─────────────────────────────────────────────
    // Phase 2: 構造的変換（Hir.Expr → ClosedHir.Expr）
    // Phase 1 で構築済みの CaptureMap を参照するため、bound / bindings / globalSymbols の
    // スレッドが不要になり、シグネチャが単純化される。
    // ─────────────────────────────────────────────

    /// Phase 2: lambda lifting のために式を再帰変換し（入力: `Hir.Expr`、出力: `ClosedHir.Expr`）、
    /// 必要な生成メソッドを state に蓄積する。
    /// `captureMap` は Phase 1 で構築済みのため、bound / bindings / globalSymbols のスレッドが不要。
    let rec private rewriteExpr
        (symbolTable: SymbolTable)
        (ownerMethod: Hir.Method)
        (captureMap: Map<Atla.Core.Data.Span, CapturedVarMetadata list * int list>)
        (expr: Hir.Expr)
        (state: ConversionState) : ClosedHir.Expr * ConversionState =
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
                    let r, s = rewriteExpr symbolTable ownerMethod captureMap instExpr state
                    Some r, s
                | None -> None, state
            ClosedHir.Expr.MemberAccess(mem, rewrittenInstance, tid, span), nextState
        | Hir.Expr.Call (func, instance, args, tid, span) ->
            let rewrittenInstance, instanceState =
                match instance with
                | Some instExpr ->
                    let r, s = rewriteExpr symbolTable ownerMethod captureMap instExpr state
                    Some r, s
                | None -> None, state
            let rewrittenArgs, finalState =
                args
                |> List.fold
                    (fun (acc, st) arg ->
                        let r, s = rewriteExpr symbolTable ownerMethod captureMap arg st
                        acc @ [ r ], s)
                    ([], instanceState)
            ClosedHir.Expr.Call(func, rewrittenInstance, rewrittenArgs, tid, span), finalState
        | Hir.Expr.Block (stmts, body, tid, span) ->
            let rewrittenStmts, stateAfterStmts = rewriteStmts symbolTable ownerMethod captureMap stmts state
            let rewrittenBody, finalState = rewriteExpr symbolTable ownerMethod captureMap body stateAfterStmts
            ClosedHir.Expr.Block(rewrittenStmts, rewrittenBody, tid, span), finalState
        | Hir.Expr.If (cond, thenBranch, elseBranch, tid, span) ->
            let rc, cs = rewriteExpr symbolTable ownerMethod captureMap cond state
            let rt, ts = rewriteExpr symbolTable ownerMethod captureMap thenBranch cs
            let re, es = rewriteExpr symbolTable ownerMethod captureMap elseBranch ts
            ClosedHir.Expr.If(rc, rt, re, tid, span), es
        | Hir.Expr.Lambda (args, ret, body, tid, span) ->
            // Phase 1 で構築済みの captureMap を参照して捕捉変数メタデータを取得する。
            let capturedMetadata, unknownSids =
                captureMap |> Map.tryFind span |> Option.defaultValue ([], [])
            let rewrittenBody, bodyState = rewriteExpr symbolTable ownerMethod captureMap body state
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
                match capturedMetadata with
                | [] ->
                    // 非捕捉ラムダ: static delegate として lambda lifting する。
                    let methodArgs = args |> List.map (fun arg -> arg.sid, arg.typ)
                    let liftedMethodType = TypeId.Fn(methodArgs |> List.map snd, ret)
                    // SymbolTable を使って一元採番し、lifted メソッドの SymbolId を確保する。
                    let liftedSid = allocateSymbolId symbolTable liftedMethodType
                    let liftedMethod = ClosedHir.Method(liftedSid, methodArgs, rewrittenBody, liftedMethodType, span)
                    let updatedState = { bodyState with generatedMethods = bodyState.generatedMethods @ [ liftedMethod ] }
                    ClosedHir.Expr.Id(liftedSid, tid, span), updatedState
                | _ ->
                    // 捕捉ラムダ: env-class 方式でクロージャーを生成する。
                    // env クラスの SymbolId を採番する（SymbolTable で一元管理）。
                    // env クラスはコンパイラ生成のクラス型であり、SymbolTable の型情報は Gen.fs が
                    // typeBuilders で管理するため、ここでの型はプレースホルダー（TypeId.Unit）で十分。
                    let envTypeSid = allocateSymbolId symbolTable TypeId.Unit
                    // lifted method 内で env インスタンス引数として使う SymbolId を採番する。
                    // envTypeSid が確定した後に採番し、実際の引数型（TypeId.Name envTypeSid）を登録する。
                    let envArgSid = allocateSymbolId symbolTable (TypeId.Name envTypeSid)

                    // lifted invoke method の引数型・メソッド型を確定してから SymbolId を採番する。
                    // 第一引数は env インスタンス（TypeId.Name envTypeSid）、残りはラムダ引数。
                    let methodArgs = (envArgSid, TypeId.Name envTypeSid) :: (args |> List.map (fun arg -> arg.sid, arg.typ))
                    let liftedMethodType = TypeId.Fn(methodArgs |> List.map snd, ret)
                    // liftedMethodSid を採番するときに実際のメソッド型を SymbolTable に登録する。
                    let liftedMethodSid = allocateSymbolId symbolTable liftedMethodType

                    // env クラスの ClosedHir.Field エントリを生成（各捕捉変数に対して 1 フィールド）。
                    let envFields =
                        capturedMetadata
                        |> List.map (fun cm ->
                            // フィールドの body は Unit で代用する（実際の値は外側スコープから StoreEnvField で格納される）。
                            ClosedHir.Field(SymbolId cm.sid, cm.typ, ClosedHir.Expr.Unit span, span))

                    // ラムダ本体内の捕捉変数参照を EnvFieldLoad へ書き換える。
                    let capturedSidSet = capturedMetadata |> List.map (fun cm -> cm.sid) |> Set.ofList
                    let rewrittenBodyWithEnv = rewriteCapturedRefs envArgSid capturedSidSet rewrittenBody

                    let liftedMethod = ClosedHir.Method(liftedMethodSid, methodArgs, rewrittenBodyWithEnv, liftedMethodType, span)

                    // env-class の ClosedHir.Type を生成する。
                    let envType = ClosedHir.Type(envTypeSid, None, envFields, [])

                    // ClosureCreate 式を生成する。
                    let capturedForCreate = capturedMetadata |> List.map (fun cm -> SymbolId cm.sid, cm.typ, cm.isMutable)
                    let closureExpr = ClosedHir.Expr.ClosureCreate(envTypeSid, liftedMethodSid, capturedForCreate, tid, span)

                    let captureInfo = { ownerMethodSid = ownerMethod.sym.id; span = ownerMethod.span; captured = capturedMetadata }
                    let updatedState =
                        { bodyState with
                            generatedMethods = bodyState.generatedMethods @ [ liftedMethod ]
                            generatedTypes = bodyState.generatedTypes @ [ envType ]
                            closureInvokeMethods = bodyState.closureInvokeMethods |> Map.add liftedMethodSid.id envTypeSid.id
                            captureMetadata = bodyState.captureMetadata @ [ captureInfo ] }

                    closureExpr, updatedState

    /// Phase 2: lambda lifting のために文を再帰変換し（入力: `Hir.Stmt`、出力: `ClosedHir.Stmt`）。
    and private rewriteStmt
        (symbolTable: SymbolTable)
        (ownerMethod: Hir.Method)
        (captureMap: Map<Atla.Core.Data.Span, CapturedVarMetadata list * int list>)
        (stmt: Hir.Stmt)
        (state: ConversionState) : ClosedHir.Stmt * ConversionState =
        match stmt with
        | Hir.Stmt.Let (sid, isMutable, value, span) ->
            let rewrittenValue, nextState = rewriteExpr symbolTable ownerMethod captureMap value state
            ClosedHir.Stmt.Let(sid, isMutable, rewrittenValue, span), nextState
        | Hir.Stmt.Assign (sid, value, span) ->
            let rewrittenValue, nextState = rewriteExpr symbolTable ownerMethod captureMap value state
            ClosedHir.Stmt.Assign(sid, rewrittenValue, span), nextState
        | Hir.Stmt.ExprStmt (value, span) ->
            let rewrittenValue, nextState = rewriteExpr symbolTable ownerMethod captureMap value state
            ClosedHir.Stmt.ExprStmt(rewrittenValue, span), nextState
        | Hir.Stmt.For (sid, tid, iterable, body, span) ->
            let rewrittenIterable, iterableState = rewriteExpr symbolTable ownerMethod captureMap iterable state
            // for 反復変数 sid はボディ内で束縛済みとして扱い、ラムダからの捕捉候補となる（C#互換: 反復ごと新規束縛）。
            let rewrittenBody, bodyState = rewriteStmts symbolTable ownerMethod captureMap body iterableState
            ClosedHir.Stmt.For(sid, tid, rewrittenIterable, rewrittenBody, span), bodyState
        | Hir.Stmt.ErrorStmt (msg, span) -> ClosedHir.Stmt.ErrorStmt(msg, span), state

    /// Phase 2: 文列を逐次変換し、最終 state を返す。
    and private rewriteStmts
        (symbolTable: SymbolTable)
        (ownerMethod: Hir.Method)
        (captureMap: Map<Atla.Core.Data.Span, CapturedVarMetadata list * int list>)
        (stmts: Hir.Stmt list)
        (state: ConversionState) : ClosedHir.Stmt list * ConversionState =
        stmts
        |> List.fold
            (fun (rewrittenStmts, currentState) stmt ->
                let rewrittenStmt, nextState = rewriteStmt symbolTable ownerMethod captureMap stmt currentState
                rewrittenStmts @ [ rewrittenStmt ], nextState)
            ([], state)

    /// モジュール内 method を順序保持で変換し、生成メソッドと型を末尾追加した一覧を返す。
    /// Phase 1 で各メソッドの CaptureMap を構築し、Phase 2 でラムダを lambda lifting / env-class 方式で lower する。
    let private rewriteMethods (symbolTable: SymbolTable) (hirModule: Hir.Module) : ClosedHir.Method list * ClosedHir.Type list * Map<int, int> * Diagnostic list =
        let globalSymbols = hirModule.methods |> List.map (fun methodInfo -> methodInfo.sym.id) |> Set.ofList
        let initialState =
            { generatedMethods = []
              generatedTypes = []
              closureInvokeMethods = Map.empty
              diagnostics = []
              captureMetadata = [] }

        let rewrittenMethods, finalState =
            hirModule.methods
            |> List.fold (fun (accMethods, state) methodInfo ->
                // Phase 1: メソッド本体の全ラムダについて捕捉変数を解析し CaptureMap を構築する。
                let captureMap = buildCaptureMap globalSymbols methodInfo
                // Phase 2: CaptureMap を参照してラムダを lambda lifting / env-class へ変換する。
                let rewrittenBody, bodyState = rewriteExpr symbolTable methodInfo captureMap methodInfo.body state
                let rewrittenMethod = ClosedHir.Method(methodInfo.sym, methodInfo.args, rewrittenBody, methodInfo.typ, methodInfo.span)
                accMethods @ [ rewrittenMethod ], bodyState) ([], initialState)

        rewrittenMethods @ finalState.generatedMethods,
        finalState.generatedTypes,
        finalState.closureInvokeMethods,
        finalState.diagnostics

    /// クロージャー変換前処理を行い、ラムダを lambda lifting / env-class 方式で lower する。
    /// 入力: `Hir.Assembly`（ソース意味論 IR）および意味解析で構築済みの `SymbolTable`。
    /// 出力: `ClosedHir.Assembly`（変換後 IR）。
    /// `symbolTable` を使って新しいシンボル ID を採番することで、意味解析との ID 空間を統一する。
    let preprocessAssembly (symbolTable: SymbolTable, asm: Hir.Assembly) : PhaseResult<ClosedHir.Assembly> =
        let rewrittenModules, diagnostics =
            asm.modules
            |> List.fold (fun (modules, allDiagnostics) hirModule ->
                let rewrittenMethods, generatedTypes, closureInvokeMethods, moduleDiagnostics = rewriteMethods symbolTable hirModule
                // 元の HIR 型定義を ClosedHir.Type へ構造的に変換する。
                let convertedTypes =
                    hirModule.types
                    |> List.map (fun hirType ->
                        let convertedTypeMethods =
                            hirType.methods
                            |> List.map (fun methodInfo ->
                                ClosedHir.Method(methodInfo.sym, methodInfo.args, convertHirExpr methodInfo.body, methodInfo.typ, methodInfo.span))
                        ClosedHir.Type(
                            hirType.sym,
                            hirType.baseType,
                            hirType.fields |> List.map (fun f -> ClosedHir.Field(f.sym, f.typ, convertHirExpr f.body, f.span)),
                            convertedTypeMethods))
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
