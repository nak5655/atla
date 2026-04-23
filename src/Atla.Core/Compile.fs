namespace Atla.Compiler

open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data
open Atla.Core.Semantics
open Atla.Core.Semantics.Data
open Atla.Core.Lowering
open System.IO

module Compiler =
    type ResolvedDependency =
        { name: string
          version: string
          source: string
          compileReferencePaths: string list
          runtimeLoadPaths: string list
          /// ネイティブランタイム DLL のパスリスト（runtimes/<rid>/native/ 配下のファイル）。
          nativeRuntimePaths: string list }

    type CompileRequest =
        { asmName: string
          source: string
          outDir: string
          dependencies: ResolvedDependency list }

    type CompileResult =
        { succeeded: bool
          diagnostics: Diagnostic list
          /// 意味解析が成功した場合に得られる HIR アセンブリ。
          /// パイプラインの後続フェーズが失敗した場合でも返される（IntelliSense 用）。
          hir: Hir.Assembly option
          /// 意味解析が成功した場合に得られるシンボルテーブル。
          /// パイプラインの後続フェーズが失敗した場合でも返される（IntelliSense 用）。
          symbolTable: SymbolTable option }

        member this.hasErrors = this.diagnostics |> List.exists (fun diagnostic -> diagnostic.isError)

    /// コンパイル失敗の結果を構築する。hir・symbolTable はオプションで提供できる。
    let private failed
        (diagnostics: Diagnostic list)
        (hir: Hir.Assembly option)
        (symbolTable: SymbolTable option)
        : CompileResult =
        { succeeded   = false
          diagnostics = diagnostics
          hir         = hir
          symbolTable = symbolTable }

    /// コンパイル成功の結果を構築する。
    let private succeeded
        (diagnostics: Diagnostic list)
        (hir: Hir.Assembly option)
        (symbolTable: SymbolTable option)
        : CompileResult =
        { succeeded   = true
          diagnostics = diagnostics
          hir         = hir
          symbolTable = symbolTable }

    let compile (request: CompileRequest) : CompileResult =
        // Lexing
        let input: Input<SourceChar> = StringInput request.source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            // Parsing
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) ->
                try
                    // Dependency loading
                    let dependencyInputs =
                        request.dependencies
                        |> List.map (fun dependency -> dependency.name, dependency.runtimeLoadPaths)

                    match DependencyLoader.loadDependencies dependencyInputs with
                    | { succeeded = false; diagnostics = dependencyDiagnostics } ->
                        failed dependencyDiagnostics None None
                    | { loadContext = dependencyLoadContext } ->
                        try
                            // Semantic Analysis
                            let symbolTable = SymbolTable()
                            let typeSubst = TypeSubst()
                            match Analyze.analyzeModule(symbolTable, typeSubst, "main", moduleAst) with
                            | { succeeded = false; diagnostics = diagnostics } ->
                                failed diagnostics None None
                            | { value = Some hir; diagnostics = analyzeDiagnostics } ->
                                let hirAsm = Hir.Assembly("hello", [ hir ])
                                // Closure Conversion
                                match ClosureConversion.preprocessAssembly(symbolTable, hirAsm) with
                                | { succeeded = false; diagnostics = closureDiagnostics } ->
                                    failed (analyzeDiagnostics @ closureDiagnostics) (Some hirAsm) (Some symbolTable)
                                | { value = Some closedAsm; diagnostics = closureDiagnostics } ->
                                    // Lowering
                                    match Layout.layoutAssembly(request.asmName, closedAsm) with
                                    | { succeeded = false; diagnostics = layoutDiagnostics } ->
                                        failed (analyzeDiagnostics @ closureDiagnostics @ layoutDiagnostics) (Some hirAsm) (Some symbolTable)
                                    | { value = Some mir; diagnostics = layoutDiagnostics } ->
                                        // Code Generation
                                        let outPath = Path.Join(request.outDir, sprintf "%s.dll" request.asmName)
                                        match Gen.genAssembly(mir, outPath, symbolTable) with
                                        | { succeeded = false; diagnostics = genDiagnostics } ->
                                            failed (analyzeDiagnostics @ closureDiagnostics @ layoutDiagnostics @ genDiagnostics) (Some hirAsm) (Some symbolTable)
                                        | { diagnostics = genDiagnostics } ->
                                            succeeded (analyzeDiagnostics @ closureDiagnostics @ layoutDiagnostics @ genDiagnostics) (Some hirAsm) (Some symbolTable)
                                    | _ ->
                                        failed (analyzeDiagnostics @ closureDiagnostics @ [ Diagnostic.Error("Lowering failed with unknown state", Span.Empty) ]) (Some hirAsm) (Some symbolTable)
                                | _ ->
                                    failed (analyzeDiagnostics @ [ Diagnostic.Error("Closure conversion failed with unknown state", Span.Empty) ]) (Some hirAsm) (Some symbolTable)
                            | _ ->
                                failed [ Diagnostic.Error("Semantic analysis failed with unknown state", Span.Empty) ] None None
                        finally
                            DependencyLoader.unloadDependencies dependencyLoadContext
                with ex ->
                    failed [ Diagnostic.Error($"Compilation failed: {ex.Message}", Span.Empty) ] None None
            | Failure (reason, span) ->
                failed [ Diagnostic.Error($"Parsing failed: {reason}", span) ] None None
        | Failure (reason, span) ->
            failed [ Diagnostic.Error($"Lexing failed: {reason}", span) ] None None
