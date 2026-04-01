# Copilot instructions for Atla compiler

## Big picture (read this first)
- This repo is an F# compiler pipeline in `Atla.Compiler/`: `Syntax` → `Semantics` → `Lowering` → IL emission.
- The end-to-end entry point is `Atla.Compiler/Compile.fs` (`Compiler.compile`). Data flow there is:
  1) `Lexer.tokenize` over `StringInput`
  2) `Parser.fileModule()` over `TokenInput`
  3) `Analyze.analyzeModule`
  4) `Typing.typingModule`
  5) `Layout.layoutAssembly`
  6) `Gen.GenAssembly`
- Core IR layers are split by folder:
  - AST: `Syntax/Data/Ast.fs`
  - HIR: `Semantics/Data/Hir.fs`
  - MIR: `Lowering/Data/Mir.fs`

## Project structure and boundaries
- `Syntax/` owns lexing/parsing only; keep parse errors as `Failure(reason, span)` (`Syntax/Data/ParseResult.fs`).
- `Semantics/` owns name resolution + typing; symbol/type infrastructure lives in `Semantics/Data/{Scope.fs,Symbol.fs,Type.fs}`.
- `Lowering/` converts typed HIR to MIR (`Lowering/Data/Layout.fs`) and emits .NET assemblies (`Lowering/Gen.fs`).
- Tests are in `Atla.Compiler.Tests/` using xUnit.

## F#-specific constraints in this repo
- Compile order matters: update `Atla.Compiler/Atla.Compiler.fsproj` when adding files/modules.
- Prefer existing namespace/module layout (`Atla.Compiler.Syntax`, `.Semantics`, `.Lowering`); do not introduce parallel naming trees.
- Many nodes are OO-style classes implementing marker interfaces (e.g., `Ast.Expr`, `Hir.Expr`) with mutable members (`typ`) used across phases—preserve this style when extending nodes.

## Parsing conventions to preserve
- Parser is combinator-based (`Syntax/PackratParser.fs`, `Syntax/Parser.fs`) with `Delay`, `Many`, `<|>`, `<&>`, etc.
- Indentation/offside behavior is implemented via `BlockInput` in `Syntax/Parser.fs`; block forms (`do`, `if`, `let`, `fn`, `data`, `import`) depend on it.
- Example pattern: `block (asToken (keyword "fn")) (...)` for declaration bodies.

## Semantics/lowering conventions to preserve
- Type unification uses `TypeId.Unify`/`CanUnify` (`Semantics/Data/Type.fs`); prefer propagating `TypeId.Error` over introducing ad-hoc error channels.
- `Analyze` resolves AST to HIR while threading `SymbolTable` + `Scope` (`Semantics/Analyze.fs`).
- `Typing` mutates `Hir.Expr.typ` in place (`Semantics/Typing.fs`), then lowering reads those types (`Lowering/Data/Layout.fs`).
- `Gen` writes both `*.dll` and a matching `*.runtimeconfig.json` (`Lowering/Gen.fs`).

## Build/test workflow (from repo root)
- Solution file: `Atla.Compiler.slnx` (projects: compiler + tests).
- Run tests with:
  - `dotnet test .\Atla.Compiler.slnx`
- Current observed state (2026-03-30): `dotnet test` fails due to syntax errors in `Atla.Compiler/Lowering/Data/Layout.fs` around line 52. Avoid unrelated cleanup unless requested.

## Practical guidance for AI edits
- Make phase-local changes first (Syntax vs Semantics vs Lowering) and only thread cross-phase changes when necessary.
- When adding syntax, update AST + parser + semantic analyzer + typing/lowering consumers together, then adjust tests.
- Prefer extending existing xUnit style (`[<Fact>]`, explicit `Assert.True(result.IsSuccess)` / `res.IsOk`) in `Atla.Compiler.Tests/*`.
- Keep paths/output conventions stable (`files` output in lowering tests, `Path.Join(outDir, $"{asmName}.dll")` in `Compile.fs`).
