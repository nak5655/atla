# AGENTS.md

## 1. Overview
- This repository MUST implement a compiler in F# with the phase order: `AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL`.
- Every code change MUST preserve semantic equivalence across lowering stages.
- Every compiler phase output MUST satisfy the invariants defined in this file before entering the next phase.

## 2. Core Principles
- All compiler logic MUST be deterministic for identical inputs.
- All compiler rules MUST be encoded as explicit data transformations.
- All phase boundaries MUST be explicit module boundaries.
- All invariants MUST be validated by tests.

### GOOD
```fsharp
let lowerProgram (hir: HIR.Program) : MIR.Program =
    hir |> Lowering.lower
```

### BAD
```fsharp
let mutable lastProgram = None
let lowerProgram hir =
    lastProgram <- Some hir
    Lowering.lower hir
```

## 3. Compiler Architecture Constraints
- The compiler pipeline MUST execute in this exact order: `AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL`.
- A phase MUST NOT consume input from any non-adjacent phase.
- A phase MUST NOT mutate data produced by earlier phases.
- Frame Allocation MUST consume typed HIR and MUST produce MIR-compatible frame metadata.
- CIL emission MUST consume MIR only.

### GOOD
- `SemanticAnalysis.analyze : AST.Program -> Result<TypedSymbols * AST.Program, Diagnostic list>`
- `HIR.lower : TypedSymbols * AST.Program -> HIR.Program`

### BAD
- `MIR.lower : AST.Program -> MIR.Program`
- `CIL.emit : AST.Program -> CIL.Module`

## 4. Semantic Analysis Rules
- All symbols MUST be resolved before HIR generation.
- Type checking MUST be complete before lowering to HIR.
- No unresolved identifier MUST exist past Semantic Analysis.
- Semantic Analysis MUST return explicit diagnostics for all symbol and type failures.
- Semantic Analysis MUST NOT emit MIR or CIL constructs.

### GOOD
```fsharp
match SymbolTable.tryFind name symbols with
| Some symbol -> Ok symbol
| None -> Error [ Diagnostic.unresolvedIdentifier name span ]
```

### BAD
```fsharp
let symbol = symbols.[name] // throws when missing
```

## 5. IR Design Rules (AST / HIR / MIR)

### AST Rules
- AST MUST represent source syntax only.
- AST MUST NOT contain resolved symbols.
- AST MUST NOT contain inferred or checked types.
- AST nodes MUST preserve source spans.

### HIR Rules
- HIR MUST be fully typed.
- HIR MUST be immutable.
- HIR MUST NOT contain parser-level syntax sugar.
- HIR MUST NOT contain unresolved identifiers.

### MIR Rules
- MIR MUST be low-level and explicit.
- MIR MUST NOT contain pattern matching.
- MIR MUST NOT contain closures.
- MIR control flow MUST be represented with explicit basic blocks and jumps.
- MIR operands MUST be explicit temporaries, constants, or addresses.

### GOOD
- HIR expression: `HIR.Add of TypedExpr * TypedExpr * TypeRef`
- MIR block terminator: `MIR.Branch of condition:Temp * ifTrue:BlockId * ifFalse:BlockId`

### BAD
- MIR node containing `match` expression
- AST node containing `TypeRef`

## 6. F# Coding Rules
- All data structures MUST be immutable.
- `mutable` bindings MUST NOT be used in compiler phases.
- Discriminated unions MUST be used for compiler IR and diagnostics.
- Class inheritance hierarchies MUST NOT model IR variants.
- Pattern matching MUST be exhaustive.
- Compiler phase functions MUST be total over valid input domains.

### BAD
```fsharp
let mutable x = 0
```

### GOOD
```fsharp
let x = 0
```

### BAD
```fsharp
match value with
| Some x -> useValue x
```

### GOOD
```fsharp
match value with
| Some x -> useValue x
| None -> handleMissing ()
```

## 7. State and Side Effects Policy
- Mutable shared state MUST NOT be introduced.
- Compiler phases MUST be pure functions from input IR to output IR or diagnostics.
- Side effects MUST be isolated to infrastructure layers (CLI, file I/O, logging).
- Infrastructure layers MUST NOT encode semantic rules.

### GOOD
```fsharp
let compileText (text: string) : Result<CIL.Module, Diagnostic list> =
    text
    |> parse
    |> Result.bind analyze
    |> Result.map lowerToCil
```

### BAD
```fsharp
let diagnostics = ResizeArray<Diagnostic>()
let analyze ast =
    diagnostics.Add (Diagnostic.info "start")
    (* analysis logic *)
```

## 8. Error Handling Policy
- Errors MUST be represented explicitly with `Result` and discriminated unions.
- Exceptions MUST NOT be used for control flow.
- Each compiler phase MUST return structured diagnostics with spans and error codes.
- Error accumulation MUST be deterministic.

### GOOD
```fsharp
type CompileError =
    | UnresolvedIdentifier of name:string * span:Span
    | TypeMismatch of expected:TypeRef * actual:TypeRef * span:Span
```

### BAD
```fsharp
if not found then failwith "name missing"
```

## 9. File & Module Structure
- Each compiler phase MUST exist in its own top-level module.
- AST, HIR, and MIR types MUST be defined in separate modules/files.
- No cyclic module dependencies are allowed.
- Shared utilities MUST depend only on lower-level primitives, not compiler phases.
- Tests for each phase MUST be colocated in a corresponding test module.

### GOOD
- `Compiler.AST`
- `Compiler.Semantic`
- `Compiler.HIR`
- `Compiler.FrameAllocation`
- `Compiler.MIR`
- `Compiler.CIL`

### BAD
- `Compiler.IR.AllInOne`
- `Compiler.Semantic` depending on `Compiler.CIL`

## 10. Development Workflow
1. You MUST update `PLANS.md` before implementation.
2. You MUST implement the feature in the smallest coherent unit.
3. You MUST add or update tests for the change.
4. You MUST run the full test suite.
5. You MUST update documentation for user-visible or invariant-impacting changes.

### GOOD
- Commit includes: `PLANS.md`, implementation, tests, docs.

### BAD
- Commit includes implementation only with no plan or tests.

## 11. Testing Policy
- Every transformation MUST have unit tests.
- Edge cases MUST be covered for each phase boundary.
- Snapshot tests for AST, HIR, and MIR representations are REQUIRED.
- Tests MUST assert both successful lowering and expected diagnostics.
- Regression tests MUST be added for every fixed bug.

### GOOD
- `LoweringTests` verifies deterministic MIR output for identical HIR input.

### BAD
- Test only checks that no exception was thrown.

## 12. Forbidden Actions
- You MUST NOT introduce mutable shared state.
- You MUST NOT bypass Semantic Analysis before HIR generation.
- You MUST NOT mix IR layers in a single representation.
- You MUST NOT introduce reflection-based or runtime code-generation hacks.
- You MUST NOT suppress diagnostics to force successful compilation.

## 13. Definition of Done
- Code MUST compile with zero warnings.
- All tests MUST pass.
- IR invariants MUST be preserved across all transformed outputs.
- Documentation MUST be updated to reflect behavioral or invariant changes.
- New code MUST conform to all rules in this file.

## 14. Self-Check Procedure
Before committing, you MUST verify:
- [ ] No rule violations exist.
- [ ] No mutable state was introduced.
- [ ] All pattern matches are exhaustive.
- [ ] AST/HIR/MIR invariants are preserved.
- [ ] Tests were added or updated and are passing.
