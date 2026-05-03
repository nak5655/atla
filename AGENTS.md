# AGENTS.md (ExecPlan Format)

## 1) Mission
Maintain and evolve this repository as an F# compiler with strict, explicit lowering boundaries:

`AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL`

All changes must preserve semantic equivalence across lowering stages and keep phase invariants valid before handing data to the next phase.

---

## 2) Non-Negotiable Constraints

### Pipeline and phase boundaries
- The pipeline order above is mandatory.
- A phase must only consume adjacent-phase inputs.
- A phase must not mutate artifacts from earlier phases.
- Frame Allocation consumes typed HIR and produces MIR-compatible frame metadata.
- CIL emission consumes MIR only.

### IR invariants
- **AST**: source syntax only, no resolved symbols/types, spans preserved.
- **HIR**: fully typed, immutable, no unresolved identifiers, no parser sugar.
- **MIR**: explicit low-level control flow (basic blocks/jumps), no pattern matching, no closures, explicit operands.

### Semantics invariants
- Symbol resolution and type checking must complete before HIR lowering.
- No unresolved identifier may pass Semantic Analysis.
- Diagnostics must be explicit and structured (code + span + deterministic ordering).

### Purity and state
- Compiler phases must be pure transformations (`input -> output` or `input -> diagnostics`).
- Mutable shared state is forbidden in compiler phases.
- Side effects are allowed only in infra layers (CLI, file I/O, logging).

### Error handling
- Use `Result` + discriminated unions for expected errors.
- Do not use exceptions for control flow.

### F# style constraints
- Prefer immutable data everywhere.
- Use discriminated unions for IR and diagnostics.
- Avoid class inheritance hierarchies for IR variants.
- Pattern matches must be exhaustive.
- Phase functions must be total on valid input domains.

### Change policy (AI execution priority)
- Backward compatibility must not be preserved. Prefer breaking changes whenever they improve correctness, architecture, maintainability, or simplification.
- Do not add compatibility layers, migration shims, dual behavior flags, or deprecated paths to preserve old behavior.
- If current code contains defects or structurally inappropriate responsibilities, better designs/proposals must be prioritized and may replace existing implementations.
- Any code made obsolete by breaking changes must be deleted. Do not leave dead paths, compatibility shims, or unused branches behind.

---

## 3) Execution Workflow (Required per change)

1. **Small unit**: implement the smallest coherent unit.
2. **Test**: add/update tests for transformation boundaries and correctness.
3. **Validate**: run full test suite.
4. **Document**: update docs for any user-visible or invariant-impacting change.
5. **Preserve comments**: do not remove comments from unchanged blocks.
6. **Add comments**: document every function and major processing block.

### Change-control rule
- If a task appears to require edits to `src/Atla.Core/Syntax/Lexer.fs` or `src/Atla.Core/Syntax/Parser.fs`, request explicit confirmation before changing them.

---

## 4) Testing Requirements
- Every transformation must have unit tests.
- Edge cases are required at phase boundaries.
- Snapshot tests for AST/HIR/MIR representations are required.
- Tests must verify both successful lowering and expected diagnostics.
- Every bug fix requires a regression test.
- Regression tests are **not required** for intentional specification changes; instead, update or replace existing tests to match the new specification and invariants.

---

## 5) Forbidden Actions
- Introducing mutable shared compiler state.
- Bypassing Semantic Analysis before HIR generation.
- Mixing IR layers into a single representation.
- Reflection/runtime code-generation hacks.
- Suppressing diagnostics to force compilation success.

---

## 6) Definition of Done
- Build succeeds with zero warnings.
- Full tests pass.
- IR invariants are preserved across all transformed outputs.
- Documentation is updated where needed.
- New code follows this file.

---

## 7) Self-Check Checklist
Before commit, verify:
- [ ] No invariant/rule violations.
- [ ] No mutable shared state introduced.
- [ ] Exhaustive pattern matches.
- [ ] AST/HIR/MIR invariants preserved.
- [ ] Tests added/updated and passing.

---

## 8) Surprises & Discoveries (ExecPlan log)
Record unexpected findings encountered while implementing tasks (root causes, impact, and chosen mitigation).
