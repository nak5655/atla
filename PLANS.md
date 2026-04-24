# Execution Plan

This file follows the OpenAI Codex execution-plan style and is kept intentionally concise.
Detailed historical plans and design notes are stored under `notes/`.

## Objective
- Keep planning lightweight at the repository root.
- Track only active work and near-term execution details here.

## Scope
- In scope: active implementation tasks, sequencing, risks, and verification steps.
- Out of scope: long-term archival history, completed deep-dive investigations (moved to `notes/`).

## Constraints
- Preserve compiler phase order: `AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL`.
- Keep phase boundaries explicit and deterministic.
- Add/update tests for all invariant-impacting changes.

## Plan
### Active Epic (2026-04-24): Dot-Only Calls + Apostrophe Member Access

#### Mission
- Introduce new surface syntax while preventing downstream churn in HIR and later phases.
- New syntax:
  - Function calls use `.` only.
  - Member access uses `'`.

#### Frozen Language Rules
- `x f.` => `f(x)`.
- `x f. g.` => `g(f(x))` (left-to-right evaluation).
- `f.` is valid (zero-argument call).
- `x .` is valid (direct zero-argument call on callable expression `x`).
- `a'b` => `a.b`.
- Member access binds as primary expression: `a'b c.` => `(a.b)(c)`.
- Invalid forms:
  - `x f` is an error (missing `.` for call).
  - `a'` is an error (missing member identifier).

#### Scope Boundaries
- AST/Parser/Semantics: in scope.
- HIR/Frame Allocation/MIR/CIL: no intentional structural changes; only non-regression validation.

#### Execution Steps
1. Update grammar and parser for `.` calls and `'` member access with preserved source spans.
2. Add/adjust parser diagnostics for invalid forms (`x f`, `a'`, malformed postfix sequences).
3. Normalize parsed forms in Semantics into existing canonical call/member shapes.
4. Reuse existing HIR lowering contracts; avoid introducing new HIR variants where possible.
5. Add parser + semantic unit tests for positive/negative cases.
6. Add/update AST/HIR/MIR snapshot tests for regression coverage.
7. Run full test suite and verify deterministic diagnostics ordering.
8. Update user-facing syntax documentation.

#### Test Matrix
- Positive:
  - `x f.`
  - `x f. g.`
  - `f.`
  - `x .`
  - `a'b`
  - `a'b c.`
- Negative:
  - `x f`
  - `a'`
  - malformed chained postfix call/member forms
- Boundary:
  - AST snapshot stability
  - HIR snapshot remains canonical (Call/MemberAccess)
  - MIR snapshot non-regression

#### Risks & Mitigations
- Dot token ambiguity in expression tails:
  - Mitigation: strict postfix-call parsing and explicit primary-expression boundaries.
- Parser churn causing broad test breakage:
  - Mitigation: stage parser tests first, then semantic normalization and snapshots.
- Accidental HIR contract drift:
  - Mitigation: lock HIR shape assertions in regression tests before refactors.

#### Done Criteria (Epic)
- Build succeeds with zero warnings.
- Full tests pass.
- Diagnostics remain structured and deterministic.
- HIR/MIR/CIL behavior is regression-equivalent for canonicalized forms.
- Documentation updated for final syntax.

## Surprises & Discoveries
- (Record unexpected findings discovered during implementation.)

## Validation
- Build succeeds with zero warnings.
- Full tests pass.
- IR invariants hold across lowering stages.

## Decision Log
- 2026-04-24: Replaced oversized root plan with concise execution template.
- 2026-04-24: Moved historical plan content to `notes/plans-archive.md`.
- 2026-04-24: Converted AGENTS.md to ExecPlan-formatted guidance.
- 2026-04-24: Started extracting phase-specific design notes from `notes/plans-archive.md` into `notes/phases/`.
- 2026-04-24: Added explicit Closure Conversion phase documentation in `notes/phases/` and adjusted phase index order.
- 2026-04-24: Frozen syntax plan for dot-only function calls (`.`) and apostrophe member access (`'`) with left-to-right call chaining and no intentional HIR+ changes.
- 2026-04-24: Implemented parser-side syntax switch to apostrophe member access and dot-only call chaining; updated syntax parser tests to the new call/member forms.
- 2026-04-24: Completed remaining Step 1/2 work: added explicit parser diagnostics for dangling apostrophe member access, required EOI for full module parse, and propagated `Ast.Expr.Error` / `Ast.Stmt.Error` through semantic analysis instead of emitting generic unsupported-type errors.
- 2026-04-24: Completed Step 4/5 by adding non-regression semantic tests that assert dot/apostrophe syntax lowers to canonical `Hir.Expr.Call` forms, plus parser boundary tests for primary-binding member calls.
- 2026-04-24: Expanded Step 5 coverage with parser/semantic tests for left-to-right chained dot calls (`x f. g.`), identifier zero-arg dot calls (`f.`), and successful semantic analysis of chained/zero-arg dot-only programs.
- 2026-04-24: Migrated `examples/` source programs to dot-only calls and apostrophe member-access notation so sample code reflects the new surface syntax.

## References
- Historical plans: `notes/plans-archive.md`
- Technical notes index: `notes/README.md`
