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
1. Define the smallest coherent implementation unit.
2. Implement with explicit phase boundaries and immutable data flow.
3. Add/update unit and regression tests.
4. Run full test suite.
5. Update docs when behavior/invariants change.

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

## References
- Historical plans: `notes/plans-archive.md`
- Technical notes index: `notes/README.md`
