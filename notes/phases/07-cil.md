# 07. CIL Emission Phase Notes

## CIL invariants
- CIL emission consumes MIR only.
- Runtime-target details (method builders, delegates, env types) are resolved from MIR metadata, not from AST/HIR shortcuts.

## Retained design decisions from archive
- Target-bound delegate emission (`target + ldftn + newobj`) was formalized for closure scenarios.
- Env-class members and invoke methods were generated with explicit builder registration/resolution strategy.
- Publish and packaging workflow notes were kept separate from compiler semantics to avoid mixing infra concerns with lowering rules.

## Why retained
CIL emission correctness depends on keeping runtime codegen decisions downstream and isolated from semantic analysis policy.
