# 06. MIR Phase Notes

## MIR invariants
- MIR control flow is explicit (basic blocks + jumps/branches).
- MIR operands are explicit temporaries/constants/addresses.
- MIR does not contain closures or high-level pattern constructs.

## Retained design decisions from archive
- MIR model was extended for closure environment operations (env allocation/field load/store) as explicit low-level primitives.
- Layout-to-MIR lowering was aligned with closure invoke routing so invoke methods are emitted deterministically.
- Deterministic output constraints were reinforced through regression tests for repeated identical inputs.

## Why retained
MIR is the contract consumed by CIL emission; explicitness and determinism here directly influence generated IL correctness.
