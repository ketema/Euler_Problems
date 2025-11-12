# Maude Setup for Problem 957 Symbolic Solver

## Installation

**Maude binary location**: `/usr/local/bin/maude`
**Prelude location**: `~/Library/Maude/prelude.maude`
**Version**: Maude 3.5.1 ARM64

## Running Maude

To execute Maude files, use:

```bash
MAUDE_LIB=~/Library/Maude /usr/local/bin/maude -no-banner <file.maude>
```

## Verified Test Module

File: `test_maude.maude`

```maude
fmod GENESIS is
  sorts Point .

  --- Red points (fixed)
  ops R1 R2 R3 : -> Point [ctor] .

  --- Blue points (use valid Maude identifiers without underscores in position)
  ops B01 B02 : -> Point [ctor] .

  --- Pair operator (commutative)
  op pair : Point Point -> Point [comm] .

  --- Idempotency
  vars A B C D : Point .
  eq pair(A, A) = A .

  --- Simple Pappus (unguarded for now - just to test syntax)
  ---  eq pair(pair(A, B), pair(C, D)) = pair(pair(A, D), pair(C, B)) .

endfm

--- Test reduction
red pair(B01, B02) .
red pair(R1, B01) .
red pair(pair(B01, B02), pair(R1, R2)) .
```

## Output Format

Maude produces output in this format:

```
==========================================
reduce in GENESIS : pair(B01, B02) .
rewrites: 0 in 0ms cpu (0ms real) (~ rewrites/second)
result Point: pair(B01, B02)
==========================================
```

**Parsing regex**: `^result Point\\s*:\\s*(\\S+)`

## Key Syntax Rules

1. **Operator names**: No underscores in positions (use `B01` not `b0_1`)
2. **Commands**: Use `red` or `reduce` (both work)
3. **Commutativity**: `[comm]` attribute makes `pair(A, B) = pair(B, A)`
4. **Idempotency**: `eq pair(A, A) = A .` eliminates self-pairs
5. **Conditional rules**: Use `ceq` with `if <condition>` for guarded rewrites

## Next Steps

1. Add Bool predicates for provenance certificates:
   - `op onSameRange : Point Point -> Bool .`
   - `op triangleEdge : Point Point Point -> Bool .`

2. Add guarded Pappus rule:
   ```maude
   ceq pair(pair(A, B), pair(C, D)) = pair(pair(A, D), pair(C, B))
     if onSameRange(A, C) == true /\\ onSameRange(B, D) == true .
   ```

3. Integrate with Python `ProvenanceFSM` to track certificates externally
