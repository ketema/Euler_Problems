# Project Euler Problem #28: Number Spiral Diagonals

**Language**: F# (functional-first .NET language)
**Difficulty**: Easy
**Status**: ✅ Complete

## Problem Statement

Starting with the number 1 and moving to the right in a clockwise direction, a 5 by 5 spiral is formed as follows:

```
21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13
```

It can be verified that the sum of the numbers on the diagonals is 101.

**What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?**

## Solution

**Answer**: `669171001`

## Implementation

### Approach

Mathematical closed-form formula for calculating diagonal sums without constructing the entire spiral grid.

**Algorithm**: For an n×n grid where n = 2k+1 (odd):
- Formula: `(4n³ + 3n² + 8n - 9) / 6`
- Complexity: O(1) time, O(1) space
- Performance: Instant for any grid size

### Files

- `fsharp/src/SpiralDiagonal.fs` - Core implementation (26 lines)
- `fsharp/tests/SpiralDiagonalTests.fs` - Comprehensive test suite (249 lines, 13 tests)
- `fsharp/SpiralDiagonal.fsproj` - F# project configuration

### Running

```bash
cd problem28/fsharp

# Build
dotnet build

# Run tests
dotnet test

# Run solution
dotnet run
```

### Test Results

```
13 tests run - 13 passed, 0 failed
- Basic functionality: 1×1, 3×3, 5×5, 7×7, 9×9, 11×11 (exact values)
- Production case: 1001×1001 → 669171001
- Error handling: Even/negative/zero inputs
- Performance: 9999×9999 < 500ms
- Boundary cases: Large grids
```

## Design

### Pattern

**Transaction Script** (PoEAA): Pure functional computation with no side effects.

### Principles

- **DRY**: Single formula, no duplication
- **Functional**: Pure function, immutable data, explicit errors
- **Type-safe**: F# strong static typing with inference
- **Performant**: Closed-form mathematical solution

## Development

### TDD Workflow

Adversarial TDD with sub-agents:
1. **RED phase**: test-writer created 13 failing tests (blind to implementation)
2. **GREEN phase**: coder implemented solution (blind to test source, guided by error messages only)
3. **REFACTOR**: Corrected test-writer calculation error (11×11: 1261→961)

### Quality Metrics

- Test coverage: >85%
- Compiler warnings: 0
- Constitutional compliance: Full M1-M5 workflow
- AI Panel reviews: M3 (plan), M4 (code)

## Lessons Learned

1. **Test Quality**: In adversarial TDD, error messages must describe BEHAVIOR (WHAT), never implementation approach (HOW)
2. **Exact Validation**: Deterministic math requires exact value assertions, not range checks
3. **No Special Cases**: Hardcoded workarounds violate DRY - tests must specify correct behavior
4. **Mathematical Verification**: Always verify calculated test expectations independently

## References

- [Project Euler Problem #28](https://projecteuler.net/problem=28)
- Implementation date: 2025-01-18
- Constitutional framework: M1-M5 workflow with sub-agent TDD
