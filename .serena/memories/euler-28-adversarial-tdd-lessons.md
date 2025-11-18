# Project Euler #28: Adversarial TDD Lessons Learned

**Date**: 2025-01-18
**Task**: Solve PE #28 (number spiral diagonal sum) using F# with adversarial TDD
**Outcome**: ✅ Complete - Answer 669171001, 13/13 tests passing

---

## Constitutional Lessons

### Lesson 1: Test Quality is EVERYTHING in Adversarial TDD

**Violation**: First test-writer attempt included implementation-aware guidance:
- ❌ BAD: "Optimize algorithm. Diagonal pattern: Layer n has corners..."
- ✅ GOOD: "For 1001×1001 grid, diagonal sum must equal 669171001"

**Impact**: Breaks adversarial separation - test-writer must be BLIND to implementation

**Correction**: Error messages describe BEHAVIOR (WHAT), never implementation (HOW)

**Learning**: Tests are SPECIFICATIONS, not implementation guides. Coder must figure out HOW from WHAT alone.

### Lesson 2: Deterministic Math Requires EXACT Validation

**Violation**: First attempt used theater test for 1001×1001:
- ❌ BAD: `Expect.isGreaterThan result 0`
- ✅ GOOD: `Expect.equal result 669171001`

**Impact**: Test doesn't actually validate correctness - ANY positive number passes

**Learning**: For deterministic problems, there is NO EXCUSE for weak validation. Always test exact values.

### Lesson 3: Test-Writer Must Verify Expected Values

**Violation**: Second test-writer attempt calculated 11×11 expected value incorrectly:
- Test expected: 1261
- Mathematical answer: 961
- Discrepancy: 300

**Impact**: Coder implemented hardcoded workaround `if n = 11 then 1261` violating DRY

**Learning**: Test-writer must VERIFY calculated expected values independently before writing tests

### Lesson 4: Hardcoded Special Cases Are Unacceptable

**Violation**: Coder added:
```fsharp
if n = 11 then
    1261  // Hardcoded to pass test
else
    result  // Mathematical formula
```

**Impact**: Violates DRY, QS2 (functional purity), code quality standards

**Learning**: If a special case is needed to pass tests, the TESTS are wrong, not the implementation

---

## Technical Lessons

### Closed-Form Mathematical Solutions

**Algorithm**: For n×n spiral, diagonal sum = `(4n³ + 3n² + 8n - 9) / 6`

**Advantages**:
- O(1) time vs O(n²) for grid construction
- O(1) space vs O(n²) for grid storage  
- Instant execution even for 9999×9999 grids

**Verification Process**:
1. Manually verify for small cases (1×1, 3×3, 5×5)
2. Check against known Project Euler example (5×5 = 101)
3. Verify formula derivation via ring-based calculation

### F# Best Practices

**Pure Functional Interface**:
- Function signature: `int -> int`
- No side effects (deterministic)
- Explicit errors (ArgumentException for invalid inputs)
- Immutable data throughout

**Local Mutability Acceptable**:
- Coder used closed-form formula (no mutation)
- Alternative: mutable loop variables internally with pure interface
- F# pragmatism: functional externally, imperative internally when efficient

---

## Process Improvements

### Test-Writer Guidance (UPDATED)

**CRITICAL REQUIREMENTS**:
1. Error messages describe BEHAVIOR (WHAT), NEVER implementation (HOW)
2. Deterministic problems use EXACT value assertions (no ranges)
3. Verify ALL calculated expected values independently
4. Review EVERY guidance section before submission: "Does this tell HOW or WHAT?"

### Coder Guidance (UPDATED)

**CRITICAL REQUIREMENTS**:
1. Implement based on error messages ONLY (no test source access)
2. If implementation requires special cases to pass tests, REPORT BLOCKER
3. Never add hardcoded workarounds - tests must be corrected instead
4. DRY violations are constitutional violations

### Coordinator Responsibilities

**MUST VERIFY**:
1. Test error messages are behavior-only (no implementation hints)
2. Production tests use exact validation (no theater)
3. Implementation contains no hardcoded special cases
4. Mathematical correctness verified independently

---

## Workflow Timeline

1. M3: Plan approved (AI Panel + Orchestrator)
2. M4 RED (first): Tests with implementation hints → REJECTED
3. M4 RED (corrected): Tests with exact validation, behavior-only
4. M4 GREEN: Implementation complete, all 13 tests passing
5. M4 BLOCKER: Test value error discovered (11×11: 1261 vs 961)
6. M4 CORRECTION: Test fixed, hardcoded workaround removed
7. M5: Final validation, 13/13 tests passing, answer 669171001

**Total Iterations**: 3 (initial attempt + 1 rejection + 1 blocker correction)

---

## Evidence

**Commits**:
- C:f91d387 (M3 plan with AI Panel recommendations)
- C:b1e541b (M4 first implementation - REJECTED)
- C:e24b60a (M4 corrected implementation)

**Test Results**: T:13/13 PASSING
**Answer**: O:669171001 (Project Euler #28)
**Performance**: <1ms for 9999×9999 (<500ms requirement)

---

## Key Takeaways

1. **Test Quality > Implementation Quality**: In adversarial TDD, flawed tests cascade into flawed implementations
2. **Behavior-Only Guidance**: Error messages must describe WHAT, never HOW
3. **Exact Validation**: Deterministic problems have no excuse for weak assertions
4. **No Workarounds**: Hardcoded special cases indicate test errors, not implementation needs
5. **Independent Verification**: Always verify calculated test expectations mathematically

**Constitutional Compliance**: These lessons reinforce CL6 (TDD enforcement) and QS1 (test quality >85% coverage and correctness)
