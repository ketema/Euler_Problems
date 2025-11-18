# M4 REJECTION: Constitutional Violations in Test Quality

**Task ID**: euler-28
**Date**: 2025-01-18
**Orchestrator Decision**: REJECT - Tests require revision
**Phase**: M4 TDD Cycle - Test Quality Audit

---

## ORCHESTRATOR VERDICT

**Tests are GOOD but NOT PERFECT**

User directive: "this is a simple problem and the tests should be perfect"

**Violations Identified**: 2 constitutional violations
- **Violation 1**: Implementation-aware guidance (MEDIUM severity)
- **Violation 2**: Theater test for deterministic math (HIGH severity)

---

## CONSTITUTIONAL PRINCIPLE: TEST QUALITY IS EVERYTHING

**Why This Matters**:

In adversarial TDD, tests are the ONLY communication channel between test-writer and coder:
- test-writer CANNOT see implementation (blind to HOW)
- coder CANNOT see test source (blind to WHAT was tested)
- Error messages are the ONLY specification

**Result**: Tests must be PERFECT specifications of behavior, not implementation hints.

**For Simple Problems**: There is NO EXCUSE for weak tests. Simple math problems like Euler #28 demand exact validation, not approximations.

---

## VIOLATION 1: Implementation-Aware Guidance (MEDIUM Severity)

**Constitutional Standard Violated**: CL6 (TDD Enforcement) - Adversarial separation

### Evidence from SpiralDiagonalTests.fs

**Test 2 (Line 41) - 3×3 grid**:
```fsharp
Guidance: "Build spiral clockwise from center 1, then sum values where (row=col) OR (row+col=gridSize-1)."
```

**Why This Violates Adversarial TDD**:
- ❌ Tells coder to "build spiral" - prescribes implementation approach
- ❌ Specifies diagonal detection formula `(row=col) OR (row+col=gridSize-1)` - reveals HOW to identify diagonals
- ❌ This is IMPLEMENTATION GUIDANCE, not behavioral specification

**What Guidance Should Say** (Behavioral):
```fsharp
Guidance: "3×3 spiral has diagonals with values [7,1,3] and [5,1,9]. Sum all diagonal values: 7+5+1+3+9 = 25. Verify algorithm correctly identifies and sums these specific positions."
```
- ✅ Describes WHAT the diagonals ARE (specific values)
- ✅ Describes EXPECTED BEHAVIOR (sum = 25)
- ✅ Doesn't prescribe HOW to find them

---

**Test 9 (Line 141) - 1001×1001 production**:
```fsharp
Guidance: "Consider optimization: diagonal values follow pattern, no need to build full grid."
```

**Why This Violates**:
- ❌ Tells coder NOT to build grid - prescribes algorithm choice
- ❌ Hints at optimization approach - reveals implementation strategy
- ❌ test-writer should NOT know whether coder builds grid or uses formula

**What Guidance Should Say**:
```fsharp
Guidance: "For 1001×1001 grid, algorithm must compute exact diagonal sum. Expected value: 669171001. Verify computation produces this exact result efficiently."
```
- ✅ Specifies exact expected value (behavioral requirement)
- ✅ Doesn't reveal implementation approach

---

**Test 10 (Line 179) - 9999×9999 stress**:
```fsharp
Guidance: "Diagonal pattern:
- Layer n has corners at positions that can be computed directly
- No need to build full grid, calculate diagonal values mathematically
- Sum diagonal values across all layers from center outward."
```

**Why This Violates**:
- ❌ Reveals mathematical formula approach - prescribes algorithm
- ❌ Tells coder to avoid grid construction - dictates implementation
- ❌ This is a MINI-TUTORIAL on how to solve the problem

**What Guidance Should Say**:
```fsharp
Guidance: "For 9999×9999 grid, algorithm must:
1. Complete in <500ms (performance requirement)
2. Produce positive result (no integer overflow)
3. Return exact diagonal sum value
Optimize algorithm to meet performance constraint on large grids."
```
- ✅ Specifies BEHAVIORAL requirements (performance, correctness)
- ✅ Doesn't reveal mathematical approach

---

## VIOLATION 2: Theater Test for Deterministic Math (HIGH Severity)

**Constitutional Standard Violated**: CL6 (TDD) - Tests are literal documentation of intended behavior

### Evidence from SpiralDiagonalTests.fs

**Test 9 (Lines 131-150) - 1001×1001 production**:
```fsharp
let result = calculateDiagonalSum 1001

// Only validates: result > 0 and result < Int32.MaxValue
Expect.isGreaterThan result 0 errorMsg1
Expect.isLessThan result System.Int32.MaxValue errorMsg2
```

**Why This Is a Theater Test**:

Project Euler #28 is a **deterministic mathematical problem**:
- Input: 1001 (fixed)
- Expected output: 669171001 (exact, known value per M4 evidence)

Current test validates:
- ✅ Result is positive
- ✅ No integer overflow

**BUT**: For deterministic math, this is like testing `1+1` with:
```fsharp
Expect.isGreaterThan result 0  // Just checks result > 0
```
Instead of:
```fsharp
Expect.equal result 2  // Checks exact value
```

**This is a THEATER TEST**: Creates illusion of testing without validating actual correctness.

---

### Why Theater Tests Are Unacceptable

1. **False Security**: Test passes with ANY positive integer <2^31
   - `calculateDiagonalSum 1001 = 123456789` → Test PASSES ✅ (WRONG!)
   - `calculateDiagonalSum 1001 = 999999999` → Test PASSES ✅ (WRONG!)
   - `calculateDiagonalSum 1001 = 669171001` → Test PASSES ✅ (CORRECT!)

2. **No Behavioral Specification**: Coder doesn't know WHAT the correct answer is
   - Guidance says "compute diagonal sum" but doesn't say WHICH value is correct
   - Coder could implement ANY formula that produces positive result

3. **Simple Problem, Simple Test**: For deterministic math, test MUST validate exact value
   - Euler #28 has ONE correct answer: 669171001
   - Test should enforce this EXACTLY

---

### What Test Should Be

```fsharp
testCase "1001x1001 grid computes Project Euler answer" <| fun () ->
    // REQ-PE28-PROD: Calculate diagonal sum for 1001x1001 grid
    // Expected: 669171001 (Project Euler #28 answer)
    let result = calculateDiagonalSum 1001
    let errorMsg =
        sprintf "REQ-PE28-PROD: For 1001×1001 grid, diagonal sum must be 669171001.
Expected: calculateDiagonalSum 1001 = 669171001
Actual: calculateDiagonalSum 1001 = %d
Guidance: Verify algorithm correctly computes diagonal sum for production grid size. Expected answer is 669171001 per Project Euler #28 specification." result
    Expect.equal result 669171001 errorMsg
```

**Why This Is Better**:
- ✅ Validates EXACT expected value (669171001)
- ✅ Provides clear behavioral specification to blind coder
- ✅ Test fails if implementation is incorrect (ANY other value fails)
- ✅ No theater - test validates actual correctness

---

## ROOT CAUSE: Sub-Agent Prompts Matter

**Constitutional Gap Identified**:

When coordinator invokes test-writer sub-agent, prompt must EMPHASIZE:

1. **Adversarial TDD Non-Negotiable**:
   - You are BLIND to implementation
   - Guidance describes BEHAVIOR only, never HOW to implement
   - Think: "If I couldn't see the code, would this guidance be enough?"

2. **Theater Test Detection**:
   - For deterministic problems: Validate EXACT values, not ranges
   - Ask: "Does this test fail if implementation is wrong?"
   - If implementation can be incorrect and test still passes → THEATER TEST

3. **5-Point Error Message Rigor**:
   - Guidance must work for coder who CANNOT see tests
   - Describe WHAT behavior is expected, not HOW to achieve it
   - Example values, not algorithm hints

---

## REQUIRED ACTIONS

**Agent must invoke refactor-test-writer sub-agent** with:

1. **Fix Implementation-Aware Guidance** (3 tests):
   - Test 2 (line 41): Remove spiral construction hint, use behavioral description
   - Test 9 (line 141): Remove optimization hint, specify exact expected value
   - Test 10 (line 179): Remove mathematical formula tutorial, use behavioral requirements

2. **Fix Theater Test** (1 test):
   - Test 9 (lines 142-150): Change from range check (`>0, <Int32.Max`) to exact value validation (`= 669171001`)

3. **Constitutional Compliance**:
   - All 12 tests must pass with revised guidance
   - Error messages must guide blind coder without revealing implementation
   - Production test must validate exact answer for deterministic math

---

## LEARNING PRINCIPLE

**Test Quality Is EVERYTHING**:

In adversarial TDD, tests are the contract between requirements and implementation:
- Weak tests → weak implementation → weak solutions
- Perfect tests → correct implementation → correct solutions

**Simple problems deserve perfect tests** because:
- No complexity to hide behind
- Exact answers are known
- Theater tests are easily detected
- Learning opportunity to build discipline

**When coordinator sends prompts to sub-agents, emphasize**:
- Adversarial separation is NON-NEGOTIABLE
- Guidance describes WHAT, never HOW
- For deterministic math: Exact values, not ranges
- Test quality determines implementation quality

---

## EXPECTED OUTCOME

After test revision:
- All implementation hints removed from guidance
- Production test validates exact value (669171001)
- Tests are PERFECT specifications of behavior
- Adversarial TDD spirit maintained throughout

**Then**: Re-run coder sub-agent (GREEN phase) with revised tests. Implementation may need zero changes if it's already correct (likely), but tests will be constitutionally sound.

---

**Constitutional Reference**: CL6 (TDD Enforcement), QS1 (>85% coverage), Adversarial TDD Architecture
**Protocol Step**: M4 Iteration Cycle - test_sound=False → invoke refactor-test-writer
