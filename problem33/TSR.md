# Test Specification Review - Euler Problem 33: Digit Cancelling Fractions

## Problem Statement

The fraction 49/98 is a curious fraction. An inexperienced mathematician might incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like 30/50 = 3/5 to be trivial examples (trailing zeros).

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

## Requirements

### REQ-001: Digit Cancelling Detection
A fraction n/d is "digit cancelling" if:
- Both n and d are two-digit numbers (10-99)
- n < d (fraction is less than 1)
- There exists a common digit c that appears in both n and d
- Removing c from both n and d yields n'/d' such that n/d = n'/d'
- The cancellation is non-trivial (not just removing trailing zeros)

### REQ-002: Non-Trivial Filter
Exclude trivial examples where both numerator and denominator end in 0 (like 30/50).

### REQ-003: Find All Four Fractions
The solution MUST find exactly 4 digit-cancelling fractions.

### REQ-004: Product Calculation
Multiply all four fractions together and reduce to lowest terms.

### REQ-005: Answer Extraction
Return the DENOMINATOR of the reduced product.

## Test Cases (Expected Behaviors)

### TC-001: Known Example Verification
- Input: 49/98
- Expected: IS a digit-cancelling fraction
- Verification: 49/98 = 4/8 = 1/2 (cancelling 9 works)

### TC-002: Trivial Example Rejection
- Input: 30/50
- Expected: NOT a digit-cancelling fraction (trivial - trailing zeros)

### TC-003: Non-Cancelling Rejection
- Input: 12/34
- Expected: NOT a digit-cancelling fraction (no valid cancellation)

### TC-004: Count Verification
- Expected: Exactly 4 fractions found

### TC-005: Final Answer
- Expected: Denominator = 100 (known answer from Project Euler)
- The four fractions are: 16/64, 19/95, 26/65, 49/98
- Product: (16*19*26*49)/(64*95*65*98) = 1/100

## Architectural Constraints

### AC-001: Pure Functions
All computation functions MUST be pure (no side effects).

### AC-002: Module Structure
- `DigitCancelling.elm` - Core logic
- `Main.elm` - Solution entry point (if needed)
- `tests/DigitCancellingTest.elm` - Test suite

### AC-003: GCD Function
MUST implement or use GCD for reducing fractions to lowest terms.

## Error Message Standards (5-Point)

Each test failure MUST provide:
1. What failed (test name)
2. Why (which requirement violated)
3. Expected behavior (specification)
4. Actual behavior (what happened)
5. Guidance (behavioral only - WHAT to fix, not HOW)

## AI Panel Recommendations (Incorporated)

1. **Add negative tests**: Include explicit tests for non-digit-cancelling and trivial cases
2. **Table-driven tests**: Sufficient for deterministic small search space (no fuzz testing needed)
3. **Edge case coverage**: Shared digits in different positions, all digit pairs
4. **Custom Fraction type**: Use Elm record type `{ numerator : Int, denominator : Int }`
5. **Document function signatures**: Clear type annotations in both modules
6. **5-point error messages**: Explicit assertions in test failures

## Notes for Adversarial TDD

- test-writer: BLIND to implementation. Tests describe BEHAVIOR only.
- coder: BLIND to test source. Implements based on error messages only.
- Deterministic problem: Use EXACT expected values (100), not ranges.
- Strict discipline: No leakage between test and implementation phases.
