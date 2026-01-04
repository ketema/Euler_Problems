# Euler Problem 33: Digit Cancelling Fractions (Elm)

## Solution Summary
- **Answer**: 100 (denominator of product of 4 fractions in lowest terms)
- **Language**: Elm 0.19.1
- **Location**: problem33/

## The Four Fractions
1. 16/64 → cancels to 1/4
2. 19/95 → cancels to 1/5
3. 26/65 → cancels to 2/5
4. 49/98 → cancels to 4/8

Product: (16*19*26*49)/(64*95*65*98) = 387296/38729600 = 1/100

## Key Implementation Insights

### Cross-Multiplication for Fraction Equality
Used `n1 * d2 == n2 * d1` instead of floating-point division to avoid precision issues.

### Digit Cancellation Algorithm
- Extract digits as tuple: `(tens, units) = (n // 10, n mod 10)`
- Try all 4 combinations of digit positions
- Check if shared digit exists and cancellation preserves value

### Trivial Case Filter
Both numerator and denominator ending in 0 (like 30/50) are excluded.

## Adversarial TDD Success
- test-writer: Created 16 tests with 5-point error messages (blind to implementation)
- coder: Implemented based on error messages only (blind to test source)
- All 16 tests passed first try

## elm LSP Resolution
- **Root Cause**: elm-language-server requires elm.json to initialize
- **Fix**: Creating Elm project in problem33/ provided valid elm.json
- **Result**: 26/26 LSPs now working (elm re-enabled in project.yml)

## Files
- src/DigitCancelling.elm (166 lines) - Pure functional implementation
- tests/DigitCancellingTest.elm (335 lines) - 16 tests with self-documenting errors
- TSR.md - Test Specification Review
- elm.json - Project manifest
- .gitignore - Elm-specific ignores

## Commit
9169990 feat(euler-33): Solve Digit Cancelling Fractions in Elm
