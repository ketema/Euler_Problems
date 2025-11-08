# Project Euler Problem 30: Digit Fifth Powers

## Problem Statement

Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

```
1634 = 1^4 + 6^4 + 3^4 + 4^4
8208 = 8^4 + 2^4 + 0^4 + 8^4
9474 = 9^4 + 4^4 + 7^4 + 4^4
```

As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

**Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.**

## Analysis

### Constraints

- Single digit numbers (1-9) should be excluded as they are not sums
- Need to find upper bound for search

### Upper Bound Reasoning

What's the maximum number we need to check?

- For n digits, maximum sum of fifth powers = n × 9^5
- 9^5 = 59,049
- For 6 digits: 6 × 59,049 = 354,294 (still 6 digits) ✓
- For 7 digits: 7 × 59,049 = 413,343 (only 6 digits) ✗

The smallest 7-digit number is 1,000,000, but even if all digits were 9, the sum would be 7 × 59,049 = 413,343 (only 6 digits). So no 7-digit number can equal the sum of fifth powers of its digits.

**Upper limit**: 354,294 (or 6 × 9^5)

### Algorithm

```
For each number n from 10 to 354,294:
  1. Extract digits of n
  2. Calculate sum of fifth powers of digits
  3. If sum equals n, include n in result
  4. Return sum of all such numbers
```

### Example Verification (Fourth Powers)

Before finding fifth powers, let's verify the fourth power example:
- 1634: 1^4 + 6^4 + 3^4 + 4^4 = 1 + 1296 + 81 + 256 = 1634 ✓
- 8208: 8^4 + 2^4 + 0^4 + 8^4 = 4096 + 16 + 0 + 4096 = 8208 ✓
- 9474: 9^4 + 4^4 + 7^4 + 4^4 = 6561 + 256 + 2401 + 256 = 9474 ✓

## Implementation Plan

**Language**: OCaml (purely functional)
**Testing**: OUnit2
**Methodology**: TDD (RED → GREEN → REFACTOR)

### Test Cases

1. **Helper Function Tests**:
   - `digits 1634` should return `[1; 6; 3; 4]`
   - `sum_of_fifth_powers [1; 6; 3; 4]` should return correct sum
   - `is_digit_fifth_power 1634` should test fourth power (helper validation)

2. **Small Range Tests**:
   - Search 10-1000 should find valid numbers if any
   - Validate against fourth power examples

3. **Full Problem**:
   - Search 10-354294 should find all fifth power numbers
   - Sum should be correct answer

## Expected Complexity

- **Time**: O(n × d) where n = 354,294 numbers, d ≈ 6 digits average
- **Space**: O(1) - only store running sum
- **Execution**: < 1 second (functional, but compiled)
