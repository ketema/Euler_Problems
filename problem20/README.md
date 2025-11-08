# Project Euler Problem 20: Factorial Digit Sum

## Problem Statement

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3,628,800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!

## Approach

This is a straightforward problem that requires:
1. Computing 100! (a very large number)
2. Summing the digits

### Implementation Strategy

**Direct Calculation**:
1. Calculate n! using arbitrary precision arithmetic
2. Convert to string
3. Sum the digits

**Time**: O(n²) for factorial calculation (multiplication of large numbers)
**Space**: O(n log n) for storing the result

Python is ideal for this because:
- Built-in arbitrary precision integers
- `math.factorial()` is highly optimized
- String conversion is simple

## Mathematical Notes

- 100! ≈ 9.33 × 10^157 (has 158 digits)
- Stirling's approximation: ln(n!) ≈ n ln(n) - n
- Number of digits in n! ≈ log₁₀(√(2πn)) + n(log₁₀(n) - log₁₀(e))

## Example

```
10! = 3,628,800
Digits: 3, 6, 2, 8, 8, 0, 0
Sum: 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27
```

## Answer

**648**

**Verification**:
- 100! has 158 digits
- Average digit ≈ 4.1 (648/158)
- This is reasonable given random digit distribution
