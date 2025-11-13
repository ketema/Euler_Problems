# Project Euler Problem 23: Non-Abundant Sums

## Problem Statement

A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

**Question**: Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

## Approach

1. **Calculate proper divisor sums**: Use O(√n) iteration to find sum of divisors
2. **Generate abundant numbers**: Find all abundant numbers ≤ 28123 (~6965 numbers)
3. **Generate abundant sums**: Create set of all possible sums of two abundant numbers
4. **Sum non-abundant numbers**: Sum all integers not in the abundant sums set

### Implementation Strategy

**Dense Python approach**:
- Inline list comprehensions and set operations
- Single-line function definitions where possible
- Prioritize code brevity over readability (per user requirement)

**Time Complexity**:
- Divisor sum: O(√n) per number
- Finding abundants: O(n√n) where n=28123
- Generating sums: O(a²) where a≈6965
- Total: O(n√n + a²) ≈ O(48M operations)

**Space Complexity**:
- O(n) for abundant list and sum set

## Answer

**4179871**

**Verification**:
- All 6 test cases pass
- Runtime: ~2 seconds on modern hardware
- 12 lines of code (including imports and main)