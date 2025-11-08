# Project Euler Problem 14: Longest Collatz Sequence

## Problem Statement

The following iterative sequence is defined for the set of positive integers:

- n → n/2 (n is even)
- n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.

**Question**: Which starting number, under one million, produces the longest chain?

**Note**: Once the chain starts, the terms are allowed to go above one million.

## Solution Approach

1. **Memoization**: Cache chain lengths to avoid recalculation
2. **Iterative calculation**: For each starting number, compute chain length
3. **Optimization**: Use previously computed values

## Complexity

- **Time**: O(n log n) with memoization
- **Space**: O(n) for cache

## Answer

**837799** (chain length: 525)

**Verification**: All tests pass (13/13), solution verified correct.
