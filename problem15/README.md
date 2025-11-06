# Project Euler Problem 15: Lattice Paths

## Problem Statement

Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

```
→→↓↓   →↓→↓   →↓↓→
↓→→↓   ↓→↓→   ↓↓→→
```

**Question**: How many such routes are there through a 20×20 grid?

## Mathematical Approach

For an n×n grid, we need to make:
- n moves right (R)
- n moves down (D)
- Total: 2n moves

This is a **combinatorics problem**: Choose n positions for R (or D) from 2n total positions.

**Formula**: C(2n, n) = (2n)! / (n! × n!)

For n=20: C(40, 20) = 40! / (20! × 20!)

## Optimization

Direct factorial calculation risks overflow. Better approach:
- Use Pascal's triangle (dynamic programming)
- Or simplify the binomial coefficient calculation
- Or use the formula: C(n, k) = C(n-1, k-1) + C(n-1, k)

## Complexity

- **Time**: O(n²) with dynamic programming
- **Space**: O(n²) for DP table, or O(n) with rolling array

## Answer

**137846528820**

**Verification**: All 7 tests pass, both DP and combinatorial methods agree.
