# Project Euler Problem 18: Maximum Path Sum I

## Problem Statement

By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

```
   3
  7 4
 2 4 6
8 5 9 3
```

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle in the provided data file.

## Approach

This is a classic **dynamic programming** problem.

### Brute Force (Exponential)
Try all possible paths: O(2^n) where n is number of rows - infeasible for large triangles.

### Dynamic Programming (Optimal)
Work **bottom-up**:
1. Start from second-to-last row
2. For each position, add the maximum of the two adjacent values below
3. Continue upward until reaching the top
4. The top value is the maximum path sum

**Time**: O(n²) where n is number of rows
**Space**: O(n²) or O(n) with in-place modification

## Example

```
   3              23           (3 + 20)
  7 4      =>    20 19         (7 + max(12,13), 4 + max(13,15))
 2 4 6          10 13 15       (2 + max(8,5), 4 + max(5,9), 6 + max(9,3))
8 5 9 3         8  5  9  3
```

## Answer

**1074**

**Verification**: Small example (23) verified, dynamic programming solution tested.
