# Project Euler Problem 21: Amicable Numbers

## Problem Statement

Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).

If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.

The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

## Approach

### Algorithm

1. **Calculate proper divisors efficiently**:
   - Iterate from 1 to √n
   - For each divisor i found, also add n/i (if different)
   - Time: O(√n)

2. **Check if number is amicable**:
   - Calculate b = d(a)
   - If a = b, return false (must be different)
   - Calculate c = d(b)
   - Return c = a

3. **Find all amicable numbers under limit**:
   - Check each number from 2 to limit
   - Time: O(n√n)

### Optimization Notes

- **Efficient divisor sum**: Only check up to √n, reducing iterations
- **Perfect numbers**: Numbers where d(n) = n are NOT amicable (like 6, 28)
- **Pairs**: When you find one amicable number, you've found its pair too

### Known Amicable Pairs

Under 10,000:
- (220, 284)
- (1184, 1210)
- (2620, 2924)
- (5020, 5564)
- (6232, 6368)

## Implementation Details

- **Language**: JavaScript (Node.js)
- **Why JavaScript**: Good for number manipulation, testing framework easy to implement
- Algorithm is straightforward, no special library requirements

## Answer

**31626**

**Verification**:
- Sum = 220 + 284 + 1184 + 1210 + 2620 + 2924 + 5020 + 5564 + 6232 + 6368
- All amicable pairs under 10000 accounted for
- Tests verify proper divisor calculations and amicable pair properties
