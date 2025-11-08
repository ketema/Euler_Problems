# Project Euler Problem 27: Quadratic Primes

## Problem Statement

Euler discovered the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive integer values 0 ≤ n ≤ 39. However, when n = 40, 40² + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

The incredible formula n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values 0 ≤ n ≤ 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n² + an + b, where |a| < 1000 and |b| ≤ 1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

## Key Constraints

- The quadratic form is: n² + an + b
- |a| < 1000 (so -999 ≤ a ≤ 999)
- |b| ≤ 1000 (so -1000 ≤ b ≤ 1000)
- We start with n = 0, so b must be prime (since n² + an + b = b when n = 0)
- For n = 1: 1 + a + b must be prime
  - If b is prime and b > 2, then b is odd
  - For 1 + a + b to be prime (and odd when > 2), a must be odd
  - Actually, for 1 + a + b to be prime, we need specific constraints

## Examples Given

1. **n² + n + 41**
   - Produces primes for n = 0 to 39 (40 primes)
   - When n = 40: 40² + 40 + 41 = 40(41) + 41 = 41 × 41 = 1681 (composite)

2. **n² - 79n + 1601**
   - Produces primes for n = 0 to 79 (80 primes)
   - Product of coefficients: -79 × 1601 = -126479

## Approach Ideas

1. **Brute Force with Optimizations**:
   - Since b must be prime (when n=0), only test prime values of b
   - For each (a, b) pair, count consecutive primes starting from n=0
   - Track the maximum

2. **Constraints to Reduce Search Space**:
   - b must be prime and positive (since it's the value at n=0)
   - b ≤ 1000, so we need primes up to 1000
   - If b = 2, then for n=1: 1 + a + 2 = 3 + a must be prime
     - This limits a values
   - For most cases, b should be odd and > 2

3. **Algorithm**:
   ```
   Generate all primes up to some limit (maybe 100,000 or more)
   For each prime b ≤ 1000:
       For each a in [-999, 999]:
           Count consecutive primes from n=0
           Track maximum count and corresponding (a, b)
   Return a × b
   ```

## Complexity

- Search space: ~2000 values of a × ~170 primes ≤ 1000 = ~340,000 combinations
- For each combination, we might check up to 100+ values of n
- Each check requires primality test
- Overall: O(2000 × 170 × 100 × √p) where p is the value being tested
- This is feasible (~34 million operations)

## Mathematical Insights

1. **When n=0**: The value is b, which must be prime
2. **When n=1**: The value is 1 + a + b, which must be prime
3. **Parity considerations**: 
   - If b = 2 (only even prime), then 1 + a + 2 must be prime
   - For b > 2 (odd primes), we need 1 + a + b to be odd (for prime > 2), so a must be even
   
4. **Growth rate**: For large n, the n² term dominates, so we can't get infinitely many primes

## Expected Answer Range

Given the example produces a product of -126479, our answer might be in a similar range or larger/smaller.
