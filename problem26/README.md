# Project Euler Problem 26: Reciprocal Cycles

## Problem Statement

A unit fraction contains 1 in the numerator. The decimal representation of the unit fraction with denominators 2 to 10 are given:

```
1/2  = 0.5
1/3  = 0.(3)        [cycle length 1]
1/4  = 0.25
1/5  = 0.2
1/6  = 0.1(6)       [cycle length 1]
1/7  = 0.(142857)   [cycle length 6]
1/8  = 0.125
1/9  = 0.(1)        [cycle length 1]
1/10 = 0.1
```

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

## Approach

### Algorithm: Long Division with Remainder Tracking

The key insight is that when performing long division:
- Track the remainders at each step
- When a remainder repeats, we've found the start of the cycle
- The cycle length is the distance between occurrences

### Terminating vs Recurring

A fraction 1/d has a **terminating decimal** if and only if d = 2^a × 5^b (only factors of 2 and 5).

For other values, the decimal recurs with a cycle length ≤ d-1.

### Mathematical Background

For a prime p (where gcd(10, p) = 1), the cycle length of 1/p is the **multiplicative order** of 10 modulo p. This divides p-1, and for many primes equals p-1.

## Complexity

- **Time**: O(n × d) where n is the limit and d is average cycle length
- **Space**: O(d) for storing remainders
- **For this problem**: ~1000 × 1000 operations

## Implementation Details

- **Language**: R 4.3
- **Why R**:
  - **First R solution in repository!**
  - Excellent for statistical and mathematical problems
  - Vectorized operations and clean syntax
  - Built-in arbitrary precision (not needed here but available)
  - Native data structures (lists, vectors)

### Functions

```r
cycle_length(d)          # Calculate cycle length for 1/d
find_longest_cycle(limit) # Find d with longest cycle
solve()                  # Solve Problem 26
```

## TDD Methodology

Following AGENTS.md constitutional framework:

### Phase 1: RED
- Created custom R testing framework (testthat unavailable)
- Wrote 23 comprehensive tests
- Tests for: cycle lengths, edge cases, longest cycle finding
- All tests initially failed with stubs

### Phase 2: GREEN
- Implemented long division algorithm with remainder tracking
- Optimized with terminating decimal check
- All 23 tests passing ✓
- Answer: 983

### Phase 3: REFACTOR
- Clean, well-documented R code
- Roxygen-style documentation
- Proper error handling
- Executable script with informative output

## Test Coverage

✓ **23 tests, all passing:**

**Cycle length calculations** (12 tests):
- Terminating decimals (1/2, 1/4, 1/5, 1/8, 1/10)
- Single-digit cycles (1/3, 1/6, 1/9, 1/12)
- Multi-digit cycles (1/7, 1/11, 1/13)

**Edge cases** (3 tests):
- 1/1 special case
- Primes (1/17, 1/19)

**Finding longest cycle** (4 tests):
- Small limits (< 10, < 20)
- Verification of d and cycle length

**Solution verification** (4 tests):
- Result type and range
- Cycle length > 900 (must be near p-1 for large prime)

## Answer

**983**

1/983 has a recurring cycle of length **982**.

This makes perfect sense:
- 983 is prime
- For prime p, cycle length divides p-1
- Here, the cycle length is exactly 982 = 983 - 1
- This is the maximum possible cycle length for d < 1000

## Performance

R's performance on this problem:
- **Execution time**: ~0.5 seconds
- **Tests passing**: 23/23 ✓
- **Algorithm**: Efficient long division tracking

## Running the Solution

```bash
# Navigate to directory
cd problem26/r

# Run tests
Rscript test_reciprocal_cycles.R

# Run solution
Rscript reciprocal_cycles.R

# Interactive R session
R
> source("reciprocal_cycles.R")
> solve()
[1] 983
> cycle_length(983)
[1] 982
```

## Sample Output

```
$ Rscript reciprocal_cycles.R

Project Euler Problem 26: Reciprocal Cycles
============================================

Finding d < 1000 with longest recurring cycle in 1/d

Answer: d = 983
Cycle length: 982

1/983 has a 982-digit recurring cycle
```

## R Language Features Used

- **Functions**: First-class functions with documentation
- **Roxygen comments**: `#'` for function documentation
- **Control flow**: while loops, for loops, if/else
- **Vector operations**: `which()`, `%%`, `%/%`
- **Lists**: Named lists for returning multiple values
- **Modular arithmetic**: Remainder and division operators
- **Script detection**: `sys.nframe()` to check if sourced
- **String formatting**: `sprintf()` for output

## Example Cycles

```
1/7  = 0.142857 142857 142857...   (cycle: 142857, length 6)
1/13 = 0.076923 076923 076923...   (cycle: 076923, length 6)
1/17 = 0.0588235294117647 repeat  (cycle length 16)
1/983 = 0.001017... (982-digit cycle)
```

## Mathematical Insight

The cycle length of 1/p for prime p is related to **Fermat's Little Theorem**:
- 10^(p-1) ≡ 1 (mod p) for prime p where gcd(10, p) = 1
- The cycle length is the smallest k where 10^k ≡ 1 (mod p)
- This k divides p-1
- For p = 983, the cycle length is exactly 982 = p-1

This property (cycle length = p-1) occurs for **full reptend primes**.

## Learning Notes

This is the **first R solution** in the Euler_Problems repository! Demonstrates:
- R's suitability for mathematical algorithms
- Custom testing framework creation
- Long division simulation
- Number theory concepts (multiplicative order)
- Clean functional programming style

**Language Count**: 23 different languages now used in this repository!

## Trivia

Numbers with cycle length equal to d-1 are called **full reptend primes**. The first few are:
- 7 (cycle 6)
- 17 (cycle 16)
- 19 (cycle 18)
- 23 (cycle 22)
- 29 (cycle 28)
- ...
- 983 (cycle 982)

In the range d < 1000, **983 is the largest full reptend prime**, making it the answer!
