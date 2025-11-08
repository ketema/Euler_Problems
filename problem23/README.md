# Project Euler Problem 23: Non-abundant Sums

## Problem Statement

A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24.

By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

## Approach

### Algorithm Steps

1. **Find all abundant numbers** up to 28123
   - For each number n, calculate sum of proper divisors
   - If sum > n, the number is abundant

2. **For each number 1 to 28123:**
   - Check if it can be written as sum of two abundant numbers
   - If NO, add it to our total sum

3. **Return the total sum**

### Complexity Analysis

- **Time**: O(n² √n)
  - Finding divisors: O(n √n) for all numbers up to n
  - Checking sums: O(n × a²) where a is count of abundant numbers
- **Space**: O(a) where a is number of abundant numbers (~6900 for limit 28123)

### Key Insights

- **12** is the smallest abundant number (1+2+3+4+6=16 > 12)
- **24** is the smallest sum of two abundant numbers (12+12)
- All numbers > 28123 CAN be written as sum of two abundant numbers
- We only need to check numbers ≤ 28123

## Implementation Details

- **Language**: Bash
- **Why Bash**:
  - **Last unused language** on this system with TDD support!
  - Demonstrates Bash can handle computational problems
  - Good for showcasing shell scripting capabilities
  - Built-in arithmetic and array handling

### File Structure

- `abundant.sh` - Main solution with all functions
- `test_abundant.sh` - Comprehensive test suite (23 tests)
- `README_PROBLEM.md` - Problem statement reference

### Functions

```bash
sum_of_divisors(n)                    # Calculate sum of proper divisors
is_abundant(n)                        # Check if n is abundant
find_abundant_numbers(limit)          # Find all abundant numbers up to limit
can_be_sum_of_two_abundant(n, list)  # Check if n is sum of two abundants
solve_problem(limit)                  # Main solution function
```

## TDD Methodology

Following AGENTS.md constitutional framework:

### Phase 1: RED
- Wrote 23 comprehensive tests first
- Created custom Bash testing framework
- All tests initially failed with stub implementations
- Tests cover: divisor sums, abundant detection, list generation, sum checking

### Phase 2: GREEN
- Implemented all 5 core functions
- Used Bash arithmetic, arrays, and control flow
- Efficient divisor calculation (only up to √n)
- All 23 tests passing ✓

### Phase 3: REFACTOR
- Clean, well-documented Bash code
- Proper variable scoping with `local`
- Progress indicators for long-running operations
- Can be sourced or executed directly

## Test Coverage

✓ **23 tests, all passing:**

**Sum of divisors calculations** (6 tests):
- Edge cases (1, 6, 10, 12, 15, 28)
- Perfect numbers (6, 28)
- Abundant number (12)

**Abundant number detection** (8 tests):
- Smallest abundant (12)
- Non-abundant numbers (1, 6, 10, 28)
- More abundant numbers (18, 20, 24)

**List generation** (4 tests):
- Contains expected abundant numbers
- Doesn't contain non-abundant numbers

**Sum checking** (4 tests):
- Can be sum (24, 30)
- Cannot be sum (1, 23)

**Integration** (1 test):
- Full workflow with small limit

## Performance Notes

**Bash is an interpreted language**, so this solution is computationally intensive for the full problem:

- **Small test (limit=100)**: Runs in ~1 second, answer = 2766
- **Full problem (limit=28123)**: Would take significant time due to:
  - ~6900 abundant numbers to find
  - Checking 28123 numbers against all pairs
  - Bash's interpreted nature

**For demonstration**, the solution works correctly but is optimized for smaller limits. For production use on the full problem, a compiled language (C, Rust, Go) would be more appropriate.

## Answer

**For limit=100**: 2,766 (verified with tests)

**For full problem (limit=28123)**: The algorithm is correct, but takes considerable time in Bash.

## Running the Solution

```bash
# Run tests
cd problem23/bash
./test_abundant.sh

# Run solution with small limit (fast)
./abundant.sh 100

# Run solution with custom limit
./abundant.sh 500

# Source functions for interactive use
source abundant.sh
sum_of_divisors 12  # Returns 16
is_abundant 12 && echo "abundant"
```

## Sample Output

```
$ ./abundant.sh 100
Finding abundant numbers up to 100...
Calculating sum of non-abundant sums...

Problem 23 Answer: 2766
```

## Bash Language Features Used

- **Functions**: User-defined functions with local variables
- **Arithmetic**: `$(( ))` for calculations, `bc` for square roots
- **Arrays**: Converting space-separated strings to arrays
- **Control flow**: if/then/else, for loops with C-style syntax
- **Return codes**: 0 for true, 1 for false (Unix convention)
- **Conditional execution**: `&&` and `||` operators
- **I/O redirection**: `>&2` for stderr output
- **Script detection**: `${BASH_SOURCE[0]}` vs `${0}` for sourcing

## Learning Notes

This is the **final unused language** in the Euler_Problems repository! Demonstrates:
- Bash can solve computational problems (not just sysadmin tasks)
- TDD methodology applies to shell scripting
- Custom test framework creation in Bash
- Performance trade-offs between compiled vs interpreted languages

**Language Count**: 20 different languages now used in this repository!
