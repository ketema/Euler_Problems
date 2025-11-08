# Project Euler Problem 24: Lexicographic Permutations

## Problem Statement

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8, 9?

## Approach

### Naive Approach: Generate All Permutations

Generate all 10! = 3,628,800 permutations and sort them.

**Complexity**: O(n! × n log n) - Impractical!

### Optimal Approach: Factorial Number System

Use the factorial number system to directly compute the nth permutation without generating all of them.

#### Algorithm

For n elements, there are n! total permutations in lexicographic order:
- The first (n-1)! permutations start with the smallest element
- The next (n-1)! permutations start with the second smallest element
- And so on...

**Example with {0, 1, 2, 3}:**
- Total permutations: 4! = 24
- Permutations 1-6 start with 0 (6 = 3!)
- Permutations 7-12 start with 1
- Permutations 13-18 start with 2
- Permutations 19-24 start with 3

To find the nth permutation:
1. Calculate which "block" it falls into: `index = (n-1) / (n-1)!`
2. Pick that element from available elements
3. Recursively find the remaining positions with `k = (n-1) % (n-1)!`

**Complexity**: O(n²) - Much better!

## Implementation Details

- **Language**: Lua 5.4
- **Why Lua**:
  - Lightweight and elegant scripting language
  - **First Lua solution in repository!**
  - Excellent for algorithmic problems
  - Simple table manipulation
  - Clean, readable syntax

### Functions

```lua
factorial(n)              -- Calculate n!
nth_permutation(elems, n) -- Find nth lexicographic permutation
perm_to_string(perm)      -- Convert permutation array to string
solve(n)                  -- Main solution function
```

## TDD Methodology

Following AGENTS.md constitutional framework:

### Phase 1: RED
- Wrote 19 comprehensive tests first
- Created custom Lua testing framework
- Tests for: factorial, nth_permutation, string conversion, integration
- All tests initially failed with stubs

### Phase 2: GREEN
- Implemented factorial number system algorithm
- All 19 tests passing ✓
- Correct answer: 2783915460

### Phase 3: REFACTOR
- Clean, well-documented Lua code
- Efficient O(n²) algorithm
- Proper table copying to avoid mutation

## Test Coverage

✓ **19 tests, all passing:**

**Factorial calculations** (7 tests):
- Edge cases (0, 1)
- Small values (2, 3, 4, 5)
- Larger value (10 = 3,628,800)

**Permutation generation** (6 tests):
- Small sets ({0,1,2}, {0,1,2,3})
- First permutation (smallest)
- Last permutation (largest)
- Middle permutations

**String conversion** (3 tests):
- Small arrays
- Full 10-digit array
- Correct string concatenation

**Integration** (3 tests):
- Result properties validation
- Type checking
- Length verification

## Answer

**2783915460**

The millionth (1,000,000th) lexicographic permutation of digits 0-9.

## Performance

The factorial number system approach is extremely efficient:
- **Time**: O(n²) where n = 10
- **Space**: O(n)
- **Execution**: Instant (<0.01 seconds)

Compare to naive approach:
- Would need to generate 3,628,800 permutations
- Then sort them
- Then pick the millionth one
- Would take seconds or minutes instead of instant

## Running the Solution

```bash
# Run tests
cd problem24/lua
lua test_lexicographic_perm.lua

# Run solution (default: 1,000,000th permutation)
lua lexicographic_perm.lua

# Run for different n
lua lexicographic_perm.lua 100

# Use as a module
lua -e 'require("lexicographic_perm"); print(solve(1000000))'
```

## Sample Output

```
$ lua lexicographic_perm.lua

Problem 24: Lexicographic Permutations
Finding the 1000000th permutation of 0-9

Answer: 2783915460
```

## Lua Language Features Used

- **Tables**: Dynamic arrays for elements and results
- **Functions**: First-class functions, multiple returns
- **Math library**: `math.floor` for integer division
- **String operations**: Concatenation with `..`
- **Module system**: `require` for importing
- **Metaprogramming**: `arg` table for command-line arguments
- **Pattern matching**: String patterns for script detection

## Algorithm Walkthrough

Finding the 1,000,000th permutation of {0,1,2,3,4,5,6,7,8,9}:

```
n = 1,000,000 (convert to k = 999,999 for 0-indexing)

Position 1: k / 9! = 999,999 / 362,880 = 2.75... → index 2 → digit 2
  Remaining: {0,1,3,4,5,6,7,8,9}, k = 999,999 % 362,880 = 274,239

Position 2: k / 8! = 274,239 / 40,320 = 6.80... → index 6 → digit 7
  Remaining: {0,1,3,4,5,6,8,9}, k = 274,239 % 40,320 = 32,319

Position 3: k / 7! = 32,319 / 5,040 = 6.41... → index 6 → digit 8
  Remaining: {0,1,3,4,5,6,9}, k = 32,319 % 5,040 = 2,239

... (continue for all 10 positions)

Result: 2783915460
```

## Learning Notes

This is the **first Lua solution** in the Euler_Problems repository! Demonstrates:
- Lua's suitability for algorithmic problems
- Clean table manipulation
- How to implement custom test frameworks in Lua
- Factorial number system for combinatorics

**Language Count**: 21 different languages now used in this repository!
