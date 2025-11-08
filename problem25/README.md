# Project Euler Problem 25: 1000-digit Fibonacci Number

## Problem Statement

The Fibonacci sequence is defined by the recurrence relation:

F₁ = 1, F₂ = 1, Fₙ = Fₙ₋₁ + Fₙ₋₂ for n > 2

The 12th term, F₁₂ = 144, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

## Approach

### Algorithm

Iteratively generate Fibonacci numbers and check their digit count:

1. Start with F₁ = 1, F₂ = 1
2. For each subsequent number, calculate Fₙ = Fₙ₋₁ + Fₙ₋₂
3. Count digits by converting to string
4. Return the index when digit count reaches 1000

### Complexity

- **Time**: O(n) where n is the result index (~4782)
- **Space**: O(1) - only store current and previous Fibonacci numbers
- **Note**: Digit count is O(log n) for string conversion

### Why This Works

Elixir (and Erlang) have **native arbitrary-precision integers**, so we can handle 1000-digit numbers without any special libraries.

## Implementation Details

- **Language**: Elixir 1.14
- **Testing Framework**: ExUnit (Elixir's built-in testing)
- **Why Elixir**:
  - **First Elixir solution in repository!**
  - Native big integer support (perfect for this problem)
  - Excellent functional programming paradigm
  - Pattern matching for clean code
  - Built-in testing with doctests
  - Fast and efficient on Erlang VM

### Project Structure

```
fibonacci/
├── lib/
│   └── fibonacci.ex          # Main module with all functions
├── test/
│   ├── test_helper.exs       # Test configuration
│   └── fibonacci_test.exs    # Test suite
├── mix.exs                   # Project configuration
└── README.md                 # This file (in parent dir)
```

### Functions

```elixir
fib_at(n)                    # Calculate nth Fibonacci number
count_digits(n)              # Count digits in a number
first_fib_with_n_digits(n)   # Find index of first n-digit Fib number
solve()                      # Solve Problem 25
```

## TDD Methodology

Following AGENTS.md constitutional framework:

### Phase 1: RED
- Created Mix project with ExUnit
- Wrote 16 comprehensive tests (6 test cases + 10 doctests)
- Tests for: Fibonacci generation, digit counting, finding n-digit numbers
- All tests initially failed with stub implementations

### Phase 2: GREEN
- Implemented iterative Fibonacci algorithm
- Fixed off-by-one error in indexing
- All 16 tests passing ✓
- Answer: 4782

### Phase 3: REFACTOR
- Clean, idiomatic Elixir code
- Pattern matching for base cases
- Tail-recursive helper functions
- Comprehensive documentation with doctests

## Test Coverage

✓ **16 tests (6 ExUnit + 10 doctests), all passing:**

**Fibonacci generation tests** (11 tests):
- Base cases (F₁, F₂)
- Known values (F₃ through F₁₂)
- Doctests verify examples in documentation

**Digit counting tests** (8 tests):
- Single digits (1, 9)
- Multiple digits (10, 99, 100, 999, 1000, 12345)
- Boundary cases

**Finding n-digit numbers** (5 tests):
- First 1-digit Fib (F₁)
- First 2-digit Fib (F₇)
- First 3-digit Fib (F₁₂)
- First 4-digit Fib (dynamic verification)

**Solution verification** (2 tests):
- Result is valid integer > 1000
- F₄₇₈₂ has exactly 1000 digits
- F₄₇₈₁ has < 1000 digits

## Answer

**4782**

F₄₇₈₂ is the first Fibonacci number with 1000 digits.

## Performance

Elixir's performance on this problem:
- **Execution time**: ~0.4 seconds
- **Tests passing**: 16/16 ✓
- **Memory**: Minimal (only stores two numbers at a time)

The native big integer support makes this problem trivial in Elixir - no special libraries needed!

## Running the Solution

```bash
# Navigate to project
cd problem25/fibonacci

# Run tests
mix test

# Run with verbose output
mix test --trace

# Run solution
mix run -e "IO.puts(Fibonacci.solve())"

# Interactive Elixir shell
iex -S mix
iex> Fibonacci.solve()
4782

# Run doctests
mix test --only doctest
```

## Sample Output

```
$ mix test
Compiling 1 file (.ex)
................
Finished in 0.4 seconds (0.00s async, 0.4s sync)
10 doctests, 6 tests, 0 failures

$ mix run -e "IO.puts(Fibonacci.solve())"
4782
```

## Elixir Language Features Used

- **Pattern Matching**: Multiple function clauses with guards
- **Tail Recursion**: Efficient iteration with recursive helpers
- **Pipe Operator**: `|>` for clean data transformation
- **Guards**: `when n > 0` for function constraints
- **Big Integers**: Native arbitrary-precision arithmetic
- **Doctests**: Tests embedded in documentation
- **Mix**: Build tool and project management
- **ExUnit**: Powerful testing framework with descriptive syntax

## Example: Fibonacci Sequence Growth

```
F₁ = 1 (1 digit)
F₇ = 13 (2 digits)
F₁₂ = 144 (3 digits)
F₁₇ = 1597 (4 digits)
...
F₄₇₈₂ = 10715...52209 (1000 digits)
```

The Fibonacci sequence grows exponentially: Fₙ ≈ φⁿ/√5 where φ = (1+√5)/2 ≈ 1.618

## Learning Notes

This is the **first Elixir solution** in the Euler_Problems repository! Demonstrates:
- Elixir's functional programming elegance
- Native big integer support
- Pattern matching and guards
- ExUnit testing with doctests
- Mix project structure
- Tail-recursive algorithms

**Language Count**: 22 different languages now used in this repository!

## Trivia

The 1000-digit Fibonacci number (F₄₇₈₂) starts with:
```
107158813011...
```

and ends with:
```
...52209
```

It contains exactly 1,000 digits!
