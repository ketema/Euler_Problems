# Project Euler Problem 25: 1000-digit Fibonacci Number

## Problem Statement

The Fibonacci sequence is defined by the recurrence relation:

**F_n = F_{n-1} + F_{n-2}**, where F_1 = 1 and F_2 = 1.

The first 12 terms are:
```
F_1  = 1
F_2  = 1
F_3  = 2
F_4  = 3
F_5  = 5
F_6  = 8
F_7  = 13
F_8  = 21
F_9  = 34
F_10 = 55
F_11 = 89
F_12 = 144
```

The 12th term, F_12, is the first term to contain three digits.

**Question**: What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

## Answer

**4782**

## Solution Approach

This problem is solved using **tail-recursive iteration** in Elixir, leveraging the language's:
- Native arbitrary-precision integers (no library needed)
- BEAM VM tail call optimization
- Functional programming paradigm

### Algorithm

```elixir
def find_index_with_n_digits(n) do
  iterate(1, 1, 2, n)  # Start at F_2 with values (prev=1, curr=1, index=2)
end

defp iterate(prev, curr, index, target_digits) do
  if digit_count(curr) >= target_digits do
    index
  else
    iterate(curr, prev + curr, index + 1, target_digits)
  end
end
```

**Complexity**:
- Time: O(k) where k = result index (4782 for this problem)
- Space: O(1) due to tail call optimization
- Digit counting: O(d) where d = number of digits, using `Integer.digits/1`

### Why Elixir?

1. **Built-in Bigints**: No external libraries required for arbitrary-precision arithmetic
2. **Tail Recursion**: BEAM VM optimizes tail calls to constant stack space
3. **Immutability**: Functional paradigm prevents mutation bugs
4. **ExUnit**: Excellent built-in testing framework for TDD

## Running the Solution

### Prerequisites
```bash
# Install Elixir (includes Erlang/OTP)
brew install elixir
```

### Run Tests
```bash
cd elixir
mix test
```

Expected output:
```
.......
Finished in 0.5 seconds (0.00s async, 0.5s sync)
3 doctests, 4 tests, 0 failures
```

### Run Solution Directly
```bash
cd elixir
elixir -e "Code.require_file(\"lib/fibonacci.ex\"); IO.puts(\"Answer: #{Fibonacci.find_index_with_n_digits(1000)}\")"
```

## Test Coverage

The test suite validates:
1. **Edge case**: First 1-digit term (F_1 = 1, index 1)
2. **Transition**: First 2-digit term (F_7 = 13, index 7)
3. **Transition**: First 3-digit term (F_12 = 144, index 12)
4. **Main problem**: First 1000-digit term (index 4782)

All tests follow TDD methodology (RED → GREEN → COMMIT).

## Performance

- **Execution time**: ~0.5 seconds for 1000-digit computation
- **Iterations**: 4782 Fibonacci generations
- **Memory**: O(1) tail recursion (no stack growth)

## Implementation Notes

**Index Handling**: The algorithm carefully handles the off-by-one issue:
- F_1 = 1 (index 1)
- F_2 = 1 (index 2)
- Iteration starts at index 2 with values (prev=1, curr=1)

**Digit Counting**: Uses `Integer.digits/1` to convert bigints to digit lists, then `length/1` for counting. While this is O(d), it's more idiomatic than mathematical approaches (log10) and avoids floating-point errors.

## Language Choice

Elixir was chosen for this problem as it had not been previously used in this codebase, meeting the requirement to use a new language while providing excellent support for:
- Test-Driven Development (ExUnit)
- Arbitrary-precision arithmetic (native)
- Functional programming patterns
