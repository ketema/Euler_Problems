# Project Euler Problem 24: Lexicographic Permutations

## Educational Case Study: Brute Force vs Mathematical Efficiency

### Problem Statement

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.

The lexicographic permutations of 0, 1 and 2 are:
```
012   021   102   120   201   210
```

**Question**: What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

## Answer

**2783915460**

## Comparative Implementation Study

This solution implements **both** approaches to demonstrate the dramatic difference between:
1. **Brute Force** (simple, concise)
2. **Mathematical** (efficient, elegant)

### Approach 1: Brute Force (std::next_permutation)

```cpp
std::string brute_force_permutation(const std::string& digits, int k) {
    std::string result = digits;
    for (int i = 1; i < k; ++i) {
        std::next_permutation(result.begin(), result.end());
    }
    return result;
}
```

- **Lines of Code**: 6 lines
- **Time Complexity**: O(k × n) where k=1,000,000, n=10
- **Operations**: ~10,000,000
- **Pros**: Simple, uses STL, easy to understand
- **Cons**: Extremely slow for large k

### Approach 2: Mathematical (Factorial Number System)

```cpp
std::string mathematical_permutation(const std::string& digits, int k) {
    std::vector<char> available(digits.begin(), digits.end());
    std::string result;
    int n = available.size();
    k--; // Convert to 0-indexed

    // Precompute factorials
    std::vector<long long> factorial(n);
    factorial[0] = 1;
    for (int i = 1; i < n; ++i) {
        factorial[i] = factorial[i-1] * i;
    }

    // Build permutation digit by digit
    for (int i = 0; i < n; ++i) {
        int index = k / factorial[n-1-i];
        result += available[index];
        available.erase(available.begin() + index);
        k %= factorial[n-1-i];
    }

    return result;
}
```

- **Lines of Code**: 20 lines
- **Time Complexity**: O(n²) where n=10
- **Operations**: ~100
- **Pros**: Extremely fast, mathematically elegant
- **Cons**: More complex, requires understanding factorial number system

## Benchmark Results (1000 iterations)

```
╔════════════════════════════════════════════════════════════╗
║                      BENCHMARK RESULTS                     ║
╠════════════════════════════════════════════════════════════╣
║ Approach          │ Total (ms)  │ Avg (ms)   │ Speedup    ║
╠═══════════════════╪═════════════╪════════════╪════════════╣
║ Brute Force      │    92773.26 │    92.7733 │ 1.00x      ║
║ Mathematical     │        2.02 │     0.0020 │ 45881.93x ║
╚═══════════════════╧═════════════╧════════════╧════════════╝
```

### Key Findings

**Performance Comparison:**
- **Brute Force**: 92.77 ms per call
- **Mathematical**: 0.002 ms per call
- **Speedup**: **45,882×** faster!

**Theoretical vs Actual:**
- Theoretical speedup: ~100,000× (10M ops vs 100 ops)
- Actual speedup: 45,882×
- The difference is due to:
  - Memory allocation overhead in mathematical approach
  - Cache efficiency of std::next_permutation
  - Compiler optimizations (-O3)

## Educational Insights

### For Students

This case study demonstrates a fundamental principle in computer science:

**Algorithmic efficiency trumps code brevity.**

While the brute force approach is only 6 lines vs 20 lines for the mathematical approach, the 14 extra lines buy you:
- **45,882× performance improvement**
- **Scalability**: Mathematical approach time is constant regardless of k
- **Elegance**: Direct computation vs iterative generation

### When to Use Each

**Brute Force:**
- Small datasets (k < 10,000)
- Prototyping/testing
- When developer time > compute time
- Educational purposes (easy to understand)

**Mathematical:**
- Large k values (k > 100,000)
- Production systems
- Performance-critical applications
- When compute time matters

## Complexity Analysis

| Approach     | Time        | Space | Operations for k=1M |
|--------------|-------------|-------|---------------------|
| Brute Force  | O(k × n)    | O(n)  | ~10,000,000         |
| Mathematical | O(n²)       | O(n)  | ~100                |

**Space complexity is identical** - both use O(n) space.

**Time complexity difference is massive** - k makes all the difference!

## How the Mathematical Approach Works

The factorial number system allows direct computation of the kth permutation:

1. **Factorial Base**: Represent k in factorial base
2. **Digit Selection**: For each position, determine which remaining digit to use
3. **Index Calculation**: `index = k / (n-i-1)!`
4. **Remainder Update**: `k %= (n-i-1)!`

**Example:** Finding 4th permutation of "012"
```
k = 3 (0-indexed)
Factorials: [1, 1, 2]

Position 0: index = 3/2 = 1 → pick '1' from [0,1,2] → remaining [0,2]
            k = 3%2 = 1
Position 1: index = 1/1 = 1 → pick '2' from [0,2]   → remaining [0]
            k = 1%1 = 0
Position 2: index = 0/1 = 0 → pick '0' from [0]     → remaining []

Result: "120" ✓
```

## Running the Code

### Compile and Run Tests
```bash
cd cpp
g++ -std=c++17 -c lexicographic_permutations.cpp
g++ -std=c++17 test_lexicographic_permutations.cpp lexicographic_permutations.o -o test
./test
```

### Run Benchmark
```bash
g++ -std=c++17 -O3 benchmark.cpp lexicographic_permutations.o -o benchmark
./benchmark
```

## Conclusion

**For Project Euler Problem 24, the mathematical approach is unequivocally superior.**

The ~5× increase in code complexity yields a **45,882× performance improvement**. This demonstrates why understanding algorithmic complexity and choosing the right algorithm is one of the most important skills in computer science.

**Trade-off Assessment:**
- 14 extra lines of code
- 45,882× faster execution
- **ROI: 3,277× performance gain per line of code**

The lesson: **Always analyze algorithmic complexity before optimizing for code brevity.**
