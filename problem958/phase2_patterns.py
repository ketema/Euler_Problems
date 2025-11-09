#!/usr/bin/env python3
"""
Phase 2: Pattern Analysis

From Phase 1:
- d(n,m) = sum of CF coefficients - 1
- f(F_n) appears to equal F_{n-2}
- Consecutive Fibs have CF = [1,1,...,1,2]

Questions:
1. What's the pattern for non-Fibonacci n?
2. How do we compute f(10^12 + 39)?
3. Is there a closed-form or efficient algorithm?
"""

import math


def continued_fraction(n, m):
    """Get continued fraction representation of n/m."""
    cf = []
    while m != 0:
        cf.append(n // m)
        n, m = m, n % m
    return cf


def cf_sum(n, m):
    """Sum of continued fraction coefficients."""
    return sum(continued_fraction(n, m))


def d(n, m):
    """Number of steps in subtraction algorithm."""
    return cf_sum(n, m) - 1


def find_f(n, max_search=None):
    """
    Find f(n) = smallest m coprime to n that minimizes d(n,m).

    For efficiency, we limit search range.
    """
    if max_search is None:
        max_search = n

    min_steps = float('inf')
    best_m = None

    for m in range(1, min(n, max_search)):
        if math.gcd(n, m) == 1:  # coprime
            steps = d(n, m)
            if steps < min_steps or (steps == min_steps and (best_m is None or m < best_m)):
                min_steps = steps
                best_m = m

    return best_m, min_steps


def fibonacci_pattern_test():
    """Test the hypothesis: f(F_n) = F_{n-2}."""
    print("="*70)
    print("FIBONACCI PATTERN HYPOTHESIS")
    print("="*70)
    print()

    # Generate Fibonacci numbers
    fibs = [1, 1]
    while len(fibs) < 20:
        fibs.append(fibs[-1] + fibs[-2])

    print("Testing: f(F_n) = F_{n-2}?")
    print()
    print(f"{'n':<5} {'F_n':<10} {'f(F_n) computed':<20} {'F_{n-2}':<10} {'Match?':<10}")
    print("-"*70)

    for i in range(4, 13):  # Start from F_4 to have F_{n-2}
        n = fibs[i]
        expected = fibs[i-2]

        # Compute f(n) - but limit search for efficiency
        # We expect the answer to be around F_{i-2}, so search nearby
        search_range = min(n, max(1000, expected * 2))

        computed_m, computed_steps = find_f(n, max_search=search_range)

        # Also directly test if F_{i-2} gives minimum
        if expected < n and math.gcd(n, expected) == 1:
            expected_steps = d(n, expected)

        match = "✓" if computed_m == expected else "✗"

        print(f"{i:<5} {n:<10} {computed_m:<20} {expected:<10} {match}")

    print()


def generalized_fibonacci_test():
    """
    Test if the pattern extends beyond Fibonacci numbers.

    Key insight: The continued fraction [1,1,1,...,1,2] minimizes the sum.
    This corresponds to ratios of consecutive Fibonacci numbers.

    For arbitrary n, we want to find m such that n/m has a CF close to this pattern.
    """
    print("="*70)
    print("GENERALIZED PATTERN INVESTIGATION")
    print("="*70)
    print()

    print("For non-Fibonacci numbers, what pattern emerges?")
    print()

    test_values = [10, 15, 20, 100, 1000]

    print(f"{'n':<10} {'f(n)':<10} {'d(n,f(n))':<15} {'CF of n/f(n)':<30}")
    print("-"*75)

    for n in test_values:
        m, steps = find_f(n, max_search=min(n, 2000))
        cf = continued_fraction(n, m)

        print(f"{n:<10} {m:<10} {steps:<15} {str(cf)[:28]}")

    print()
    print("Observation: CF coefficients should be as small as possible")
    print("Best case: all 1s (Fibonacci-like)")
    print()


def test_given_examples():
    """Verify the given examples."""
    print("="*70)
    print("VERIFY GIVEN EXAMPLES")
    print("="*70)
    print()

    examples = [
        (7, 2),
        (89, 34),
        (8191, 1856),
    ]

    print(f"{'n':<10} {'f(n) given':<15} {'d(n,f(n))':<15} {'CF sum':<10}")
    print("-"*55)

    for n, m in examples:
        steps = d(n, m)
        cf_s = cf_sum(n, m)
        print(f"{n:<10} {m:<15} {steps:<15} {cf_s}")

    print()

    # Analyze 8191 = 2^13 - 1 (Mersenne prime)
    print("Special case: n = 8191 = 2^13 - 1 (Mersenne prime)")
    print(f"  f(8191) = 1856")
    print(f"  Ratio: 1856/8191 ≈ {1856/8191:.6f}")
    print()

    # Check if 1856 has special properties
    print(f"  Factorization of 1856: ", end="")
    n = 1856
    factors = []
    for p in [2, 3, 5, 7, 11, 13]:
        while n % p == 0:
            factors.append(p)
            n //= p
    if n > 1:
        factors.append(n)
    print(" × ".join(map(str, factors)))
    print()


def theoretical_minimum():
    """
    Theoretical analysis: What's the minimum possible d(n,m)?

    For n/m with continued fraction [a₀, a₁, ..., aₖ]:
    - d(n,m) = a₀ + a₁ + ... + aₖ - 1
    - Minimum sum occurs when coefficients are small
    - Best: all coefficients = 1, giving CF = [1,1,...,1]
    - This gives sum = k+1, so d = k

    But we also need n/m ≈ some value. The number of terms k depends on n/m.
    """
    print("="*70)
    print("THEORETICAL MINIMUM")
    print("="*70)
    print()

    print("For a fraction n/m, the CF length depends on the ratio:")
    print()
    print("Golden ratio φ = (1+√5)/2 ≈ 1.618...")
    print("  CF(φ) = [1; 1, 1, 1, ...] (infinite)")
    print("  F_{k+1}/F_k → φ as k → ∞")
    print()
    print("For finite n/m, CF terminates when remainder is 0.")
    print()
    print("Strategy to minimize d(n,m):")
    print("1. Find m coprime to n")
    print("2. Such that n/m has CF with small coefficients")
    print("3. Ideally close to Fibonacci ratio")
    print()


def main():
    """Phase 2: Pattern exploration."""
    fibonacci_pattern_test()
    generalized_fibonacci_test()
    test_given_examples()
    theoretical_minimum()

    print("="*70)
    print("PHASE 2 FINDINGS")
    print("="*70)
    print()
    print("KEY PATTERNS:")
    print("1. f(F_n) = F_{n-2} for Fibonacci numbers")
    print("2. For general n, f(n) gives n/f(n) with minimal CF sum")
    print("3. Optimal CFs have small coefficients (ideally all 1s)")
    print("4. This relates to best rational approximations")
    print()
    print("NEXT: Phase 3 - Analyze 'Euclid's Labour' name clue")
    print()


if __name__ == "__main__":
    main()
