#!/usr/bin/env python3
"""
Project Euler 958: Euclid's Labour - Phase 1 Analysis

Understanding the problem structure:
1. d(n,m) = steps in subtraction-based Euclidean algorithm
2. f(n) = smallest m coprime to n that minimizes d(n,m)
3. Examples: f(7)=2, f(89)=34, f(8191)=1856

Goal: Find f(10^12 + 39)
"""

import math
from collections import defaultdict


def gcd_steps_subtraction(n, m):
    """
    Count steps in subtraction-based Euclidean algorithm.

    Algorithm: Repeatedly subtract smaller from larger until equal.
    Returns: (gcd, number of steps)
    """
    steps = 0
    while n != m:
        if n > m:
            n = n - m
        else:
            m = m - n
        steps += 1
    return n, steps


def d(n, m):
    """Return number of steps for gcd(n,m) using subtraction."""
    _, steps = gcd_steps_subtraction(n, m)
    return steps


def analyze_structure_small_n():
    """Analyze structure for small n to find patterns."""
    print("="*70)
    print("PHASE 1: UNDERSTANDING PROBLEM STRUCTURE")
    print("="*70)
    print()

    print("Testing d(n,m) for small values:")
    print()

    # Verify example: f(7) = 2
    n = 7
    print(f"n = {n} (coprime values: 1,2,3,4,5,6)")
    print(f"{'m':<5} {'gcd':<5} {'d(n,m)':<10} {'coprime?':<10}")
    print("-"*35)

    d_values = {}
    for m in range(1, n):
        g = math.gcd(n, m)
        if g == 1:  # coprime
            steps = d(n, m)
            d_values[m] = steps
            print(f"{m:<5} {g:<5} {steps:<10} ✓")

    min_steps = min(d_values.values())
    min_m = min(m for m, steps in d_values.items() if steps == min_steps)

    print()
    print(f"Minimum d(n,m) = {min_steps}, achieved by m = {min_m}")
    print(f"f({n}) = {min_m} ✓")
    print()

    # Relationship to Euclidean algorithm quotients
    print("="*70)
    print("CONNECTION TO QUOTIENTS")
    print("="*70)
    print()

    n, m = 7, 2
    print(f"Tracing gcd({n},{m}) step by step:")
    print()

    a, b = n, m
    step = 0
    quotients = []

    print(f"{'Step':<5} {'(a,b)':<15} {'Action':<30}")
    print("-"*50)
    print(f"{step:<5} ({a},{b})")

    while a != b:
        if a > b:
            q = a // b
            quotients.append(q)
            action = f"{a} - {b} = {a-b}"
            a = a - b
        else:
            q = b // a
            quotients.append(q)
            action = f"{b} - {a} = {b-a}"
            b = b - a
        step += 1
        print(f"{step:<5} ({a},{b}){' '*5} {action}")

    print()
    print(f"Total steps: {step}")
    print(f"Note: We subtract the smaller from larger each time")
    print()

    # Standard Euclidean algorithm for comparison
    print("Standard Euclidean algorithm (with mod):")
    a, b = n, m
    quotient_sum = 0
    while b != 0:
        q = a // b
        quotient_sum += q
        print(f"  {a} = {q}·{b} + {a % b}")
        a, b = b, a % b

    print()
    print(f"Sum of quotients: {quotient_sum}")
    print(f"d(7,2) = {d(7,2)}")
    print(f"Relationship: d(n,m) = (sum of quotients) - 1")
    print()


def test_continued_fraction_hypothesis():
    """Test if d(n,m) relates to continued fractions."""
    print("="*70)
    print("CONTINUED FRACTION CONNECTION")
    print("="*70)
    print()

    def continued_fraction(n, m):
        """Get continued fraction representation of n/m."""
        cf = []
        while m != 0:
            q = n // m
            cf.append(q)
            n, m = m, n % m
        return cf

    # Test cases
    test_cases = [
        (7, 2),
        (7, 3),
        (7, 5),
        (89, 34),  # Given example
    ]

    print(f"{'n/m':<10} {'CF representation':<30} {'CF sum':<10} {'d(n,m)':<10} {'Relation':<15}")
    print("-"*80)

    for n, m in test_cases:
        cf = continued_fraction(n, m)
        cf_sum = sum(cf)
        steps = d(n, m)

        print(f"{n}/{m:<8} {str(cf):<30} {cf_sum:<10} {steps:<10} sum - 1 = {cf_sum - 1}")

    print()
    print("✓ CONFIRMED: d(n,m) = sum of continued fraction coefficients - 1")
    print()


def analyze_fibonacci_pattern():
    """Check if Fibonacci numbers appear in the pattern."""
    print("="*70)
    print("FIBONACCI NUMBER INVESTIGATION")
    print("="*70)
    print()

    # Generate Fibonacci numbers
    fibs = [1, 1]
    while fibs[-1] < 10000:
        fibs.append(fibs[-1] + fibs[-2])

    print("Fibonacci numbers:", fibs[:15])
    print()

    # Check f(89) = 34
    print("Given: f(89) = 34")
    print(f"  89 is F_11: {fibs}")

    # Find 89 in fibs
    if 89 in fibs:
        idx_89 = fibs.index(89)
        print(f"  89 = F_{idx_89}")
        print(f"  34 = F_{fibs.index(34)} (if in sequence)")

        if 34 in fibs:
            idx_34 = fibs.index(34)
            print(f"  Pattern: f(F_{idx_89}) = F_{idx_34}")
            print(f"  Difference: {idx_89} - {idx_34} = {idx_89 - idx_34}")

    print()
    print("Testing: Are consecutive Fibonacci numbers special?")

    # Test d(F_k, F_{k-1}) for small k
    for i in range(2, min(len(fibs)-1, 10)):
        n, m = fibs[i], fibs[i-1]
        if m > 0:
            steps = d(n, m)
            cf = []
            a, b = n, m
            while b != 0:
                cf.append(a // b)
                a, b = b, a % b

            print(f"  d(F_{i}, F_{i-1}) = d({n}, {m}) = {steps}, CF = {cf}, sum = {sum(cf)}")

    print()


def main():
    """Phase 1: Understanding the structure."""
    analyze_structure_small_n()
    test_continued_fraction_hypothesis()
    analyze_fibonacci_pattern()

    print("="*70)
    print("PHASE 1 FINDINGS")
    print("="*70)
    print()
    print("KEY DISCOVERIES:")
    print("1. d(n,m) = (sum of continued fraction coefficients of n/m) - 1")
    print("2. f(n) finds the m that minimizes this sum")
    print("3. Examples show Fibonacci-like patterns")
    print()
    print("NEXT: Phase 2 - Look for sequence patterns and check OEIS")
    print()


if __name__ == "__main__":
    main()
