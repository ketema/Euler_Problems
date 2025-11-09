#!/usr/bin/env python3
"""
Phase 4: Efficient Search Algorithm

Goal: Find f(10^12 + 39) efficiently.

Key insights from Phases 1-3:
1. d(n,m) = sum of CF coefficients - 1
2. f(F_n) = F_{n-2} (Fibonacci pattern)
3. For Fibonacci: F_n/F_{n-2} → φ²
4. But pattern differs for non-Fibonacci numbers

STRATEGY:
Use continued fraction convergents to find candidates!

The convergents of a continued fraction are the best rational
approximations. We'll search among convergents and nearby values.
"""

import math


def gcd(a, b):
    """Compute GCD."""
    while b:
        a, b = b, a % b
    return a


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
    """Number of steps = sum of CF coefficients - 1."""
    return cf_sum(n, m) - 1


def convergents(cf):
    """
    Compute convergents of a continued fraction.

    For CF = [a0, a1, a2, ...], convergents are p_k/q_k
    where:
    p_{-1} = 1, p_0 = a0
    q_{-1} = 0, q_0 = 1
    p_k = a_k * p_{k-1} + p_{k-2}
    q_k = a_k * q_{k-1} + q_{k-2}

    Returns: list of (numerator, denominator) pairs
    """
    if not cf:
        return []

    # Initialize
    p_prev2, p_prev1 = 1, cf[0]
    q_prev2, q_prev1 = 0, 1

    result = [(p_prev1, q_prev1)]

    for i in range(1, len(cf)):
        a_i = cf[i]
        p_i = a_i * p_prev1 + p_prev2
        q_i = a_i * q_prev1 + q_prev2

        result.append((p_i, q_i))

        p_prev2, p_prev1 = p_prev1, p_i
        q_prev2, q_prev1 = q_prev1, q_i

    return result


def fibonacci_numbers(max_val):
    """Generate Fibonacci numbers up to max_val."""
    fibs = [1, 1]
    while fibs[-1] < max_val:
        fibs.append(fibs[-1] + fibs[-2])
    return fibs


def smart_search(n, search_radius=10000):
    """
    Smart search for f(n) using multiple strategies.

    Strategies:
    1. Test Fibonacci numbers (if n is Fibonacci, answer is F_{n-2})
    2. Test multiples and divisors of Fibonacci numbers
    3. Test convergents of n
    4. Test values near n/φ^k for k=1,2,3

    Returns: (best_m, min_steps)
    """
    phi = (1 + math.sqrt(5)) / 2

    min_steps = float('inf')
    best_m = None

    candidates = set()

    # Strategy 1: Check if n is Fibonacci
    fibs = fibonacci_numbers(n)
    if n in fibs:
        idx = fibs.index(n)
        if idx >= 2:
            # f(F_n) = F_{n-2}
            return fibs[idx-2], d(n, fibs[idx-2])

    # Strategy 2: Test Fibonacci numbers as candidates
    for fib in fibs:
        if fib < n and fib > 0:
            candidates.add(fib)

    # Strategy 3: Test values near n/φ^k
    for k in [1, 2, 3, 4]:
        target = n / (phi ** k)
        for offset in range(-search_radius, search_radius):
            m = int(target) + offset
            if 1 <= m < n:
                candidates.add(m)

    # Strategy 4: Use convergents approach
    # Compute CF of n (as n/1)
    # Then look for denominators that work well

    # Strategy 5: Test values with small prime factors
    # (These tend to give simpler CFs)
    for p in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]:
        for k in range(1, 10):
            m = p ** k
            if m < n:
                candidates.add(m)
            # Also test nearby values
            for offset in range(-100, 100):
                m = p ** k + offset
                if 1 <= m < n:
                    candidates.add(m)

    # Now test all candidates
    print(f"Testing {len(candidates)} candidates for n={n}...")

    for m in sorted(candidates):
        if gcd(n, m) == 1:  # coprime
            steps = d(n, m)
            if steps < min_steps or (steps == min_steps and (best_m is None or m < best_m)):
                min_steps = steps
                best_m = m

    return best_m, min_steps


def verify_known_examples():
    """Verify our algorithm on known examples."""
    print("="*70)
    print("VERIFYING KNOWN EXAMPLES")
    print("="*70)
    print()

    examples = [
        (7, 2),
        (89, 34),
        (8191, 1856),
    ]

    print(f"{'n':<10} {'Expected f(n)':<15} {'Computed f(n)':<15} {'Match?':<10}")
    print("-"*55)

    for n, expected in examples:
        computed, steps = smart_search(n, search_radius=5000)
        match = "✓" if computed == expected else "✗"

        print(f"{n:<10} {expected:<15} {computed:<15} {match}")

        if computed != expected:
            print(f"  WARNING: Mismatch detected!")
            print(f"  d({n},{expected}) = {d(n, expected)}")
            print(f"  d({n},{computed}) = {steps}")

    print()


def compute_answer():
    """Compute f(10^12 + 39)."""
    print("="*70)
    print("COMPUTING f(10^12 + 39)")
    print("="*70)
    print()

    n = 10**12 + 39
    print(f"n = {n:,}")
    print()

    print("This will take some time...")
    print()

    # Use smart search with larger radius for this big number
    m, steps = smart_search(n, search_radius=100000)

    print(f"RESULT:")
    print(f"  f({n:,}) = {m:,}")
    print(f"  d({n:,}, {m:,}) = {steps}")
    print()

    # Verify
    print("Verification:")
    print(f"  gcd({n}, {m}) = {gcd(n, m)}")
    cf = continued_fraction(n, m)
    print(f"  CF length: {len(cf)}")
    print(f"  CF sum: {sum(cf)}")
    print(f"  d(n,m) = CF sum - 1 = {sum(cf) - 1}")
    print()

    return m


def main():
    """Phase 4: Efficient search algorithm."""

    # First verify on known examples
    verify_known_examples()

    # Then compute the answer
    answer = compute_answer()

    print("="*70)
    print("PHASE 4 COMPLETE")
    print("="*70)
    print()
    print(f"ANSWER: f(10^12 + 39) = {answer:,}")
    print()


if __name__ == "__main__":
    main()
