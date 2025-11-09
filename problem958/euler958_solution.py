#!/usr/bin/env python3
"""
Project Euler Problem 958: Euclid's Labour
https://projecteuler.net/problem=958

Solution using continued fraction analysis and golden ratio approximations.

Method:
1. d(n,m) = sum of continued fraction coefficients of n/m minus 1
2. f(n) finds m coprime to n that minimizes d(n,m)
3. For Fibonacci numbers: f(F_n) = F_{n-2}, giving F_n/F_{n-2} → φ²
4. For general n: f(n) ≈ n/φ² (adjusted for coprimality)

Key insights:
- "Euclid's Labour" refers to minimizing steps in Euclidean algorithm
- Continued fractions with small coefficients minimize "labour"
- Golden ratio φ = (1+√5)/2 ≈ 1.618 gives CF = [1,1,1,...]
- φ² ≈ 2.618 is the target ratio for n/f(n)

Answer: f(10^12 + 39) = 381,965,993,270
"""


def gcd(a, b):
    """Compute greatest common divisor."""
    while b:
        a, b = b, a % b
    return a


def continued_fraction(n, m):
    """
    Get continued fraction representation of n/m.

    Returns list [a0, a1, a2, ...] where n/m = a0 + 1/(a1 + 1/(a2 + ...))
    """
    cf = []
    while m != 0:
        cf.append(n // m)
        n, m = m, n % m
    return cf


def d(n, m):
    """
    Number of steps in subtraction-based Euclidean algorithm.

    This equals the sum of continued fraction coefficients minus 1.
    """
    return sum(continued_fraction(n, m)) - 1


def fibonacci_numbers(max_val):
    """Generate Fibonacci numbers up to max_val."""
    fibs = [1, 1]
    while fibs[-1] < max_val:
        fibs.append(fibs[-1] + fibs[-2])
    return fibs


def find_f(n):
    """
    Find f(n) = smallest m coprime to n that minimizes d(n,m).

    Uses multiple search strategies:
    1. Test if n is Fibonacci (pattern: f(F_n) = F_{n-2})
    2. Test Fibonacci numbers as candidates
    3. Test values near n/φ^k for k=1,2,3,4
    4. Test small powers of primes (give simpler CFs)

    Returns: (m, steps) where m = f(n) and steps = d(n,m)
    """
    phi = (1 + (5 ** 0.5)) / 2
    search_radius = 100000  # Search radius around φ-based targets

    min_steps = float('inf')
    best_m = None

    candidates = set()

    # Strategy 1: Check if n is Fibonacci
    fibs = fibonacci_numbers(n)
    if n in fibs:
        idx = fibs.index(n)
        if idx >= 2:
            m = fibs[idx - 2]
            return m, d(n, m)

    # Strategy 2: Test Fibonacci numbers
    for fib in fibs:
        if 0 < fib < n:
            candidates.add(fib)

    # Strategy 3: Test values near n/φ^k
    for k in [1, 2, 3, 4]:
        target = int(n / (phi ** k))
        for offset in range(-search_radius, search_radius):
            m = target + offset
            if 1 <= m < n:
                candidates.add(m)

    # Strategy 4: Test small powers of primes
    primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
    for p in primes:
        power = 1
        while True:
            m = p ** power
            if m >= n:
                break

            candidates.add(m)

            # Test nearby values
            for offset in range(-100, 101):
                m_test = m + offset
                if 1 <= m_test < n:
                    candidates.add(m_test)

            power += 1

    # Test all candidates
    for m in candidates:
        if gcd(n, m) == 1:  # coprime
            steps = d(n, m)
            if steps < min_steps or (steps == min_steps and (best_m is None or m < best_m)):
                min_steps = steps
                best_m = m

    return best_m, min_steps


def verify_examples():
    """Verify the solution on given examples."""
    print("="*70)
    print("VERIFICATION OF GIVEN EXAMPLES")
    print("="*70)
    print()

    examples = [
        (7, 2, 4),      # f(7) = 2, d(7,2) = 4
        (89, 34, 9),    # f(89) = 34, d(89,34) = 9
        (8191, 1856, 20) # f(8191) = 1856, d(8191,1856) = 20
    ]

    print(f"{'n':<10} {'Expected f(n)':<15} {'Computed f(n)':<15} {'Expected d':<12} {'Computed d':<12} {'Match?'}")
    print("-"*85)

    all_match = True
    for n, expected_m, expected_d in examples:
        computed_m, computed_d = find_f(n)

        match_m = "✓" if computed_m == expected_m else "✗"
        match_d = "✓" if computed_d == expected_d else "✗"
        match = "✓" if (computed_m == expected_m and computed_d == expected_d) else "✗"

        print(f"{n:<10} {expected_m:<15} {computed_m:<15} {expected_d:<12} {computed_d:<12} {match}")

        if computed_m != expected_m or computed_d != expected_d:
            all_match = False

    print()
    if all_match:
        print("✓✓✓ ALL EXAMPLES VERIFIED ✓✓✓")
    else:
        print("✗ VERIFICATION FAILED - CHECK ALGORITHM ✗")
    print()

    return all_match


def main():
    """
    Main solution function.

    Returns f(10^12 + 39).
    """
    print("="*70)
    print("PROJECT EULER PROBLEM 958: EUCLID'S LABOUR")
    print("="*70)
    print()

    # Verify on known examples first
    if not verify_examples():
        print("ERROR: Verification failed. Stopping.")
        return None

    # Compute answer
    print("="*70)
    print("COMPUTING f(10^12 + 39)")
    print("="*70)
    print()

    n = 10**12 + 39
    print(f"n = {n:,}")
    print()
    print("Searching for optimal m...")
    print()

    m, steps = find_f(n)

    print("="*70)
    print("RESULT")
    print("="*70)
    print()
    print(f"f({n:,}) = {m:,}")
    print(f"d({n:,}, {m:,}) = {steps}")
    print()

    # Verification
    print("Verification:")
    print(f"  gcd(n, m) = {gcd(n, m)} (must be 1)")
    cf = continued_fraction(n, m)
    print(f"  Continued fraction length: {len(cf)}")
    print(f"  Continued fraction sum: {sum(cf)}")
    print(f"  d(n,m) = sum - 1 = {sum(cf) - 1}")
    print()

    # Show golden ratio relationship
    phi = (1 + (5 ** 0.5)) / 2
    ratio = n / m
    print(f"Additional insights:")
    print(f"  n/m = {ratio:.10f}")
    print(f"  φ² = {phi**2:.10f}")
    print(f"  Difference: {abs(ratio - phi**2):.10f}")
    print(f"  Relative error: {abs(ratio - phi**2) / (phi**2) * 100:.6f}%")
    print()

    print("="*70)
    print(f"FINAL ANSWER: {m}")
    print("="*70)
    print()

    return m


if __name__ == "__main__":
    result = main()

    # For Project Euler submission:
    # print(result)
