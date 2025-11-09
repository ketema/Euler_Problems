#!/usr/bin/env python3
"""
Phase 3: Analyzing 'Euclid's Labour' as a Clue

Following Problem 957's approach where "Point Genesis" was key,
let's decode what "Euclid's Labour" tells us:

EUCLID'S LABOUR:
- Euclid's algorithm: computing GCD
- Labour: work, effort, steps
- Minimizing labour = minimizing steps

KEY INSIGHT FROM PHASE 2:
- f(F_n) = F_{n-2} for Fibonacci numbers
- These give CF = [1,1,1,...,1,2]
- Golden ratio φ has CF = [1,1,1,1,...] (infinite)
- This is the "least laborious" ratio!

HYPOTHESIS:
For arbitrary n, f(n) is the best rational approximation
to n/φ (or φ) that is coprime to n.

The "labour" is minimized when the CF has small coefficients.
"""

import math


def continued_fraction(n, m):
    """Get continued fraction representation of n/m."""
    cf = []
    while m != 0:
        cf.append(n // m)
        n, m = m, n % m
    return cf


def d(n, m):
    """Number of steps = sum of CF coefficients - 1."""
    return sum(continued_fraction(n, m)) - 1


def convergents_of_phi(max_n):
    """
    Generate convergents of golden ratio φ = (1+√5)/2.

    φ has CF = [1; 1, 1, 1, ...]
    Convergents are F_{k+1}/F_k (consecutive Fibonacci ratios)

    Returns: list of (numerator, denominator) pairs
    """
    phi = (1 + math.sqrt(5)) / 2

    # Generate Fibonacci numbers
    fibs = [1, 1]
    while fibs[-1] < max_n:
        fibs.append(fibs[-1] + fibs[-2])

    # Convergents are F_{k+1}/F_k
    convergents = []
    for i in range(1, len(fibs)):
        convergents.append((fibs[i], fibs[i-1]))

    return convergents, phi


def best_approximation_to_phi_coprime(n):
    """
    Find the best rational approximation to φ that:
    1. Denominator divides/approximates n
    2. Is coprime to n

    This should give us f(n).
    """
    phi = (1 + math.sqrt(5)) / 2

    # For Fibonacci n = F_k, we know f(n) = F_{k-2}
    # Which means n/f(n) = F_k/F_{k-2} ≈ φ²

    # So we're looking for m such that n/m ≈ φ² (or n ≈ φ² * m)
    phi_squared = phi ** 2

    target_m = n / phi_squared

    return target_m


def test_phi_hypothesis():
    """Test if f(n) relates to φ approximations."""
    print("="*70)
    print("EUCLID'S LABOUR: THE φ CONNECTION")
    print("="*70)
    print()

    phi = (1 + math.sqrt(5)) / 2
    print(f"Golden ratio φ = {phi:.10f}")
    print(f"φ² = {phi**2:.10f}")
    print()

    # Test Fibonacci numbers
    fibs = [1, 1]
    for _ in range(15):
        fibs.append(fibs[-1] + fibs[-2])

    print("Testing Fibonacci numbers:")
    print(f"{'n':<5} {'F_n':<10} {'f(F_n)':<10} {'F_n/f(F_n)':<15} {'φ²':<15} {'Ratio/φ²':<12}")
    print("-"*75)

    for i in range(4, 13):
        n = fibs[i]
        f_n = fibs[i-2]
        ratio = n / f_n
        ratio_over_phi2 = ratio / (phi**2)

        print(f"{i:<5} {n:<10} {f_n:<10} {ratio:<15.10f} {phi**2:<15.10f} {ratio_over_phi2:<12.6f}")

    print()
    print("OBSERVATION: F_n/f(F_n) = F_n/F_{n-2} → φ² as n → ∞")
    print()

    # Test non-Fibonacci numbers
    print("="*70)
    print("NON-FIBONACCI NUMBERS")
    print("="*70)
    print()

    test_values = [7, 10, 15, 20, 100, 1000]

    print(f"{'n':<10} {'f(n)':<10} {'n/f(n)':<15} {'φ²':<15} {'Error %':<10}")
    print("-"*65)

    for n in test_values:
        # Find f(n) by brute force
        min_steps = float('inf')
        best_m = None

        search_limit = min(n, 5000)
        for m in range(1, search_limit):
            if math.gcd(n, m) == 1:
                steps = d(n, m)
                if steps < min_steps or (steps == min_steps and (best_m is None or m < best_m)):
                    min_steps = steps
                    best_m = m

        if best_m:
            ratio = n / best_m
            error = abs(ratio - phi**2) / (phi**2) * 100

            print(f"{n:<10} {best_m:<10} {ratio:<15.6f} {phi**2:<15.6f} {error:<10.2f}%")

    print()
    print("HYPOTHESIS: f(n) ≈ n/φ² (within coprimality constraints)")
    print()


def convergent_search_strategy():
    """
    Strategy: Use continued fraction convergents to find f(n).

    The convergents of φ are F_{k+1}/F_k.
    For arbitrary n, we want m such that n/m has a CF similar to φ.
    """
    print("="*70)
    print("CONVERGENT SEARCH STRATEGY")
    print("="*70)
    print()

    print("For large n (like 10^12 + 39), brute force is impossible.")
    print("Instead, use the φ connection:")
    print()
    print("1. Compute target: m ≈ n/φ²")
    print("2. Find Fibonacci numbers F_k, F_{k-2} bracketing this ratio")
    print("3. Use convergents of n/m to find best approximation")
    print("4. Ensure gcd(n,m) = 1")
    print()

    # Example with a larger number
    n = 8191  # Given example
    phi = (1 + math.sqrt(5)) / 2
    target_m = n / (phi ** 2)

    print(f"Example: n = 8191")
    print(f"  Target m ≈ n/φ² ≈ {target_m:.2f}")
    print(f"  Actual f(8191) = 1856")
    print(f"  Ratio: 8191/1856 = {8191/1856:.6f}")
    print(f"  φ² = {phi**2:.6f}")
    print(f"  Difference: {abs(8191/1856 - phi**2):.6f}")
    print()

    # But 8191 is a Mersenne prime, so the pattern might differ
    print("Note: 8191 = 2^13 - 1 (Mersenne prime)")
    print("Special structure may affect the pattern.")
    print()


def analyze_8191_special_case():
    """Analyze why f(8191) = 1856."""
    print("="*70)
    print("SPECIAL CASE: f(8191) = 1856")
    print("="*70)
    print()

    n = 8191
    m = 1856

    print(f"n = 8191 = 2^13 - 1 (Mersenne prime)")
    print(f"m = 1856 = 2^6 × 29")
    print()

    # Continued fraction
    cf = continued_fraction(n, m)
    print(f"CF(8191/1856) = {cf}")
    print(f"d(8191, 1856) = {d(n, m)}")
    print()

    # Check nearby values
    print("Why not other values near 1856?")
    print()
    print(f"{'m':<10} {'gcd(n,m)':<12} {'d(n,m)':<10} {'CF sum':<10}")
    print("-"*45)

    for test_m in [1855, 1856, 1857, 1858]:
        g = math.gcd(n, test_m)
        if g == 1:
            steps = d(n, test_m)
            cf_sum = sum(continued_fraction(n, test_m))
            marker = " ✓" if test_m == m else ""
            print(f"{test_m:<10} {g:<12} {steps:<10} {cf_sum:<10}{marker}")
        else:
            print(f"{test_m:<10} {g:<12} (not coprime)")

    print()


def efficient_algorithm_for_large_n():
    """
    Propose an efficient algorithm for f(10^12 + 39).
    """
    print("="*70)
    print("EFFICIENT ALGORITHM FOR f(10^12 + 39)")
    print("="*70)
    print()

    n = 10**12 + 39
    phi = (1 + math.sqrt(5)) / 2

    print(f"n = {n:,}")
    print()

    print("Strategy:")
    print("1. Compute target m ≈ n/φ²")
    print("2. Find Fibonacci numbers bounding this range")
    print("3. Test nearby values for coprimality and minimal d(n,m)")
    print()

    target_m = n / (phi ** 2)
    print(f"Target m ≈ {target_m:,.2f}")
    print()

    # Generate Fibonacci numbers
    fibs = [1, 1]
    while fibs[-1] < target_m * 2:
        fibs.append(fibs[-1] + fibs[-2])

    # Find bracketing Fibonacci numbers
    for i in range(len(fibs)-1):
        if fibs[i] <= target_m < fibs[i+1]:
            print(f"Bracketing Fibonacci numbers:")
            print(f"  F_{i} = {fibs[i]:,}")
            print(f"  F_{i+1} = {fibs[i+1]:,}")
            print()
            break

    # The actual answer requires testing, but we can narrow the search
    print("Next steps:")
    print("1. Test m values near target")
    print("2. Use continued fraction theory to bound search")
    print("3. Check coprimality: gcd(10^12 + 39, m) = 1")
    print()

    # Check divisibility properties of n
    print(f"Properties of n = 10^12 + 39:")
    print(f"  n mod 2 = {n % 2}")
    print(f"  n mod 3 = {n % 3}")
    print(f"  n mod 5 = {n % 5}")
    print(f"  n mod 13 = {n % 13}")
    print()


def main():
    """Phase 3: Decode the 'Euclid's Labour' clue."""
    test_phi_hypothesis()
    convergent_search_strategy()
    analyze_8191_special_case()
    efficient_algorithm_for_large_n()

    print("="*70)
    print("PHASE 3 FINDINGS")
    print("="*70)
    print()
    print("KEY INSIGHTS:")
    print("1. f(F_n) = F_{n-2} means F_n/f(F_n) → φ² (golden ratio squared)")
    print("2. 'Euclid's Labour' = minimizing GCD algorithm steps")
    print("3. For general n, f(n) ≈ n/φ² (adjusted for coprimality)")
    print("4. For 10^12 + 39, compute target ≈ 3.819... × 10^11")
    print("5. Search near Fibonacci numbers bracketing this value")
    print()
    print("NEXT: Phase 4 - Implement efficient search algorithm")
    print()


if __name__ == "__main__":
    main()
