#!/usr/bin/env python3
"""
More thorough search for f(10^12 + 39).
Maybe we didn't search broadly enough.
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


def d(n, m):
    """Number of steps = sum of CF coefficients - 1."""
    return sum(continued_fraction(n, m)) - 1


def fibonacci_numbers(count):
    """Generate first count Fibonacci numbers."""
    fibs = [1, 1]
    for _ in range(count - 2):
        fibs.append(fibs[-1] + fibs[-2])
    return fibs


n = 10**12 + 39
print(f"Finding f({n})")
print()

# Get more Fibonacci numbers
fibs = fibonacci_numbers(70)
print(f"Generated {len(fibs)} Fibonacci numbers")
print(f"Largest: F_{len(fibs)-1} = {fibs[-1]}")
print()

# Check which Fibonacci numbers are in range
print("Fibonacci numbers in reasonable range:")
phi = (1 + math.sqrt(5)) / 2
target_phi2 = n / (phi ** 2)
target_phi3 = n / (phi ** 3)
target_phi4 = n / (phi ** 4)

print(f"n/φ² ≈ {target_phi2:.0f}")
print(f"n/φ³ ≈ {target_phi3:.0f}")
print(f"n/φ⁴ ≈ {target_phi4:.0f}")
print()

candidates_with_d = []

# Strategy: Check Fibonacci numbers near these targets
for fib in fibs:
    if fib > 1000 and fib < n and gcd(n, fib) == 1:
        d_val = d(n, fib)
        candidates_with_d.append((fib, d_val))

# Sort by d value
candidates_with_d.sort(key=lambda x: (x[1], x[0]))

print("Best Fibonacci candidates:")
print(f"{'m':<20} {'d(n,m)':<10}")
print("-" * 35)
for m, d_val in candidates_with_d[:10]:
    print(f"{m:<20} {d_val:<10}")

print()

# Check if any of these beat 61
best_d = min(candidates_with_d, key=lambda x: x[1])[1] if candidates_with_d else float('inf')
print(f"Best d from Fibonacci numbers: {best_d}")
print(f"Our current answer gives d = 61")

if best_d < 61:
    print(f"\n⚠ FOUND BETTER: d = {best_d}")
    best_m = min([m for m, d_val in candidates_with_d if d_val == best_d])
    print(f"f({n}) = {best_m}")
else:
    print("\nFibonacci numbers don't beat our answer.")
    print("Searching more broadly...")

    # Expand search around our answer
    our_m = 381965993270
    search_range = 1000000

    print(f"\nSearching {our_m - search_range} to {our_m + search_range}...")

    best_d_found = 61
    best_m_found = our_m

    for offset in range(-search_range, search_range + 1):
        test_m = our_m + offset
        if test_m > 0 and test_m < n and gcd(n, test_m) == 1:
            d_val = d(n, test_m)
            if d_val < best_d_found or (d_val == best_d_found and test_m < best_m_found):
                best_d_found = d_val
                best_m_found = test_m

    print(f"Best in range: m = {best_m_found}, d = {best_d_found}")
