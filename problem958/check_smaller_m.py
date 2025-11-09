#!/usr/bin/env python3
"""
Check which is the correct answer.
"""

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


n = 10**12 + 39

candidates = [381965064920, 381965993270]

print(f"n = {n}")
print()
print(f"{'m':<20} {'gcd(n,m)':<12} {'d(n,m)':<10}")
print("-" * 45)

for m in candidates:
    g = gcd(n, m)
    d_val = d(n, m)
    print(f"{m:<20} {g:<12} {d_val:<10}")

print()
print(f"The SMALLEST m with minimal d is: {min(candidates)}")
print()

# Let me search even MORE broadly to make sure there's no smaller one
print("Searching from 1 to 381965064920 for any with d ≤ 61...")
print("(This might take a while...)")
print()

# Actually, let me use a smarter strategy - check multiples of small numbers
best_m = 381965064920
best_d = 61

# Check around φ-based ratios more carefully
phi = (1 + (5 ** 0.5)) / 2

for k in range(1, 6):
    target = n / (phi ** k)
    for offset in range(-1000000, 1000000):
        test_m = int(target) + offset
        if 1 <= test_m < best_m and gcd(n, test_m) == 1:
            d_val = d(n, test_m)
            if d_val < best_d or (d_val == best_d and test_m < best_m):
                best_d = d_val
                best_m = test_m
                print(f"Found better: m = {best_m}, d = {best_d}")

print()
print(f"FINAL ANSWER: f({n}) = {best_m}")
