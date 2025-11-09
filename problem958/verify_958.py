#!/usr/bin/env python3
"""
Careful verification of Problem 958 answer.
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


# Verify given examples first
print("Verifying given examples:")
print(f"d(7, 2) = {d(7, 2)} (expected 4)")
print(f"d(89, 34) = {d(89, 34)} (expected 9)")
print(f"d(8191, 1856) = {d(8191, 1856)} (expected 20)")
print()

# Check our computed answer
n = 10**12 + 39
m = 381965993270

print(f"Checking our answer:")
print(f"n = {n}")
print(f"m = {m}")
print(f"gcd(n, m) = {gcd(n, m)} (must be 1)")
print(f"d(n, m) = {d(n, m)}")
print()

# Check if there's a better m
print("Searching for potentially better values...")

# Let me check nearby values more carefully
phi = (1 + (5 ** 0.5)) / 2
target = n / (phi ** 2)
print(f"Target from φ²: {target}")
print()

# Test some specific nearby values
test_values = [
    381965993270,  # Our answer
    381965993269,
    381965993271,
    381965993268,
    381965993272,
]

print(f"{'m':<20} {'gcd':<5} {'d(n,m)':<10}")
print("-" * 40)
for test_m in test_values:
    g = gcd(n, test_m)
    if g == 1:
        steps = d(n, test_m)
        marker = " ← our answer" if test_m == m else ""
        print(f"{test_m:<20} {g:<5} {steps:<10}{marker}")
    else:
        print(f"{test_m:<20} {g:<5} not coprime")

print()

# Let me also manually check the CF
print("Detailed CF check:")
cf = continued_fraction(n, m)
print(f"CF length: {len(cf)}")
print(f"CF first 10 terms: {cf[:10]}")
print(f"CF sum: {sum(cf)}")
print(f"d(n,m) = sum - 1 = {sum(cf) - 1}")
