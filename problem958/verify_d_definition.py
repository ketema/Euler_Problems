#!/usr/bin/env python3
"""
Verify understanding of d(n,m) - subtraction-based Euclidean algorithm.
"""

def d_subtraction(n, m):
    """Count steps using subtraction-based Euclidean algorithm."""
    steps = 0
    while n != m:
        if n > m:
            n = n - m
        else:
            m = m - n
        steps += 1
    return steps


def continued_fraction(n, m):
    """Get continued fraction representation of n/m."""
    cf = []
    while m != 0:
        cf.append(n // m)
        n, m = m, n % m
    return cf


def d_cf(n, m):
    """d(n,m) using continued fraction formula."""
    return sum(continued_fraction(n, m)) - 1


print("Verifying d(n,m) calculation methods:")
print("="*60)
print()

examples = [(7, 2), (89, 34), (8191, 1856)]

print(f"{'n':<10} {'m':<10} {'d_subtraction':<15} {'d_cf':<15} {'Match?'}")
print("-"*65)

for n, m in examples:
    d_sub = d_subtraction(n, m)
    d_c = d_cf(n, m)
    match = "✓" if d_sub == d_c else "✗"
    print(f"{n:<10} {m:<10} {d_sub:<15} {d_c:<15} {match}")

print()
print("If these match, my d(n,m) implementation is correct.")
print()

# Now verify the f(n) values
print("="*60)
print("Verifying f(n) values:")
print("="*60)
print()

print("f(7) should be 2:")
print(f"  d(7, 2) = {d_subtraction(7, 2)}")
for m in [1, 2, 3, 4, 5, 6]:
    if m < 7:
        from math import gcd
        if gcd(7, m) == 1:
            print(f"  d(7, {m}) = {d_subtraction(7, m)}")

print()
print("Smallest m with minimal d is f(7).")
