#!/usr/bin/env python3
"""
Verify: Does the recurrence produce EXACT integer values for g(0) through g(7)?
Or does it only produce exact values for the RATIOS a(n)?
"""
from fractions import Fraction

# Recurrence coefficients (exact rational)
A = Fraction(96212124256203, 4889951012404)
B = Fraction(-354433686806945, 4889951012404)
C = Fraction(329048751, 4889951012404)
D = Fraction(64942004263499, 4889951012404)

print("="*70)
print("CRITICAL TEST: Does recurrence produce exact integers for known g(n)?")
print("="*70)
print()

# Known values
g_known = [2, 8, 28, 184, 1644, 19068, 256388, 3748844]
a_known = [4, 7, 46, 411, 4767, 64148, 937211]

# Extend using recurrence
a = [Fraction(x) for x in a_known]

print("Extending a(n) using recurrence:")
for n in range(len(a_known), 16):
    a_n = A * a[-1] + B * a[-2] + C * a[-1] * a[-2] + D
    a.append(a_n)
    print(f"  a({n}) = {a_n}")
    if a_n.denominator == 1:
        print(f"         = {a_n.numerator} (INTEGER)")
    else:
        print(f"         = {a_n.numerator}/{a_n.denominator} (FRACTION)")
print()

# Now compute g(n)
g = [Fraction(x) for x in g_known]

print("Computing g(n) from g(n) = g(n-1) * a(n) / a(n-1):")
print()
for n in range(len(g_known), 17):
    if n-1 < len(a):
        g_n = g[-1] * a[n-1] / a[n-2]
        g.append(g_n)
        
        print(f"g({n}):")
        if g_n.denominator == 1:
            print(f"  = {g_n.numerator} (INTEGER ✓)")
        else:
            print(f"  = {g_n.numerator}/{g_n.denominator}")
            print(f"  numerator: {len(str(g_n.numerator))} digits")
            print(f"  denominator: {len(str(g_n.denominator))} digits")
            print(f"  FRACTION (not integer)")
        print()

print("="*70)
print("CRITICAL OBSERVATION")
print("="*70)
print()

# Check if known g values are exact
print("Checking if recurrence matches KNOWN integer values:")
for i, g_val in enumerate(g_known):
    if i < len(g):
        computed = g[i]
        if computed == Fraction(g_val):
            print(f"  g({i}) = {g_val} ✓ EXACT MATCH")
        else:
            print(f"  g({i}): expected {g_val}, got {computed} ✗ MISMATCH")

print()
print("When does it start producing fractions?")
for i in range(len(g)):
    if g[i].denominator != 1:
        print(f"  First fraction at g({i})")
        break
else:
    print("  All values are integers!")

