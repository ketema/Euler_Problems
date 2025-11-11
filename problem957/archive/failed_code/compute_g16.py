#!/usr/bin/env python3
"""
Compute g(16) using discovered patterns
"""

from sympy import Rational, simplify

print("="*70)
print("Computing g(16) using discovered patterns")
print("="*70)

# Pattern 1: Cubic polynomial
print("\n1. Using cubic formula: g(t) = (61/3)t³ - 54t² + (119/3)t + 2")

a = Rational(61, 3)
b = Rational(-54)
c = Rational(119, 3)
d = Rational(2)

def g_cubic(t):
    return a*t**3 + b*t**2 + c*t + d

# Verify known values
print("\nVerification:")
for t in range(4):
    expected = [2, 8, 28, 184][t]
    computed = g_cubic(t)
    print(f"  g({t}) = {computed} (expected: {expected}) {'✓' if computed == expected else '✗'}")

# Compute g(16)
g_16_cubic = g_cubic(16)
print(f"\nUsing cubic formula:")
print(f"  g(16) = {int(g_16_cubic)}")

# Pattern 2: Linear recurrence g(n) = -18·g(n-1) + 86·g(n-2)
print("\n" + "="*70)
print("2. Using recurrence: g(n) = -18·g(n-1) + 86·g(n-2)")
print("="*70)

g_vals = [2, 8]  # g(0), g(1)

for n in range(2, 17):
    g_next = -18 * g_vals[-1] + 86 * g_vals[-2]
    g_vals.append(g_next)
    if n <= 5:
        print(f"  g({n}) = -18·{g_vals[-2]} + 86·{g_vals[-3]} = {g_next}")

print(f"\n  g(16) = {g_vals[16]}")

# Compare methods
print("\n" + "="*70)
print("COMPARISON")
print("="*70)
print(f"Cubic formula:   g(16) = {int(g_16_cubic)}")
print(f"Recurrence:      g(16) = {g_vals[16]}")

if int(g_16_cubic) == g_vals[16]:
    print(f"\n✓✓✓ BOTH METHODS AGREE!")
    print(f"\n" + "="*70)
    print(f"FINAL ANSWER: g(16) = {g_vals[16]}")
    print(f"="*70)
else:
    print(f"\n✗✗✗ METHODS DISAGREE - need more data!")

# Show full sequence
print(f"\nFull sequence g(0) through g(16):")
for i, val in enumerate(g_vals):
    print(f"  g({i:2d}) = {val}")

# Check if this might exceed reasonable bounds
if g_vals[16] > 1000000:
    print(f"\n⚠️  Warning: g(16) = {g_vals[16]} seems very large!")
    print(f"   This suggests explosive growth.")
