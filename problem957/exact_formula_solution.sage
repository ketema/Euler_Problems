#!/usr/bin/env sage

"""
Find EXACT formula for new(m) = am² + bm + c

Known data:
- m=2: new=6
- m=8: new=20
- m=28: new=156
- m=184: new=1460
- m=1644: new=17424

User's insight: correction/m² → 3, which suggests new(m) → 3m²/2 - 3m/2
This means we expect: a ≈ 1.5, b ≈ -1.5, c ≈ 0

Solve exactly using first 3 data points.
"""

from sage.all import *

# Known data
data = [
    (2, 6),
    (8, 20),
    (28, 156),
    (184, 1460),
    (1644, 17424),
]

print("="*70)
print("EXACT FORMULA SOLUTION: new(m) = am² + bm + c")
print("="*70)
print()

# System of equations using first 3 points:
# 4a + 2b + c = 6     (m=2)
# 64a + 8b + c = 20   (m=8)
# 784a + 28b + c = 156 (m=28)

print("Setting up system of equations:")
print("  4a + 2b + c = 6")
print("  64a + 8b + c = 20")
print("  784a + 28b + c = 156")
print()

# Solve using Sage's linear algebra
from sage.all import matrix, vector

# Coefficient matrix
A = matrix(QQ, [
    [4, 2, 1],
    [64, 8, 1],
    [784, 28, 1]
])

# Right-hand side
b_vec = vector(QQ, [6, 20, 156])

# Solve Ax = b
solution = A.solve_right(b_vec)
a, b, c = solution

print("EXACT SOLUTION:")
print(f"  a = {a}")
print(f"  b = {b}")
print(f"  c = {c}")
print()
print(f"Formula: new(m) = {a}·m² + {b}·m + {c}")
print()

# Verify on ALL data points
print("="*70)
print("VERIFICATION ON ALL DATA POINTS")
print("="*70)
print()
print("m     | Predicted           | Actual  | Error")
print("------|---------------------|---------|-------")

all_perfect = True
for m_val, new_actual in data:
    new_predicted = a * m_val**2 + b * m_val + c
    error = abs(float(new_predicted - new_actual))
    status = "✓" if error < 0.0001 else "✗"
    if error >= 0.0001:
        all_perfect = False
    print(f"{m_val:5d} | {new_predicted:19} | {new_actual:7d} | {error:6.4f} {status}")

print()

if all_perfect:
    print("✓✓✓ PERFECT FIT on all 5 data points!")
else:
    print("✗ Formula does NOT fit all data perfectly")
    print("  This suggests new(m) is NOT a simple quadratic")

print()
print("="*70)
print("EXTRAPOLATING TO g(16)")
print("="*70)
print()

# Iterate recurrence: g(n+1) = g(n) + new(g(n))
g_current = 2  # g(0) = 2
g_sequence = [g_current]

print(f"g(0) = {g_current}")

for day in range(1, 17):
    m_start = g_current
    new_points = a * m_start**2 + b * m_start + c
    g_current = g_current + new_points
    g_sequence.append(g_current)

    # Check for known values
    if day <= 5:
        expected = [2, 8, 28, 184, 1644, 19068][day]
        match = "✓" if abs(g_current - expected) < 0.5 else "✗"
        print(f"g({day:2d}) = {int(g_current):20,} (expected {expected:8,}) {match}")
    else:
        print(f"g({day:2d}) = {int(g_current):20,}")

print()
print("="*70)
print(f"ANSWER: g(16) = {int(g_sequence[16]):,}")
print("="*70)
print()

# Sanity checks
print("Sanity checks:")
if g_sequence[16] > 0:
    print(f"  ✓ Result is positive: {int(g_sequence[16]):,}")
else:
    print(f"  ✗ Result is negative: INVALID")

if g_sequence[16] > g_sequence[5]:
    print(f"  ✓ g(16) > g(5): {int(g_sequence[16]):,} > {g_sequence[5]:,}")
else:
    print(f"  ✗ g(16) ≤ g(5): INVALID")

# Growth rate
growth_rate = (g_sequence[16] / g_sequence[5]) ** (1/11)
print(f"  ✓ Average growth rate g(5)→g(16): {float(growth_rate):.3f}× per day")

# Check if growth is monotonic
monotonic = all(g_sequence[i+1] > g_sequence[i] for i in range(16))
if monotonic:
    print(f"  ✓ Sequence is monotonically increasing")
else:
    print(f"  ✗ Sequence is NOT monotonic: INVALID")

print()
print("="*70)
print("FORMULA INTERPRETATION")
print("="*70)
print()

if a == QQ(3)/QQ(2) and b == QQ(-3)/QQ(2) and c == 0:
    print("✓ Formula matches user's asymptotic prediction!")
    print(f"  new(m) = (3/2)m² - (3/2)m")
    print(f"  new(m) = (3/2)m(m - 1)")
    print(f"  new(m) = (3/2) × C(m, 2) × 2")
    print(f"  new(m) = 3 × C(m, 2)")
    print()
    print("This is ELEGANT: new points = 3 times the binomial coefficient C(m,2)")
elif c == 0:
    print("Formula has no constant term (c=0)")
    print(f"  new(m) = {a}m² + {b}m = m({a}m + {b})")
else:
    print(f"Formula: new(m) = {a}m² + {b}m + {c}")

print()
print("="*70)
