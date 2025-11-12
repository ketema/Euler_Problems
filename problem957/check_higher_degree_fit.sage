#!/usr/bin/env sage

"""
Check if new(m) follows a higher-degree polynomial pattern.

Data:
- m=2: new=6
- m=8: new=20
- m=28: new=156
- m=184: new=1460
- m=1644: new=17424
"""

from sage.all import *

data = [
    (2, 6),
    (8, 20),
    (28, 156),
    (184, 1460),
    (1644, 17424),
]

print("="*70)
print("EXACT LAGRANGE INTERPOLATION (degree 4)")
print("="*70)
print()

# Build Lagrange polynomial through ALL 5 points
R = PolynomialRing(QQ, 'm')
m = R.gen()

m_values = [m_val for m_val, _ in data]
new_values = [new_val for _, new_val in data]

# Lagrange interpolation
poly = sum(
    new_values[i] * prod(
        (m - m_values[j]) / (m_values[i] - m_values[j])
        for j in range(len(m_values)) if j != i
    )
    for i in range(len(m_values))
)

print("Lagrange polynomial (exact fit through all 5 points):")
print(poly)
print()

# Get coefficients
coeffs = poly.coefficients(sparse=False)
print("Coefficients (m^0 to m^4):")
for i, c in enumerate(coeffs):
    print(f"  m^{i}: {c}")
print()

# Verify perfect fit
print("Verification (should be perfect):")
for m_val, new_val in data:
    predicted = poly(m=m_val)
    error = abs(float(predicted - new_val))
    print(f"  m={m_val:5d}: predicted={float(predicted):10.2f}, actual={new_val:6d}, error={error:.6f}")

print()
print("="*70)
print("EXTRAPOLATE TO g(16)")
print("="*70)
print()

g_current = Integer(2)
g_sequence = [g_current]

print(f"g(0) = {g_current}")

for day in range(1, 17):
    m_start = g_current
    new_points = poly(m=m_start)
    new_points_int = Integer(round(new_points))
    g_current = g_current + new_points_int
    g_sequence.append(g_current)

    if day <= 5:
        expected = [2, 8, 28, 184, 1644, 19068][day]
        error = abs(g_current - expected)
        match = "✓" if error == 0 else "✗"
        print(f"g({day:2d}) = {g_current:25d} (expected {expected:8d}) {match}")
    else:
        print(f"g({day:2d}) = {g_current:25d}")

print()
print("="*70)
print(f"ANSWER: g(16) = {g_sequence[16]}")
print("="*70)
print()

# Sanity checks
print("Sanity checks:")
if g_sequence[16] > 0:
    print(f"  ✓ Result is positive")
else:
    print(f"  ✗ Result is negative: INVALID")

if all(g_sequence[i+1] > g_sequence[i] for i in range(16)):
    print(f"  ✓ Sequence is monotonically increasing")
else:
    print(f"  ✗ Sequence NOT monotonic: INVALID")

if g_sequence[16] > g_sequence[5]:
    print(f"  ✓ g(16) > g(5)")

print()
print("Growth rates:")
for i in range(5):
    rate = float(g_sequence[i+1]) / float(g_sequence[i])
    print(f"  g({i+1})/g({i}) = {rate:.6f}")

print()
