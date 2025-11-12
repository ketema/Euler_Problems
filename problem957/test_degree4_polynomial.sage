#!/usr/bin/env sage

"""
Test the degree-4 polynomial that fits g(0)-g(5) data.

CRITICAL: After decisive test showed this is a COMBINATORIAL BOUND problem,
polynomial extrapolation might actually work!
"""

from sage.all import *

# Known data: (m_start, total_blues, new_added)
data = [
    (2, 2, 6),        # Day 0→1: m=2 → add 6 → total 8
    (8, 8, 20),       # Day 1→2: m=8 → add 20 → total 28
    (28, 28, 156),    # Day 2→3: m=28 → add 156 → total 184
    (184, 184, 1460), # Day 3→4: m=184 → add 1460 → total 1644
    (1644, 1644, 17424), # Day 4→5: m=1644 → add 17424 → total 19068
]

m_values = [m for m, _, _ in data]
new_values = [new for _, _, new in data]
g_values = [2, 8, 28, 184, 1644, 19068]  # g(0) through g(5)

print("="*70)
print("DEGREE-4 POLYNOMIAL FITTING")
print("="*70)
print()

# Fit polynomial: new(m) = a₄m⁴ + a₃m³ + a₂m² + a₁m + a₀
R = PolynomialRing(QQ, 'm')
m = R.gen()

# Lagrange interpolation through all 5 points
poly = sum(
    new_values[i] * prod(
        (m - m_values[j]) / (m_values[i] - m_values[j])
        for j in range(len(m_values)) if j != i
    )
    for i in range(len(m_values))
)

print(f"Polynomial new(m):")
print(f"  {poly}")
print()

# Coefficients
coeffs = poly.coefficients(sparse=False)
print("Coefficients (low to high degree):")
for i, c in enumerate(coeffs):
    print(f"  m^{i}: {c} = {float(c):.10f}")
print()

# Test on known values
print("Verification on known data:")
print("m_start | Predicted | Actual | Error")
print("--------|-----------|--------|-------")
for m_val, _, new_val in data:
    predicted = poly(m=m_val)
    error = abs(float(predicted - new_val))
    status = "✓" if error < 0.001 else "✗"
    print(f"{m_val:7d} | {float(predicted):9.2f} | {new_val:6d} | {error:6.2f} {status}")

print()
print("="*70)
print("EXTRAPOLATION TO g(16)")
print("="*70)
print()

# Simulate forward using the polynomial
print("Simulating g(n) using polynomial formula:")
print()

current_g = 2  # g(0) = 2
print(f"g(0) = {current_g}")

for day in range(1, 17):
    # Use polynomial to predict new points
    m_start = current_g
    new_points = poly(m=m_start)

    # Update g(n)
    current_g = current_g + new_points

    if day <= 5:
        expected = g_values[day]
        match = "✓" if abs(current_g - expected) < 0.1 else "✗"
        print(f"g({day:2d}) = {int(current_g):15d} (expected {expected:8d}) {match}")
    else:
        print(f"g({day:2d}) = {int(current_g):15d}")

    # Check for negative (indicates formula failure)
    if current_g < 0:
        print()
        print(f"ERROR: g({day}) is NEGATIVE! Formula fails beyond n={day-1}")
        break

print()
print("="*70)
print(f"ANSWER: g(16) = {int(current_g)}")
print("="*70)
print()

# Sanity checks
if current_g > 0:
    print("Sanity checks:")
    print(f"  ✓ Result is positive: {int(current_g)}")
    print(f"  ✓ Growth from g(5): {current_g / 19068:.2f}× larger")
    print(f"  ✓ Average daily growth g(5)→g(16): {(current_g - 19068) / 11:.2e} points/day")
else:
    print("FAILED: Formula produces negative values - not a valid bound")
