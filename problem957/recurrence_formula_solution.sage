#!/usr/bin/env sage

"""
RECURRENCE APPROACH - Problem 957

KEY INSIGHT: Fit polynomial to GROWTH FUNCTION, not sequence.

Given data:
- new(2) = 6  (since g(1) = g(0) + new(g(0)) = 2 + 6 = 8)
- new(8) = 20 (since g(2) = g(1) + new(g(1)) = 8 + 20 = 28)
- new(28) = 156
- new(184) = 1460
- new(1644) = 17424

Find: new(m) = am² + bm + c

Then iterate: m_{n+1} = m_n + new(m_n)
"""

from sage.all import *

print("="*70)
print("RECURRENCE FORMULA APPROACH")
print("="*70)
print()

# Growth function data: (m, new_points_added)
growth_data = [
    (2, 6),
    (8, 20),
    (28, 156),
    (184, 1460),
    (1644, 17424),
]

print("Growth function data: new(m) = number of points added when starting with m blues")
print()
for m, new in growth_data:
    print(f"  new({m:4d}) = {new:6d}")
print()

# Use first 3 points to solve for exact quadratic coefficients
print("="*70)
print("SOLVING FOR EXACT QUADRATIC: new(m) = am² + bm + c")
print("="*70)
print()

# System of equations using first 3 points
# new(2) = 4a + 2b + c = 6
# new(8) = 64a + 8b + c = 20
# new(28) = 784a + 28b + c = 156

A = matrix(QQ, [
    [4, 2, 1],
    [64, 8, 1],
    [784, 28, 1]
])

b_vec = vector(QQ, [6, 20, 156])
solution = A.solve_right(b_vec)
a, b, c = solution

print(f"Exact solution from first 3 points:")
print(f"  a = {a} = {float(a):.10f}")
print(f"  b = {b} = {float(b):.10f}")
print(f"  c = {c} = {float(c):.10f}")
print()
print(f"Formula: new(m) = ({a})m² + ({b})m + ({c})")
print()

# Check if this is close to 3/2, -3/2, 0
if abs(float(a) - 1.5) < 0.01:
    print("✓ Coefficient a ≈ 3/2 (user's prediction)")
if abs(float(b) + 1.5) < 0.01:
    print("✓ Coefficient b ≈ -3/2 (user's prediction)")
if abs(float(c)) < 0.01:
    print("✓ Coefficient c ≈ 0 (user's prediction)")
print()

# Verify on ALL growth data points
print("="*70)
print("VERIFICATION ON ALL GROWTH DATA")
print("="*70)
print()
print("m     | Predicted  | Actual  | Error    | Status")
print("------|------------|---------|----------|-------")

max_error = 0
for m, new_actual in growth_data:
    new_predicted = a * m**2 + b * m + c
    error = abs(float(new_predicted - new_actual))
    pct_error = 100 * error / new_actual if new_actual > 0 else 0
    status = "✓" if pct_error < 1 else "✗"
    max_error = max(max_error, pct_error)
    print(f"{m:5d} | {float(new_predicted):10.2f} | {new_actual:7d} | {error:8.2f} | {status} ({pct_error:5.2f}%)")

print()
if max_error < 1:
    print(f"✓ Excellent fit (max error: {max_error:.2f}%)")
elif max_error < 10:
    print(f"⚠ Acceptable fit (max error: {max_error:.2f}%)")
else:
    print(f"✗ Poor fit (max error: {max_error:.2f}%) - formula may be incorrect")
print()

# ITERATE THE RECURRENCE
print("="*70)
print("ITERATING RECURRENCE: m_{n+1} = m_n + new(m_n)")
print("="*70)
print()

# Start with m_0 = 2 (g(0) = 2)
m_current = Integer(2)
g_sequence = [m_current]

print(f"g(0) = {m_current}")

# Known values for verification
known_values = [2, 8, 28, 184, 1644, 19068]

for day in range(1, 17):
    # Compute new points using formula
    new_points = a * m_current**2 + b * m_current + c

    # Round to nearest integer (formula may not be exact)
    new_points_int = Integer(round(new_points))

    # Update
    m_current = m_current + new_points_int
    g_sequence.append(m_current)

    # Display with verification for known values
    if day <= 5:
        expected = known_values[day]
        error = abs(m_current - expected)
        match = "✓" if error == 0 else "✗"
        print(f"g({day:2d}) = {m_current:20,} (expected {expected:8,}) {match}")
    else:
        print(f"g({day:2d}) = {m_current:20,}")

print()
print("="*70)
print(f"ANSWER: g(16) = {g_sequence[16]:,}")
print("="*70)
print()

# Sanity checks
print("Sanity checks:")
if g_sequence[16] > 0:
    print(f"  ✓ Result is positive: {g_sequence[16]:,}")
else:
    print(f"  ✗ Result is negative: INVALID")

if all(g_sequence[i+1] > g_sequence[i] for i in range(16)):
    print(f"  ✓ Sequence is monotonically increasing")
else:
    print(f"  ✗ Sequence NOT monotonic: INVALID")

if g_sequence[16] > g_sequence[5]:
    print(f"  ✓ g(16) > g(5): {g_sequence[16]:,} > {g_sequence[5]:,}")

# Check if result is reasonable (< 10^15 per PE convention)
if g_sequence[16] < 10**15:
    print(f"  ✓ Result is reasonable PE size (< 10^15)")
else:
    print(f"  ✗ Result is too large: LIKELY INVALID")

print()
print("Growth rate analysis:")
for i in range(min(6, len(g_sequence)-1)):
    rate = float(g_sequence[i+1]) / float(g_sequence[i])
    print(f"  g({i+1})/g({i}) = {rate:.6f}")

print()
print("="*70)
