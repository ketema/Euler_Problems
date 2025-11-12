#!/usr/bin/env sage

"""
SIX POINT LEAST SQUARES FIT - Using g(6) = 256,388

NEW DATA from exact projective solver:
- g(6) = 256,388 (computed in 10 seconds!)

Growth function data:
- new(2) = 6
- new(8) = 20
- new(28) = 156
- new(184) = 1,460
- new(1,644) = 17,424
- new(19,068) = 237,320  ← NEW!
"""

from sage.all import *

print("="*70)
print("SIX POINT LEAST SQUARES FIT")
print("="*70)
print()

# Growth function data: (m, new_points_added)
growth_data = [
    (2, 6),
    (8, 20),
    (28, 156),
    (184, 1460),
    (1644, 17424),
    (19068, 237320),  # NEW from g(6)
]

print("Growth function data: new(m) = number of points added")
print()
for m, new in growth_data:
    print(f"  new({m:5d}) = {new:7d}")
print()

# Least squares fit: new(m) = am² + bm + c
# Minimize sum of squared errors
print("="*70)
print("LEAST SQUARES FIT: new(m) = am² + bm + c")
print("="*70)
print()

# Build overdetermined system
A = matrix(QQ, [[m**2, m, 1] for m, _ in growth_data])
b_vec = vector(QQ, [new for _, new in growth_data])

# Solve (A^T A)x = A^T b
ATA = A.transpose() * A
ATb = A.transpose() * b_vec
solution = ATA.solve_right(ATb)
a, b, c = solution

print(f"Least squares solution (6 points):")
print(f"  a = {a}")
print(f"  a ≈ {float(a):.10f}")
print(f"  b = {b}")
print(f"  b ≈ {float(b):.10f}")
print(f"  c = {c}")
print(f"  c ≈ {float(c):.10f}")
print()
print(f"Formula: new(m) = ({a})m² + ({b})m + ({c})")
print()

# Check if close to 3/2, -3/2, 0
if abs(float(a) - 1.5) < 0.01:
    print("✓ Coefficient a ≈ 3/2")
else:
    print(f"  a difference from 3/2: {float(a) - 1.5:.6f}")

if abs(float(b) + 1.5) < 0.01:
    print("✓ Coefficient b ≈ -3/2")
else:
    print(f"  b difference from -3/2: {float(b) + 1.5:.6f}")

if abs(float(c)) < 0.01:
    print("✓ Coefficient c ≈ 0")
else:
    print(f"  c ≈ {float(c):.6f}")
print()

# Verify on ALL growth data points
print("="*70)
print("VERIFICATION ON ALL 6 DATA POINTS")
print("="*70)
print()
print("m     | Predicted  | Actual   | Error    | Pct Error")
print("------|------------|----------|----------|----------")

max_pct_error = 0
for m, new_actual in growth_data:
    new_predicted = a * m**2 + b * m + c
    error = abs(float(new_predicted - new_actual))
    pct_error = 100 * error / new_actual if new_actual > 0 else 0
    status = "✓" if pct_error < 5 else "✗"
    max_pct_error = max(max_pct_error, pct_error)
    print(f"{m:5d} | {float(new_predicted):10.2f} | {new_actual:8d} | {error:8.2f} | {pct_error:5.2f}% {status}")

print()
if max_pct_error < 1:
    print(f"✓✓✓ EXCELLENT fit (max error: {max_pct_error:.2f}%)")
elif max_pct_error < 5:
    print(f"✓ GOOD fit (max error: {max_pct_error:.2f}%)")
elif max_pct_error < 10:
    print(f"⚠ ACCEPTABLE fit (max error: {max_pct_error:.2f}%)")
else:
    print(f"✗ POOR fit (max error: {max_pct_error:.2f}%)")
print()

# ITERATE THE RECURRENCE
print("="*70)
print("ITERATING RECURRENCE: m_{n+1} = m_n + new(m_n)")
print("="*70)
print()

m_current = Integer(2)
g_sequence = [m_current]

print(f"g(0) = {m_current}")

# Known values for verification
known_values = [2, 8, 28, 184, 1644, 19068, 256388]

for day in range(1, 17):
    # Compute new points using formula
    new_points = a * m_current**2 + b * m_current + c
    new_points_int = Integer(round(new_points))

    m_current = m_current + new_points_int
    g_sequence.append(m_current)

    # Display with verification for known values
    if day <= 6:
        expected = known_values[day]
        error = abs(m_current - expected)
        pct_error = 100 * float(error) / float(expected) if expected > 0 else 0
        match = "✓" if pct_error < 1 else "✗"
        print(f"g({day:2d}) = {m_current:20,} (expected {expected:8,}, {pct_error:5.2f}%) {match}")
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

if g_sequence[16] > g_sequence[6]:
    print(f"  ✓ g(16) > g(6): {g_sequence[16]:,} > {g_sequence[6]:,}")

# Check if result is reasonable (< 10^15 per PE convention)
if g_sequence[16] < 10**15:
    print(f"  ✓ Result is reasonable PE size (< 10^15)")
elif g_sequence[16] < 10**100:
    print(f"  ⚠ Result is large but not absurd (< 10^100)")
else:
    print(f"  ✗ Result is absurdly large (≥ 10^100): LIKELY INVALID")

print()
print("Growth rate analysis:")
for i in range(min(7, len(g_sequence)-1)):
    rate = float(g_sequence[i+1]) / float(g_sequence[i])
    print(f"  g({i+1})/g({i}) = {rate:.6f}")

print()
print("="*70)
print("RATIO CONVERGENCE ANALYSIS")
print("="*70)
print()

# Check if new(m)/m² converges
print("Checking new(m)/m² ratio:")
for m, new in growth_data:
    ratio = float(new) / float(m**2)
    print(f"  new({m:5d})/m² = {ratio:.6f}")

print()
print("="*70)
