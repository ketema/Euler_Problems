#!/usr/bin/env sage

"""
Search for formula relating m (blues at start) to new points generated.

Key insight from decisive test: This is a COMBINATORIAL BOUND, not specific geometry.

With m blues and 3 reds:
- Lines: 3m total (forming 3 pencils)
- Intersection bound: Lines from different pencils can intersect

Pencil structure:
- Pencil 1 (at R1): m lines
- Pencil 2 (at R2): m lines
- Pencil 3 (at R3): m lines

Maximum intersections between pencils:
- P1 × P2: m² intersections
- P1 × P3: m² intersections
- P2 × P3: m² intersections
- Total: 3m² potential new points

But actual new points < 3m² due to:
- Points falling on existing blues/reds
- Multiple lines through same point (multiplicity > 2)
"""

from sage.all import *

# Known sequence
data = [
    (2, 2, 6),    # m₀=2 → +6 → total 8
    (8, 8, 20),   # m₁=8 → +20 → total 28
    (28, 28, 156),  # m₂=28 → +156 → total 184
    (184, 184, 1460),  # m₃=184 → +1460 → total 1644
    (1644, 1644, 17424),  # m₄=1644 → +17424 → total 19068
]

print("="*70)
print("PENCIL INTERSECTION FORMULA SEARCH")
print("="*70)
print()

print("Known sequence:")
print("Day | m_start | new_points | g(n) | 3m² | Efficiency")
print("----|---------|------------|------|-----|------------")
for day, (m_start, g_prev, new_pts) in enumerate(data):
    g_n = g_prev + new_pts
    theoretical_max = 3 * m_start**2
    efficiency = 100.0 * new_pts / theoretical_max if theoretical_max > 0 else 0
    print(f"  {day} | {m_start:7d} | {new_pts:10d} | {g_n:4d} | {theoretical_max:7d} | {efficiency:5.2f}%")

print()
print("="*70)
print("HYPOTHESIS 1: new_points = f(m) for some polynomial f")
print("="*70)
print()

# Extract m and new_points
m_values = [m for m, _, _ in data]
new_values = [new for _, _, new in data]

# Try polynomial fits of various degrees
for degree in [1, 2, 3, 4]:
    # Fit polynomial using Lagrange interpolation
    R = PolynomialRing(QQ, 'm')
    m = R.gen()

    # Build Lagrange basis
    poly = sum(
        new_values[i] * prod(
            (m - m_values[j]) / (m_values[i] - m_values[j])
            for j in range(len(m_values)) if j != i
        )
        for i in range(min(degree+1, len(m_values)))
    )

    print(f"Degree {degree} polynomial through first {min(degree+1, len(m_values))} points:")
    print(f"  new(m) = {poly}")
    print()

    # Test on all known values
    errors = []
    for m_val, _, new_val in data:
        predicted = poly(m=m_val)
        error = abs(float(predicted - new_val))
        errors.append(error)
        status = "✓" if error < 0.001 else "✗"
        print(f"  m={m_val:4d}: predicted={float(predicted):10.2f}, actual={new_val:5d}, error={error:8.2f} {status}")

    print(f"  Max error: {max(errors):.2f}")
    print()

print("="*70)
print("HYPOTHESIS 2: new_points/m² = constant (efficiency)")
print("="*70)
print()

ratios = [new / (m**2) for m, _, new in data]
print("new_points / m²:")
for i, ((m, _, new), ratio) in enumerate(zip(data, ratios)):
    print(f"  Day {i}: {new}/{m}² = {ratio:.6f}")

print()
avg_ratio = sum(ratios) / len(ratios)
print(f"Average ratio: {avg_ratio:.6f}")
print(f"Std deviation: {(sum((r-avg_ratio)**2 for r in ratios)/len(ratios))**0.5:.6f}")
print()

if max(ratios) / min(ratios) < 1.5:
    print("Ratios are relatively stable - possible approximate formula:")
    print(f"  new(m) ≈ {avg_ratio:.4f} * m²")
    print()

print("="*70)
print("HYPOTHESIS 3: g(n) follows recurrence relation")
print("="*70)
print()

# Try g(n) = a*g(n-1) + b
print("Linear: g(n) = a*g(n-1) + b")
g_values = [g for _, g, _ in data]

if len(g_values) >= 3:
    # Use first two points to find a, b
    # g₁ = a*g₀ + b → 8 = a*2 + b
    # g₂ = a*g₁ + b → 28 = a*8 + b

    # Solve: 8 = 2a + b, 28 = 8a + b
    # Subtract: 20 = 6a → a = 10/3
    # Then: b = 8 - 2(10/3) = 8 - 20/3 = 4/3

    a = QQ(10)/QQ(3)
    b = QQ(4)/QQ(3)

    print(f"  a = {a}, b = {b}")
    print(f"  Formula: g(n) = ({a}) * g(n-1) + ({b})")
    print()

    # Test
    g_pred = [g_values[0]]
    for i in range(1, len(g_values)):
        g_next = a * g_pred[-1] + b
        g_pred.append(g_next)

    print("  Test:")
    for i, (actual, predicted) in enumerate(zip(g_values, g_pred)):
        error = abs(float(predicted - actual))
        status = "✓" if error < 0.1 else "✗"
        print(f"    g({i}) = {actual:5d}, predicted = {float(predicted):10.2f}, error = {error:8.2f} {status}")

print()
print("="*70)
print("HYPOTHESIS 4: new(m) = 3m² - correction_term")
print("="*70)
print()

corrections = [3*m**2 - new for m, _, new in data]
print("Correction term (3m² - new):")
for i, ((m, _, new), corr) in enumerate(zip(data, corrections)):
    ratio = corr / m if m > 0 else 0
    print(f"  Day {i}: 3({m})² - {new} = {corr} (= {ratio:.2f}m)")

print()
if all(c > 0 for c in corrections):
    # Check if corrections are linear in m
    corr_over_m = [c/m for (m, _, _), c in zip(data, corrections) if m > 0]
    print("Correction / m ratios:")
    for i, ratio in enumerate(corr_over_m):
        print(f"  Day {i}: {ratio:.4f}")

    if max(corr_over_m) / min(corr_over_m) < 2:
        avg_corr_ratio = sum(corr_over_m) / len(corr_over_m)
        print(f"\nApproximate formula: new(m) ≈ 3m² - {avg_corr_ratio:.2f}m")

print()
print("="*70)
