#!/usr/bin/env python3

"""
CONSTRAINED GENERAL POSITION ANALYSIS

After Decisive Test (Outcome 2), we know:
- g(n) = maximum COUNT in general position
- Multiple configs achieve this maximum
- This is a COMBINATORIAL BOUND problem

Framework:
- m_n = total blues after n days
- Lines: 3 × m_n (each red connects to each blue)
- Max intersections: C(3m_n, 2)
- Actual new blues: C(3m_n, 2) - (unavoidable degeneracies)

Key constraint: Lines form 3 PENCILS (all through fixed reds)
This creates unavoidable degeneracies beyond pure general position.
"""

from math import comb

# Known sequence
g = [2, 8, 28, 184, 1644, 19068]

print("="*70)
print("CONSTRAINED GENERAL POSITION ANALYSIS")
print("="*70)
print()

print("Analyzing degeneracy pattern:")
print()

for n in range(len(g) - 1):
    m_n = g[n]  # Blues at start of day
    m_n1 = g[n+1]  # Blues at end of day

    new_blues = m_n1 - m_n
    total_lines = 3 * m_n

    # Maximum possible intersections
    max_intersections = comb(total_lines, 2)

    # Actual new blues
    actual_new = new_blues

    # "Wasted" intersections
    wasted = max_intersections - actual_new

    # Efficiency
    efficiency = 100.0 * actual_new / max_intersections if max_intersections > 0 else 0

    print(f"Day {n}→{n+1}:")
    print(f"  m_n = {m_n}, lines = {total_lines}")
    print(f"  Max intersections: C({total_lines}, 2) = {max_intersections}")
    print(f"  Actual new blues: {actual_new}")
    print(f"  Wasted intersections: {wasted} ({100-efficiency:.2f}%)")
    print(f"  Efficiency: {efficiency:.2f}%")
    print()

print("="*70)
print("PATTERN ANALYSIS: What causes the waste?")
print("="*70)
print()

# Analyze the waste pattern
print("1. THREE PENCILS create unavoidable concurrent lines:")
print("   - Pencil at R1: m_n lines")
print("   - Pencil at R2: m_n lines")
print("   - Pencil at R3: m_n lines")
print("   - Lines within same pencil DON'T intersect (concurrent at red)")
print()

print("2. Waste from PENCIL STRUCTURE:")
for n in range(len(g) - 1):
    m_n = g[n]

    # Lines that DON'T intersect (within same pencil)
    within_pencil = 3 * comb(m_n, 2)  # Each pencil has C(m_n, 2) non-intersecting pairs

    # Lines that DO intersect (between different pencils)
    between_pencils = m_n * m_n * 3  # Pencil_i × Pencil_j for i≠j

    total_line_pairs = comb(3*m_n, 2)

    print(f"Day {n}→{n+1}: m_n={m_n}")
    print(f"  Non-intersecting (within pencil): 3×C({m_n},2) = {within_pencil}")
    print(f"  Potentially intersecting (between pencils): {between_pencils}")
    print(f"  Total line pairs: C({3*m_n},2) = {total_line_pairs}")
    print(f"  Check: {within_pencil} + {between_pencils} = {within_pencil + between_pencils} (should be {total_line_pairs})")
    print()

print("="*70)
print("THEORETICAL MAXIMUM (ignoring duplicates on existing points):")
print("="*70)
print()

print("If all between-pencil intersections were NEW blues:")
for n in range(len(g) - 1):
    m_n = g[n]
    theoretical_max = m_n * m_n * 3
    actual_new = g[n+1] - g[n]

    print(f"Day {n}→{n+1}:")
    print(f"  Theoretical max (3×m_n²): 3×{m_n}² = {theoretical_max}")
    print(f"  Actual new blues: {actual_new}")
    print(f"  Difference: {theoretical_max - actual_new} ({100*(theoretical_max-actual_new)/theoretical_max:.1f}% waste)")
    print()

print("="*70)
print("HYPOTHESIS: new(m_n) = 3×m_n² - correction_term(m_n)")
print("="*70)
print()

corrections = []
for n in range(len(g) - 1):
    m_n = g[n]
    actual_new = g[n+1] - g[n]
    theoretical = 3 * m_n * m_n
    correction = theoretical - actual_new

    corrections.append(correction)

    print(f"Day {n}→{n+1}: m_n={m_n}")
    print(f"  correction = 3×{m_n}² - {actual_new} = {correction}")
    print(f"  correction/m_n = {correction/m_n:.2f}")
    print(f"  correction/m_n² = {correction/(m_n**2):.4f}")
    print()

print("Correction term ratios:")
for i, corr in enumerate(corrections):
    m = g[i]
    print(f"  Day {i}: correction/{m} = {corr/m:.2f}, correction/{m}² = {corr/(m**2):.4f}")

print()
print("="*70)
print("TESTING: Is correction ≈ c₁×m_n + c₂×m_n² ?")
print("="*70)
print()

# Try to fit correction = c1*m_n + c2*m_n^2
# Using least squares

m_vals = [g[i] for i in range(len(g)-1)]
corr_vals = corrections

# Solve for c1, c2 in correction = c1*m + c2*m^2
# Using first two equations
m1, m2 = m_vals[0], m_vals[1]
c1_eq1, c2_eq1 = corrections[0], corrections[1]

# corrections[0] = c1*m1 + c2*m1^2
# corrections[1] = c1*m2 + c2*m2^2

# Solve 2×2 system
# c1*2 + c2*4 = 6
# c1*8 + c2*64 = 44

# From first: c1 = (6 - 4*c2)/2 = 3 - 2*c2
# Substitute into second: (3-2*c2)*8 + c2*64 = 44
# 24 - 16*c2 + 64*c2 = 44
# 48*c2 = 20
# c2 = 20/48 = 5/12

c2 = 5/12
c1 = 3 - 2*c2

print(f"From first two data points:")
print(f"  c1 = {c1:.6f}")
print(f"  c2 = {c2:.6f}")
print()
print(f"Formula: correction(m) = {c1:.4f}×m + {c2:.4f}×m²")
print()
print("Testing on all data:")
for i, m in enumerate(m_vals):
    predicted = c1 * m + c2 * m**2
    actual = corrections[i]
    error = abs(predicted - actual)
    pct_error = 100 * error / actual if actual > 0 else 0
    status = "✓" if pct_error < 1 else "✗"
    print(f"  m={m:5d}: predicted={predicted:10.2f}, actual={actual:6d}, error={error:8.2f} ({pct_error:5.2f}%) {status}")

print()
print("="*70)
print("FINAL FORMULA:")
print("="*70)
print()
print(f"new(m) = 3×m² - ({c1:.4f}×m + {c2:.4f}×m²)")
print(f"       = 3×m² - {c1:.4f}×m - {c2:.4f}×m²")
print(f"       = {3-c2:.4f}×m² - {c1:.4f}×m")
print()

a = 3 - c2
b = -c1

print(f"SIMPLIFIED: new(m) = {a:.4f}×m² + {b:.4f}×m")
print()

print("Extrapolating to g(16):")
current_g = 2  # g(0)
print(f"g(0) = {current_g}")

for day in range(1, 17):
    m_n = current_g
    new_points = a * m_n**2 + b * m_n
    current_g = current_g + new_points

    if day <= 5:
        expected = g[day]
        match = "✓" if abs(current_g - expected) < 0.5 else "✗"
        print(f"g({day:2d}) = {int(current_g):15,} (expected {expected:8,}) {match}")
    else:
        print(f"g({day:2d}) = {int(current_g):15,}")

print()
print("="*70)
print(f"ANSWER: g(16) = {int(current_g):,}")
print("="*70)
