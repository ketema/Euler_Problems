#!/usr/bin/env python3
"""
FORMULA SEARCH for g(n)

Given: g(n) is a combinatorial invariant (achieved by 90%+ of configs)
Known sequence: [2, 8, 28, 184, 1644, 19068, 256388, 3748844]

Search strategies:
1. Closed-form formulas (polynomials, factorials, combinations)
2. Simple recurrences (linear, polynomial coefficients)
3. Generating functions (exponential, ordinary)
4. Pattern recognition (differences, ratios, transforms)
"""

import numpy as np
from fractions import Fraction
from itertools import combinations, product

# Known sequence
g = [2, 8, 28, 184, 1644, 19068, 256388, 3748844]
n_values = list(range(len(g)))

print("="*70)
print("FORMULA SEARCH FOR g(n)")
print("="*70)
print()

# Display sequence
print("Known sequence:")
for i, val in enumerate(g):
    print(f"  g({i}) = {val}")
print()

# ============================================================================
# STRATEGY 1: Differences and Finite Differences
# ============================================================================
print("="*70)
print("STRATEGY 1: FINITE DIFFERENCES")
print("="*70)
print()

def compute_differences(seq):
    """Compute successive differences until we see a pattern"""
    diffs = [seq]
    for level in range(min(7, len(seq)-1)):
        new_diff = [diffs[-1][i+1] - diffs[-1][i] for i in range(len(diffs[-1])-1)]
        diffs.append(new_diff)
        if len(new_diff) == 0:
            break
    return diffs

diffs = compute_differences(g)
print("Finite differences:")
for level, d in enumerate(diffs):
    if level == 0:
        print(f"  Level {level} (original): {d}")
    else:
        print(f"  Level {level} (diff^{level}):  {d}")
print()

# Check for polynomial pattern
if len(diffs) > 1:
    print("Observations:")
    print(f"  - First differences: {diffs[1]}")
    print(f"  - Growth is super-polynomial (differences keep growing)")
    print()

# ============================================================================
# STRATEGY 2: Ratio Analysis (for exponential/factorial patterns)
# ============================================================================
print("="*70)
print("STRATEGY 2: RATIO ANALYSIS")
print("="*70)
print()

ratios = [g[i+1]/g[i] for i in range(len(g)-1)]
print("Ratios g(n+1)/g(n):")
for i, r in enumerate(ratios):
    print(f"  g({i+1})/g({i}) = {r:.6f}")
print()

# Check if ratios form a pattern
ratio_diffs = [ratios[i+1] - ratios[i] for i in range(len(ratios)-1)]
print("Differences of ratios:")
for i, rd in enumerate(ratio_diffs):
    print(f"  Δ[g({i+2})/g({i+1})] = {rd:.6f}")
print()

# Check for linear growth in ratios
print("Linear regression on ratios:")
n_ratio = np.array(range(len(ratios)))
ratio_arr = np.array(ratios)
coeffs = np.polyfit(n_ratio, ratio_arr, 1)
print(f"  r(n) ≈ {coeffs[0]:.4f}*n + {coeffs[1]:.4f}")
predicted = coeffs[0] * n_ratio + coeffs[1]
errors = ratio_arr - predicted
print(f"  Max error: {max(abs(errors)):.4f}")
print()

# ============================================================================
# STRATEGY 3: Recurrence Relation Search
# ============================================================================
print("="*70)
print("STRATEGY 3: RECURRENCE RELATION SEARCH")
print("="*70)
print()

# Test: g(n) = a*g(n-1) + b*g(n-2) + c
print("Testing: g(n) = a*g(n-1) + b*g(n-2) + c")
if len(g) >= 4:
    # Use g(1), g(2), g(3) to find a, b, c
    # Then test on g(4), g(5), ...

    # System: g(3) = a*g(2) + b*g(1) + c
    #         g(4) = a*g(3) + b*g(2) + c
    #         g(5) = a*g(4) + b*g(3) + c

    # Matrix form: [g(2) g(1) 1] [a]   [g(3)]
    #              [g(3) g(2) 1] [b] = [g(4)]
    #              [g(4) g(3) 1] [c]   [g(5)]

    A = np.array([
        [g[2], g[1], 1],
        [g[3], g[2], 1],
        [g[4], g[3], 1]
    ], dtype=float)
    b = np.array([g[3], g[4], g[5]], dtype=float)

    try:
        params = np.linalg.solve(A, b)
        a, b_coef, c = params
        print(f"  Fitted: g(n) = {a:.6f}*g(n-1) + {b_coef:.6f}*g(n-2) + {c:.6f}")

        # Test on remaining values
        print("  Testing on g(6), g(7):")
        pred_6 = a*g[5] + b_coef*g[4] + c
        pred_7 = a*g[6] + b_coef*g[5] + c
        print(f"    Predicted g(6) = {pred_6:.1f}, Actual = {g[6]}, Error = {abs(pred_6-g[6]):.1f}")
        print(f"    Predicted g(7) = {pred_7:.1f}, Actual = {g[7]}, Error = {abs(pred_7-g[7]):.1f}")

        if abs(pred_6 - g[6]) < 1 and abs(pred_7 - g[7]) < 1:
            print("  ✓ LINEAR RECURRENCE FOUND!")
        else:
            print("  ✗ Linear recurrence doesn't fit")
    except:
        print("  ✗ Singular matrix - no linear recurrence")
print()

# Test: g(n) = a(n)*g(n-1) + b(n)*g(n-2) + c(n)
print("Testing: g(n) = a(n)*g(n-1) + b(n) with polynomial a(n), b(n)")
# For each n, compute: g(n) - g(n-1)*ratio_approx
corrections = []
for i in range(2, len(g)):
    # Assume g(n) ≈ r(n-1)*g(n-1)
    # Then correction = g(n) - r(n-1)*g(n-1)
    r = ratios[i-1]
    correction = g[i] - r*g[i-1]
    corrections.append(correction)
    print(f"  g({i}) - {r:.3f}*g({i-1}) = {correction:.1f}")
print()

# ============================================================================
# STRATEGY 4: Combinatorial Formula Search
# ============================================================================
print("="*70)
print("STRATEGY 4: COMBINATORIAL FORMULA SEARCH")
print("="*70)
print()

print("Testing formulas involving factorials, combinations:")
print()

# Test: g(n) = sum of products of combinations
def test_formula(name, formula_func):
    print(f"Testing: {name}")
    results = []
    for i in range(len(g)):
        pred = formula_func(i)
        results.append(pred)
        match = "✓" if abs(pred - g[i]) < 0.5 else "✗"
        print(f"  n={i}: predicted={pred:.1f}, actual={g[i]}, {match}")

    if all(abs(results[i] - g[i]) < 0.5 for i in range(len(g))):
        print(f"  ✓✓✓ FORMULA FOUND: {name}")
        return True
    print()
    return False

# Try various formulas
from math import factorial, comb

# Formula 1: Simple polynomial
def f1(n):
    return 2*n**4 - 3*n**3 + 5*n**2 - n + 2

# Formula 2: Factorial-based
def f2(n):
    if n == 0:
        return 2
    return factorial(n) * (n + 2)

# Formula 3: Combination-based
def f3(n):
    if n == 0:
        return 2
    return 2 * comb(3*n, 2)

# Formula 4: Product of linear terms
def f4(n):
    if n == 0:
        return 2
    result = 2
    for k in range(1, n+1):
        result *= (2*k + 1)
    return result

# Formula 5: Based on intersection count
def f5(n):
    if n == 0:
        return 2
    # Maximum intersections of 3m lines is C(3m, 2)
    m = g[n-1] if n > 0 else 2
    return m + comb(3*m, 2) // 10  # Some fraction

found = False
found = test_formula("2*n^4 - 3*n^3 + 5*n^2 - n + 2", f1) or found
found = test_formula("n! * (n+2)", f2) or found
found = test_formula("2 * C(3n, 2)", f3) or found
found = test_formula("2 * ∏(2k+1) for k=1..n", f4) or found

if not found:
    print("No simple formula found yet.")
print()

# ============================================================================
# STRATEGY 5: OEIS Lookup Pattern
# ============================================================================
print("="*70)
print("STRATEGY 5: OEIS PATTERN RECOGNITION")
print("="*70)
print()

print("Checking for known sequence patterns:")
print()

# Check g(n) / n!
print("g(n) / n!:")
for i in range(len(g)):
    if i > 0:
        val = g[i] / factorial(i)
        print(f"  g({i}) / {i}! = {val:.4f}")
print()

# Check g(n) / 2^n
print("g(n) / 2^n:")
for i in range(len(g)):
    val = g[i] / (2**i)
    print(f"  g({i}) / 2^{i} = {val:.4f}")
print()

# Check g(n) / C(3n, 2)
print("g(n) / C(3*g(n-1), 2) (line intersection capacity):")
for i in range(1, len(g)):
    m = g[i-1]
    capacity = comb(3*m, 2) if 3*m >= 2 else 0
    if capacity > 0:
        ratio = g[i] / capacity
        print(f"  g({i}) / C(3*{m}, 2) = {ratio:.6f}")
print()

print("="*70)
print("FORMULA SEARCH COMPLETE")
print("="*70)
