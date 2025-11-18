#!/usr/bin/env python3
"""
SYSTEMATIC RECURRENCE SEARCH for a(n)

Given: a(n) = ratio numerators [4, 7, 46, 411, 4767, 64148, 937211]
       g(n) = g(n-1) * a(n) / a(n-1)
       g = [2, 8, 28, 184, 1644, 19068, 256388, 3748844]

Search strategies:
1. Multiplicative: a(n) = a(n-1) * f(n)
2. Linear recurrence with polynomial coefficients
3. Nonlinear recurrence: a(n) = P(a(n-1), a(n-2), n)
4. Factorial/combinatorial forms
"""

import numpy as np
from math import factorial, comb
from fractions import Fraction

# Known sequences
g = [2, 8, 28, 184, 1644, 19068, 256388, 3748844]
a = [4, 7, 46, 411, 4767, 64148, 937211]  # Ratio numerators

print("="*70)
print("SYSTEMATIC RECURRENCE SEARCH FOR a(n)")
print("="*70)
print()

print("Known sequence a(n):")
for i, val in enumerate(a):
    print(f"  a({i}) = {val}")
print()

# ============================================================================
# STRATEGY 1: Multiplicative Recurrence a(n) = a(n-1) * f(n)
# ============================================================================
print("="*70)
print("STRATEGY 1: MULTIPLICATIVE RECURRENCE")
print("="*70)
print()

print("Testing: a(n) = a(n-1) * f(n)")
print()

# Compute multiplicative factors
mult_factors = [a[i] / a[i-1] for i in range(1, len(a))]
print("Multiplicative factors f(n) = a(n) / a(n-1):")
for i, f in enumerate(mult_factors, start=1):
    print(f"  f({i}) = a({i})/a({i-1}) = {f:.6f}")
print()

# Check if factors are polynomial in n
print("Testing if f(n) is polynomial in n:")
n_vals = np.array(range(1, len(mult_factors)+1))
f_vals = np.array(mult_factors)

for degree in [1, 2, 3]:
    coeffs = np.polyfit(n_vals, f_vals, degree)
    predicted = np.polyval(coeffs, n_vals)
    max_error = max(abs(predicted - f_vals))

    if degree == 1:
        print(f"  Degree {degree}: f(n) ≈ {coeffs[0]:.6f}*n + {coeffs[1]:.6f}")
    elif degree == 2:
        print(f"  Degree {degree}: f(n) ≈ {coeffs[0]:.6f}*n^2 + {coeffs[1]:.6f}*n + {coeffs[2]:.6f}")
    else:
        print(f"  Degree {degree}: coeffs = {coeffs}")
    print(f"    Max error: {max_error:.6f}")

    if max_error < 0.01:
        print(f"  ✓ FOUND POLYNOMIAL f(n)!")
        break
print()

# ============================================================================
# STRATEGY 2: Linear Recurrence with Constant Coefficients
# ============================================================================
print("="*70)
print("STRATEGY 2: LINEAR RECURRENCE (CONSTANT COEFFICIENTS)")
print("="*70)
print()

print("Testing: a(n) = c1*a(n-1) + c2*a(n-2) + c3")
if len(a) >= 4:
    # Use a(1), a(2), a(3) to solve for c1, c2, c3
    # a(3) = c1*a(2) + c2*a(1) + c3
    # a(4) = c1*a(3) + c2*a(2) + c3
    # a(5) = c1*a(4) + c2*a(3) + c3

    A_matrix = np.array([
        [a[2], a[1], 1],
        [a[3], a[2], 1],
        [a[4], a[3], 1]
    ], dtype=float)
    b_vec = np.array([a[3], a[4], a[5]], dtype=float)

    try:
        params = np.linalg.solve(A_matrix, b_vec)
        c1, c2, c3 = params
        print(f"  Fitted: a(n) = {c1:.6f}*a(n-1) + {c2:.6f}*a(n-2) + {c3:.6f}")

        # Test on remaining value
        pred_6 = c1*a[5] + c2*a[4] + c3
        print(f"  Testing: predicted a(6) = {pred_6:.1f}, actual = {a[6]}")
        error = abs(pred_6 - a[6])
        print(f"  Error: {error:.1f} ({100*error/a[6]:.2f}%)")

        if error < 1:
            print("  ✓ LINEAR RECURRENCE FOUND!")
        else:
            print("  ✗ Linear recurrence doesn't fit")
    except:
        print("  ✗ Singular matrix - no linear recurrence with constant coefficients")
print()

# ============================================================================
# STRATEGY 3: Linear Recurrence with Linear Coefficients
# ============================================================================
print("="*70)
print("STRATEGY 3: LINEAR RECURRENCE WITH n-DEPENDENT COEFFICIENTS")
print("="*70)
print()

print("Testing: a(n) = (A*n + B)*a(n-1) + C")
print()

# For each n, we have: a(n) = (A*n + B)*a(n-1) + C
# Let's compute: (a(n) - C) / a(n-1) = A*n + B
# We need to find A, B, C such that this holds

# Try various values of C
best_C = None
best_error = float('inf')
best_A = None
best_B = None

for C in range(-100, 100):
    ratios_adjusted = [(a[i] - C) / a[i-1] for i in range(1, len(a))]
    n_vals_adj = np.array(range(1, len(a)))
    ratios_adj_arr = np.array(ratios_adjusted)

    # Fit linear: ratio = A*n + B
    coeffs = np.polyfit(n_vals_adj, ratios_adj_arr, 1)
    A_fit, B_fit = coeffs

    # Compute error
    predicted_ratios = A_fit * n_vals_adj + B_fit
    errors = ratios_adj_arr - predicted_ratios
    max_error = max(abs(errors))

    if max_error < best_error:
        best_error = max_error
        best_C = C
        best_A = A_fit
        best_B = B_fit

print(f"Best fit: a(n) = ({best_A:.6f}*n + {best_B:.6f})*a(n-1) + {best_C}")
print(f"  Max error in ratio: {best_error:.6f}")

# Validate by computing a(1) through a(6)
print()
print("Validation:")
a_computed = [4]  # a(0) = 4
for n in range(1, len(a)):
    a_next = (best_A * n + best_B) * a_computed[-1] + best_C
    a_computed.append(a_next)
    error = abs(a_next - a[n])
    match = "✓" if error < 0.5 else "✗"
    print(f"  a({n}): predicted={a_next:.1f}, actual={a[n]}, error={error:.1f} {match}")

if all(abs(a_computed[i] - a[i]) < 0.5 for i in range(len(a))):
    print()
    print("  ✓✓✓ EXACT RECURRENCE FOUND!")
    print(f"  a(n) = ({best_A:.6f}*n + {best_B:.6f})*a(n-1) + {best_C}")
    print()

    # Extend to compute a(7) through a(16)
    print("="*70)
    print("EXTENDING TO COMPUTE g(8) THROUGH g(16)")
    print("="*70)
    print()

    g_extended = list(g)
    a_extended = list(a_computed)

    for n in range(len(a), 16):
        # Compute next a(n)
        a_next = (best_A * n + best_B) * a_extended[-1] + best_C
        a_extended.append(a_next)

        # Compute g(n) = g(n-1) * a(n) / a(n-1)
        g_next = g_extended[-1] * a_next / a_extended[-2]
        g_extended.append(g_next)

        print(f"  n={n+1}: a({n}) = {a_next:.1f}, g({n+1}) = {g_next:.1f}")

    print()
    print("="*70)
    print(f"SOLUTION: g(16) = {g_extended[16]:.0f}")
    print("="*70)
print()

# ============================================================================
# STRATEGY 4: Quadratic in Previous Terms
# ============================================================================
print("="*70)
print("STRATEGY 4: NONLINEAR RECURRENCE")
print("="*70)
print()

print("Testing: a(n) = A*a(n-1) + B*a(n-2) + C*a(n-1)*a(n-2) + D")
print()

if len(a) >= 5:
    # Use a(2) through a(5) to solve for A, B, C, D
    # a(3) = A*a(2) + B*a(1) + C*a(2)*a(1) + D
    # a(4) = A*a(3) + B*a(2) + C*a(3)*a(2) + D
    # a(5) = A*a(4) + B*a(3) + C*a(4)*a(3) + D
    # a(6) = A*a(5) + B*a(4) + C*a(5)*a(4) + D

    A_nl = np.array([
        [a[2], a[1], a[2]*a[1], 1],
        [a[3], a[2], a[3]*a[2], 1],
        [a[4], a[3], a[4]*a[3], 1],
        [a[5], a[4], a[5]*a[4], 1]
    ], dtype=float)
    b_nl = np.array([a[3], a[4], a[5], a[6]], dtype=float)

    try:
        params_nl = np.linalg.solve(A_nl, b_nl)
        A_coef, B_coef, C_coef, D_coef = params_nl
        print(f"  Fitted: a(n) = {A_coef:.6f}*a(n-1) + {B_coef:.6f}*a(n-2)")
        print(f"                 + {C_coef:.9f}*a(n-1)*a(n-2) + {D_coef:.6f}")

        # Validate
        print()
        print("Validation:")
        for i in range(3, len(a)):
            pred = A_coef*a[i-1] + B_coef*a[i-2] + C_coef*a[i-1]*a[i-2] + D_coef
            error = abs(pred - a[i])
            match = "✓" if error < 0.5 else "✗"
            print(f"  a({i}): predicted={pred:.1f}, actual={a[i]}, error={error:.1f} {match}")

        if all(abs(A_coef*a[i-1] + B_coef*a[i-2] + C_coef*a[i-1]*a[i-2] + D_coef - a[i]) < 0.5
               for i in range(3, len(a))):
            print()
            print("  ✓✓✓ NONLINEAR RECURRENCE FOUND!")
    except:
        print("  ✗ Singular matrix or no solution")
print()

print("="*70)
print("RECURRENCE SEARCH COMPLETE")
print("="*70)
