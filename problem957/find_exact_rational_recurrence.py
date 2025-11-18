#!/usr/bin/env python3
"""
Find EXACT RATIONAL coefficients for the nonlinear recurrence

The recurrence a(n) = A*a(n-1) + B*a(n-2) + C*a(n-1)*a(n-2) + D
fits perfectly with floating point, but we need exact rational coefficients
for precise computation of g(16).
"""

from fractions import Fraction

# Known sequence
a = [4, 7, 46, 411, 4767, 64148, 937211]

print("="*70)
print("FINDING EXACT RATIONAL RECURRENCE COEFFICIENTS")
print("="*70)
print()

# We have: a(n) = A*a(n-1) + B*a(n-2) + C*a(n-1)*a(n-2) + D
# Using a(3), a(4), a(5), a(6) to solve for A, B, C, D

# Set up system:
# a(3) = A*a(2) + B*a(1) + C*a(2)*a(1) + D
# a(4) = A*a(3) + B*a(2) + C*a(3)*a(2) + D
# a(5) = A*a(4) + B*a(3) + C*a(4)*a(3) + D
# a(6) = A*a(5) + B*a(4) + C*a(5)*a(4) + D

# Use Fraction arithmetic for exact solution
from fractions import Fraction

def solve_exact_recurrence():
    """Solve for exact rational coefficients using Gaussian elimination"""

    # Convert to Fractions
    a_frac = [Fraction(x) for x in a]

    # Build augmented matrix
    # Each row: [a(n-1), a(n-2), a(n-1)*a(n-2), 1 | a(n)]

    rows = []
    for i in range(3, 7):  # Use a(3) through a(6)
        row = [
            a_frac[i-1],           # coefficient of A
            a_frac[i-2],           # coefficient of B
            a_frac[i-1] * a_frac[i-2],  # coefficient of C
            Fraction(1),           # coefficient of D
            a_frac[i]              # RHS
        ]
        rows.append(row)

    # Gaussian elimination with partial pivoting
    n = 4  # number of unknowns

    for col in range(n):
        # Find pivot
        max_row = col
        for row in range(col + 1, len(rows)):
            if abs(rows[row][col]) > abs(rows[max_row][col]):
                max_row = row

        # Swap rows
        rows[col], rows[max_row] = rows[max_row], rows[col]

        # Make zeros below pivot
        for row in range(col + 1, len(rows)):
            if rows[col][col] == 0:
                continue
            factor = rows[row][col] / rows[col][col]
            for j in range(n + 1):
                rows[row][j] -= factor * rows[col][j]

    # Back substitution
    solution = [Fraction(0)] * n
    for i in range(n - 1, -1, -1):
        solution[i] = rows[i][n]
        for j in range(i + 1, n):
            solution[i] -= rows[i][j] * solution[j]
        solution[i] /= rows[i][i]

    return solution

print("Solving for exact rational coefficients...")
print()

A, B, C, D = solve_exact_recurrence()

print(f"A = {A} = {float(A):.10f}")
print(f"B = {B} = {float(B):.10f}")
print(f"C = {C} = {float(C):.15f}")
print(f"D = {D} = {float(D):.10f}")
print()

print("Recurrence:")
print(f"a(n) = ({A}) * a(n-1)")
print(f"     + ({B}) * a(n-2)")
print(f"     + ({C}) * a(n-1) * a(n-2)")
print(f"     + ({D})")
print()

# Verify on known values
print("="*70)
print("VERIFICATION")
print("="*70)
print()

a_frac = [Fraction(x) for x in a]

print("Testing on known values:")
for i in range(3, len(a)):
    predicted = A * a_frac[i-1] + B * a_frac[i-2] + C * a_frac[i-1] * a_frac[i-2] + D
    actual = a_frac[i]
    match = "✓" if predicted == actual else "✗"
    print(f"  a({i}): predicted = {predicted}, actual = {actual} {match}")

print()

# Now compute g(16) with EXACT arithmetic
print("="*70)
print("COMPUTING g(16) WITH EXACT RATIONAL ARITHMETIC")
print("="*70)
print()

g_frac = [Fraction(2), Fraction(8), Fraction(28), Fraction(184), Fraction(1644),
          Fraction(19068), Fraction(256388), Fraction(3748844)]
a_extended = list(a_frac[:7])

for n in range(7, 16):
    # Compute a(n) exactly
    a_n = A * a_extended[-1] + B * a_extended[-2] + C * a_extended[-1] * a_extended[-2] + D
    a_extended.append(a_n)

    # Compute g(n+1) = g(n) * a(n) / a(n-1)
    g_n = g_frac[-1] * a_n / a_extended[-2]
    g_frac.append(g_n)

    print(f"  n={n+1}:")
    print(f"    a({n}) = {a_n}")
    print(f"    g({n+1}) = {g_n}")
    print()

print("="*70)
print("FINAL RESULT")
print("="*70)
print()

print(f"g(16) = {g_frac[16]}")
print()

# Convert to integer if it's a whole number
if g_frac[16].denominator == 1:
    g16_int = g_frac[16].numerator
    print(f"g(16) as integer: {g16_int}")
    print(f"Number of digits: {len(str(g16_int))}")
else:
    print(f"g(16) numerator: {g_frac[16].numerator}")
    print(f"g(16) denominator: {g_frac[16].denominator}")
    print(f"g(16) as decimal: {float(g_frac[16]):.15e}")
