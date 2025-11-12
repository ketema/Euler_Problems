#!/usr/bin/env python3
"""
SYMBOLIC BREAKTHROUGH SEARCH
AI Panel unanimous guidance: Use SymPy symbolic tools to find closed-form formula

Priority approaches:
1. Line coefficient pattern analysis
2. Symbolic recurrence solver (rsolve)
3. Groebner basis for polynomial invariants
4. Projective geometry transformation
"""

from sympy import *
from sympy.geometry import Point, Line
from typing import Set, List, Dict
import json

def create_point(x, y) -> Point:
    """Create SymPy Point with exact Rational coordinates"""
    return Point(Rational(x), Rational(y))

# Configuration
reds = [
    create_point(0, 0),
    create_point(4, 0),
    create_point(2, 3)
]

initial_blues = {
    create_point(1, 1),
    create_point(3, 2)
}

print("="*80)
print("SYMBOLIC BREAKTHROUGH SEARCH")
print("="*80)
print()

# Known sequence
sequence = [2, 8, 28, 184, 1644]
print(f"Known sequence: {sequence}")
print()

#============================================================================
# APPROACH 1: SYMBOLIC RECURRENCE SOLVER (SymPy.rsolve)
#============================================================================
print("="*80)
print("APPROACH 1: SYMBOLIC RECURRENCE SOLVER (SymPy.rsolve)")
print("="*80)
print()

n = Symbol('n', integer=True, positive=True)
a = Function('a')

# Test various recurrence patterns
recurrence_patterns = [
    # Linear recurrences
    ("Linear 1st order", a(n) - symbols('c1')*a(n-1) - symbols('c0')),
    ("Linear 2nd order", a(n) - symbols('c2')*a(n-1) - symbols('c1')*a(n-2) - symbols('c0')),
    ("Linear 3rd order", a(n) - symbols('c3')*a(n-1) - symbols('c2')*a(n-2) - symbols('c1')*a(n-3) - symbols('c0')),

    # Quadratic in previous term
    ("Quadratic", a(n) - symbols('c2')*a(n-1)**2 - symbols('c1')*a(n-1) - symbols('c0')),

    # Mixed
    ("Mixed n*a(n-1)", a(n) - n*symbols('c1')*a(n-1) - symbols('c0')),
]

print("Testing recurrence patterns against known values...")
print()

for pattern_name, recurrence in recurrence_patterns:
    print(f"Testing: {pattern_name}")
    print(f"  Recurrence: {recurrence} = 0")

    # Try to fit coefficients to known data
    # This is a heuristic approach - find coefficients that satisfy the recurrence
    try:
        # For demonstration, we'll just note the pattern
        print(f"  (Symbolic fitting would happen here with solve())")
    except Exception as e:
        print(f"  Error: {e}")
    print()

#============================================================================
# APPROACH 2: LINE COEFFICIENT PATTERN ANALYSIS
#============================================================================
print("="*80)
print("APPROACH 2: LINE COEFFICIENT ANALYSIS")
print("="*80)
print()

def analyze_line_coefficients(reds: List[Point], blues: Set[Point]):
    """Extract and analyze line equation coefficients"""
    lines = []
    for red in reds:
        for blue in blues:
            if red != blue:
                line = Line(red, blue)
                lines.append(line)

    # Extract coefficients
    coeffs_data = []
    for line in lines:
        a_coef, b_coef, c_coef = line.coefficients
        coeffs_data.append((a_coef, b_coef, c_coef))

    return lines, coeffs_data

blues = initial_blues.copy()
for day in range(3):  # Analyze days 0-2
    print(f"Day {day}:")
    print(f"  Blues: {len(blues)}")

    lines, coeffs = analyze_line_coefficients(reds, blues)
    print(f"  Lines: {len(lines)}")
    print(f"  Sample line equations:")

    for i, (a_c, b_c, c_c) in enumerate(coeffs[:5]):
        print(f"    Line {i}: {a_c}*x + {b_c}*y + {c_c} = 0")

    # Look for patterns in coefficients
    a_coeffs = [c[0] for c in coeffs]
    b_coeffs = [c[1] for c in coeffs]
    c_coeffs = [c[2] for c in coeffs]

    print(f"  GCD of all a coefficients: {gcd(a_coeffs)}")
    print(f"  GCD of all b coefficients: {gcd(b_coeffs)}")
    print(f"  GCD of all c coefficients: {gcd(c_coeffs)}")

    # Check for algebraic relationships
    x, y = symbols('x y')

    # Create polynomial from first few lines
    if len(coeffs) >= 2:
        poly1 = a_coeffs[0]*x + b_coeffs[0]*y + c_coeffs[0]
        poly2 = a_coeffs[1]*x + b_coeffs[1]*y + c_coeffs[1]

        # Resultant - eliminates one variable
        try:
            res_x = resultant(poly1, poly2, x)
            res_y = resultant(poly1, poly2, y)
            print(f"  Resultant (eliminate x): {res_x}")
            print(f"  Resultant (eliminate y): {res_y}")
        except:
            pass

    print()

    # Compute next day for iteration
    if day < 2:
        all_points = set(reds).union(blues)
        new_blues = set()
        for i, line1 in enumerate(lines):
            for line2 in lines[i+1:]:
                result = line1.intersection(line2)
                if result:
                    if hasattr(result[0], 'x'):
                        p = result[0]
                        if p not in all_points:
                            new_blues.add(p)
        blues = blues.union(new_blues)

#============================================================================
# APPROACH 3: GROEBNER BASIS FOR INVARIANTS
#============================================================================
print("="*80)
print("APPROACH 3: GROEBNER BASIS ANALYSIS")
print("="*80)
print()

print("Extracting coordinates of all blues at Day 1...")
blues_day1 = initial_blues.copy()
# Compute day 1
all_points = set(reds).union(blues_day1)
lines = []
for red in reds:
    for blue in blues_day1:
        if red != blue:
            lines.append(Line(red, blue))

new_blues = set()
for i, line1 in enumerate(lines):
    for line2 in lines[i+1:]:
        result = line1.intersection(line2)
        if result:
            if hasattr(result[0], 'x'):
                p = result[0]
                if p not in all_points:
                    new_blues.add(p)

blues_day1 = blues_day1.union(new_blues)

# Extract x,y coordinates
coords = [(p.x, p.y) for p in blues_day1]
print(f"Day 1 blues: {len(coords)}")
print(f"Sample coordinates:")
for i, (px, py) in enumerate(coords[:5]):
    print(f"  Point {i}: ({px}, {py})")

# Try to find polynomial vanishing on all points
x, y = symbols('x y')
print("\nSearching for polynomial curve through points...")
print("(Testing low-degree polynomials)")

# Test if points satisfy specific curves
test_polys = [
    ("x*y", x*y),
    ("x^2 + y^2", x**2 + y**2),
    ("x^2 - y^2", x**2 - y**2),
    ("x^2 + x*y + y^2", x**2 + x*y + y**2),
    ("x^3 + y^3", x**3 + y**3),
]

for poly_name, poly_expr in test_polys:
    values = [poly_expr.subs([(x, px), (y, py)]) for px, py in coords[:8]]
    unique_vals = set(values)
    if len(unique_vals) <= max(2, len(coords) // 4):
        print(f"  ⚠️  {poly_name}: {len(unique_vals)} distinct values (potential pattern!)")
        print(f"     Values: {list(unique_vals)[:5]}")

print()

#============================================================================
# APPROACH 4: SEQUENCE ANALYSIS (DIFFERENCES, RATIOS)
#============================================================================
print("="*80)
print("APPROACH 4: SEQUENCE DIFFERENCE/RATIO ANALYSIS")
print("="*80)
print()

print("Sequence:", sequence)

# Differences
diffs1 = [sequence[i+1] - sequence[i] for i in range(len(sequence)-1)]
print(f"First differences: {diffs1}")

diffs2 = [diffs1[i+1] - diffs1[i] for i in range(len(diffs1)-1)]
print(f"Second differences: {diffs2}")

if len(diffs2) >= 2:
    diffs3 = [diffs2[i+1] - diffs2[i] for i in range(len(diffs2)-1)]
    print(f"Third differences: {diffs3}")

# Ratios
ratios = [Rational(sequence[i+1], sequence[i]) for i in range(len(sequence)-1)]
print(f"\nRatios g(n+1)/g(n): {ratios}")

# Check for pattern in ratios
ratio_diffs = [ratios[i+1] - ratios[i] for i in range(len(ratios)-1)]
print(f"Ratio differences: {ratio_diffs}")

# Check if sequence matches g(n) = C(something, 2) pattern
print("\nChecking binomial patterns:")
for k in range(2, 20):
    vals = [binomial(k*n, 2) for n in range(5)]
    if vals == sequence:
        print(f"  ✓ MATCH: g(n) = C({k}n, 2)")

# Check g(n) = a*n^k + ...
print("\nChecking polynomial patterns:")
n_sym = Symbol('n')
for degree in [2, 3, 4]:
    # Fit polynomial
    import numpy as np
    n_vals = np.array([0, 1, 2, 3, 4])
    g_vals = np.array(sequence)

    try:
        coeffs = np.polyfit(n_vals, g_vals, degree)
        poly = sum(coeffs[i] * n_sym**(degree-i) for i in range(degree+1))

        # Test fit
        predicted = [int(poly.subs(n_sym, i)) for i in range(5)]
        if predicted == sequence:
            print(f"  ✓ EXACT FIT: g(n) = {poly}")
        else:
            error = sum(abs(predicted[i] - sequence[i]) for i in range(5))
            if error < 10:
                print(f"  ~ Near fit (error={error}): g(n) ≈ {poly}")
    except:
        pass

print()
print("="*80)
print("NEXT STEPS")
print("="*80)
print()
print("1. If polynomial pattern found → use it to compute g(16)")
print("2. If recurrence found → iterate symbolically to g(16)")
print("3. If invariant found → derive closed-form formula")
print("4. Check if 99% degeneracy implies g(n) stabilizes/converges")
print()
