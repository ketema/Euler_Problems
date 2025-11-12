#!/usr/bin/env python3
"""
Test if blue points lie on a special algebraic curve
AI Panel fallback hypothesis after multiplicity analysis failure
"""

from sympy import Point, Line, Rational, intersection, symbols, solve, simplify
from sympy import Poly, groebner, Matrix
from typing import Set, List
import json

def create_point(x, y) -> Point:
    """Create SymPy Point with exact Rational coordinates"""
    return Point(Rational(x), Rational(y))

def compute_day(reds: List[Point], blues: Set[Point]) -> Set[Point]:
    """Compute next day's blues"""
    lines = []
    for red in reds:
        for blue in blues:
            if red != blue:
                lines.append(Line(red, blue))

    existing = set(reds).union(blues)
    new_intersections = set()

    for i, line1 in enumerate(lines):
        for line2 in lines[i+1:]:
            result = intersection(line1, line2)
            if result:
                if hasattr(result[0], 'x'):
                    p = result[0]
                    if p not in existing:
                        new_intersections.add(p)

    return blues.union(new_intersections)

# Configuration
reds = [
    create_point(0, 0),
    create_point(4, 0),
    create_point(2, 3)
]

blues = {
    create_point(1, 1),
    create_point(3, 2)
}

print("="*80)
print("ALGEBRAIC CURVE HYPOTHESIS TEST")
print("="*80)
print()

# Compute days 0-3 (day 4 takes too long)
all_blues_by_day = [blues]
for day in range(3):
    blues = compute_day(reds, blues)
    all_blues_by_day.append(blues.copy())
    print(f"g({day+1}) = {len(blues)} computed")

print()

# Extract coordinates
def extract_coords(points: Set[Point]) -> List[tuple]:
    coords = []
    for p in points:
        x_val = float(p.x)
        y_val = float(p.y)
        coords.append((x_val, y_val))
    return sorted(coords)

# Test common algebraic curves
print("="*80)
print("TESTING COMMON ALGEBRAIC CURVES")
print("="*80)
print()

x, y = symbols('x y', real=True)

test_curves = [
    ("Circle: x² + y² = r²", lambda coords: [px**2 + py**2 for px, py in coords]),
    ("Parabola: y = ax²", lambda coords: [py - (coords[0][1]/coords[0][0]**2)*px**2 if px != 0 else py for px, py in coords]),
    ("Hyperbola: xy = k", lambda coords: [px*py for px, py in coords]),
    ("Ellipse: x²/a² + y²/b² = 1", lambda coords: [(px/4)**2 + (py/3)**2 for px, py in coords]),
    ("Line: y = mx + b", lambda coords: [py - coords[0][1]/coords[0][0]*px if coords[0][0] != 0 else py for px, py in coords]),
]

for day_idx, blues_set in enumerate(all_blues_by_day[:3]):  # Test days 0-2
    coords = extract_coords(blues_set)
    print(f"Day {day_idx}: {len(coords)} blues")
    print(f"Sample coords: {coords[:5]}")

    for curve_name, test_func in test_curves:
        try:
            values = test_func(coords)
            unique_values = set([round(v, 6) for v in values])
            if len(unique_values) <= max(2, len(coords) // 4):
                print(f"  ⚠️  {curve_name}: Only {len(unique_values)} distinct values")
        except:
            pass
    print()

# Polynomial fit attempt
print("="*80)
print("POLYNOMIAL CURVE FITTING")
print("="*80)
print()

for day_idx in [0, 1, 2]:
    coords = extract_coords(all_blues_by_day[day_idx])
    print(f"\nDay {day_idx}: Attempting polynomial fit (degree 2-4)")

    # Try fitting y = a + bx + cx² + dx³ + ex⁴
    import numpy as np

    xs = np.array([c[0] for c in coords])
    ys = np.array([c[1] for c in coords])

    for degree in [2, 3, 4]:
        try:
            coeffs = np.polyfit(xs, ys, degree)
            poly_func = np.poly1d(coeffs)
            residuals = ys - poly_func(xs)
            max_error = np.max(np.abs(residuals))

            if max_error < 0.1:
                print(f"  ✓ Degree {degree}: max_error = {max_error:.6f}")
                print(f"    Coefficients: {coeffs}")
            else:
                print(f"  ✗ Degree {degree}: max_error = {max_error:.6f} (too large)")
        except:
            print(f"  ✗ Degree {degree}: fitting failed")

print()
print("="*80)
print("GEOMETRIC STRUCTURE ANALYSIS")
print("="*80)
print()

# Check if points form a lattice
coords_day2 = extract_coords(all_blues_by_day[2])
print(f"Day 2: {len(coords_day2)} points")

# Check for rational coordinates
all_rational = True
for p in all_blues_by_day[2]:
    if not (isinstance(p.x, Rational) or p.x.is_rational):
        all_rational = False
        break
    if not (isinstance(p.y, Rational) or p.y.is_rational):
        all_rational = False
        break

print(f"All coordinates are rational: {all_rational}")

# Check for integer linear combinations
print("\nChecking if points form integer lattice...")
coords_day2_exact = [(p.x, p.y) for p in all_blues_by_day[2]]
print(f"Sample exact coords:")
for i, (x, y) in enumerate(coords_day2_exact[:10]):
    print(f"  ({x}, {y})")

print()
print("="*80)
print("CONCLUSION")
print("="*80)
print()
print("If no simple algebraic curve found:")
print("  → Points NOT on standard curve (circle, parabola, hyperbola, line)")
print("  → May lie on higher-degree algebraic variety")
print("  → OR: Configuration is projectively special but not curve-based")
print()
print("Next steps:")
print("  1. If curve found: Use Bezout's theorem for intersection counting")
print("  2. If no curve: Return to combinatorial approach or search for")
print("     alternative geometric structure (incidence geometry, design theory)")
print()
