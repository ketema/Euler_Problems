#!/usr/bin/env python3
"""
Test language-based hypotheses for g(n).

NOT testing simulation, but testing if "number" means something other than count.
"""

from sympy import Point2D, Line, Rational, Polygon
from sympy.geometry import intersection
import sys

print("="*70)
print("TESTING LANGUAGE-BASED HYPOTHESES")
print("="*70)
print()

# Our verified configuration
reds = [
    Point2D(Rational(0), Rational(0)),
    Point2D(Rational(4), Rational(0)),
    Point2D(Rational(2), Rational(3))
]
blues_day0 = [
    Point2D(Rational(1), Rational(1)),
    Point2D(Rational(3), Rational(2))
]

def simulate_one_day(reds, current_blues):
    """Get new blues for one day"""
    lines = []
    for red in reds:
        for blue in current_blues:
            lines.append(Line(red, blue))

    new_points = set()
    for i in range(len(lines)):
        for j in range(i+1, len(lines)):
            result = intersection(lines[i], lines[j])
            if not result:
                continue
            p = result[0]
            if not hasattr(p, 'x'):
                continue
            if p not in reds and p not in current_blues and p not in new_points:
                new_points.add(p)

    return new_points

# Get configurations for day 1 and day 2
print("Computing actual configurations...")
print()

day1_new = simulate_one_day(reds, blues_day0)
blues_day1 = set(blues_day0) | day1_new

day2_new = simulate_one_day(reds, blues_day1)
blues_day2 = set(blues_day1) | day2_new

print(f"Day 0: {len(blues_day0)} blues")
print(f"Day 1: {len(blues_day1)} blues")
print(f"Day 2: {len(blues_day2)} blues")
print()

# ============================================================================
# HYPOTHESIS 1: Simple algebraic pattern from g(1)=8, g(2)=28
# ============================================================================

print("="*70)
print("H1: ALGEBRAIC PATTERNS FROM g(1)=8, g(2)=28")
print("="*70)
print()

# Linear: g(n) = an + b
# g(1) = a + b = 8
# g(2) = 2a + b = 28
# → a = 20, b = -12

print("Linear pattern: g(n) = 20n - 12")
print(f"  g(1) = 20(1) - 12 = {20*1 - 12}")
print(f"  g(2) = 20(2) - 12 = {20*2 - 12}")
print(f"  g(16) = 20(16) - 12 = {20*16 - 12}")
print()

# Quadratic was already rejected (1778)
print("Quadratic: g(n) = 7n² - n + 2 → g(16) = 1778 ❌ (already rejected)")
print()

# Other simple patterns
print("Other patterns:")
patterns = [
    ("g(n) = 4n² + 2n", lambda n: 4*n**2 + 2*n),
    ("g(n) = n³ + 7n", lambda n: n**3 + 7*n),
    ("g(n) = 10n - 2", lambda n: 10*n - 2),
    ("g(n) = 2^n + n²", lambda n: 2**n + n**2),
]

for name, f in patterns:
    if f(1) == 8 and f(2) == 28:
        print(f"  {name}")
        print(f"    g(1) = {f(1)}, g(2) = {f(2)} ✓")
        print(f"    g(16) = {f(16)}")
print()

# ============================================================================
# HYPOTHESIS 2: "Number" means SUM of coordinates
# ============================================================================

print("="*70)
print("H2: 'NUMBER' MEANS SUM OF COORDINATES")
print("="*70)
print()

def coord_sum(points):
    """Sum of all coordinates"""
    return sum(float(p.x) + float(p.y) for p in points)

sum_day0 = coord_sum(blues_day0)
sum_day1 = coord_sum(blues_day1)
sum_day2 = coord_sum(blues_day2)

print(f"Day 0: coordinate sum = {sum_day0:.2f} (expect 2)")
print(f"Day 1: coordinate sum = {sum_day1:.2f} (expect 8)")
print(f"Day 2: coordinate sum = {sum_day2:.2f} (expect 28)")
print()

if abs(sum_day1 - 8) < 0.01:
    print("✓ SUM OF COORDINATES matches g(1)=8!")
    print("  This could be it!")
elif abs(sum_day1 - 8) < 1:
    print("? Close but not exact")
else:
    print("✗ Doesn't match")
print()

# ============================================================================
# HYPOTHESIS 3: "Number" means PRODUCT of coordinates
# ============================================================================

print("="*70)
print("H3: 'NUMBER' MEANS PRODUCT OF COORDINATES")
print("="*70)
print()

def coord_product(points):
    """Product of all coordinates (tricky with zeros)"""
    prod = 1
    for p in points:
        x, y = float(p.x), float(p.y)
        if x != 0 and y != 0:
            prod *= x * y
    return prod

prod_day1 = coord_product(blues_day1)
prod_day2 = coord_product(blues_day2)

print(f"Day 1: coordinate product = {prod_day1:.2f} (expect 8)")
print(f"Day 2: coordinate product = {prod_day2:.2f} (expect 28)")
print()

if abs(prod_day1 - 8) < 0.01:
    print("✓ PRODUCT matches!")
else:
    print("✗ Doesn't match")
print()

# ============================================================================
# HYPOTHESIS 4: Geometric invariants
# ============================================================================

print("="*70)
print("H4: GEOMETRIC INVARIANTS (area, perimeter, etc.)")
print("="*70)
print()

def convex_hull_area(points):
    """Area of convex hull"""
    try:
        if len(points) < 3:
            return 0
        poly = Polygon(*list(points))
        return float(poly.area)
    except:
        return None

area_day1 = convex_hull_area(blues_day1)
area_day2 = convex_hull_area(blues_day2)

print(f"Day 1: convex hull area = {area_day1} (expect 8?)")
print(f"Day 2: convex hull area = {area_day2} (expect 28?)")
print()

# ============================================================================
# HYPOTHESIS 5: Number of lines
# ============================================================================

print("="*70)
print("H5: COUNT OF LINES (not points)")
print("="*70)
print()

def count_lines(reds, blues):
    """How many distinct lines?"""
    lines_set = set()
    for red in reds:
        for blue in blues:
            line = Line(red, blue)
            # Use canonical form for comparison
            lines_set.add(line)
    return len(lines_set)

lines_day1 = count_lines(reds, blues_day1)
lines_day2 = count_lines(reds, blues_day2)

print(f"Day 1: {lines_day1} distinct red-to-blue lines (expect 8?)")
print(f"Day 2: {lines_day2} distinct red-to-blue lines (expect 28?)")
print()

if lines_day1 == 8:
    print("✓ NUMBER OF LINES matches g(1)=8!")
    print("  Could g(n) count lines, not points?")
else:
    print(f"✗ Lines = {lines_day1} ≠ 8")
print()

# ============================================================================
# SUMMARY
# ============================================================================

print("="*70)
print("SUMMARY OF LANGUAGE HYPOTHESES")
print("="*70)
print()

print("Matches found:")
matches = []

if abs(sum_day1 - 8) < 0.01:
    matches.append("Coordinate sum")
if abs(prod_day1 - 8) < 0.01:
    matches.append("Coordinate product")
if lines_day1 == 8:
    matches.append("Number of lines")
if area_day1 and abs(area_day1 - 8) < 0.01:
    matches.append("Convex hull area")

if matches:
    for m in matches:
        print(f"  • {m}")
else:
    print("  None of the tested interpretations match!")
    print()
    print("  Simple algebraic pattern most promising:")
    print(f"    g(n) = 20n - 12 → g(16) = 308")
print()
