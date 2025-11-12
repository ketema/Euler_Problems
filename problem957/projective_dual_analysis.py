#!/usr/bin/env python3
"""
PROJECTIVE DUAL ANALYSIS
AI Panel unanimous recommendation: Transform to dual space

In projective duality:
- Point (a,b,c) ↔ Line ax + by + cz = 0
- Incidence preserved: point P on line L ↔ line P* passes through point L*

With 99% degeneracy, dual space may reveal simple structure.
"""

from sympy import Point, Line, Rational, simplify
from typing import Set, List, Dict, Tuple
import json

def create_point(x, y) -> Point:
    """Create SymPy Point with exact Rational coordinates"""
    return Point(Rational(x), Rational(y))

def to_projective(p: Point) -> Tuple[Rational, Rational, Rational]:
    """Convert affine point (x,y) to projective [x:y:1]"""
    return (p.x, p.y, Rational(1))

def to_dual_line(p: Point) -> Tuple[Rational, Rational, Rational]:
    """
    Convert affine point (a,b) to dual line ax + by + z = 0 in projective space
    In affine coords (setting z=1): ax + by + 1 = 0
    """
    return (p.x, p.y, Rational(1))

def from_dual_line(coeffs: Tuple[Rational, Rational, Rational]) -> Point:
    """Convert dual line ax + by + cz = 0 back to primal point"""
    a, b, c = coeffs
    if c != 0:
        # Affine point: (-c/a, -c/b) if we interpret as ax + by + c = 0
        # But for dual: line ax + by + z = 0 ↔ point (a, b)
        return create_point(a, b)
    else:
        # Point at infinity
        return None

def line_to_dual_point(line: Line) -> Tuple[Rational, Rational, Rational]:
    """
    Convert affine line to dual point
    Line: ax + by + c = 0 → Dual point [a:b:c]
    """
    # Get line equation coefficients
    a, b, c = line.coefficients
    return (a, b, c)

def compute_day(reds: List[Point], blues: Set[Point]) -> Set[Point]:
    """Compute next day's blues (primal space)"""
    lines = []
    for red in reds:
        for blue in blues:
            if red != blue:
                lines.append(Line(red, blue))

    existing = set(reds).union(blues)
    new_intersections = set()

    for i, line1 in enumerate(lines):
        for line2 in lines[i+1:]:
            result = line1.intersection(line2)
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

initial_blues = {
    create_point(1, 1),
    create_point(3, 2)
}

print("="*80)
print("PROJECTIVE DUAL ANALYSIS")
print("="*80)
print()

# Compute days 0-3 in primal space
print("PRIMAL SPACE (Standard Problem)")
print("-" * 80)
all_blues_by_day = [initial_blues]
blues = initial_blues
for day in range(3):
    blues = compute_day(reds, blues)
    all_blues_by_day.append(blues.copy())
    print(f"Day {day}: g({day}) = {len(all_blues_by_day[day])}")
    print(f"Day {day}→{day+1}: {len(blues) - len(all_blues_by_day[day])} new blues")

print()
print("="*80)
print("DUAL SPACE TRANSFORMATION")
print("="*80)
print()

# Transform to dual space for each day
print("Each primal point becomes a dual line")
print("Counting: How many DISTINCT dual lines?")
print()

for day_idx in range(len(all_blues_by_day)):
    blues_set = all_blues_by_day[day_idx]

    # Transform each blue point to dual line
    dual_lines = set()
    for blue in blues_set:
        # Dual of point (a,b): line ax + by + 1 = 0
        dual = (blue.x, blue.y, Rational(1))
        dual_lines.add(dual)

    print(f"Day {day_idx}: {len(blues_set)} primal blues → {len(dual_lines)} dual lines")

    # Check if dual lines have special property
    # In dual space, collinear primal points → concurrent dual lines
    if len(dual_lines) != len(blues_set):
        print(f"  ⚠️  Degeneracy in duality transformation!")

print()
print("="*80)
print("DUAL SPACE LINE COUNTING")
print("="*80)
print()

print("Analyzing lines in PRIMAL space (become points in DUAL)")
print()

for day_idx in range(len(all_blues_by_day)):
    blues_set = all_blues_by_day[day_idx]

    # Compute all lines from reds to blues
    lines = []
    for red in reds:
        for blue in blues_set:
            if red != blue:
                line = Line(red, blue)
                lines.append(line)

    # Transform to dual points
    dual_points = set()
    for line in lines:
        a, b, c = line.coefficients
        # Normalize: divide by gcd or choose canonical form
        dual_pt = (a, b, c)
        dual_points.add(dual_pt)

    print(f"Day {day_idx}:")
    print(f"  Primal: {len(blues_set)} blues, {len(lines)} lines")
    print(f"  Dual: {len(lines)} primal lines → {len(dual_points)} distinct dual points")
    print(f"  Degeneracy: {len(lines) - len(dual_points)} duplicate lines")

print()
print("="*80)
print("DUAL SEQUENCE ANALYSIS")
print("="*80)
print()

print("If dual space has simpler structure:")
print("  → Dual sequence may match OEIS")
print("  → Dual growth may be polynomial")
print("  → Dual recurrence may be tractable")
print()

# Compute dual sequence
dual_sequence = []
for day_idx in range(len(all_blues_by_day)):
    blues_set = all_blues_by_day[day_idx]
    lines = []
    for red in reds:
        for blue in blues_set:
            if red != blue:
                line = Line(red, blue)
                lines.append(line)

    dual_points = set()
    for line in lines:
        a, b, c = line.coefficients
        # Canonical form: smallest integer coefficients
        from math import gcd as math_gcd
        from functools import reduce

        # Convert to integers for gcd
        def to_int_ratio(r):
            return (int(r.p), int(r.q))

        # Skip canonical form for now, just count distinct
        dual_pt = (a, b, c)
        dual_points.add(dual_pt)

    dual_sequence.append(len(dual_points))

print("Primal sequence (blues): ", [len(d) for d in all_blues_by_day])
print("Dual sequence (lines): ", dual_sequence)
print()

print("Dual ratios:")
for i in range(len(dual_sequence) - 1):
    ratio = dual_sequence[i+1] / dual_sequence[i] if dual_sequence[i] > 0 else 0
    print(f"  h({i+1})/h({i}) = {ratio:.3f}")

print()
print("="*80)
print("INTERPRETATION")
print("="*80)
print()
print("Key insight from AI Panel:")
print("  'With 99% degeneracy, the dual may exhibit simple structure.'")
print("  'Each blue ↔ bundle of lines through reds.'")
print("  'Count bundles instead of points.'")
print()
print("If dual sequence is simpler:")
print("  1. May match OEIS entry")
print("  2. May have closed-form formula")
print("  3. May enable computing dual_h(16), then inverting to g(16)")
print()
