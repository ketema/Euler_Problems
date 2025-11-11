#!/usr/bin/env python3
"""
Test the hyperbola configuration from git history
"""

from sympy import Point, Line, Rational
from sympy.geometry import intersection

# Configuration from git history (on hyperbola x(x-1) = 3y(y-1))
reds = [Point(0, 0), Point(1, 0), Point(0, 1)]
blues = [Point(1, 1), Point(3, 2)]

print("="*70)
print("TESTING HYPERBOLA CONFIGURATION")
print("="*70)
print(f"\nReds: {reds}")
print(f"Blues: {blues}")

# Verify they lie on hyperbola x(x-1) = 3y(y-1)
print("\nVerifying hyperbola equation: x(x-1) = 3y(y-1)")
all_points = reds + blues
for p in all_points:
    x, y = Rational(p.x), Rational(p.y)
    lhs = x * (x - 1)
    rhs = 3 * y * (y - 1)
    print(f"  {p}: {lhs} = {rhs} {'✓' if lhs == rhs else '✗'}")

# Simulate day 1
print("\n" + "="*70)
print("SIMULATING DAY 1")
print("="*70)

# Draw all red-to-blue lines
lines = []
for r in reds:
    for b in blues:
        line = Line(r, b)
        lines.append(line)
        print(f"  Line from {r} to {b}")

print(f"\nTotal lines: {len(lines)}")

# Find all intersections
new_blues = set()
for p in blues:
    new_blues.add((Rational(p.x), Rational(p.y)))

for i, line1 in enumerate(lines):
    for line2 in lines[i+1:]:
        if line1 != line2:
            result = intersection(line1, line2)
            if result:
                p = result[0]
                if hasattr(p, 'x'):
                    new_blues.add((Rational(p.x), Rational(p.y)))

g1 = len(new_blues)
print(f"\ng(1) = {g1}")
print(f"Expected: 8")
print(f"Match: {'✓' if g1 == 8 else '✗'}")

if g1 != 8:
    print(f"\n⚠️  This configuration does NOT give g(1)=8!")
    print(f"Let me list all intersection points:")
    for pt in sorted(new_blues):
        print(f"  {pt}")
