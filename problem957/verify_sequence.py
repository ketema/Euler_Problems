#!/usr/bin/env python3
"""
Verify my projective plane implementation against the OEIS sequence.
Use optimized approach with rational coordinates.
"""

from fractions import Fraction
from itertools import combinations

class Point:
    def __init__(self, x, y, z):
        # Normalize projective coordinates
        if x != 0:
            self.x, self.y, self.z = Fraction(1), Fraction(y, x), Fraction(z, x)
        elif y != 0:
            self.x, self.y, self.z = Fraction(0), Fraction(1), Fraction(z, y)
        elif z != 0:
            self.x, self.y, self.z = Fraction(0), Fraction(0), Fraction(1)
        else:
            raise ValueError("Invalid point")

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y and self.z == other.z

    def __hash__(self):
        return hash((self.x, self.y, self.z))

    def __repr__(self):
        return f"[{self.x}:{self.y}:{self.z}]"


class Line:
    def __init__(self, p1, p2):
        # Cross product for line through two points
        a = p1.y * p2.z - p1.z * p2.y
        b = p1.z * p2.x - p1.x * p2.z
        c = p1.x * p2.y - p1.y * p2.x

        if a != 0:
            self.a, self.b, self.c = Fraction(1), Fraction(b, a), Fraction(c, a)
        elif b != 0:
            self.a, self.b, self.c = Fraction(0), Fraction(1), Fraction(c, b)
        elif c != 0:
            self.a, self.b, self.c = Fraction(0), Fraction(0), Fraction(1)
        else:
            raise ValueError("Points not distinct")

    def intersect(self, other):
        # Cross product for intersection of two lines
        x = self.b * other.c - self.c * other.b
        y = self.c * other.a - self.a * other.c
        z = self.a * other.b - self.b * other.a

        if x == 0 and y == 0 and z == 0:
            return None

        return Point(x, y, z)

    def __eq__(self, other):
        return self.a == other.a and self.b == other.b and self.c == other.c

    def __hash__(self):
        return hash((self.a, self.b, self.c))


def simulate_day(reds, blues):
    """Simulate one day in projective plane."""
    lines = []
    for r in reds:
        for b in blues:
            try:
                lines.append(Line(r, b))
            except:
                pass

    new_blues = set(blues)
    all_colored = reds | blues

    for l1, l2 in combinations(lines, 2):
        if l1 != l2:
            pt = l1.intersect(l2)
            if pt and pt not in all_colored:
                new_blues.add(pt)

    return new_blues


# OEIS sequence from my files
oeis = [2, 8, 28, 184, 1646, 19161, 261788, 4118024, 73099464]

# Test with simple configuration that gave g(1)=8, g(2)=28, g(3)=184
reds = {Point(0,0,1), Point(1,0,1), Point(0,1,1)}
blues = {Point(1,1,1), Point(2,3,1)}

print("Verifying projective plane implementation against OEIS:")
print("="*70)

blues_current = blues
for day in range(len(oeis)):
    count = len(blues_current)
    expected = oeis[day]
    match = "✓" if count == expected else "✗"

    print(f"Day {day}: {count:>10} (expected {expected:>10}) {match}")

    if count != expected:
        print(f"  DIVERGENCE at day {day}!")
        break

    if day < len(oeis) - 1:
        blues_current = simulate_day(reds, blues_current)
        if len(blues_current) > 100000:
            print(f"\n  Stopping - too many points for rational arithmetic")
            break
