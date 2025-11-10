#!/usr/bin/env python3
"""
Efficient projective plane computation using exact rational arithmetic.

Goal: Compute g(1) through g(10+) to find pattern in the sequence.
"""

from fractions import Fraction
from itertools import combinations

class ProjPoint:
    """Point in projective plane [x:y:z]."""
    def __init__(self, x, y, z):
        # Normalize: make first non-zero coordinate = 1
        x, y, z = Fraction(x), Fraction(y), Fraction(z)
        if x != 0:
            self.coords = (Fraction(1), y/x, z/x)
        elif y != 0:
            self.coords = (Fraction(0), Fraction(1), z/y)
        elif z != 0:
            self.coords = (Fraction(0), Fraction(0), Fraction(1))
        else:
            raise ValueError("Invalid point")

    def __eq__(self, other):
        return self.coords == other.coords

    def __hash__(self):
        return hash(self.coords)

    def __repr__(self):
        return f"[{self.coords[0]}:{self.coords[1]}:{self.coords[2]}]"


class ProjLine:
    """Line in projective plane [a:b:c] where ax+by+cz=0."""
    def __init__(self, p1, p2):
        # Line through p1 and p2 is cross product
        x1, y1, z1 = p1.coords
        x2, y2, z2 = p2.coords

        a = y1*z2 - z1*y2
        b = z1*x2 - x1*z2
        c = x1*y2 - y1*x2

        # Normalize
        if a != 0:
            self.coords = (Fraction(1), b/a, c/a)
        elif b != 0:
            self.coords = (Fraction(0), Fraction(1), c/b)
        elif c != 0:
            self.coords = (Fraction(0), Fraction(0), Fraction(1))
        else:
            raise ValueError("Points not distinct")

    def intersect(self, other):
        """Find intersection point (cross product of lines)."""
        a1, b1, c1 = self.coords
        a2, b2, c2 = other.coords

        x = b1*c2 - c1*b2
        y = c1*a2 - a1*c2
        z = a1*b2 - b1*a2

        if x == 0 and y == 0 and z == 0:
            return None  # Same line

        return ProjPoint(x, y, z)

    def __eq__(self, other):
        return self.coords == other.coords

    def __hash__(self):
        return hash(self.coords)


def simulate_one_day(reds, blues, verbose=False):
    """Simulate one day in projective plane."""
    # Create all (red, blue) lines
    lines = set()
    for r in reds:
        for b in blues:
            try:
                lines.add(ProjLine(r, b))
            except:
                pass

    if verbose:
        print(f"  Created {len(lines)} distinct lines from {len(reds)}×{len(blues)} pairs")

    # Find all intersections
    new_blues = set(blues)
    all_colored = reds | blues

    intersection_count = 0
    new_point_count = 0

    for l1, l2 in combinations(lines, 2):
        intersection_count += 1
        pt = l1.intersect(l2)
        if pt and pt not in all_colored:
            if pt not in new_blues:
                new_point_count += 1
            new_blues.add(pt)

    if verbose:
        print(f"  Checked {intersection_count} line pairs")
        print(f"  Found {new_point_count} new white points turning blue")

    return new_blues


def compute_sequence(max_days=10):
    """Compute g(n) sequence using general position configuration."""
    print("="*70)
    print("COMPUTING PROJECTIVE PLANE SEQUENCE")
    print("="*70)
    print()

    # Use general position configuration that gave us g(1)=8, g(2)=28, g(3)=184
    reds = {
        ProjPoint(0, 0, 1),
        ProjPoint(1, 0, 1),
        ProjPoint(0, 1, 1)
    }

    blues = {
        ProjPoint(1, 1, 1),
        ProjPoint(2, 3, 1)
    }

    sequence = [len(blues)]

    print(f"Day 0: {len(blues)} blues")

    for day in range(1, max_days + 1):
        print(f"\nDay {day}:", end=" ")
        blues = simulate_one_day(reds, blues, verbose=True)
        sequence.append(len(blues))
        print(f"  Total blues: {len(blues)}")

        if len(blues) > 100000:
            print(f"\nStopping at day {day} - point count too large")
            break

    return sequence


def analyze_pattern(seq):
    """Analyze the sequence for patterns."""
    print("\n" + "="*70)
    print("SEQUENCE ANALYSIS")
    print("="*70)
    print()

    print("g(n) sequence:")
    for i, val in enumerate(seq):
        print(f"  g({i}) = {val}")

    print("\nNew blues per day:")
    new_blues = [seq[i] - seq[i-1] for i in range(1, len(seq))]
    for i, val in enumerate(new_blues, 1):
        print(f"  Day {i}: {val} new")

    print("\nGrowth ratios:")
    for i in range(1, len(seq)):
        ratio = seq[i] / seq[i-1]
        print(f"  g({i})/g({i-1}) = {ratio:.6f}")


if __name__ == "__main__":
    seq = compute_sequence(max_days=10)
    analyze_pattern(seq)

    print("\n" + "="*70)
    print("NEXT STEPS")
    print("="*70)
    print()
    print("If this sequence matches g(1)=8, g(2)=28, g(3)=184,")
    print("then continue computation or look for recurrence pattern.")
