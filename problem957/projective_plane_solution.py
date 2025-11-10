#!/usr/bin/env python3
"""
Problem 957: Point Genesis - PROJECTIVE PLANE Interpretation

Key insight: "There is a plane" - in projective plane:
- Every two distinct lines intersect at exactly one point
- No parallel lines exist
- Points at infinity are included

This completely changes the problem from Euclidean geometry!
"""

from itertools import combinations


class ProjectivePoint:
    """
    Point in projective plane represented as [x : y : z] homogeneous coordinates.
    Points are equivalent up to scalar multiplication: [x:y:z] = [kx:ky:kz] for k≠0
    """
    def __init__(self, x, y, z):
        # Normalize so that first non-zero coordinate is 1
        if x != 0:
            self.coords = (1, y/x, z/x)
        elif y != 0:
            self.coords = (0, 1, z/y)
        elif z != 0:
            self.coords = (0, 0, 1)
        else:
            raise ValueError("Invalid projective point [0:0:0]")

    def __eq__(self, other):
        # Check if points are equivalent (up to scaling)
        return abs(self.coords[0] - other.coords[0]) < 1e-9 and \
               abs(self.coords[1] - other.coords[1]) < 1e-9 and \
               abs(self.coords[2] - other.coords[2]) < 1e-9

    def __hash__(self):
        return hash(tuple(round(c, 6) for c in self.coords))

    def __repr__(self):
        return f"[{self.coords[0]:.3f}:{self.coords[1]:.3f}:{self.coords[2]:.3f}]"


class ProjectiveLine:
    """
    Line in projective plane represented as [a : b : c] where ax + by + cz = 0
    """
    def __init__(self, p1, p2):
        """Line through two points using cross product."""
        # Line through p1=[x1:y1:z1] and p2=[x2:y2:z2] is their cross product
        x1, y1, z1 = p1.coords
        x2, y2, z2 = p2.coords

        # Cross product
        a = y1*z2 - z1*y2
        b = z1*x2 - x1*z2
        c = x1*y2 - y1*x2

        # Normalize
        if a != 0:
            self.coords = (1, b/a, c/a)
        elif b != 0:
            self.coords = (0, 1, c/b)
        elif c != 0:
            self.coords = (0, 0, 1)
        else:
            raise ValueError("Points are not distinct")

    def intersect(self, other):
        """Find intersection point of two lines (always exists in projective plane!)."""
        # Intersection of [a1:b1:c1] and [a2:b2:c2] is their cross product
        a1, b1, c1 = self.coords
        a2, b2, c2 = other.coords

        x = b1*c2 - c1*b2
        y = c1*a2 - a1*c2
        z = a1*b2 - b1*a2

        if x == 0 and y == 0 and z == 0:
            return None  # Same line

        return ProjectivePoint(x, y, z)

    def __eq__(self, other):
        return abs(self.coords[0] - other.coords[0]) < 1e-9 and \
               abs(self.coords[1] - other.coords[1]) < 1e-9 and \
               abs(self.coords[2] - other.coords[2]) < 1e-9

    def __hash__(self):
        return hash(tuple(round(c, 6) for c in self.coords))


def simulate_projective_day(reds, blues):
    """
    Simulate one day in projective plane:
    - Draw all lines through (red, blue) pairs
    - Find all intersection points of distinct line pairs
    - Add new white points to blues
    """
    # Create all (red, blue) lines
    lines = []
    for r in reds:
        for b in blues:
            try:
                line = ProjectiveLine(r, b)
                lines.append(line)
            except ValueError:
                pass  # Same point

    # Find all pairwise intersections
    new_blues = set(blues)
    already_colored = reds | blues

    for line1, line2 in combinations(lines, 2):
        if line1 != line2:  # Different lines
            pt = line1.intersect(line2)
            if pt is not None and pt not in already_colored:
                # This is a WHITE point that turns blue!
                new_blues.add(pt)

    return new_blues


def test_projective_config():
    """Test with simple projective configuration."""
    print("="*70)
    print("PROBLEM 957: PROJECTIVE PLANE SOLUTION")
    print("="*70)
    print()

    # Start with simple configuration in projective plane
    # Use affine points (z=1 for points not at infinity)
    reds = {
        ProjectivePoint(0, 0, 1),   # Origin
        ProjectivePoint(1, 0, 1),   # (1,0)
        ProjectivePoint(0, 1, 1),   # (0,1)
    }

    blues = {
        ProjectivePoint(1, 1, 1),   # (1,1)
        ProjectivePoint(2, 3, 1),   # (2,3)
    }

    print(f"Configuration:")
    print(f"  Reds:  {len(reds)} points")
    print(f"  Blues: {len(blues)} points (initial)")
    print()

    print(f"Day 0: {len(blues)} blue points")

    # Simulate day 1
    blues_day1 = simulate_projective_day(reds, blues)
    print(f"Day 1: {len(blues_day1)} blue points (expected 8)")

    if len(blues_day1) == 8:
        print("  ✓ Matches g(1) = 8!")
    else:
        print(f"  ✗ Does not match g(1) = 8")

    print()

    # Continue to day 2
    blues_day2 = simulate_projective_day(reds, blues_day1)
    print(f"Day 2: {len(blues_day2)} blue points (expected 28)")

    if len(blues_day2) == 28:
        print("  ✓ Matches g(2) = 28!")
    else:
        print(f"  ✗ Does not match g(2) = 28")

    return reds, blues_day2


def main():
    """Main solution."""
    reds, blues = test_projective_config()

    print()
    print("="*70)
    print("CONTINUING TO DAY 16")
    print("="*70)
    print()

    # Continue simulation
    for day in range(3, 17):
        blues = simulate_projective_day(reds, blues)
        print(f"Day {day}: {len(blues)} blue points")

        if len(blues) > 100000000:
            print(f"\nReached day {day}, but point count too large for simple simulation.")
            break

    if day == 16:
        print()
        print("="*70)
        print(f"ANSWER: g(16) = {len(blues)}")
        print("="*70)


if __name__ == "__main__":
    main()
