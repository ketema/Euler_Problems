#!/usr/bin/env python3
"""
Search for optimal configuration that gives g(1) = 8, g(2) = 28

Strategy:
1. Work in projective plane to capture ALL intersections (including at infinity)
2. Use homogeneous coordinates [x:y:z]
3. Test various configurations systematically
"""

from sympy import Point3D, Line3D, Rational, simplify
from itertools import combinations
import time

class ProjectiveSolver:
    """Solver using homogeneous coordinates in ℝℙ²"""

    def __init__(self, reds_homogeneous, blues_homogeneous):
        """
        Points in homogeneous coordinates [x:y:z]
        Stored as Point3D
        """
        self.reds = reds_homogeneous
        self.blues_by_day = {0: blues_homogeneous}

    def normalize_point(self, p: Point3D) -> tuple:
        """Normalize homogeneous coordinates to canonical form for comparison"""
        x, y, z = p.x, p.y, p.z

        # Find first non-zero coordinate
        if z != 0:
            # Normalize by z
            return (Rational(x/z), Rational(y/z), Rational(1))
        elif y != 0:
            # Point at infinity, normalize by y
            return (Rational(x/y), Rational(1), Rational(0))
        elif x != 0:
            # Normalize by x
            return (Rational(1), Rational(0), Rational(0))
        else:
            raise ValueError("Invalid point [0:0:0]")

    def points_equal(self, p1: Point3D, p2: Point3D) -> bool:
        """Check if two homogeneous points are equal"""
        return self.normalize_point(p1) == self.normalize_point(p2)

    def line_from_points(self, p1: Point3D, p2: Point3D) -> Point3D:
        """
        Line through two points in projective plane.
        Line is represented as a point in dual space [a:b:c]
        where line equation is ax + by + cz = 0
        Computed as cross product: p1 × p2
        """
        x1, y1, z1 = p1.x, p1.y, p1.z
        x2, y2, z2 = p2.x, p2.y, p2.z

        a = y1*z2 - y2*z1
        b = z1*x2 - z2*x1
        c = x1*y2 - x2*y1

        return Point3D(a, b, c)

    def intersection_of_lines(self, l1: Point3D, l2: Point3D) -> Point3D:
        """
        Intersection of two lines in projective plane.
        Computed as cross product: l1 × l2
        """
        a1, b1, c1 = l1.x, l1.y, l1.z
        a2, b2, c2 = l2.x, l2.y, l2.z

        x = b1*c2 - b2*c1
        y = c1*a2 - c2*a1
        z = a1*b2 - a2*b1

        return Point3D(x, y, z)

    def simulate_day(self, from_day: int, verbose=True) -> int:
        """Simulate one day transition"""
        if verbose:
            print(f"\n{'='*70}")
            print(f"Day {from_day} → Day {from_day + 1}")
            print(f"{'='*70}")

        current_blues = []
        for d in range(from_day + 1):
            current_blues.extend(self.blues_by_day[d])

        if verbose:
            print(f"Starting blues: g({from_day}) = {len(current_blues)}")

        # Build set of existing points
        existing = set()
        for r in self.reds:
            existing.add(self.normalize_point(r))
        for b in current_blues:
            existing.add(self.normalize_point(b))

        # Construct all lines
        lines = []
        for r in self.reds:
            for b in current_blues:
                line = self.line_from_points(r, b)
                lines.append(line)

        if verbose:
            print(f"Lines: {len(lines)}")

        # Find intersections
        new_points = []
        new_points_set = set()

        for i, l1 in enumerate(lines):
            for l2 in lines[i+1:]:
                # Intersect
                p = self.intersection_of_lines(l1, l2)
                p_norm = self.normalize_point(p)

                # Check if new
                if p_norm not in existing and p_norm not in new_points_set:
                    new_points.append(p)
                    new_points_set.add(p_norm)

        self.blues_by_day[from_day + 1] = new_points

        if verbose:
            print(f"New blues: {len(new_points)}")
            print(f"g({from_day + 1}) = {len(current_blues) + len(new_points)}")

        return len(new_points)


def test_config(name, reds, blues, target_days=3):
    """Test a configuration"""
    print(f"\n{'='*70}")
    print(f"Config: {name}")
    print(f"{'='*70}")

    solver = ProjectiveSolver(reds, blues)

    results = [2]  # g(0) = 2
    for day in range(target_days):
        solver.simulate_day(day, verbose=False)
        current_blues = []
        for d in range(day + 2):
            if d in solver.blues_by_day:
                current_blues.extend(solver.blues_by_day[d])
        results.append(len(current_blues))

    print(f"Sequence: {results}")
    print(f"g(1) = {results[1]} (target: 8) {'✓' if results[1] == 8 else '✗'}")
    if len(results) > 2:
        print(f"g(2) = {results[2]} (target: 28) {'✓' if results[2] == 8 else '✗'}")

    return results


# Test configurations
print("Searching for optimal configuration...")
print("="*70)

# Config 1: Standard triangle
reds1 = [
    Point3D(0, 0, 1),   # (0, 0)
    Point3D(3, 0, 1),   # (3, 0)
    Point3D(0, 3, 1),   # (0, 3)
]
blues1 = [
    Point3D(1, 1, 1),   # (1, 1)
    Point3D(2, 2, 1),   # (2, 2)
]

seq1 = test_config("Standard triangle + diagonal blues", reds1, blues1)

# Config 2: Larger triangle
reds2 = [
    Point3D(0, 0, 1),
    Point3D(12, 0, 1),
    Point3D(0, 12, 1),
]
blues2 = [
    Point3D(3, 3, 1),
    Point3D(4, 5, 1),
]

seq2 = test_config("Large triangle + offset blues", reds2, blues2)

# Config 3: Asymmetric
reds3 = [
    Point3D(0, 0, 1),
    Point3D(10, 0, 1),
    Point3D(3, 7, 1),
]
blues3 = [
    Point3D(4, 2, 1),
    Point3D(5, 3, 1),
]

seq3 = test_config("Asymmetric triangle", reds3, blues3)

# Config 4: Include point at infinity
reds4 = [
    Point3D(0, 0, 1),
    Point3D(1, 0, 1),
    Point3D(0, 1, 1),
]
blues4 = [
    Point3D(1, 1, 1),
    Point3D(1, 0, 0),  # Point at infinity in x-direction
]

seq4 = test_config("With point at infinity", reds4, blues4)

print("\n" + "="*70)
print("ANALYSIS")
print("="*70)
print("Need to find configuration where:")
print("- g(0) = 2")
print("- g(1) = 8 (adds 6 new blues)")
print("- g(2) = 28 (adds 20 new blues)")
print("\nAll configs tested give similar results...")
print("This suggests g(1)=8 requires a special geometric arrangement.")
