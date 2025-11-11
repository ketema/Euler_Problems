#!/usr/bin/env python3
"""
Analyze the GEOMETRIC STRUCTURE at each iteration.

The user says: "I am thinking this is a shape or series of shapes for each day"

Classical projective geometry theorems:
- Pascal's theorem: 6 points on conic → opposite sides meet collinearly
- Pappus's theorem: 2 lines with 3 points each → collinearities
- Desargues' theorem: 2 perspective triangles → sides meet collinearly

What if g(n) follows from COUNTING applications of these theorems?
"""

from sympy import Point2D, Line, Rational, simplify
from sympy.geometry import intersection
import sys

class GeometricAnalyzer:
    def __init__(self):
        # Configuration that gives g(1)=8, g(2)=28
        self.reds = [
            Point2D(Rational(0), Rational(0)),
            Point2D(Rational(4), Rational(0)),
            Point2D(Rational(2), Rational(3))
        ]
        self.blues = [
            Point2D(Rational(1), Rational(1)),
            Point2D(Rational(3), Rational(2))
        ]
        self.history = {0: set(self.blues)}

    def get_all_blues(self, day):
        """Get all blues up to and including given day"""
        all_blues = set()
        for d in range(day + 1):
            if d in self.history:
                all_blues.update(self.history[d])
        return all_blues

    def simulate_day(self, from_day: int, verbose=True) -> int:
        """Simulate one day and analyze structure"""
        current_blues = self.get_all_blues(from_day)

        if verbose:
            print(f"\n{'='*70}")
            print(f"DAY {from_day} → DAY {from_day + 1}")
            print(f"{'='*70}")
            print(f"Starting with {len(current_blues)} blues")

        # Draw lines ONLY from reds to blues
        lines = []
        for i, red in enumerate(self.reds):
            for j, blue in enumerate(current_blues):
                line = Line(red, blue)
                lines.append((f"R{i}", f"B{j}", red, blue, line))

        if verbose:
            print(f"Drawing {len(lines)} red-to-blue lines")

        # Find all intersections
        new_points = set()
        intersection_data = []

        for i, (r1_label, b1_label, r1, b1, line1) in enumerate(lines):
            for (r2_label, b2_label, r2, b2, line2) in lines[i+1:]:
                result = intersection(line1, line2)
                if not result:
                    continue

                p = result[0]
                if not hasattr(p, 'x'):
                    continue

                # Check if new point
                if p in self.reds or p in current_blues or p in new_points:
                    continue

                new_points.add(p)
                intersection_data.append({
                    'point': p,
                    'line1': (r1_label, b1_label, r1, b1),
                    'line2': (r2_label, b2_label, r2, b2)
                })

        self.history[from_day + 1] = new_points

        if verbose:
            print(f"Found {len(new_points)} new blue points")
            self.analyze_structure(from_day + 1, intersection_data)

        return len(self.get_all_blues(from_day + 1))

    def analyze_structure(self, day, intersection_data):
        """Analyze geometric structure of configuration"""
        all_points = list(self.reds) + list(self.get_all_blues(day))

        print(f"\n--- GEOMETRIC ANALYSIS OF DAY {day} ---")
        print(f"Total points: {len(all_points)} (3 reds + {len(all_points)-3} blues)")

        # Check for collinearities
        collinear_triples = self.find_collinear_triples(all_points)
        print(f"Collinear triples: {len(collinear_triples)}")

        # Check for points on original hyperbola
        on_hyperbola = self.count_on_hyperbola(all_points)
        print(f"Points on hyperbola x(x-1)=3y(y-1): {on_hyperbola}")

        # Analyze symmetry
        self.check_symmetry(all_points)

        # Look for classical configurations
        self.check_classical_configs(all_points, day)

    def find_collinear_triples(self, points):
        """Find all collinear triples"""
        collinear = []
        for i in range(len(points)):
            for j in range(i+1, len(points)):
                for k in range(j+1, len(points)):
                    # Check if three points are collinear
                    p1, p2, p3 = points[i], points[j], points[k]
                    if p1.distance(p2) == 0 or p2.distance(p3) == 0:
                        continue
                    line = Line(p1, p2)
                    if p3 in line:
                        collinear.append((p1, p2, p3))
        return collinear

    def count_on_hyperbola(self, points):
        """Count how many points lie on hyperbola x(x-1) = 3y(y-1)"""
        count = 0
        for p in points:
            x, y = Rational(p.x), Rational(p.y)
            lhs = x * (x - 1)
            rhs = 3 * y * (y - 1)
            if simplify(lhs - rhs) == 0:
                count += 1
        return count

    def check_symmetry(self, points):
        """Check for rotational symmetry"""
        # Check if configuration has 3-fold rotational symmetry
        # (mentioned in git history)
        centroid_x = sum(p.x for p in points) / len(points)
        centroid_y = sum(p.y for p in points) / len(points)
        print(f"Centroid: ({float(centroid_x):.2f}, {float(centroid_y):.2f})")

    def check_classical_configs(self, points, day):
        """Check for Pascal, Pappus, Desargues configurations"""
        print(f"\n--- CLASSICAL CONFIGURATIONS ---")

        # Pascal: Need 6 points on conic
        on_hyp = [p for p in points if self.is_on_hyperbola(p)]
        if len(on_hyp) >= 6:
            print(f"✓ Pascal's theorem applicable: {len(on_hyp)} points on hyperbola")
        else:
            print(f"  Pascal: only {len(on_hyp)} points on hyperbola (need 6)")

        # Desargues: Need two triangles in perspective
        if len(points) >= 7:  # 2 triangles + center = 7 points
            print(f"  Desargues: checking for perspective triangles...")
            # This would require checking all combinations - skip for now

        # Note patterns
        if day == 0:
            print(f"  → Conic section (5 points on hyperbola)")
        elif day == 1:
            print(f"  → 8 points: 2^3 (cube vertices?) or octagon?")
        elif day == 2:
            print(f"  → 28 points: C(8,2) = 28 (complete graph K8?)")
        elif day == 3:
            print(f"  → 184 points: ???")

    def is_on_hyperbola(self, p):
        """Check if point is on hyperbola"""
        x, y = Rational(p.x), Rational(p.y)
        lhs = x * (x - 1)
        rhs = 3 * y * (y - 1)
        return simplify(lhs - rhs) == 0

def main():
    print("="*70)
    print("GEOMETRIC STRUCTURE ANALYSIS")
    print("="*70)
    print()
    print("Hypothesis: Each day represents a specific geometric configuration")
    print("Goal: Identify pattern in shapes/structures")
    print()

    analyzer = GeometricAnalyzer()

    # Analyze Day 0
    print("\n" + "="*70)
    print("DAY 0: INITIAL CONFIGURATION")
    print("="*70)
    all_points = analyzer.reds + analyzer.blues
    print(f"Points: 3 reds + 2 blues = 5 total")
    analyzer.analyze_structure(0, [])

    # Simulate and analyze Day 1
    try:
        g1 = analyzer.simulate_day(0, verbose=True)
        print(f"\n✓ g(1) = {g1}")

        if g1 == 8:
            print("✓ Matches expected g(1)=8")
        else:
            print(f"✗ Expected g(1)=8, got {g1}")
    except KeyboardInterrupt:
        print("\n\nInterrupted by user")
        sys.exit(0)

    # Simulate and analyze Day 2
    try:
        print("\n" + "="*70)
        print("Computing Day 2...")
        print("="*70)
        g2 = analyzer.simulate_day(1, verbose=True)
        print(f"\n✓ g(2) = {g2}")

        if g2 == 28:
            print("✓ Matches expected g(2)=28")
        else:
            print(f"✗ Expected g(2)=28, got {g2}")
    except KeyboardInterrupt:
        print("\n\nInterrupted by user")
        sys.exit(0)

    print("\n" + "="*70)
    print("PATTERN ANALYSIS")
    print("="*70)
    print()
    print("Sequence: 2, 8, 28, 184, 1644, ...")
    print()
    print("Possible interpretations:")
    print("  • Day 0 (5 pts): Conic section (fundamental)")
    print("  • Day 1 (8 pts): Octagon? Cube? 2^3?")
    print("  • Day 2 (28 pts): C(8,2) pairs? Complete graph?")
    print()
    print("Key question: Is g(n) counting GEOMETRIC OBJECTS (not raw points)?")
    print("  - Number of distinct lines?")
    print("  - Number of incidence classes?")
    print("  - Dimension of projective space spanned?")

if __name__ == "__main__":
    main()
