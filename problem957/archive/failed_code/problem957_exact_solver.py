#!/usr/bin/env python3
"""
Problem 957: Exact Deterministic Solver

PROBLEM STATEMENT:
- Initially: 3 red points (fixed) + 2 blue points (initial)
- Each day:
  1. Draw all lines through (one red, one blue)
  2. Find intersections of two DIFFERENT such lines
  3. White points at intersections turn blue
- g(n) = MAXIMAL possible number of blue points after n days
- Given: g(1) = 8, g(2) = 28
- Find: g(16)

APPROACH:
- Use exact rational geometry (SymPy)
- Test configurations to find one that maximizes blue points
- Verify g(1) = 8, g(2) = 28
- Infer recurrence relation
- Compute g(16) symbolically
"""

from sympy import Point, Line, Rational, simplify
from sympy.geometry import intersection
from itertools import combinations
from typing import List, Set, Tuple
import time

class ExactGeometricSolver:
    def __init__(self, reds: List[Point], blues: List[Point]):
        """
        Initialize with 3 red points and 2 blue points.
        All coordinates must be exact rationals.
        """
        assert len(reds) == 3, "Must have exactly 3 red points"
        assert len(blues) == 2, "Must have exactly 2 blue points initially"

        self.reds = reds
        self.blues_by_day = {0: blues}
        self.current_day = 0

    def get_all_blues(self, day: int) -> List[Point]:
        """Get all blue points accumulated up to given day."""
        all_blues = []
        for d in range(day + 1):
            if d in self.blues_by_day:
                all_blues.extend(self.blues_by_day[d])
        return all_blues

    def point_to_tuple(self, p: Point) -> Tuple[Rational, Rational]:
        """Convert point to hashable tuple for deduplication."""
        return (Rational(p.x), Rational(p.y))

    def simulate_day(self, from_day: int) -> int:
        """
        Simulate transition from_day → from_day + 1.
        Returns number of new blue points created.
        """
        print(f"\n{'='*70}")
        print(f"Day {from_day} → Day {from_day + 1}")
        print(f"{'='*70}")

        # Get all current blue points
        current_blues = self.get_all_blues(from_day)
        print(f"Starting blues: g({from_day}) = {len(current_blues)}")

        # Build set of existing points (for fast lookup)
        existing_points = set()
        for r in self.reds:
            existing_points.add(self.point_to_tuple(r))
        for b in current_blues:
            existing_points.add(self.point_to_tuple(b))

        # Step 1: Draw all lines through (red, blue) pairs
        lines = []
        for r in self.reds:
            for b in current_blues:
                line = Line(r, b)
                lines.append(line)

        print(f"Lines constructed: {len(lines)}")

        # Step 2: Find intersections of two DIFFERENT lines
        new_points = []
        new_points_set = set()

        total_pairs = len(lines) * (len(lines) - 1) // 2
        print(f"Checking {total_pairs} line pairs...")

        start_time = time.time()
        checked = 0

        for i, line1 in enumerate(lines):
            for line2 in lines[i+1:]:
                checked += 1
                if checked % 100 == 0:
                    elapsed = time.time() - start_time
                    rate = checked / elapsed if elapsed > 0 else 0
                    print(f"  Progress: {checked}/{total_pairs} ({rate:.0f} pairs/sec)  ", end='\r')

                # Check if lines are different (not the same line)
                if line1.equals(line2):
                    continue

                # Find intersection
                result = intersection(line1, line2)

                if not result:
                    # Parallel lines (shouldn't happen with general position)
                    continue

                # Extract intersection point
                p = result[0]

                # Check if it's a new point
                p_tuple = self.point_to_tuple(p)

                if p_tuple not in existing_points and p_tuple not in new_points_set:
                    new_points.append(p)
                    new_points_set.add(p_tuple)

        elapsed = time.time() - start_time
        print(f"\n  Completed in {elapsed:.2f} seconds")

        # Store new blues
        self.blues_by_day[from_day + 1] = new_points

        print(f"New blues: {len(new_points)}")
        print(f"g({from_day + 1}) = {len(self.get_all_blues(from_day + 1))}")

        return len(new_points)

    def simulate_to_day(self, target_day: int) -> List[int]:
        """
        Simulate from day 0 to target_day.
        Returns sequence [g(0), g(1), ..., g(target_day)]
        """
        sequence = [len(self.blues_by_day[0])]  # g(0)

        for day in range(target_day):
            _ = self.simulate_day(day)
            g_next = len(self.get_all_blues(day + 1))
            sequence.append(g_next)

            # Safety check
            if g_next > 5000:
                print(f"\n⚠️  g({day+1}) = {g_next} is very large, stopping.")
                break

        return sequence


def find_quadratic_formula(sequence: List[int]) -> Tuple[Rational, Rational, Rational]:
    """
    Fit g(t) = a*t² + b*t + c to the sequence.
    Uses first three values: g(0), g(1), g(2).
    """
    if len(sequence) < 3:
        raise ValueError("Need at least 3 values")

    g0, g1, g2 = sequence[0], sequence[1], sequence[2]

    # g(0) = c
    c = Rational(g0)

    # g(1) = a + b + c
    # g(2) = 4a + 2b + c
    # => a + b = g1 - c
    # => 4a + 2b = g2 - c
    # => 2(a + b) = 2(g1 - c)
    # => 4a + 2b = g2 - c
    # => 2a = (g2 - c) - 2(g1 - c) = g2 - 2*g1 + c

    a = Rational(g2 - 2*g1 + g0, 2)
    b = Rational(g1 - g0) - a

    return a, b, c


def verify_formula(sequence: List[int], a: Rational, b: Rational, c: Rational) -> bool:
    """Verify that formula matches all computed values."""
    for t, actual in enumerate(sequence):
        predicted = a * t**2 + b * t + c
        if predicted != actual:
            return False
    return True


def analyze_sequence(sequence: List[int]):
    """Analyze sequence and extract recurrence relation."""
    print(f"\n{'='*70}")
    print(f"SEQUENCE ANALYSIS")
    print(f"{'='*70}")

    print(f"\nComputed sequence: {sequence}")

    # Compute differences
    print(f"\n{'Day':<5} {'g(t)':<8} {'Δg':<8} {'Δ²g':<8}")
    print("-" * 35)

    deltas = []
    second_deltas = []

    for i, val in enumerate(sequence):
        delta_str = ""
        second_delta_str = ""

        if i > 0:
            delta = val - sequence[i-1]
            deltas.append(delta)
            delta_str = str(delta)

            if i > 1:
                second_delta = delta - deltas[i-2]
                second_deltas.append(second_delta)
                second_delta_str = str(second_delta)

        print(f"{i:<5} {val:<8} {delta_str:<8} {second_delta_str:<8}")

    # Check if quadratic
    if len(second_deltas) > 0:
        if all(d == second_deltas[0] for d in second_deltas):
            print(f"\n✓ Second differences are CONSTANT = {second_deltas[0]}")
            print(f"  This confirms QUADRATIC growth!")

            # Fit formula
            a, b, c = find_quadratic_formula(sequence)

            print(f"\nFitted formula:")
            print(f"  g(t) = {a}·t² + {b}·t + {c}")

            # Simplify for display
            if a == int(a):
                a_str = str(int(a))
            else:
                a_str = str(a)
            if b == int(b):
                b_str = str(int(b)) if b >= 0 else str(int(b))
            else:
                b_str = str(b)
            if c == int(c):
                c_str = str(int(c))
            else:
                c_str = str(c)

            b_sign = "+" if b >= 0 else ""
            c_sign = "+" if c >= 0 else ""
            print(f"  g(t) = {a_str}t² {b_sign}{b_str}t {c_sign}{c_str}")

            # Verify
            print(f"\nVerification:")
            all_match = True
            for i in range(len(sequence)):
                predicted = a * i**2 + b * i + c
                actual = sequence[i]
                match = "✓" if predicted == actual else "✗"
                print(f"  g({i}) = {predicted} (actual: {actual}) {match}")
                if predicted != actual:
                    all_match = False

            if all_match:
                print(f"\n✓✓✓ Formula PERFECTLY matches all computed values!")

                # Compute g(16)
                g_16 = a * 16**2 + b * 16 + c
                print(f"\n{'='*70}")
                print(f"FINAL ANSWER")
                print(f"{'='*70}")
                print(f"g(16) = {a_str}·(16)² {b_sign}{b_str}·(16) {c_sign}{c_str}")
                print(f"g(16) = {a_str}·256 {b_sign}{16*b} {c_sign}{c_str}")
                print(f"g(16) = {256*a} {b_sign}{16*b} {c_sign}{c_str}")
                print(f"g(16) = {int(g_16)}")

                return {'a': a, 'b': b, 'c': c, 'g_16': g_16}

    return None


def test_configuration(config_name: str, reds: List[Point], blues: List[Point], target_days: int = 3):
    """Test a specific configuration."""
    print(f"\n{'='*70}")
    print(f"Testing Configuration: {config_name}")
    print(f"{'='*70}")

    print(f"\nRed points:")
    for i, r in enumerate(reds, 1):
        print(f"  r{i} = ({r.x}, {r.y})")

    print(f"\nInitial blue points:")
    for i, b in enumerate(blues, 1):
        print(f"  b{i} = ({b.x}, {b.y})")

    # Create solver
    solver = ExactGeometricSolver(reds, blues)

    # Simulate
    sequence = solver.simulate_to_day(target_days)

    # Check if we got g(1) = 8 and g(2) = 28
    if len(sequence) >= 2:
        if sequence[1] == 8:
            print(f"\n✓✓✓ g(1) = 8 MATCHES!")
        else:
            print(f"\n✗✗✗ g(1) = {sequence[1]}, expected 8")

    if len(sequence) >= 3:
        if sequence[2] == 28:
            print(f"✓✓✓ g(2) = 28 MATCHES!")
        else:
            print(f"✗✗✗ g(2) = {sequence[2]}, expected 28")

    # Analyze
    result = analyze_sequence(sequence)

    return sequence, result


def main():
    print("="*70)
    print("Problem 957: Deterministic Exact Solver")
    print("="*70)
    print("\nObjective: Find configuration with g(1)=8, g(2)=28")
    print("Then compute g(16) using inferred formula")

    # Configuration 1: Triangle with interior points
    config1_reds = [
        Point(Rational(0), Rational(0)),
        Point(Rational(4), Rational(0)),
        Point(Rational(2), Rational(3)),
    ]
    config1_blues = [
        Point(Rational(2), Rational(1)),
        Point(Rational(2), Rational(2)),
    ]

    seq1, result1 = test_configuration("Triangle + Interior", config1_reds, config1_blues)

    # If first config doesn't work, try another
    if seq1[1] != 8:
        print(f"\n{'='*70}")
        print("Trying alternative configuration...")
        print(f"{'='*70}")

        # Configuration 2: Different spacing
        config2_reds = [
            Point(Rational(0), Rational(0)),
            Point(Rational(6), Rational(0)),
            Point(Rational(3), Rational(5)),
        ]
        config2_blues = [
            Point(Rational(2), Rational(1)),
            Point(Rational(4), Rational(2)),
        ]

        seq2, result2 = test_configuration("Wider Triangle", config2_reds, config2_blues)

    print(f"\n{'='*70}")
    print("GEOMETRIC INVARIANT")
    print(f"{'='*70}")
    print("""
The growth is driven by:
1. Complete bipartite graph K(3, g(t)) - all lines from 3 reds to g(t) blues
2. Each day adds intersections of these lines
3. Quadratic growth arises from:
   - Number of lines: 3 × g(t)
   - Number of line pairs: ~(3·g(t))² / 2
   - Intersections grow as ~g(t)²
   - After subtracting existing points: Δg(t) ≈ linear in t
   - Cumulative: g(t) ≈ quadratic in t
""")


if __name__ == "__main__":
    main()
