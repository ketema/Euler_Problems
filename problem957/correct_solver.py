#!/usr/bin/env python3
"""
Problem 957: CORRECT Deterministic Solver

CORRECT CONSTRUCTION RULE (key insight!):
- ONLY draw lines connecting (a red point) to (a blue point)
- NOT all lines between all pairs of points
- This is the difference that makes g(1)=8 correct

Algorithm:
1. Day 0: 3 reds + 2 blues
2. Draw 6 lines (3 reds × 2 blues = 6 red-blue connections)
3. Find intersections of these 6 lines
4. White points at intersections turn blue
5. Repeat with all accumulated blues
"""

from sympy import Point, Line, Rational
from sympy.geometry import intersection
from itertools import combinations
from typing import List, Set, Tuple
import time

class Problem957Solver:
    def __init__(self, reds: List[Point], initial_blues: List[Point]):
        """
        Initialize with 3 red points (fixed) and 2 blue points (initial).
        Reds never change, blues accumulate.
        """
        assert len(reds) == 3, "Must have exactly 3 red points"
        assert len(initial_blues) == 2, "Must have exactly 2 initial blue points"

        self.reds = reds
        self.blues_by_day = {0: initial_blues}

    def get_all_blues(self, day: int) -> List[Point]:
        """Get all blue points accumulated through given day"""
        all_blues = []
        for d in range(day + 1):
            if d in self.blues_by_day:
                all_blues.extend(self.blues_by_day[d])
        return all_blues

    def point_to_tuple(self, p: Point) -> Tuple[Rational, Rational]:
        """Convert to hashable tuple for deduplication"""
        return (Rational(p.x), Rational(p.y))

    def simulate_day(self, from_day: int, verbose=True) -> int:
        """
        Simulate transition: day from_day → day (from_day + 1)
        Returns number of NEW blue points created
        """
        if verbose:
            print(f"\n{'='*70}")
            print(f"Day {from_day} → Day {from_day + 1}")
            print(f"{'='*70}")

        # Get all current blues
        current_blues = self.get_all_blues(from_day)

        if verbose:
            print(f"Current state: g({from_day}) = {len(current_blues)}")
            print(f"  Reds: {len(self.reds)} (fixed)")
            print(f"  Blues: {len(current_blues)}")

        # CORRECT RULE: Draw lines ONLY from reds to blues
        lines = []
        for red in self.reds:
            for blue in current_blues:
                line = Line(red, blue)
                lines.append((red, blue, line))

        if verbose:
            print(f"Lines constructed: {len(lines)} (3 reds × {len(current_blues)} blues)")

        # Build set of existing points
        existing = set()
        for r in self.reds:
            existing.add(self.point_to_tuple(r))
        for b in current_blues:
            existing.add(self.point_to_tuple(b))

        # Find all intersections of pairs of lines
        new_points = []
        new_points_set = set()

        total_pairs = len(lines) * (len(lines) - 1) // 2

        if verbose:
            print(f"Checking {total_pairs} line pairs for intersections...")

        start = time.time()

        for i, (r1, b1, line1) in enumerate(lines):
            for (r2, b2, line2) in lines[i+1:]:
                # Find intersection
                result = intersection(line1, line2)

                if not result:
                    continue  # Parallel (shouldn't happen in general position)

                p = result[0]

                # Skip if result is a line (lines are identical)
                if not hasattr(p, 'x'):
                    continue

                p_tuple = self.point_to_tuple(p)

                # Check if it's a new point
                if p_tuple not in existing and p_tuple not in new_points_set:
                    new_points.append(p)
                    new_points_set.add(p_tuple)

        elapsed = time.time() - start

        if verbose:
            print(f"  Completed in {elapsed:.3f} seconds")
            print(f"New blues created: {len(new_points)}")
            print(f"g({from_day + 1}) = {len(current_blues) + len(new_points)}")

        # Store new blues for next day
        self.blues_by_day[from_day + 1] = new_points

        return len(new_points)

    def simulate_to_day(self, target_day: int) -> List[int]:
        """
        Simulate from day 0 to target_day.
        Returns sequence [g(0), g(1), ..., g(target_day)]
        """
        print(f"\n{'='*70}")
        print(f"SIMULATING DAYS 0 THROUGH {target_day}")
        print(f"{'='*70}")

        sequence = [len(self.blues_by_day[0])]  # g(0) = 2

        for day in range(target_day):
            new_count = self.simulate_day(day, verbose=True)
            g_next = len(self.get_all_blues(day + 1))
            sequence.append(g_next)

            # Safety check
            if g_next > 100000:
                print(f"\n⚠️  g({day+1}) = {g_next} is extremely large!")
                print(f"   Stopping simulation.")
                break

        return sequence


def analyze_sequence(sequence: List[int]):
    """Analyze sequence to find patterns and recurrence"""
    print(f"\n{'='*70}")
    print(f"SEQUENCE ANALYSIS")
    print(f"{'='*70}")

    print(f"\nComputed sequence: {sequence}")

    # Compute differences
    print(f"\n{'Day':<6} {'g(t)':<10} {'Δg(t)':<10} {'Δ²g(t)':<10}")
    print("-" * 40)

    deltas = []
    second_deltas = []

    for i, val in enumerate(sequence):
        delta_str = ""
        second_str = ""

        if i > 0:
            delta = val - sequence[i-1]
            deltas.append(delta)
            delta_str = str(delta)

            if i > 1:
                second_delta = delta - deltas[i-2]
                second_deltas.append(second_delta)
                second_str = str(second_delta)

        print(f"{i:<6} {val:<10} {delta_str:<10} {second_str:<10}")

    # Check for patterns
    if len(second_deltas) > 0 and all(d == second_deltas[0] for d in second_deltas):
        print(f"\n✓✓✓ Second differences are CONSTANT = {second_deltas[0]}")
        print(f"    This indicates QUADRATIC growth: g(t) = a·t² + b·t + c")

        # Fit formula
        if len(sequence) >= 3:
            g0, g1, g2 = sequence[0], sequence[1], sequence[2]
            c = Rational(g0)
            a = Rational(g2 - 2*g1 + g0, 2)
            b = Rational(g1 - g0) - a

            print(f"\nFitted formula: g(t) = {a}·t² + {b}·t + {c}")

            # Verify
            print(f"\nVerification:")
            all_match = True
            for i in range(len(sequence)):
                predicted = a * i**2 + b * i + c
                actual = sequence[i]
                match = "✓" if predicted == actual else "✗"
                print(f"  g({i}) = {predicted} {'='if predicted==actual else '≠'} {actual} {match}")
                if predicted != actual:
                    all_match = False

            if all_match:
                print(f"\n{'='*70}")
                print(f"FORMULA VERIFIED!")
                print(f"{'='*70}")
                print(f"\ng(t) = {a}·t² + {b}·t + {c}")

                # Compute g(16)
                g_16 = a * 16**2 + b * 16 + c

                print(f"\n{'='*70}")
                print(f"FINAL ANSWER")
                print(f"{'='*70}")
                print(f"\ng(16) = {a}·(16)² + {b}·(16) + {c}")
                print(f"g(16) = {a}·256 + {16*b} + {c}")
                print(f"g(16) = {256*a} + {16*b} + {c}")
                print(f"\ng(16) = {int(g_16)}")

                return {'a': a, 'b': b, 'c': c, 'g_16': int(g_16)}

    return None


def main():
    print("="*70)
    print("Problem 957: CORRECT Deterministic Solver")
    print("="*70)

    print("\nKey Insight: ONLY draw lines from reds to blues!")
    print("(Not all lines between all points)")

    # Configuration: 3 reds + 2 blues in general position
    # Using simple rational coordinates
    reds = [
        Point(Rational(0), Rational(0)),    # r1
        Point(Rational(4), Rational(0)),    # r2
        Point(Rational(2), Rational(3)),    # r3
    ]

    blues = [
        Point(Rational(1), Rational(1)),    # b1
        Point(Rational(3), Rational(2)),    # b2
    ]

    print(f"\nConfiguration:")
    print(f"  Reds:  {reds}")
    print(f"  Blues: {blues}")

    # Create solver
    solver = Problem957Solver(reds, blues)

    # Simulate to day 6 (to see pattern clearly)
    target_day = 6
    sequence = solver.simulate_to_day(target_day)

    # Analyze
    result = analyze_sequence(sequence)

    if result:
        print(f"\n{'='*70}")
        print(f"GEOMETRIC INVARIANT")
        print(f"{'='*70}")
        print(f"""
The growth is driven by:
1. Complete bipartite graph K(3, g(t))
   - 3 fixed red vertices
   - g(t) blue vertices (growing)

2. Lines: 3 × g(t) red-blue connections

3. New blues per day: Intersections of these lines
   - Line pairs: C(3·g(t), 2) ≈ (9/2)·g(t)²
   - After subtracting existing points
   - Δg(t) grows linearly with t
   - g(t) grows quadratically

4. The formula g(t) = {result['a']}·t² + {result['b']}·t + {result['c']}
   encodes this geometric structure exactly.
""")

    return sequence, result


if __name__ == "__main__":
    sequence, result = main()
