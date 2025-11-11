#!/usr/bin/env python3
"""
Problem 957: Deterministic Exact Solver
Using exact rational geometry with SymPy

PROBLEM STATEMENT (from research):
- 5 red points (fixed vertices)
- Start with 2 blue points
- Each day: Draw segments from each red to all blues
- New blues = intersection points of segments from DIFFERENT red vertices
- g(t) = number of blue points on day t

TARGET:
- Verify g(1) = 8, g(2) = 28
- Find recurrence relation
- Compute g(16)
"""

from sympy import Point, Line, Rational, intersection
from sympy.geometry import Segment
from itertools import combinations
from fractions import Fraction
from typing import List, Set, Tuple, Dict
import time

class ExactSolver:
    def __init__(self):
        """Initialize with 5 red points in general position"""
        # Use exact rational coordinates
        # Place 5 reds in convex position (pentagon)
        self.reds = [
            Point(Rational(0), Rational(0)),
            Point(Rational(5), Rational(0)),
            Point(Rational(6), Rational(4)),
            Point(Rational(2), Rational(6)),
            Point(Rational(-2), Rational(3)),
        ]

        # Initial 2 blue points (inside the convex hull)
        self.blues_by_day = {
            0: [
                Point(Rational(2), Rational(2)),
                Point(Rational(3), Rational(3)),
            ]
        }

        # Track history
        self.history = []
        self.current_day = 0

    def get_all_blues(self, day: int) -> List[Point]:
        """Get all blue points up to given day"""
        all_blues = []
        for d in range(day + 1):
            if d in self.blues_by_day:
                all_blues.extend(self.blues_by_day[d])
        return all_blues

    def point_to_tuple(self, p: Point) -> Tuple:
        """Convert point to hashable tuple"""
        return (Rational(p.x), Rational(p.y))

    def segments_intersect_at_new_point(
        self,
        seg1: Tuple[Point, Point],
        seg2: Tuple[Point, Point],
        existing_points: Set[Tuple]
    ) -> Tuple[bool, Point]:
        """
        Check if two segments intersect at a new point.
        Returns (True, intersection_point) if they do, (False, None) otherwise.

        Segments must be from DIFFERENT red vertices.
        """
        r1, b1 = seg1
        r2, b2 = seg2

        # Segments must be from different reds
        if r1 == r2:
            return False, None

        # Create lines (infinite)
        line1 = Line(r1, b1)
        line2 = Line(r2, b2)

        # Find intersection
        result = intersection(line1, line2)

        if not result:
            return False, None  # Parallel (shouldn't happen in general position)

        p = result[0]

        # Check if intersection is a new point
        p_tuple = self.point_to_tuple(p)
        if p_tuple in existing_points:
            return False, None

        # Check if intersection is in the interior of both segments
        # For Problem 957, we only count crossings in the interior
        seg1_obj = Segment(r1, b1)
        seg2_obj = Segment(r2, b2)

        # Point must be strictly in the interior of both segments
        # (not at endpoints)
        if p == r1 or p == b1 or p == r2 or p == b2:
            return False, None

        # Check if point lies on segments
        if p in seg1_obj and p in seg2_obj:
            return True, p

        return False, None

    def simulate_day(self, day: int) -> int:
        """
        Simulate one day: day -> day+1
        Returns number of new blues created
        """
        print(f"\n{'='*70}")
        print(f"Simulating Day {day} → Day {day+1}")
        print(f"{'='*70}")

        # Get all blues up to current day
        all_blues = self.get_all_blues(day)
        print(f"Current blues: {len(all_blues)}")
        print(f"Current g({day}) = {len(all_blues)}")

        # Build set of existing points for fast lookup
        existing_points = set()
        for r in self.reds:
            existing_points.add(self.point_to_tuple(r))
        for b in all_blues:
            existing_points.add(self.point_to_tuple(b))

        print(f"Total existing points: {len(existing_points)}")

        # Generate all segments from reds to blues
        segments = []
        for r in self.reds:
            for b in all_blues:
                segments.append((r, b))

        print(f"Total segments: {len(segments)}")

        # Find all intersections
        new_points = []
        new_points_set = set()

        total_pairs = len(segments) * (len(segments) - 1) // 2
        print(f"Checking {total_pairs} segment pairs...")

        start_time = time.time()
        checked = 0

        for i, seg1 in enumerate(segments):
            for seg2 in segments[i+1:]:
                checked += 1
                if checked % 100 == 0:
                    elapsed = time.time() - start_time
                    rate = checked / elapsed if elapsed > 0 else 0
                    print(f"  Checked {checked}/{total_pairs} pairs ({rate:.0f} pairs/sec)", end='\r')

                intersects, p = self.segments_intersect_at_new_point(
                    seg1, seg2, existing_points
                )

                if intersects:
                    p_tuple = self.point_to_tuple(p)
                    if p_tuple not in new_points_set:
                        new_points.append(p)
                        new_points_set.add(p_tuple)

        elapsed = time.time() - start_time
        print(f"\n  Completed in {elapsed:.2f} seconds")

        # Store new blues
        self.blues_by_day[day + 1] = new_points

        print(f"\nNew blues created: {len(new_points)}")
        print(f"g({day+1}) = {len(self.get_all_blues(day + 1))}")

        return len(new_points)

    def simulate_to_day(self, target_day: int) -> List[int]:
        """
        Simulate from day 0 to target_day
        Returns list of g(t) for t = 0, 1, ..., target_day
        """
        results = [len(self.blues_by_day[0])]  # g(0) = 2

        for day in range(target_day):
            new_count = self.simulate_day(day)
            g_next = len(self.get_all_blues(day + 1))
            results.append(g_next)

            # Safety check
            if g_next > 10000:
                print(f"\n⚠️  WARNING: g({day+1}) = {g_next} is very large!")
                print(f"   Stopping simulation for computational reasons.")
                break

        return results


def analyze_sequence(sequence: List[int]) -> Dict:
    """
    Analyze the sequence to find patterns and recurrence relations.
    """
    print(f"\n{'='*70}")
    print(f"SEQUENCE ANALYSIS")
    print(f"{'='*70}")

    n = len(sequence)
    print(f"\nSequence: {sequence}")
    print(f"\nDay | g(t) | Δg (added) | Δ²g (accel)")
    print(f"----+------+------------+-----------")

    deltas = []
    second_deltas = []

    for i, val in enumerate(sequence):
        delta_str = ""
        accel_str = ""

        if i > 0:
            delta = val - sequence[i-1]
            deltas.append(delta)
            delta_str = f"{delta:4d}"

            if i > 1:
                accel = delta - deltas[i-2]
                second_deltas.append(accel)
                accel_str = f"{accel:4d}"

        print(f"{i:3d} | {val:4d} | {delta_str:10s} | {accel_str:9s}")

    # Check if second differences are constant (quadratic)
    if len(second_deltas) > 0:
        if all(d == second_deltas[0] for d in second_deltas):
            print(f"\n✓ Second differences are CONSTANT = {second_deltas[0]}")
            print(f"  This indicates a QUADRATIC recurrence!")

            # Fit g(t) = a*t² + b*t + c
            if len(sequence) >= 3:
                g0, g1, g2 = sequence[0], sequence[1], sequence[2]
                # g(0) = c = g0
                c = g0
                # g(1) = a + b + c = g1
                # g(2) = 4a + 2b + c = g2
                # From these: a + b = g1 - c, 4a + 2b = g2 - c
                # => 2(a + b) = 2(g1 - c), 4a + 2b = g2 - c
                # => 2a = (g2 - c) - 2(g1 - c) = g2 - 2g1 + c
                a = Rational(g2 - 2*g1 + g0, 2)
                b = Rational(g1 - g0 - a)

                print(f"\n  Fitted formula: g(t) = {a}*t² + {b}*t + {c}")
                print(f"  Simplified: g(t) = {a}t² + ({b})t + {c}")

                # Verify fit
                print(f"\n  Verification:")
                for i in range(len(sequence)):
                    predicted = a * i**2 + b * i + c
                    actual = sequence[i]
                    match = "✓" if predicted == actual else "✗"
                    print(f"    g({i}) = {predicted} (actual: {actual}) {match}")

                return {
                    'type': 'quadratic',
                    'formula': f'{a}t² + {b}t + {c}',
                    'coefficients': (a, b, c),
                    'constant_second_diff': second_deltas[0]
                }

    return {'type': 'unknown'}


def main():
    print("="*70)
    print("Problem 957: Exact Deterministic Solver")
    print("="*70)
    print("\nObjective:")
    print("1. Simulate days 0→1, 1→2, ..., up to day 6")
    print("2. Verify g(1) = 8 and g(2) = 28")
    print("3. Infer recurrence relation")
    print("4. Compute g(16)")
    print("="*70)

    # Create solver
    solver = ExactSolver()

    print(f"\nInitial Configuration:")
    print(f"  Red points (5): {solver.reds}")
    print(f"  Blue points (2): {solver.blues_by_day[0]}")
    print(f"  g(0) = {len(solver.blues_by_day[0])}")

    # Simulate first few days
    target_day = 3  # Start with 3 days to see the pattern

    sequence = solver.simulate_to_day(target_day)

    # Analyze sequence
    analysis = analyze_sequence(sequence)

    # If we found a quadratic formula, use it to compute g(16)
    if analysis['type'] == 'quadratic':
        a, b, c = analysis['coefficients']
        g_16 = a * 16**2 + b * 16 + c

        print(f"\n{'='*70}")
        print(f"FINAL RESULTS")
        print(f"{'='*70}")
        print(f"\nClosed-form formula: g(t) = {a}t² + {b}t + {c}")
        print(f"\nVerification:")
        print(f"  g(0) = {c} ✓")
        print(f"  g(1) = {a + b + c} (expected: 8)")
        print(f"  g(2) = {4*a + 2*b + c} (expected: 28)")
        print(f"\nPrediction:")
        print(f"  g(16) = {g_16}")

        print(f"\n{'='*70}")
        print(f"GEOMETRIC INVARIANT")
        print(f"{'='*70}")
        print(f"The growth is driven by:")
        print(f"1. Complete bipartite graph K(5, g(t))")
        print(f"2. Crossing number in planar embedding")
        print(f"3. Cyclic order preservation around each red vertex")
        print(f"4. Δg(t) = #{crossings between segments from different reds}")
        print(f"5. Quadratic growth: Δg(t) ≈ 2a*t + b")
        print(f"   => g(t) ≈ a*t² + b*t + c")

    return sequence, analysis


if __name__ == "__main__":
    sequence, analysis = main()
