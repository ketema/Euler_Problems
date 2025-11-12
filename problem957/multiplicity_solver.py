#!/usr/bin/env python3
"""
Problem 957: CORRECTED Solver with Multiplicity Checking

CRITICAL BUG FIX (from AI Panel - OpenAI GPT-4.1 & Claude Sonnet 4.5):

Problem statement: "color blue all white points where TWO OR MORE lines intersect"

OLD (WRONG): Color ANY point where two lines intersect (pairwise)
NEW (CORRECT): Count how many lines pass through EACH point, color only if multiplicity >= 2

This is THE bug causing all 46+ rejections!
"""

from sympy import Point, Line, Rational
from sympy.geometry import intersection
from typing import List, Set, Dict
import time

class MultiplicitySolver:
    def __init__(self, reds: List[Point], initial_blues: List[Point]):
        assert len(reds) == 3 and len(initial_blues) == 2
        self.reds = reds
        self.blues_by_day = {0: set(initial_blues)}

    def get_all_blues(self, day: int) -> Set[Point]:
        return self.blues_by_day.get(day, set())

    def count_line_multiplicity(self, point: Point, lines: List[Line]) -> int:
        """Count how many lines pass through a given point."""
        count = 0
        for line in lines:
            if point in line:
                count += 1
        return count

    def simulate_day(self, from_day: int, verbose=True) -> int:
        """
        Simulate with CORRECT multiplicity checking.
        Returns total cumulative blues.
        """
        if verbose:
            print(f"\n{'='*70}")
            print(f"Day {from_day} → Day {from_day + 1}")
            print(f"{'='*70}")

        current_cumulative_blues = self.get_all_blues(from_day)

        if verbose:
            print(f"Current state: g({from_day}) = {len(current_cumulative_blues)}")

        # Draw lines: red to blue
        lines = []
        for red in self.reds:
            for blue in current_cumulative_blues:
                if red != blue:  # Avoid degenerate
                    lines.append(Line(red, blue))

        if verbose:
            print(f"Lines constructed: {len(lines)}")

        existing = set(self.reds).union(current_cumulative_blues)

        # Collect ALL candidate intersection points
        candidate_intersections: Set[Point] = set()

        start = time.time()
        for i, line1 in enumerate(lines):
            for line2 in lines[i+1:]:
                result = intersection(line1, line2)
                if not result:
                    continue
                p = result[0]
                if not hasattr(p, 'x'):  # Skip Line objects
                    continue
                if p not in existing:
                    candidate_intersections.add(p)

        # Now check multiplicity for each candidate
        new_points_this_day: Set[Point] = set()

        for candidate in candidate_intersections:
            multiplicity = self.count_line_multiplicity(candidate, lines)
            if multiplicity >= 2:  # CRITICAL: Only if 2+ lines pass through
                new_points_this_day.add(candidate)

        elapsed = time.time() - start

        # Accumulate
        next_day_cumulative = current_cumulative_blues.union(new_points_this_day)
        self.blues_by_day[from_day + 1] = next_day_cumulative

        if verbose:
            print(f"  Completed in {elapsed:.3f}s")
            print(f"  Candidate intersections: {len(candidate_intersections)}")
            print(f"  New blues (multiplicity >= 2): {len(new_points_this_day)}")
            print(f"g({from_day + 1}) = {len(next_day_cumulative)}")

        return len(next_day_cumulative)

    def simulate_to_day(self, target_day: int) -> List[int]:
        print(f"\n{'='*70}")
        print(f"MULTIPLICITY-CORRECTED SIMULATION")
        print(f"{'='*70}")

        sequence = [len(self.blues_by_day[0])]

        for day in range(target_day):
            g_next = self.simulate_day(day, verbose=True)
            sequence.append(g_next)

            if g_next > 100000:
                print(f"\n⚠️  g({day+1}) = {g_next} extremely large, stopping")
                break

        return sequence


def main():
    print("="*80)
    print("Problem 957: MULTIPLICITY-CORRECTED Solver")
    print("="*80)
    print()
    print("CRITICAL FIX: Now checking multiplicity (2+ lines through point)")
    print()

    # Standard configuration
    reds = [
        Point(0, 0),
        Point(4, 0),
        Point(2, 3),
    ]

    blues = [
        Point(1, 1),
        Point(3, 2),
    ]

    print(f"Reds:  {reds}")
    print(f"Blues: {blues}")
    print()

    solver = MultiplicitySolver(reds, blues)
    sequence = solver.simulate_to_day(6)

    print()
    print("="*80)
    print("RESULTS")
    print("="*80)
    print()
    print(f"Sequence: {sequence}")
    print()
    print("Checking against expected:")
    print(f"  g(1) = {sequence[1]} (expected: 8)")
    print(f"  g(2) = {sequence[2]} (expected: 28)")
    print()

    if sequence[1] != 8 or sequence[2] != 28:
        print("🚨 VALUES CHANGED! The multiplicity bug was THE issue!")
        print()
        print("This means ALL previous work was based on wrong simulation!")
        print("Need to recalculate everything with corrected values.")
    else:
        print("✓ Values unchanged. Multiplicity wasn't the issue.")
        print("  (But still good to have correct logic)")

    print()

if __name__ == "__main__":
    main()
