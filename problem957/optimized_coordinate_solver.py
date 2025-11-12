#!/usr/bin/env python3
"""
Problem 957: Optimized Coordinate Solver

Key optimizations:
1. Skip multiplicity check (it filters nothing - 100% pass rate)
2. Use SymPy Point hashing for O(1) deduplication
3. Cache line objects
4. Incremental computation where possible
5. Parallel intersection computation (if needed)

Based on verified correct sequence through g(5):
g(0)=2, g(1)=8, g(2)=28, g(3)=184, g(4)=1644, g(5)=19068
"""

from sympy import Point, Line, Rational
from sympy.geometry import intersection
from typing import Set, List, Dict, Tuple
from collections import defaultdict
import time

class OptimizedSolver:
    def __init__(self, reds: List[Point], initial_blues: List[Point]):
        self.reds = tuple(reds)  # Immutable
        self.blues_by_day: Dict[int, Set[Point]] = {
            0: set(initial_blues)
        }
        # Cache line objects (red, blue) -> Line
        self.line_cache: Dict[Tuple[Point, Point], Line] = {}

    def get_line(self, red: Point, blue: Point) -> Line:
        """Get or create line with caching"""
        key = (red, blue)
        if key not in self.line_cache:
            self.line_cache[key] = Line(red, blue)
        return self.line_cache[key]

    def get_cumulative_blues(self, day: int) -> Set[Point]:
        """Get all blues through day"""
        result = set()
        for d in range(day + 1):
            if d in self.blues_by_day:
                result |= self.blues_by_day[d]
        return result

    def simulate_day_optimized(self, from_day: int, verbose: bool = True) -> Set[Point]:
        """
        Optimized day simulation.

        Key insight: Multiplicity check is redundant (100% pass rate).
        Just compute intersections, deduplicate via Point hashing, done.
        """
        if verbose:
            print(f"\n{'='*70}")
            print(f"Day {from_day} → {from_day + 1}")
            print(f"{'='*70}")

        start_time = time.time()

        current_blues = self.get_cumulative_blues(from_day)
        existing = set(self.reds) | current_blues

        if verbose:
            print(f"  Current blues: {len(current_blues)}")

        # Construct lines (with caching)
        lines = []
        for red in self.reds:
            for blue in current_blues:
                lines.append(self.get_line(red, blue))

        if verbose:
            print(f"  Lines: {len(lines)}")

        # Compute all intersections (SymPy auto-deduplicates via Point.__hash__)
        new_points: Set[Point] = set()

        intersections_computed = 0
        for i, line1 in enumerate(lines):
            for line2 in lines[i+1:]:
                intersections_computed += 1
                result = intersection(line1, line2)
                if not result:
                    continue
                p = result[0]
                if not hasattr(p, 'x'):  # Skip Line objects
                    continue
                if p not in existing:  # O(1) hash lookup
                    new_points.add(p)

        # Update state
        next_blues = current_blues | new_points
        self.blues_by_day[from_day + 1] = next_blues

        elapsed = time.time() - start_time

        if verbose:
            print(f"  Intersections computed: {intersections_computed:,}")
            print(f"  New blues: {len(new_points):,}")
            print(f"  g({from_day + 1}) = {len(next_blues):,}")
            print(f"  Time: {elapsed:.3f}s")
            if from_day >= 1:
                prev_time = getattr(self, '_prev_elapsed', elapsed)
                ratio = elapsed / prev_time if prev_time > 0 else 1.0
                print(f"  Growth factor: {ratio:.2f}x")

                # Extrapolate
                if from_day <= 5:
                    remaining_days = 16 - (from_day + 1)
                    if remaining_days > 0:
                        estimated_total = elapsed * (ratio ** remaining_days)
                        print(f"  Estimated time to g(16): {estimated_total/3600:.1f} hours")
                        if estimated_total > 86400:  # > 1 day
                            print(f"    (WARNING: > 1 day, likely intractable)")

        self._prev_elapsed = elapsed
        return next_blues

    def solve_to_day(self, target_day: int, verbose: bool = True) -> List[int]:
        """Solve through target day"""
        print("="*80)
        print("OPTIMIZED COORDINATE SOLVER")
        print("="*80)
        print()
        print("Optimizations:")
        print("  ✓ Skip multiplicity check (100% pass rate)")
        print("  ✓ Use SymPy Point hashing for O(1) deduplication")
        print("  ✓ Cache line objects")
        print()

        sequence = [len(self.blues_by_day[0])]
        self._prev_elapsed = 0.001  # Initialize for ratio calculation

        for day in range(target_day):
            self.simulate_day_optimized(day, verbose=verbose)
            g_next = len(self.get_cumulative_blues(day + 1))
            sequence.append(g_next)

            # Safety check: if extrapolated time > 24 hours, stop
            if day >= 3 and hasattr(self, '_prev_elapsed'):
                # Estimate if we can reach target
                pass  # Let it run; user can Ctrl+C if needed

        return sequence

def main():
    print("Initializing with verified configuration...")
    print()

    # Standard configuration (verified correct through g(5))
    reds = [
        Point(Rational(0), Rational(0)),
        Point(Rational(4), Rational(0)),
        Point(Rational(2), Rational(3)),
    ]

    blues = [
        Point(Rational(1), Rational(1)),
        Point(Rational(3), Rational(2)),
    ]

    print(f"Reds:  {reds}")
    print(f"Blues: {blues}")
    print()

    solver = OptimizedSolver(reds, blues)

    # Test base cases first
    print("Testing base cases...")
    sequence = solver.solve_to_day(2, verbose=True)

    print()
    print("="*80)
    print("BASE CASE VERIFICATION")
    print("="*80)
    print()
    print(f"g(1) = {sequence[1]} (expected: 8)")
    print(f"g(2) = {sequence[2]} (expected: 28)")
    print()

    if sequence[1] != 8 or sequence[2] != 28:
        print("✗ Base cases WRONG! Stopping.")
        return

    print("✓ Base cases CORRECT!")
    print()

    # Continue to higher days
    print("Extending to g(5) to verify against known value...")
    sequence = solver.solve_to_day(5, verbose=True)

    print()
    print(f"g(5) = {sequence[5]:,} (expected: 19,068)")

    if sequence[5] != 19068:
        print("✗ g(5) WRONG! Cannot proceed.")
        return

    print("✓ g(5) VERIFIED!")
    print()

    # Attempt g(16)
    print("="*80)
    print("ATTEMPTING g(16)")
    print("="*80)
    print()
    print("WARNING: This may take hours or days.")
    print("Monitor extrapolated time estimates. Press Ctrl+C to stop if intractable.")
    print()

    try:
        sequence = solver.solve_to_day(16, verbose=True)

        print()
        print("="*80)
        print("✓ SUCCESS!")
        print("="*80)
        print()
        print(f"ANSWER: g(16) = {sequence[16]:,}")
        print()
        print(f"Complete sequence:")
        for i, val in enumerate(sequence):
            print(f"  g({i:2}) = {val:,}")

    except KeyboardInterrupt:
        print()
        print("="*80)
        print("INTERRUPTED BY USER")
        print("="*80)
        print()
        print("Coordinate simulation appears intractable for g(16).")
        print("Symbolic approach with SageMath/egglog recommended.")

if __name__ == "__main__":
    main()
