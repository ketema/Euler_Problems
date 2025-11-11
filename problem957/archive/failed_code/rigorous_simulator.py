"""
RIGOROUS Problem 957 Simulator

Uses proper geometric computation, state tracking, and constraint enforcement.
No pattern-matching - derive truth from first principles.
"""

from shapely.geometry import Point, LineString
from shapely import equals
import numpy as np
from typing import Set, Tuple, List
from collections import defaultdict

class Problem957State:
    """Explicit state machine for Problem 957."""

    def __init__(self, red_coords: List[Tuple[float, float]],
                 blue_coords: List[Tuple[float, float]]):
        """
        Initialize with exact coordinates.

        red_coords: 3 red points (stay red forever)
        blue_coords: 2 initial blue points
        """
        # Use Shapely Points for deterministic equality
        self.reds = [Point(x, y) for x, y in red_coords]
        self.blues = [Point(x, y) for x, y in blue_coords]

        assert len(self.reds) == 3, "Must have exactly 3 red points"
        assert len(self.blues) == 2, "Must have exactly 2 initial blue points"

        # Verify no duplicates
        assert len(set(self.reds)) == 3, "Red points must be distinct"
        assert len(set(self.blues)) == 2, "Blue points must be distinct"

        # Verify reds and blues don't overlap
        for r in self.reds:
            for b in self.blues:
                assert not equals(r, b), f"Red {r} overlaps blue {b}"

        self.day = 0

    def g(self) -> int:
        """Return current number of blue points."""
        return len(self.blues)

    def step(self) -> int:
        """
        Execute one day step according to problem rules:

        1. Construct ALL lines through (one red, one blue)
        2. Find ALL intersections of DIFFERENT such lines
        3. Turn intersection points blue IFF they are currently white

        Returns: number of NEW blue points created
        """
        # Step 1: Construct all red-blue lines
        lines = []
        for r in self.reds:
            for b in self.blues:
                line = LineString([r, b])
                lines.append((line, r, b))

        print(f"  Day {self.day}: {len(lines)} lines constructed")

        # Step 2: Find all intersections of DIFFERENT lines
        candidate_points = set()

        for i, (line1, r1, b1) in enumerate(lines):
            for line2, r2, b2 in lines[i+1:]:
                # Lines must be different
                if equals(line1, line2):
                    continue

                # Compute intersection using Shapely (exact, deterministic)
                intersection = line1.intersection(line2)

                # Shapely returns different types depending on intersection
                if intersection.is_empty:
                    continue
                elif intersection.geom_type == 'Point':
                    candidate_points.add(intersection)
                elif intersection.geom_type == 'LineString':
                    # Lines are identical (shouldn't happen with distinct lines)
                    continue
                elif intersection.geom_type == 'MultiPoint':
                    for pt in intersection.geoms:
                        candidate_points.add(pt)

        print(f"    {len(candidate_points)} intersection points found")

        # Step 3: Filter to WHITE points only
        new_blues = []

        for pt in candidate_points:
            # Check if point is currently white
            is_red = any(equals(pt, r) for r in self.reds)
            is_blue = any(equals(pt, b) for b in self.blues)

            if not is_red and not is_blue:
                # Point is white → turns blue
                new_blues.append(pt)

        print(f"    {len(new_blues)} are white → turn blue")

        # Update state
        self.blues.extend(new_blues)
        self.day += 1

        return len(new_blues)

    def simulate(self, max_days: int) -> List[int]:
        """
        Simulate for max_days and return sequence g(0), g(1), ..., g(max_days).
        """
        sequence = [self.g()]

        for _ in range(max_days):
            new_count = self.step()

            if new_count == 0:
                print(f"  No new blues created, stopping")
                break

            sequence.append(self.g())

        return sequence


def test_configuration(name: str, reds: List[Tuple], blues: List[Tuple],
                      expected_g1: int = None, expected_g2: int = None) -> List[int]:
    """Test a configuration and verify against expected values."""
    print(f"\n{'='*70}")
    print(f"Testing: {name}")
    print(f"{'='*70}")
    print(f"Reds:  {reds}")
    print(f"Blues: {blues}")
    print()

    state = Problem957State(reds, blues)
    sequence = state.simulate(max_days=4)

    print(f"\nSequence: {sequence}")

    # Verify against expected
    if expected_g1 is not None:
        if len(sequence) > 1:
            match = "✓" if sequence[1] == expected_g1 else "✗"
            print(f"  g(1) = {sequence[1]}, expected {expected_g1} {match}")

    if expected_g2 is not None:
        if len(sequence) > 2:
            match = "✓" if sequence[2] == expected_g2 else "✗"
            print(f"  g(2) = {sequence[2]}, expected {expected_g2} {match}")

    return sequence


if __name__ == "__main__":
    print("RIGOROUS SIMULATOR - Testing Configurations")
    print("="*70)

    # Test the configuration I found earlier
    test_configuration(
        "Previously found config",
        reds=[(0, 0), (1, 0), (0, 1)],
        blues=[(-2, -2), (-2, -1.5)],
        expected_g1=8,
        expected_g2=28
    )

    # Test simpler configurations
    test_configuration(
        "Right triangle + centered blues",
        reds=[(0, 0), (1, 0), (0, 1)],
        blues=[(0.3, 0.3), (0.5, 0.5)],
        expected_g1=8,
        expected_g2=28
    )
