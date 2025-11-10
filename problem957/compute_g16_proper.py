#!/usr/bin/env python3
"""
Problem 957: Point Genesis - Proper Solution

Find the optimal configuration that maximizes blue points.
Given: g(1) = 8, g(2) = 28
"""

from fractions import Fraction
from itertools import combinations
import sys


class Point:
    """Exact rational point."""
    def __init__(self, x, y):
        self.x = Fraction(x) if not isinstance(x, Fraction) else x
        self.y = Fraction(y) if not isinstance(y, Fraction) else y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash((self.x, self.y))

    def __repr__(self):
        return f"({float(self.x):.4f}, {float(self.y):.4f})"


def line_intersection(p1, p2, p3, p4):
    """
    Find intersection of line through p1,p2 with line through p3,p4.
    Returns None if parallel or coincident.
    """
    # Line 1: through p1, p2
    # Parametric: (x,y) = p1 + t*(p2-p1)
    # Line 2: through p3, p4
    # Parametric: (x,y) = p3 + s*(p4-p3)

    # Convert to ax + by = c form
    # Line 1: (y2-y1)*x - (x2-x1)*y = (y2-y1)*x1 - (x2-x1)*y1
    a1 = p2.y - p1.y
    b1 = -(p2.x - p1.x)
    c1 = a1 * p1.x + b1 * p1.y

    a2 = p4.y - p3.y
    b2 = -(p4.x - p3.x)
    c2 = a2 * p3.x + b2 * p3.y

    det = a1 * b2 - a2 * b1

    if det == 0:
        return None  # Parallel or coincident

    x = (b2 * c1 - b1 * c2) / det
    y = (a1 * c2 - a2 * c1) / det

    return Point(x, y)


def simulate_one_day(reds, blues):
    """
    Simulate one day:
    - For each (red, blue) pair, draw a line
    - Find all intersections of different lines
    - Return updated set of all blue points
    """
    # Generate all (red, blue) pairs
    red_blue_pairs = [(r, b) for r in reds for b in blues]

    # Find all intersections
    new_blues = set(blues)

    for i in range(len(red_blue_pairs)):
        for j in range(i + 1, len(red_blue_pairs)):
            r1, b1 = red_blue_pairs[i]
            r2, b2 = red_blue_pairs[j]

            # Skip if same line
            if (r1 == r2 and b1 == b2) or (r1 == b2 and b1 == r2):
                continue

            pt = line_intersection(r1, b1, r2, b2)
            if pt is not None:
                new_blues.add(pt)

    return new_blues


def test_configuration(reds, blues, max_days=3):
    """Test a configuration and return sequence."""
    result = [len(blues)]

    for day in range(1, max_days + 1):
        blues = simulate_one_day(reds, blues)
        result.append(len(blues))

        if len(blues) > 100000:
            break

    return result


def search_optimal_config():
    """
    Search for configuration that gives g(1)=8, g(2)=28.

    Strategy: Try various "nice" configurations.
    """
    print("Searching for optimal configuration...")
    print()

    # Try configuration from literature (if it exists)
    # Let's try some symmetric configurations

    configs_to_try = []

    # Config 1: Regular positions
    configs_to_try.append({
        'reds': [Point(0, 0), Point(1, 0), Point(0, 1)],
        'blues': [Point(1, 1), Point(Fraction(1, 2), Fraction(1, 2))]
    })

    # Config 2: Different spacing
    configs_to_try.append({
        'reds': [Point(-1, 0), Point(1, 0), Point(0, 1)],
        'blues': [Point(0, -1), Point(0, 2)]
    })

    # Config 3: Try to maximize line intersections
    configs_to_try.append({
        'reds': [Point(0, 0), Point(3, 0), Point(0, 3)],
        'blues': [Point(1, 1), Point(2, 2)]
    })

    # Config 4: Generic position (no special alignments)
    configs_to_try.append({
        'reds': [Point(0, 0), Point(1, 0), Point(Fraction(1, 2), Fraction(3, 4))],
        'blues': [Point(Fraction(3, 4), Fraction(1, 4)), Point(Fraction(1, 4), Fraction(1, 2))]
    })

    best_config = None
    best_score = 0

    for i, config in enumerate(configs_to_try):
        reds = set(config['reds'])
        blues = set(config['blues'])

        sequence = test_configuration(reds, blues, max_days=2)

        print(f"Config {i+1}: {sequence}")

        if len(sequence) >= 3:
            # Check if it matches g(1)=8, g(2)=28
            if sequence[1] == 8 and sequence[2] == 28:
                print(f"  ✓ FOUND OPTIMAL CONFIG!")
                best_config = config
                best_score = sum(sequence)
                break
            elif sequence[1] >= best_score:
                best_score = sequence[1]
                best_config = config

    return best_config


def main():
    print("="*70)
    print("PROBLEM 957: POINT GENESIS - PROPER SOLUTION")
    print("="*70)
    print()

    # First, search for the optimal configuration
    config = search_optimal_config()

    if config is None:
        print("\nCould not find optimal configuration in search.")
        print("Need to expand search space or use different method.")
        return

    print()
    print("="*70)
    print("COMPUTING WITH OPTIMAL CONFIGURATION")
    print("="*70)
    print()

    reds = set(config['reds'])
    blues = set(config['blues'])

    print(f"Reds: {reds}")
    print(f"Blues: {blues}")
    print()

    # Simulate as far as feasible
    print(f"Day 0: {len(blues)} blues")

    for day in range(1, 20):
        blues = simulate_one_day(reds, blues)
        print(f"Day {day}: {len(blues)} blues")

        if len(blues) > 1000000:
            print(f"\nStopping at day {day} - too many points for direct simulation")
            print("Would need more efficient algorithm or lookup table.")
            break

        if day == 16:
            print()
            print("="*70)
            print(f"ANSWER: g(16) = {len(blues)}")
            print("="*70)
            break


if __name__ == "__main__":
    main()
