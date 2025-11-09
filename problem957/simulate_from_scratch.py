#!/usr/bin/env python3
"""
Start COMPLETELY from scratch on Problem 957.

Problem statement (from web search):
- Start with 3 red points and 2 blue points
- Each day: draw lines through EVERY (red, blue) pair
- New blue points appear where two different such lines meet
- Count total blue points after 16 days

Let me compute g(1), g(2), g(3) directly through geometry.
"""

from fractions import Fraction
from itertools import combinations


class Point:
    """Exact rational point."""
    def __init__(self, x, y):
        self.x = Fraction(x)
        self.y = Fraction(y)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash((self.x, self.y))

    def __repr__(self):
        return f"({self.x}, {self.y})"


class Line:
    """Line through two points in form ax + by = c."""
    def __init__(self, p1, p2):
        if p1 == p2:
            raise ValueError("Cannot make line from single point")

        # Line through (x1,y1) and (x2,y2)
        dx = p2.x - p1.x
        dy = p2.y - p1.y

        # Normal vector: (-dy, dx)
        # Line equation: -dy(x - x1) + dx(y - y1) = 0
        # Simplified: -dy*x + dx*y = -dy*x1 + dx*y1

        self.a = -dy
        self.b = dx
        self.c = -dy * p1.x + dx * p1.y

    def __eq__(self, other):
        # Lines are equal if they're scalar multiples
        # Check if a1/a2 = b1/b2 = c1/c2
        if self.a == 0:
            return other.a == 0 and self.b * other.c == self.c * other.b
        ratio = other.a / self.a if self.a != 0 else None
        if ratio is None:
            return False
        return other.b == ratio * self.b and other.c == ratio * self.c

    def __hash__(self):
        # Normalize so that first non-zero coeff is positive
        if self.a != 0:
            return hash((1, self.b/self.a, self.c/self.a))
        elif self.b != 0:
            return hash((0, 1, self.c/self.b))
        else:
            return hash((0, 0, 1))

    def intersect(self, other):
        """Find intersection point of two lines."""
        # Solve: a1*x + b1*y = c1
        #        a2*x + b2*y = c2

        det = self.a * other.b - self.b * other.a
        if det == 0:
            return None  # Parallel lines

        x = (self.c * other.b - self.b * other.c) / det
        y = (self.a * other.c - self.c * other.a) / det

        return Point(x, y)


def simulate_day(reds, blues):
    """
    Simulate one day:
    - Draw all lines through (red, blue) pairs
    - Find all intersections of distinct lines
    - Add new points to blues

    Returns: new blues set
    """
    # Draw all (red, blue) lines
    lines = []
    for r in reds:
        for b in blues:
            try:
                line = Line(r, b)
                lines.append(line)
            except ValueError:
                pass  # Same point

    # Find all pairwise intersections
    new_blues = set(blues)  # Start with existing blues

    for i, line1 in enumerate(lines):
        for line2 in lines[i+1:]:
            if line1 != line2:  # Different lines
                pt = line1.intersect(line2)
                if pt is not None:
                    new_blues.add(pt)

    return new_blues


def main():
    """Simulate Problem 957 from scratch."""
    print("="*70)
    print("PROBLEM 957: POINT GENESIS - FROM SCRATCH SIMULATION")
    print("="*70)
    print()

    # Start with 3 red points and 2 blue points
    # Need to choose specific coordinates - try simple configuration
    reds = {
        Point(0, 0),
        Point(1, 0),
        Point(0, 1)
    }

    blues = {
        Point(1, 1),
        Point(2, 2)
    }

    print(f"Day 0: {len(blues)} blue points")
    print(f"  Reds: {reds}")
    print(f"  Blues: {blues}")
    print()

    # Simulate days 1-5
    for day in range(1, 6):
        blues = simulate_day(reds, blues)
        print(f"Day {day}: {len(blues)} blue points")

        if day <= 2:
            print(f"  Blues: {blues}")
        print()

        if len(blues) > 10000:
            print("Stopping early - too many points for simple simulation")
            break

    print("="*70)
    print("OBSERVATIONS:")
    print("="*70)
    print()
    print("The actual answer depends on the SPECIFIC configuration of")
    print("the initial 3 red and 2 blue points.")
    print()
    print("I was pulling numbers from OEIS without verifying they match")
    print("the specific configuration asked in the problem.")
    print()
    print("Need to:")
    print("1. Find the exact initial configuration from the problem")
    print("2. Simulate it properly")
    print("3. Get the actual answer, not a guess from OEIS")


if __name__ == "__main__":
    main()
