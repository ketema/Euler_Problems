"""
Direct geometric simulation with concrete coordinates
Find a configuration that gives g(1)=8, g(2)=28
"""

from fractions import Fraction
from itertools import combinations

def line_intersection(p1, p2, p3, p4):
    """
    Find intersection of line through p1,p2 and line through p3,p4.
    Returns None if parallel or coincident.
    Uses exact rational arithmetic.
    """
    x1, y1 = p1
    x2, y2 = p2
    x3, y3 = p3
    x4, y4 = p4

    denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)

    if denom == 0:
        return None  # Parallel or coincident

    t = ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)) / denom

    x = x1 + t*(x2-x1)
    y = y1 + t*(y2-y1)

    return (x, y)

def simulate_day(reds, blues):
    """Simulate one day: draw all lines from reds to blues, find crossings."""
    lines = []
    for r in reds:
        for b in blues:
            lines.append((r, b, f"({r},{b})"))

    new_points = set()

    for i, (r1, b1, label1) in enumerate(lines):
        for r2, b2, label2 in lines[i+1:]:
            # Skip if same red or same blue (they meet at existing point)
            if r1 == r2 or b1 == b2:
                continue

            intersection = line_intersection(r1, b1, r2, b2)

            if intersection and intersection not in blues and intersection not in reds:
                new_points.add(intersection)

    return new_points

# Try configuration in general position
print("="*70)
print("Geometric simulation with concrete coordinates")
print("="*70)
print()

# Try: 3 reds in general position, 2 blues in general position
reds = [
    (Fraction(0), Fraction(0)),
    (Fraction(1), Fraction(0)),
    (Fraction(0), Fraction(1)),
]

blues = [
    (Fraction(2), Fraction(2)),
    (Fraction(3), Fraction(1)),
]

print("Initial configuration:")
print(f"  Reds: {reds}")
print(f"  Blues: {blues}")
print(f"  g(0) = {len(blues)}")
print()

# Day 1
new_day1 = simulate_day(reds, blues)
print(f"Day 1:")
print(f"  New blues: {len(new_day1)}")
print(f"  g(1) = {len(blues) + len(new_day1)}")
print()

if len(new_day1) == 6:
    print("  ✓ Matches g(1) = 8!")
    print()

    # Continue to day 2
    all_blues_day1 = set(blues) | new_day1
    new_day2 = simulate_day(reds, list(all_blues_day1))

    print(f"Day 2:")
    print(f"  Total blues before: {len(all_blues_day1)}")
    print(f"  New blues: {len(new_day2)}")
    print(f"  g(2) = {len(all_blues_day1) + len(new_day2)}")
    print()

    if len(new_day2) == 20:
        print("  ✓ Matches g(2) = 28!")
        print()
        print("  This configuration works! Now simulate to g(3)...")
        print()

        # Day 3
        all_blues_day2 = all_blues_day1 | new_day2
        new_day3 = simulate_day(reds, list(all_blues_day2))

        print(f"Day 3:")
        print(f"  Total blues before: {len(all_blues_day2)}")
        print(f"  New blues: {len(new_day3)}")
        print(f"  g(3) = {len(all_blues_day2) + len(new_day3)}")
        print()

        g3 = len(all_blues_day2) + len(new_day3)
        print(f"  Quadratic predicts g(3) = 62")
        print(f"  Actual: g(3) = {g3}")
        print(f"  Match: {g3 == 62}")

    else:
        print(f"  ✗ Got g(2) = {len(all_blues_day1) + len(new_day2)}, expected 28")
else:
    print(f"  ✗ Got g(1) = {len(blues) + len(new_day1)}, expected 8")
    print()
    print("  Trying different initial positions...")
