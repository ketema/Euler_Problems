"""
Search for initial configuration that gives g(1) = 8 (6 new blues)
"""

from fractions import Fraction
from itertools import product

def line_intersection(p1, p2, p3, p4):
    """Find intersection of lines."""
    x1, y1 = p1
    x2, y2 = p2
    x3, y3 = p3
    x4, y4 = p4

    denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)

    if abs(denom) < Fraction(1, 1000000):  # Nearly parallel
        return None

    t = ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)) / denom

    x = x1 + t*(x2-x1)
    y = y1 + t*(y2-y1)

    return (x, y)

def count_new_blues(reds, blues):
    """Count new blues created on day 1."""
    r1, r2, r3 = reds
    b1, b2 = blues

    tests = [
        ((r1, b1), (r2, b2)),
        ((r1, b1), (r3, b2)),
        ((r1, b2), (r2, b1)),
        ((r1, b2), (r3, b1)),
        ((r2, b1), (r3, b2)),
        ((r2, b2), (r3, b1)),
    ]

    new_points = set()
    for (p1, p2), (p3, p4) in tests:
        intersection = line_intersection(p1, p2, p3, p4)
        if intersection:
            # Check if it's truly new
            if (intersection not in reds and
                intersection not in blues and
                intersection not in new_points):
                new_points.add(intersection)

    return len(new_points)

# Fix 3 reds at standard positions
reds = [
    (Fraction(0), Fraction(0)),
    (Fraction(1), Fraction(0)),
    (Fraction(0), Fraction(1)),
]

print("Searching for blue positions that give 6 new blues...")
print("Fixed reds: (0,0), (1,0), (0,1)")
print()

# Try various blue positions
coords = [Fraction(i, 2) for i in range(-4, 9)]  # -2, -1.5, ..., 4

found = False
for x1, y1, x2, y2 in product(coords, repeat=4):
    # Skip if blues are same or too close to reds
    b1 = (x1, y1)
    b2 = (x2, y2)

    if b1 == b2 or b1 in reds or b2 in reds:
        continue

    blues = [b1, b2]
    n = count_new_blues(reds, blues)

    if n == 6:
        print(f"✓ FOUND: Blues at {b1}, {b2}")
        print(f"  → {n} new blues, g(1) = {2 + n}")
        found = True
        break

if not found:
    print("✗ No configuration found in search space")
    print()
    print("Trying specific cases...")

    # Maybe all 5 points need to be in very general position
    test_configs = [
        # Try "random-ish" positions
        ([(0, 0), (1, 0), (0, 1)], [(Fraction(1,2), Fraction(1,3)), (Fraction(2,3), Fraction(1,2))]),
        ([(0, 0), (1, 0), (Fraction(1,2), Fraction(1,2))], [(Fraction(1,3), Fraction(1,4)), (Fraction(3,4), Fraction(2,3))]),
        # Try positions that avoid collinearities
        ([(0, 0), (3, 0), (1, 2)], [(1, 1), (2, Fraction(3,2))]),
    ]

    for r, b in test_configs:
        n = count_new_blues(r, b)
        print(f"  Reds {r[:2]}..., Blues {b[:2]}: {n} new blues")
        if n == 6:
            print(f"    ✓ This works!")
