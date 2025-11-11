"""
Systematic search for configuration giving g(1)=8, g(2)=28
Using rigorous Shapely-based geometry
"""

from shapely.geometry import Point, LineString
from shapely import equals
from itertools import product
from fractions import Fraction

def count_new_blues(reds, blues):
    """Count new blues using rigorous method."""
    lines = [(LineString([r, b]), i, j) for i, r in enumerate(reds) for j, b in enumerate(blues)]

    intersections = set()

    for i, (line1, r1, b1) in enumerate(lines):
        for line2, r2, b2 in lines[i+1:]:
            if equals(line1, line2):
                continue

            pt = line1.intersection(line2)

            if not pt.is_empty and pt.geom_type == 'Point':
                intersections.add(pt.coords[0])

    # Filter to white points
    new_blues = 0
    for pt_coords in intersections:
        pt = Point(pt_coords)
        is_red = any(equals(pt, r) for r in reds)
        is_blue = any(equals(pt, b) for b in blues)

        if not is_red and not is_blue:
            new_blues += 1

    return new_blues

# Fix reds
reds = [Point(0, 0), Point(1, 0), Point(0, 1)]

print("Searching for configuration with g(1)=8, g(2)=28...")
print("Fixed reds: (0,0), (1,0), (0,1)")
print()

# Search through rational coordinates
# For g(1)=8: need 6 new white blues
# Maximum possible with 3 reds and 2 blues: C(3,2) × something

# Try different coordinate ranges
coords = [Fraction(i, 2) for i in range(-10, 11)]  # -5 to 5 in halves

tested = 0
found = []

for x1, y1, x2, y2 in product(coords, repeat=4):
    blues = [Point(float(x1), float(y1)), Point(float(x2), float(y2))]

    # Skip invalid
    if equals(blues[0], blues[1]):
        continue
    if any(equals(blues[0], r) for r in reds):
        continue
    if any(equals(blues[1], r) for r in reds):
        continue

    tested += 1
    if tested % 50000 == 0:
        print(f"  Tested {tested:,} configs...")

    new_day1 = count_new_blues(reds, blues)

    if new_day1 == 6:  # Need 6 new to get g(1)=8
        print(f"\n✓ Found g(1)=8: blues at ({float(x1)}, {float(y1)}), ({float(x2)}, {float(y2)})")

        # Check day 2
        blues_day1 = blues + [Point(pt) for pt in get_new_points(reds, blues)]
        # ... implement day 2 check

        found.append((x1, y1, x2, y2))

        if len(found) >= 3:
            break

print(f"\nSearched {tested:,} configurations")
print(f"Found {len(found)} with g(1)=8")

def get_new_points(reds, blues):
    """Helper to get actual new point coordinates."""
    lines = [(LineString([r, b]), i, j) for i, r in enumerate(reds) for j, b in enumerate(blues)]
    intersections = set()

    for i, (line1, r1, b1) in enumerate(lines):
        for line2, r2, b2 in lines[i+1:]:
            if equals(line1, line2):
                continue
            pt = line1.intersection(line2)
            if not pt.is_empty and pt.geom_type == 'Point':
                intersections.add(pt.coords[0])

    new_pts = []
    for pt_coords in intersections:
        pt = Point(pt_coords)
        is_red = any(equals(pt, r) for r in reds)
        is_blue = any(equals(pt, b) for b in blues)
        if not is_red and not is_blue:
            new_pts.append(pt_coords)

    return new_pts
