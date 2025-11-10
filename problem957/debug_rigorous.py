"""
Debug why rigorous simulator gives different results
"""

from shapely.geometry import Point, LineString
from shapely import equals

# Configuration that I thought gave g(1)=8
reds = [Point(0, 0), Point(1, 0), Point(0, 1)]
blues = [Point(-2, -2), Point(-2, -1.5)]

print("Initial state:")
print(f"  Reds: {[(r.x, r.y) for r in reds]}")
print(f"  Blues: {[(b.x, b.y) for b in blues]}")
print(f"  g(0) = {len(blues)}")
print()

# Day 0->1: Construct all lines
lines = []
for r in reds:
    for b in blues:
        line = LineString([r, b])
        lines.append((line, f"r{reds.index(r)+1}", f"b{blues.index(b)+1}"))

print(f"Lines constructed: {len(lines)}")
for line, r_label, b_label in lines:
    coords = list(line.coords)
    print(f"  {r_label}-{b_label}: {coords}")
print()

# Find all intersections
intersections = {}

for i, (line1, r1, b1) in enumerate(lines):
    for line2, r2, b2 in lines[i+1:]:
        pair_label = f"{r1}-{b1} × {r2}-{b2}"

        # Skip if same line
        if equals(line1, line2):
            continue

        pt = line1.intersection(line2)

        if pt.is_empty:
            print(f"  {pair_label}: parallel (no intersection)")
        elif pt.geom_type == 'Point':
            print(f"  {pair_label}: intersection at ({pt.x:.3f}, {pt.y:.3f})")
            if pt.coords[0] not in intersections:
                intersections[pt.coords[0]] = []
            intersections[pt.coords[0]].append(pair_label)
        else:
            print(f"  {pair_label}: {pt.geom_type}")

print()
print(f"Total unique intersection points: {len(intersections)}")
print()

# Check which are white
all_points = reds + blues
white_count = 0

for pt_coords, pairs in intersections.items():
    pt = Point(pt_coords)

    is_red = any(equals(pt, r) for r in reds)
    is_blue = any(equals(pt, b) for b in blues)

    status = "RED" if is_red else "BLUE" if is_blue else "WHITE"

    print(f"  ({pt.x:.3f}, {pt.y:.3f}): {status}")
    print(f"    Created by: {pairs}")

    if status == "WHITE":
        white_count += 1

print()
print(f"WHITE points (new blues): {white_count}")
print(f"g(1) = {len(blues) + white_count}")
