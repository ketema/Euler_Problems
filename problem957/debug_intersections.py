#!/usr/bin/env python3
"""
Debug: Explicitly compute the 6 intersection points and check for coincidences
"""

from sympy import Point, Line, Rational

# Test configuration
r1 = Point(Rational(0), Rational(0))
r2 = Point(Rational(4), Rational(0))
r3 = Point(Rational(2), Rational(3))
b1 = Point(Rational(2), Rational(1))
b2 = Point(Rational(2), Rational(2))

print("Configuration:")
print(f"  r1 = {r1}")
print(f"  r2 = {r2}")
print(f"  r3 = {r3}")
print(f"  b1 = {b1}")
print(f"  b2 = {b2}")

# Construct 6 lines
lines = {
    'L1': Line(r1, b1),
    'L2': Line(r1, b2),
    'L3': Line(r2, b1),
    'L4': Line(r2, b2),
    'L5': Line(r3, b1),
    'L6': Line(r3, b2),
}

print(f"\nLines:")
for name, line in lines.items():
    print(f"  {name}: {line}")

# Compute the 6 "new" intersection points
print(f"\n{'='*70}")
print("Expected NEW intersection points:")
print(f"{'='*70}")

new_intersections = {
    'p1': ('L1', 'L4', r1, b1, r2, b2),
    'p2': ('L1', 'L6', r1, b1, r3, b2),
    'p3': ('L2', 'L3', r1, b2, r2, b1),
    'p4': ('L2', 'L5', r1, b2, r3, b1),
    'p5': ('L3', 'L6', r2, b1, r3, b2),
    'p6': ('L4', 'L5', r2, b2, r3, b1),
}

points = {}
for name, (line1_name, line2_name, red1, blue1, red2, blue2) in new_intersections.items():
    line1 = lines[line1_name]
    line2 = lines[line2_name]

    # Intersect
    result = line1.intersection(line2)

    if result:
        p = result[0]
        points[name] = p
        print(f"{name} = {line1_name} ∩ {line2_name} = line({red1}, {blue1}) ∩ line({red2}, {blue2})")
        print(f"     = {p} = ({float(p.x):.4f}, {float(p.y):.4f})")
    else:
        print(f"{name} = {line1_name} ∩ {line2_name} = PARALLEL (no intersection)")
        points[name] = None

# Check for coincidences
print(f"\n{'='*70}")
print("Checking for coincidences:")
print(f"{'='*70}")

names = list(points.keys())
coincidences = []

for i, name1 in enumerate(names):
    for name2 in names[i+1:]:
        p1 = points[name1]
        p2 = points[name2]

        if p1 is not None and p2 is not None:
            if p1.equals(p2):
                print(f"✗ COINCIDENCE: {name1} = {name2} = {p1}")
                coincidences.append((name1, name2))

if not coincidences:
    print("✓ No coincidences - all 6 points are distinct!")

# Count distinct points
distinct_points = set()
for name, p in points.items():
    if p is not None:
        distinct_points.add((Rational(p.x), Rational(p.y)))

print(f"\nDistinct new points: {len(distinct_points)}")
print(f"Expected g(1) = 2 (initial) + {len(distinct_points)} (new) = {2 + len(distinct_points)}")

# Also check if any new points coincide with existing points
existing = {
    (Rational(r1.x), Rational(r1.y)),
    (Rational(r2.x), Rational(r2.y)),
    (Rational(r3.x), Rational(r3.y)),
    (Rational(b1.x), Rational(b1.y)),
    (Rational(b2.x), Rational(b2.y)),
}

print(f"\nChecking if any new points coincide with existing points:")
for name, p in points.items():
    if p is not None:
        p_tuple = (Rational(p.x), Rational(p.y))
        if p_tuple in existing:
            print(f"  ✗ {name} = {p} coincides with an existing point!")

print(f"\n{'='*70}")
print("CONCLUSION")
print(f"{'='*70}")
print(f"With this configuration, g(1) should be: {2 + len(distinct_points)}")
