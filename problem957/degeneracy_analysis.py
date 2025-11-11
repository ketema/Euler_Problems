#!/usr/bin/env python3
"""
DEGENERACY ANALYSIS: Why only 6/15 intersections are new at g(1)?

AI Panel Critical Insight (Claude Sonnet):
"For g(1)=8: 3 reds × 2 blues = 6 lines. These 6 lines have C(6,2)=15
intersection points, but only 6 are new. WHY? Those 9 discarded
intersections reveal the degeneracy pattern."

Goal: Map ALL 15 intersections and classify each as:
- NEW blue point
- Coincident with existing red
- Coincident with existing blue
- Multiple lines through same point (collinear degeneracy)
"""

from sympy import Point, Line, Rational
from typing import Set, List, Tuple, Dict
from collections import defaultdict

def create_point(x, y) -> Point:
    """Create SymPy Point with exact Rational coordinates"""
    return Point(Rational(x), Rational(y))

# Example configuration from problem
reds = [
    create_point(0, 0),
    create_point(4, 0),
    create_point(2, 3)
]

blues_day0 = {
    create_point(1, 1),
    create_point(3, 2)
}

print("="*80)
print("DEGENERACY ANALYSIS: g(0) → g(1) Transition")
print("="*80)
print()

print("Initial Configuration:")
print(f"  Reds:  {[str(p) for p in reds]}")
print(f"  Blues: {[str(p) for p in blues_day0]}")
print()

# Step 1: Generate all lines
print("="*80)
print("STEP 1: ALL LINES FROM RED TO BLUE")
print("="*80)
print()

lines = []
for i, red in enumerate(reds):
    for j, blue in enumerate(blues_day0):
        if red != blue:
            line = Line(red, blue)
            lines.append((f"L{len(lines)}", line, f"red{i}", red, f"blue{j}", blue))
            print(f"L{len(lines)-1}: {red} → {blue}")

print(f"\nTotal lines: {len(lines)}")
print()

# Step 2: Compute ALL pairwise intersections
print("="*80)
print("STEP 2: ALL PAIRWISE INTERSECTIONS")
print("="*80)
print()

existing_points = set(reds).union(blues_day0)
print(f"Existing points (must reject): {len(existing_points)}")
for p in existing_points:
    print(f"  {p}")
print()

intersection_data = []
intersection_points = defaultdict(list)  # Point -> list of line pairs

for i, (id1, line1, r1, rp1, b1, bp1) in enumerate(lines):
    for j, (id2, line2, r2, rp2, b2, bp2) in enumerate(lines):
        if i < j:  # Avoid duplicates
            result = line1.intersection(line2)

            if result:
                p = result[0]

                # Classify intersection
                is_new = p not in existing_points
                classification = ""

                if not is_new:
                    # Find which existing point it matches
                    for existing in existing_points:
                        if p.equals(existing):
                            if existing in reds:
                                classification = f"EXISTING RED {existing}"
                            else:
                                classification = f"EXISTING BLUE {existing}"
                            break
                else:
                    classification = "NEW BLUE"

                intersection_data.append({
                    "line1": id1,
                    "line2": id2,
                    "point": p,
                    "is_new": is_new,
                    "classification": classification
                })

                # Track multiplicity (if multiple line pairs intersect here)
                intersection_points[p].append((id1, id2))

print(f"Total pairwise intersections: {len(intersection_data)}")
print()

# Step 3: Group and analyze
print("="*80)
print("STEP 3: CLASSIFICATION OF ALL 15 INTERSECTIONS")
print("="*80)
print()

new_count = 0
existing_red_count = 0
existing_blue_count = 0

# Sort: NEW first, then existing
intersection_data.sort(key=lambda x: (not x['is_new'], x['classification']))

for idx, data in enumerate(intersection_data, 1):
    p = data['point']
    line1 = data['line1']
    line2 = data['line2']
    is_new = data['is_new']
    classification = data['classification']

    # Count multiplicity
    multiplicity = len(intersection_points[p])
    mult_str = f" (MULTIPLICITY {multiplicity})" if multiplicity > 1 else ""

    status = "✓ NEW" if is_new else "✗ REJECT"

    print(f"Intersection {idx:2d}: {line1} × {line2}")
    print(f"  Point: {p}")
    print(f"  Status: {status} - {classification}{mult_str}")
    print()

    if is_new:
        new_count += 1
    elif "RED" in classification:
        existing_red_count += 1
    elif "BLUE" in classification:
        existing_blue_count += 1

# Summary
print("="*80)
print("STEP 4: DEGENERACY SUMMARY")
print("="*80)
print()

print(f"Total intersections:        {len(intersection_data)}")
print(f"  NEW blues:                {new_count}")
print(f"  Coincide with red:        {existing_red_count}")
print(f"  Coincide with blue:       {existing_blue_count}")
print()
print(f"Degeneracy rate: {100 * (1 - new_count/len(intersection_data)):.1f}%")
print()
print(f"Expected g(1): {len(blues_day0) + new_count}")
print(f"Actual g(1):   8 (from problem)")
print(f"Match: {'✓' if len(blues_day0) + new_count == 8 else '✗ MISMATCH!'}")
print()

# Analyze multiplicity
print("="*80)
print("STEP 5: MULTIPLICITY (COLLINEAR) ANALYSIS")
print("="*80)
print()

high_multiplicity = {p: lines for p, lines in intersection_points.items() if len(lines) > 1}

if high_multiplicity:
    print(f"Found {len(high_multiplicity)} points with 3+ lines (collinear):")
    print()
    for p, line_pairs in sorted(high_multiplicity.items(), key=lambda x: -len(x[1])):
        print(f"Point {p}:")
        print(f"  {len(line_pairs)} line pairs intersect here")
        print(f"  Lines involved: {set(lid for pair in line_pairs for lid in pair)}")
        if p in existing_points:
            print(f"  Type: EXISTING POINT")
        else:
            print(f"  Type: NEW BLUE POINT")
        print()
else:
    print("No high-multiplicity intersections (no 3+ collinear lines)")
    print()

# Step 6: Geometric structure analysis
print("="*80)
print("STEP 6: GEOMETRIC STRUCTURE")
print("="*80)
print()

print("Checking for special geometric properties:")
print()

# Check if reds are collinear
if len(reds) == 3:
    line_red = Line(reds[0], reds[1])
    if reds[2] in line_red.points:
        print("  ✓ ALL 3 REDS ARE COLLINEAR!")
    else:
        print("  ✗ Reds not collinear (form triangle)")
print()

# Check if blues are collinear
blues_list = list(blues_day0)
if len(blues_list) == 2:
    print(f"  Blues form line: {Line(blues_list[0], blues_list[1])}")
print()

# Check for symmetries
print("Red triangle properties:")
if len(reds) == 3:
    # Check if right triangle, isosceles, etc.
    from sympy import sqrt
    d01 = reds[0].distance(reds[1])
    d12 = reds[1].distance(reds[2])
    d20 = reds[2].distance(reds[0])
    print(f"  Side lengths: {d01}, {d12}, {d20}")

    # Check right angle
    import sympy as sp
    v1 = Point(reds[1].x - reds[0].x, reds[1].y - reds[0].y)
    v2 = Point(reds[2].x - reds[1].x, reds[2].y - reds[1].y)
    dot = v1.x * v2.x + v1.y * v2.y
    if dot == 0:
        print(f"  ✓ RIGHT TRIANGLE at {reds[1]}")
    else:
        print(f"  Dot product: {dot} (not right triangle)")
print()

print("="*80)
print("CRITICAL FINDING")
print("="*80)
print()
print(f"Out of {len(intersection_data)} possible intersections:")
print(f"  Only {new_count} are NEW ({100*new_count/len(intersection_data):.1f}%)")
print(f"  {len(intersection_data) - new_count} are DEGENERATE ({100*(1-new_count/len(intersection_data)):.1f}%)")
print()
print("This high degeneracy rate is KEY to understanding the problem!")
print("The geometric configuration has special properties that limit growth.")
print()
