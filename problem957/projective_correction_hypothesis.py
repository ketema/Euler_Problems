#!/usr/bin/env python3
"""
HYPOTHESIS: What if "maximal possible" means working in PROJECTIVE PLANE?

In projective plane:
- Every pair of lines intersects (at infinity if parallel in affine)
- Points at infinity are real points on the "line at infinity"

Question: If we ALWAYS count parallel lines as creating a point at infinity,
what are the new g(1) and g(2) values?

From minority analysis:
- Majority: 0 parallel on Day 1, ~18 parallel on Day 2
- Minority: ~0.76 parallel on Day 1, ~15.76 parallel on Day 2

But wait - the majority configs have MORE parallels on Day 2!

Let me recalculate: If majority gets +18 points at infinity on Day 2,
then g(2)_projective = 28 + 18 = 46

But this seems wrong because the problem says g(1)=8, g(2)=28 "for example"
which suggests these are the actual values, not partial counts.

Alternative: Maybe parallel lines in Day 2 are ALREADY creating duplicate
points (same intersection in affine), and we're MISSING the infinity points?

Let me think differently: What's the THEORETICAL MAXIMUM intersections?

Day 0→1:
- 2 blues, 3 reds
- Lines: 2×3 = 6 lines
- Max intersections: C(6,2) = 15 possible pairs
- But some pairs go through existing points (reds, blues)
- Affine majority gets: 6 new points → total 8 blues
- Missing: 15 - 6 = 9 intersections

Those 9 must be:
- Parallel pairs (infinity points): some
- Intersections at existing reds: some
- Invalid intersections: some

Let me count precisely for the standard config.
"""

from fractions import Fraction

def line_intersection(p1, p2, p3, p4):
    """Exact rational intersection, returns None if parallel"""
    (x1, y1), (x2, y2), (x3, y3), (x4, y4) = p1, p2, p3, p4
    d1_x, d1_y = x2 - x1, y2 - y1
    d2_x, d2_y = x4 - x3, y4 - y3
    det = d1_x * (-d2_y) - d1_y * (-d2_x)

    if det == 0:
        return None  # Parallel

    rhs_x, rhs_y = x3 - x1, y3 - y1
    t = (rhs_x * (-d2_y) - rhs_y * (-d2_x)) / det
    return (x1 + t * d1_x, y1 + t * d1_y)

# Standard configuration
reds = [
    (Fraction(0), Fraction(0)),
    (Fraction(1), Fraction(0)),
    (Fraction(0), Fraction(1))
]

blues_day0 = [
    (Fraction(1), Fraction(1)),
    (Fraction(3), Fraction(9))
]

print("="*70)
print("PROJECTIVE PLANE HYPOTHESIS: DETAILED INTERSECTION ANALYSIS")
print("="*70)
print()

# Day 0 → Day 1
print("Day 0 → Day 1 Analysis:")
print()

all_points_day0 = reds + blues_day0
lines = []
for r in reds:
    for b in blues_day0:
        lines.append((r, b))

print(f"Lines formed: {len(lines)}")
print()

line_pairs = []
for i in range(len(lines)):
    for j in range(i+1, len(lines)):
        line_pairs.append((lines[i], lines[j]))

print(f"Total line pairs to check: {len(line_pairs)}")
print()

parallel_count = 0
at_existing_red = 0
at_existing_blue = 0
new_points = 0

for (l1, l2) in line_pairs:
    pt = line_intersection(l1[0], l1[1], l2[0], l2[1])

    if pt is None:
        parallel_count += 1
    elif pt in all_points_day0:
        if pt in reds:
            at_existing_red += 1
        else:
            at_existing_blue += 1
    else:
        new_points += 1

print("Intersection classification:")
print(f"  Parallel (would be at infinity): {parallel_count}")
print(f"  At existing red points: {at_existing_red}")
print(f"  At existing blue points: {at_existing_blue}")
print(f"  New points (counted as blues): {new_points}")
print()

print("Verification:")
print(f"  Total: {parallel_count + at_existing_red + at_existing_blue + new_points} = {len(line_pairs)}")
print()

print("AFFINE result: g(1) = 2 (initial) + {new_points} (new) = {2 + new_points}")
print(f"PROJECTIVE hypothesis: g(1) = 2 + {new_points} + {parallel_count} (infinity) = {2 + new_points + parallel_count}")
print()

print("="*70)
print("CONCLUSION")
print("="*70)
print()

affine_g1 = 2 + new_points
projective_g1 = 2 + new_points + parallel_count

print(f"If problem asks for AFFINE maximum: g(1) = {affine_g1}")
print(f"If problem asks for PROJECTIVE maximum: g(1) = {projective_g1}")
print()

if affine_g1 == 8:
    print("✓ AFFINE matches known g(1)=8")
elif projective_g1 == 8:
    print("✓ PROJECTIVE matches known g(1)=8")
else:
    print(f"✗ Neither matches! Known g(1)=8, but computed {affine_g1} (affine) or {projective_g1} (projective)")

print()
print("This tells us which geometry space the problem is working in!")
