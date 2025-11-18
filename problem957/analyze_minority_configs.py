#!/usr/bin/env python3
"""
ANALYZE THE 10% MINORITY: Degenerate configurations

Why do ~10% of configs fail to achieve g(1)=8, g(2)=28?
What patterns exist in these "failure" cases?

Key questions:
1. What g(1) and g(2) values do they achieve?
2. Are the initial blues collinear with reds?
3. Are blues at special positions (on axes, on line through reds)?
4. Do parallel lines occur? (lines that never intersect)
5. Projective geometry: "Lines meet at infinity" - are we missing points?
"""

from fractions import Fraction
import random
from collections import defaultdict

def line_intersection(p1, p2, p3, p4):
    """Exact rational intersection using Fraction"""
    (x1, y1), (x2, y2), (x3, y3), (x4, y4) = p1, p2, p3, p4

    d1_x = x2 - x1
    d1_y = y2 - y1
    d2_x = x4 - x3
    d2_y = y4 - y3

    det = d1_x * (-d2_y) - d1_y * (-d2_x)
    if det == 0:
        return None  # Parallel lines

    rhs_x = x3 - x1
    rhs_y = y3 - y1
    t = (rhs_x * (-d2_y) - rhs_y * (-d2_x)) / det

    return (x1 + t * d1_x, y1 + t * d1_y)

def compute_next_day(reds, blues):
    """Compute new blues after one day"""
    existing = set([tuple(r) for r in reds] + [tuple(b) for b in blues])

    lines = []
    for r in reds:
        for b in blues:
            lines.append((r, b))

    new_points = set()
    parallel_count = 0
    for i, (p1, p2) in enumerate(lines):
        for (p3, p4) in lines[i+1:]:
            pt = line_intersection(p1, p2, p3, p4)
            if pt is None:
                parallel_count += 1
            elif pt not in existing and pt not in new_points:
                new_points.add(pt)

    return list(new_points), parallel_count

def are_collinear(p1, p2, p3):
    """Check if three points are collinear"""
    (x1, y1), (x2, y2), (x3, y3) = p1, p2, p3
    # Use cross product: (p2-p1) × (p3-p1) = 0
    cross = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
    return cross == 0

def analyze_config(reds, b1, b2):
    """Deep analysis of a single configuration"""
    blues_day0 = [b1, b2]

    # Day 1
    new_day1, parallel_day1 = compute_next_day(reds, blues_day0)
    blues_day1 = blues_day0 + new_day1
    g1 = len(blues_day1)

    # Day 2
    new_day2, parallel_day2 = compute_next_day(reds, blues_day1)
    blues_day2 = blues_day1 + new_day2
    g2 = len(blues_day2)

    # Check degeneracies
    collinear_with_reds = []
    for r1, r2 in [(reds[0], reds[1]), (reds[0], reds[2]), (reds[1], reds[2])]:
        if are_collinear(r1, r2, b1):
            collinear_with_reds.append(("B1", r1, r2))
        if are_collinear(r1, r2, b2):
            collinear_with_reds.append(("B2", r1, r2))

    # Check if blues are collinear with each other + a red
    blues_collinear_with_red = []
    for r in reds:
        if are_collinear(b1, b2, r):
            blues_collinear_with_red.append(r)

    # Check if blues on special positions
    on_axes = []
    if b1[0] == 0 or b1[1] == 0:
        on_axes.append("B1")
    if b2[0] == 0 or b2[1] == 0:
        on_axes.append("B2")

    return {
        'b1': b1,
        'b2': b2,
        'g1': g1,
        'g2': g2,
        'parallel_day1': parallel_day1,
        'parallel_day2': parallel_day2,
        'collinear_with_reds': collinear_with_reds,
        'blues_collinear_with_red': blues_collinear_with_red,
        'on_axes': on_axes,
        'new_day1_count': len(new_day1),
        'new_day2_count': len(new_day2)
    }

# Fixed reds
reds = [(Fraction(0), Fraction(0)),
        (Fraction(1), Fraction(0)),
        (Fraction(0), Fraction(1))]

print("="*70)
print("MINORITY ANALYSIS: The 10% That Don't Achieve Maximum")
print("="*70)
print()

# Test many configurations
random.seed(42)
majority = []  # g(1)=8, g(2)=28
minority = []  # anything else

print("Testing 2000 random configurations...")
for trial in range(2000):
    # Random initial blues
    b1 = (Fraction(random.randint(1, 30)), Fraction(random.randint(1, 30)))
    b2 = (Fraction(random.randint(1, 30)), Fraction(random.randint(1, 30)))

    if b1 == b2 or b1 in reds or b2 in reds:
        continue

    result = analyze_config(reds, b1, b2)

    if result['g1'] == 8 and result['g2'] == 28:
        majority.append(result)
    else:
        minority.append(result)

print(f"Tested {len(majority) + len(minority)} valid configurations")
print(f"  Majority (g(1)=8, g(2)=28): {len(majority)} ({100*len(majority)/(len(majority)+len(minority)):.1f}%)")
print(f"  Minority (other values):    {len(minority)} ({100*len(minority)/(len(majority)+len(minority)):.1f}%)")
print()

print("="*70)
print("MINORITY CHARACTERISTICS")
print("="*70)
print()

# Distribution of minority g(1), g(2)
minority_g1_dist = defaultdict(int)
minority_g2_dist = defaultdict(int)
for r in minority:
    minority_g1_dist[r['g1']] += 1
    minority_g2_dist[r['g2']] += 1

print("Minority g(1) distribution:")
for g1 in sorted(minority_g1_dist.keys(), reverse=True):
    count = minority_g1_dist[g1]
    print(f"  g(1) = {g1}: {count} configs")
print()

print("Minority g(2) distribution:")
for g2 in sorted(minority_g2_dist.keys(), reverse=True):
    count = minority_g2_dist[g2]
    print(f"  g(2) = {g2}: {count} configs")
print()

# Parallel line analysis
print("="*70)
print("PARALLEL LINE ANALYSIS")
print("="*70)
print()

print("Majority (achieving maximum):")
maj_parallel_day1 = [r['parallel_day1'] for r in majority]
maj_parallel_day2 = [r['parallel_day2'] for r in majority]
if maj_parallel_day1:
    print(f"  Day 1 parallel pairs: avg={sum(maj_parallel_day1)/len(maj_parallel_day1):.2f}, max={max(maj_parallel_day1)}")
if maj_parallel_day2:
    print(f"  Day 2 parallel pairs: avg={sum(maj_parallel_day2)/len(maj_parallel_day2):.2f}, max={max(maj_parallel_day2)}")
print()

print("Minority (degenerate cases):")
min_parallel_day1 = [r['parallel_day1'] for r in minority]
min_parallel_day2 = [r['parallel_day2'] for r in minority]
if min_parallel_day1:
    print(f"  Day 1 parallel pairs: avg={sum(min_parallel_day1)/len(min_parallel_day1):.2f}, max={max(min_parallel_day1)}")
if min_parallel_day2:
    print(f"  Day 2 parallel pairs: avg={sum(min_parallel_day2)/len(min_parallel_day2):.2f}, max={max(min_parallel_day2)}")
print()

# Collinearity analysis
print("="*70)
print("COLLINEARITY ANALYSIS")
print("="*70)
print()

maj_collinear = sum(1 for r in majority if r['collinear_with_reds'])
min_collinear = sum(1 for r in minority if r['collinear_with_reds'])

print(f"Majority: {maj_collinear}/{len(majority)} ({100*maj_collinear/len(majority):.1f}%) have collinear blues with reds")
print(f"Minority: {min_collinear}/{len(minority)} ({100*min_collinear/len(minority):.1f}%) have collinear blues with reds")
print()

if min_collinear > 0:
    print("Minority collinear cases:")
    for r in minority[:10]:
        if r['collinear_with_reds']:
            print(f"  B1={r['b1']}, B2={r['b2']} → g(1)={r['g1']}, g(2)={r['g2']}")
            print(f"    Collinear: {r['collinear_with_reds']}")
print()

# Blues collinear with each other + red
maj_blues_col = sum(1 for r in majority if r['blues_collinear_with_red'])
min_blues_col = sum(1 for r in minority if r['blues_collinear_with_red'])

print(f"Majority: {maj_blues_col}/{len(majority)} ({100*maj_blues_col/len(majority):.1f}%) have B1, B2, and a red collinear")
print(f"Minority: {min_blues_col}/{len(minority)} ({100*min_blues_col/len(minority):.1f}%) have B1, B2, and a red collinear")
print()

# Axes analysis
maj_on_axes = sum(1 for r in majority if r['on_axes'])
min_on_axes = sum(1 for r in minority if r['on_axes'])

print(f"Majority: {maj_on_axes}/{len(majority)} ({100*maj_on_axes/len(majority):.1f}%) have blues on coordinate axes")
print(f"Minority: {min_on_axes}/{len(minority)} ({100*min_on_axes/len(minority):.1f}%) have blues on coordinate axes")
print()

# Detailed minority examples
print("="*70)
print("DETAILED MINORITY EXAMPLES (first 20)")
print("="*70)
print()

for i, r in enumerate(minority[:20]):
    print(f"Config {i+1}:")
    print(f"  B1 = {r['b1']}, B2 = {r['b2']}")
    print(f"  g(1) = {r['g1']}, g(2) = {r['g2']}")
    print(f"  New points: day1={r['new_day1_count']}, day2={r['new_day2_count']}")
    print(f"  Parallel pairs: day1={r['parallel_day1']}, day2={r['parallel_day2']}")
    if r['collinear_with_reds']:
        print(f"  Collinear with reds: {r['collinear_with_reds']}")
    if r['blues_collinear_with_red']:
        print(f"  B1, B2, and red collinear: {r['blues_collinear_with_red']}")
    if r['on_axes']:
        print(f"  On axes: {r['on_axes']}")
    print()

print("="*70)
print("KEY FINDINGS")
print("="*70)
print()

# Calculate correlation statistics
print("Correlation analysis:")
print(f"  If blues collinear with reds → minority: {100*min_collinear/max(1,min_collinear+maj_collinear):.1f}%")
print(f"  If B1,B2,red collinear → minority: {100*min_blues_col/max(1,min_blues_col+maj_blues_col):.1f}%")
print(f"  If on axes → minority: {100*min_on_axes/max(1,min_on_axes+maj_on_axes):.1f}%")
print()

print("Hypothesis: Minority occurs when geometric degeneracies reduce available intersections")
print("  - Collinear configurations → fewer independent lines → fewer intersections")
print("  - Special positions (axes) → symmetries that reduce distinct points")
print()

print("Projective geometry note:")
print("  In projective plane, 'parallel' lines meet at 'point at infinity'")
print("  We're working in affine plane (no points at infinity)")
print("  This may explain why some configs fail to achieve maximum")
print()

print("="*70)
print("COMPLETE DATA WRITTEN TO: minority_analysis_data.txt")
print("="*70)

# Write complete minority data
with open('minority_analysis_data.txt', 'w') as f:
    f.write("="*70 + "\n")
    f.write("MINORITY CONFIGURATION ANALYSIS\n")
    f.write("="*70 + "\n\n")
    f.write(f"Total minority configs: {len(minority)}\n\n")

    for i, r in enumerate(minority):
        f.write(f"Config {i+1}:\n")
        f.write(f"  B1 = {r['b1']}\n")
        f.write(f"  B2 = {r['b2']}\n")
        f.write(f"  g(1) = {r['g1']}, g(2) = {r['g2']}\n")
        f.write(f"  New points: day1={r['new_day1_count']}, day2={r['new_day2_count']}\n")
        f.write(f"  Parallel pairs: day1={r['parallel_day1']}, day2={r['parallel_day2']}\n")
        if r['collinear_with_reds']:
            f.write(f"  Collinear with reds: {r['collinear_with_reds']}\n")
        if r['blues_collinear_with_red']:
            f.write(f"  B1,B2,red collinear: {r['blues_collinear_with_red']}\n")
        if r['on_axes']:
            f.write(f"  On axes: {r['on_axes']}\n")
        f.write("\n")

print("Analysis complete.")
