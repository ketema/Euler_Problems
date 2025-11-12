#!/usr/bin/env sage

"""
DECISIVE TEST: Do equivalent configurations generate SAME or DIFFERENT point sets?

Config A: B1=(1,1), B2=(3,9) → g(1)=8
Config B: B1=(2,3), B2=(5,7) → g(1)=8

OUTCOME 1: IDENTICAL POINTS → Unique geometric structure (intrinsic property)
OUTCOME 2: DIFFERENT POINTS → Combinatorial bounds (general position maximum)
"""

from sage.all import *

def line_intersection(p1, p2, p3, p4):
    """Compute intersection of line (p1,p2) with line (p3,p4)"""
    d1 = p2 - p1
    d2 = p4 - p3
    det = d1[0]*(-d2[1]) - d1[1]*(-d2[0])
    if det == 0:
        return None  # Parallel or coincident
    rhs = p3 - p1
    t = (rhs[0]*(-d2[1]) - rhs[1]*(-d2[0])) / det
    return p1 + t*d1

def compute_day_1(reds, initial_blues):
    """Compute all blue points after day 1"""
    blues = list(initial_blues)
    existing = set([tuple(r) for r in reds] + [tuple(b) for b in blues])

    # Generate all red-blue lines
    lines = []
    for r in reds:
        for b in blues:
            lines.append((r, b))

    # Find intersections
    new_points = set()
    for i, (p1, p2) in enumerate(lines):
        for (p3, p4) in lines[i+1:]:
            pt = line_intersection(p1, p2, p3, p4)
            if pt is not None:
                pt_tuple = tuple(pt)
                if pt_tuple not in existing:
                    new_points.add(pt_tuple)

    # Return ALL blues (initial + new)
    all_blues = blues + [vector(QQ, pt) for pt in new_points]
    return all_blues

# Fixed red points
R1 = vector(QQ, [0, 0])
R2 = vector(QQ, [1, 0])
R3 = vector(QQ, [0, 1])
reds = [R1, R2, R3]

print("="*70)
print("DECISIVE TEST: Point Set Comparison")
print("="*70)
print()

# Config A
print("Computing Config A: B1=(1,1), B2=(3,9)")
config_A = [vector(QQ, [1, 1]), vector(QQ, [3, 9])]
A_points = compute_day_1(reds, config_A)
print(f"  Result: {len(A_points)} blue points after day 1")

# Config B
print("Computing Config B: B1=(2,3), B2=(5,7)")
config_B = [vector(QQ, [2, 3]), vector(QQ, [5, 7])]
B_points = compute_day_1(reds, config_B)
print(f"  Result: {len(B_points)} blue points after day 1")

print()
print("="*70)

# Convert to sets for comparison
A_set = set(tuple(p) for p in A_points)
B_set = set(tuple(p) for p in B_points)

# Compare
if A_set == B_set:
    print("✓✓✓ OUTCOME 1: IDENTICAL POINT SETS!")
    print()
    print("INTERPRETATION:")
    print("  - The configurations generate THE SAME GEOMETRIC STRUCTURE")
    print("  - The problem has a unique intrinsic property")
    print("  - Different initial blues converge to same point set")
    print("  - Search for: 'unique line arrangement' theorems")
    print("  - The optimal configuration is CANONICAL")
    print()
    print("All 8 points:")
    for pt in sorted(A_set):
        print(f"  {pt}")
else:
    print("✓✓✓ OUTCOME 2: DIFFERENT POINT SETS (but same count)")
    print()
    overlap = A_set & B_set
    print(f"INTERPRETATION:")
    print(f"  - Configurations generate DIFFERENT structures")
    print(f"  - Overlap: {len(overlap)} of {len(A_set)} points")
    print(f"  - Problem is about combinatorial bounds")
    print(f"  - Search for: max intersections in general position")
    print(f"  - Multiple optimal configurations exist")
    print()
    print(f"Config A unique points: {len(A_set - B_set)}")
    for pt in sorted(A_set - B_set):
        print(f"  {pt}")
    print()
    print(f"Config B unique points: {len(B_set - A_set)}")
    for pt in sorted(B_set - A_set):
        print(f"  {pt}")
    print()
    print(f"Common points: {len(overlap)}")
    for pt in sorted(overlap):
        print(f"  {pt}")

print()
print("="*70)
