#!/usr/bin/env python3
"""
Analyze the actual distribution of g(5) points
"""

from sage.all import *
import time

# Line intersection function
def line_intersection(p1, p2, p3, p4):
    d1 = p2 - p1
    d2 = p4 - p3
    det = d1[0]*(-d2[1]) - d1[1]*(-d2[0])
    if det == 0:
        return None
    rhs = p3 - p1
    t = (rhs[0]*(-d2[1]) - rhs[1]*(-d2[0])) / det
    return p1 + t*d1

# Compute next day
def compute_next_day(reds, blues):
    existing = set([tuple(r) for r in reds] + [tuple(b) for b in blues])
    lines = []
    for r in reds:
        for b in blues:
            lines.append((r, b))
    new_points = set()
    for i, (p1, p2) in enumerate(lines):
        for (p3, p4) in lines[i+1:]:
            pt = line_intersection(p1, p2, p3, p4)
            if pt is not None:
                pt_tuple = tuple(pt)
                if pt_tuple not in existing and pt_tuple not in new_points:
                    new_points.add(pt_tuple)
    return [vector(QQ, pt) for pt in new_points]

# Configuration
R1 = vector(QQ, [0, 0])
R2 = vector(QQ, [1, 0])
R3 = vector(QQ, [0, 1])
B1 = vector(QQ, [1, 1])
B2 = vector(QQ, [3, 9])

print("="*70)
print("POINT DISTRIBUTION ANALYSIS")
print("="*70)
print()

reds = [R1, R2, R3]
all_blues = [B1, B2]

for day in range(1, 6):
    new_blues = compute_next_day(reds, all_blues)
    all_blues = all_blues + new_blues

print(f"Total g(5) = {len(all_blues):,} points")
print()

# QUESTION 1: Distribution by region
regions = [10, 100, 1000, 10000]
counts = {r: 0 for r in regions}
outliers = []

for b in all_blues:
    x, y = float(b[0]), float(b[1])
    max_coord = max(abs(x), abs(y))

    classified = False
    for r in regions:
        if max_coord < r:
            counts[r] += 1
            classified = True
            break

    if not classified:
        outliers.append((x, y, b))

print("QUESTION 1: Point Distribution by Region")
print("-" * 70)
print(f"Within |x|,|y| < 10:     {counts[10]:6,} points ({100*counts[10]/len(all_blues):.2f}%)")
print(f"Within |x|,|y| < 100:    {counts[100]:6,} points ({100*counts[100]/len(all_blues):.2f}%)")
print(f"Within |x|,|y| < 1000:   {counts[1000]:6,} points ({100*counts[1000]/len(all_blues):.2f}%)")
print(f"Within |x|,|y| < 10000:  {counts[10000]:6,} points ({100*counts[10000]/len(all_blues):.2f}%)")
print(f"Outliers (|x|,|y| >= 10000): {len(outliers):6,} points ({100*len(outliers)/len(all_blues):.2f}%)")
print()

# QUESTION 3: Analyze outliers
print("QUESTION 3: Outlier Analysis")
print("-" * 70)
print(f"Total outlier count: {len(outliers)}")
print()

# Sort by distance from origin
outliers_sorted = sorted(outliers, key=lambda p: p[0]**2 + p[1]**2, reverse=True)

print("Top 10 most extreme points:")
for i, (x, y, vec) in enumerate(outliers_sorted[:10], 1):
    dist = (x**2 + y**2)**0.5
    print(f"{i:2}. ({x:12.4f}, {y:12.4f})  distance = {dist:12.2f}")
    print(f"    Exact rational: {vec}")
    print()

# Find which day these were created
print("Tracing creation of most extreme point...")
most_extreme = outliers_sorted[0][2]
print(f"Target point: {most_extreme}")
print()

# Recompute and track
reds = [R1, R2, R3]
all_blues_tracked = [B1, B2]

for day in range(1, 6):
    new_blues = compute_next_day(reds, all_blues_tracked)

    if most_extreme in new_blues:
        print(f"*** CREATED ON DAY {day} ***")
        print(f"From intersection of lines through (red, blue) pairs")
        print(f"Number of blues available: {len(all_blues_tracked)}")
        print(f"Number of line pairs: {3 * len(all_blues_tracked)} choose 2 = {3*len(all_blues_tracked)*(3*len(all_blues_tracked)-1)//2}")

        # Find which lines created this point
        lines = []
        for r in reds:
            for b in all_blues_tracked:
                lines.append((r, b))

        print("\nSearching for the two lines that created this point...")
        found = False
        for i, (p1, p2) in enumerate(lines):
            for j, (p3, p4) in enumerate(lines[i+1:], i+1):
                pt = line_intersection(p1, p2, p3, p4)
                if pt is not None and pt == most_extreme:
                    print(f"Line 1: {p1} to {p2}")
                    print(f"Line 2: {p3} to {p4}")
                    found = True
                    break
            if found:
                break

        break

    all_blues_tracked = all_blues_tracked + new_blues

print()
print("="*70)
