#!/usr/bin/env python3
"""
Analyze ALL configurations tested - show full distribution of g(1) and g(2) values
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
        return None

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
    for i, (p1, p2) in enumerate(lines):
        for (p3, p4) in lines[i+1:]:
            pt = line_intersection(p1, p2, p3, p4)
            if pt is not None:
                pt_tuple = tuple(pt)
                if pt_tuple not in existing and pt_tuple not in new_points:
                    new_points.add(pt_tuple)

    return list(new_points)

# Fixed reds
reds = [(Fraction(0), Fraction(0)),
        (Fraction(1), Fraction(0)),
        (Fraction(0), Fraction(1))]

print("="*70)
print("COMPLETE CONFIGURATION ANALYSIS")
print("="*70)
print()

# Generate random configurations
results = []
random.seed(42)

print("Testing 1000 random configurations...")

for trial in range(1000):
    # Random initial blues
    b1 = (Fraction(random.randint(1, 20)), Fraction(random.randint(1, 20)))
    b2 = (Fraction(random.randint(1, 20)), Fraction(random.randint(1, 20)))

    if b1 == b2 or b1 in reds or b2 in reds:
        continue

    blues_day0 = [b1, b2]

    # Day 1
    new_day1 = compute_next_day(reds, blues_day0)
    blues_day1 = blues_day0 + new_day1
    g1 = len(blues_day1)

    # Day 2
    new_day2 = compute_next_day(reds, blues_day1)
    blues_day2 = blues_day1 + new_day2
    g2 = len(blues_day2)

    results.append({
        'config': (b1, b2),
        'g1': g1,
        'g2': g2
    })

print(f"Tested {len(results)} valid configurations\n")

# Distribution analysis
g1_distribution = defaultdict(int)
g2_distribution = defaultdict(int)
combo_distribution = defaultdict(int)

for r in results:
    g1_distribution[r['g1']] += 1
    g2_distribution[r['g2']] += 1
    combo_distribution[(r['g1'], r['g2'])] += 1

print("="*70)
print("DISTRIBUTION OF g(1) VALUES")
print("="*70)
for g1 in sorted(g1_distribution.keys(), reverse=True):
    count = g1_distribution[g1]
    pct = 100.0 * count / len(results)
    print(f"g(1) = {g1:3d}: {count:4d} configs ({pct:5.1f}%)")

print()
print("="*70)
print("DISTRIBUTION OF g(2) VALUES")
print("="*70)
for g2 in sorted(g2_distribution.keys(), reverse=True):
    count = g2_distribution[g2]
    pct = 100.0 * count / len(results)
    print(f"g(2) = {g2:3d}: {count:4d} configs ({pct:5.1f}%)")

print()
print("="*70)
print("DISTRIBUTION OF (g(1), g(2)) COMBINATIONS")
print("="*70)
for combo in sorted(combo_distribution.keys(), reverse=True):
    count = combo_distribution[combo]
    pct = 100.0 * count / len(results)
    print(f"(g(1)={combo[0]:2d}, g(2)={combo[1]:2d}): {count:4d} configs ({pct:5.1f}%)")

print()
print("="*70)
print("KEY FINDINGS")
print("="*70)
max_g1 = max(g1_distribution.keys())
max_g2 = max(g2_distribution.keys())
print(f"Maximum g(1) observed: {max_g1}")
print(f"Maximum g(2) observed: {max_g2}")

if max_g1 > 8:
    print(f"\n*** FOUND g(1) > 8! Maximum is {max_g1} ***")
    configs_above_8 = [r for r in results if r['g1'] > 8]
    print(f"Configurations achieving g(1) > 8: {len(configs_above_8)}")
    for r in configs_above_8[:10]:
        print(f"  B1={r['config'][0]}, B2={r['config'][1]} → g(1)={r['g1']}, g(2)={r['g2']}")
else:
    print(f"\nNo configuration achieved g(1) > 8")

if max_g2 > 28:
    print(f"\n*** FOUND g(2) > 28! Maximum is {max_g2} ***")
    configs_above_28 = [r for r in results if r['g2'] > 28]
    print(f"Configurations achieving g(2) > 28: {len(configs_above_28)}")
    for r in configs_above_28[:10]:
        print(f"  B1={r['config'][0]}, B2={r['config'][1]} → g(1)={r['g1']}, g(2)={r['g2']}")
else:
    print(f"\nNo configuration achieved g(2) > 28")

print()
print("="*70)
print("SAMPLE CONFIGURATIONS TABLE (first 50)")
print("="*70)
print(f"{'Index':>5} | {'B1_x':>4} {'B1_y':>4} | {'B2_x':>4} {'B2_y':>4} | {'g(1)':>4} | {'g(2)':>4}")
print("-"*70)
for i, r in enumerate(results[:50]):
    b1, b2 = r['config']
    print(f"{i:5d} | {int(b1[0]):4d} {int(b1[1]):4d} | {int(b2[0]):4d} {int(b2[1]):4d} | {r['g1']:4d} | {r['g2']:4d}")

print()
print("="*70)
print("COMPLETE DATA WRITTEN TO: all_configs_data.txt")
print("="*70)

# Write complete data to file
with open('all_configs_data.txt', 'w') as f:
    f.write("Complete Configuration Data\n")
    f.write("="*70 + "\n")
    f.write(f"{'Index':>5} | {'B1_x':>4} {'B1_y':>4} | {'B2_x':>4} {'B2_y':>4} | {'g(1)':>4} | {'g(2)':>4}\n")
    f.write("-"*70 + "\n")
    for i, r in enumerate(results):
        b1, b2 = r['config']
        f.write(f"{i:5d} | {int(b1[0]):4d} {int(b1[1]):4d} | {int(b2[0]):4d} {int(b2[1]):4d} | {r['g1']:4d} | {r['g2']:4d}\n")

print("All data saved.")
