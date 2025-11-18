#!/usr/bin/env python3
"""
TEST: Are g(1)=8 and g(2)=28 independent optimization results?

HYPOTHESIS: g(n) = max over all configurations of blues after n days
- g(1) might come from config A
- g(2) might come from config B (different from A)
- They are NOT necessarily the same evolutionary path

APPROACH:
1. Generate many random initial blue configurations
2. For each, compute blues after day 1 and day 2
3. Find: max(day 1 blues) and max(day 2 blues)
4. Check if they come from SAME or DIFFERENT configs
"""

from fractions import Fraction
import random
from itertools import combinations

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
print("INDEPENDENT OPTIMIZATION TEST")
print("="*70)
print()

# Generate random configurations
results = []
random.seed(42)

print("Testing 1000 random configurations...")
print()

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

print(f"Tested {len(results)} valid configurations")
print()

# Find maxima
max_g1 = max(r['g1'] for r in results)
max_g2 = max(r['g2'] for r in results)

print(f"Maximum g(1) found: {max_g1}")
print(f"Maximum g(2) found: {max_g2}")
print()

# Configs achieving maxima
configs_max_g1 = [r for r in results if r['g1'] == max_g1]
configs_max_g2 = [r for r in results if r['g2'] == max_g2]

print(f"Configurations achieving max g(1)={max_g1}: {len(configs_max_g1)}")
for i, r in enumerate(configs_max_g1[:5]):
    print(f"  Config {i+1}: B1={r['config'][0]}, B2={r['config'][1]} → g(1)={r['g1']}, g(2)={r['g2']}")

print()
print(f"Configurations achieving max g(2)={max_g2}: {len(configs_max_g2)}")
for i, r in enumerate(configs_max_g2[:5]):
    print(f"  Config {i+1}: B1={r['config'][0]}, B2={r['config'][1]} → g(1)={r['g1']}, g(2)={r['g2']}")

print()
print("="*70)
print("KEY QUESTION: Are optimal configs for g(1) and g(2) the SAME?")
print("="*70)
print()

# Check overlap
configs_max_g1_set = set(r['config'] for r in configs_max_g1)
configs_max_g2_set = set(r['config'] for r in configs_max_g2)
overlap = configs_max_g1_set & configs_max_g2_set

if overlap:
    print(f"✓ YES - {len(overlap)} configuration(s) achieve BOTH max g(1) AND max g(2)")
    print("  This means g(1)=8 and g(2)=28 come from the SAME evolutionary path")
else:
    print(f"✗ NO - Zero overlap!")
    print("  g(1) is optimized by one set of configs")
    print("  g(2) is optimized by a DIFFERENT set of configs")
    print()
    print("  This would mean g(n) is truly INDEPENDENT optimization per n")

print()
print("="*70)
