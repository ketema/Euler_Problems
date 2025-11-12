#!/usr/bin/env python3
"""
HIRZEBRUCH INEQUALITY ANALYSIS FOR PE 957

Hirzebruch's Inequality (for arrangements in CP^2):
    t_2 + t_3 > d + sum_{r>5} (r-4)*t_r

Where:
- d = number of lines
- t_r = number of points where exactly r lines meet

For our problem at day n:
- We have 3 reds and g(n) blues
- Number of lines: d = 3 * g(n) (each red connects to each blue)
- Each NEW blue is an intersection point with some multiplicity r ≥ 2

This analysis computes the t_r distribution and checks Hirzebruch's inequality.
"""

from sympy import Point, Line, Rational
from sympy.geometry import intersection
from typing import Set, List, Dict
from collections import Counter

def compute_day_with_multiplicities(reds: List[Point], blues: Set[Point]) -> tuple:
    """
    Compute next day's blues AND track multiplicity distribution.

    Returns: (new_blues_set, t_r_distribution)
    where t_r_distribution is dict: multiplicity r -> count of points with that multiplicity
    """
    # Construct all lines through (red, blue) pairs
    lines = []
    for red in reds:
        for blue in blues:
            if red != blue:
                lines.append(Line(red, blue))

    d = len(lines)  # Number of lines

    existing = set(reds).union(blues)

    # For each intersection point, track which lines pass through it
    point_to_line_indices = {}

    for i, line1 in enumerate(lines):
        for j in range(i+1, len(lines)):
            line2 = lines[j]
            result = intersection(line1, line2)
            if result and hasattr(result[0], 'x'):
                p = result[0]
                if isinstance(p, Point):
                    if p not in point_to_line_indices:
                        point_to_line_indices[p] = set()
                    point_to_line_indices[p].add(i)
                    point_to_line_indices[p].add(j)

    # Compute t_r distribution (only for NEW blues)
    t_r = Counter()
    new_blues = set()

    for p, line_set in point_to_line_indices.items():
        if p not in existing:
            r = len(line_set)  # Multiplicity
            t_r[r] += 1
            new_blues.add(p)

    all_blues = blues.union(new_blues)

    return all_blues, dict(t_r), d

# Configuration that produces [2,8,28,184,1644,19068]
reds = [
    Point(Rational(0), Rational(0)),
    Point(Rational(4), Rational(0)),
    Point(Rational(2), Rational(3))
]

blues = {
    Point(Rational(1), Rational(1)),
    Point(Rational(3), Rational(2))
}

print("="*80)
print("HIRZEBRUCH INEQUALITY ANALYSIS")
print("="*80)
print()

sequence = [len(blues)]
all_t_r_data = []

import time
for day in range(5):  # Compute days 0→1 through 4→5
    day_start = time.time()

    print(f"Day {day} → {day+1}")
    print(f"  Current g({day}) = {len(blues)}")

    blues, t_r, d = compute_day_with_multiplicities(reds, blues)

    g_n = len(blues)
    sequence.append(g_n)
    new_count = sum(t_r.values())

    print(f"  Lines constructed: d = {d}")
    print(f"  New blues: {new_count}")
    print(f"  Total g({day+1}) = {g_n}")
    print(f"  Multiplicity distribution t_r:")

    # Sort by multiplicity
    for r in sorted(t_r.keys()):
        print(f"    t_{r} = {t_r[r]} (points where exactly {r} lines meet)")

    # Compute Hirzebruch's inequality terms
    t_2 = t_r.get(2, 0)
    t_3 = t_r.get(3, 0)
    sum_high = sum((r-4) * count for r, count in t_r.items() if r > 5)

    LHS = t_2 + t_3
    RHS = d + sum_high

    print(f"  Hirzebruch check:")
    print(f"    LHS = t_2 + t_3 = {t_2} + {t_3} = {LHS}")
    print(f"    RHS = d + sum_{{r>5}} (r-4)*t_r = {d} + {sum_high} = {RHS}")
    print(f"    Inequality satisfied? {LHS} > {RHS} : {LHS > RHS}")
    print(f"    Margin: {LHS - RHS}")

    elapsed = time.time() - day_start
    print(f"  Time: {elapsed:.1f}s")
    print()

    all_t_r_data.append({
        'day': day,
        'd': d,
        't_r': t_r,
        'g_n': g_n,
        't_2': t_2,
        't_3': t_3,
        'LHS': LHS,
        'RHS': RHS,
        'margin': LHS - RHS
    })

    if day >= 4:
        print(f"Stopping at day 5 (already have g(5) = {g_n})")
        break

print("="*80)
print("SUMMARY")
print("="*80)
print()
print(f"Sequence: {sequence}")
print()

print("Evolution of Hirzebruch inequality:")
print("Day | d (lines) | t_2   | t_3   | LHS   | RHS   | Margin | Satisfied?")
print("----|-----------|-------|-------|-------|-------|--------|------------")
for data in all_t_r_data:
    satisfied = "YES" if data['LHS'] > data['RHS'] else "NO"
    print(f"{data['day']:3} | {data['d']:9} | {data['t_2']:5} | {data['t_3']:5} | "
          f"{data['LHS']:5} | {data['RHS']:5} | {data['margin']:6} | {satisfied}")

print()
print("="*80)
print("ANALYSIS")
print("="*80)
print()

# Check if there's a pattern in the margin
margins = [d['margin'] for d in all_t_r_data]
print(f"Hirzebruch margins: {margins}")
print()

# Check if margin/d ratio is stable
if all_t_r_data:
    print("Margin as fraction of d:")
    for data in all_t_r_data:
        if data['d'] > 0:
            ratio = data['margin'] / data['d']
            print(f"  Day {data['day']}→{data['day']+1}: margin/d = {data['margin']}/{data['d']} = {ratio:.4f}")

print()
print("Key Question: Does the t_r distribution pattern extend to day 16?")
print("If we can characterize the evolution of t_r, we might predict g(16).")
