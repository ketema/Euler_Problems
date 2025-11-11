#!/usr/bin/env python3
"""
Test if DIFFERENT configurations at each day can give g(n).

TWO INTERPRETATIONS:
A) ONE configuration evolves: 3 fixed reds, blues grow
B) DIFFERENT optimal config each day: choose best 3 reds + 2 blues each time

User's insight: "classic puzzle misdirect" - don't dismiss B!
"""

from sympy import Point2D, Line, Rational
from sympy.geometry import intersection
from itertools import combinations
import sys

def count_new_blues(reds, blues):
    """Given 3 reds and 2 blues, count new blues generated"""
    # Draw lines from reds to blues
    lines = []
    for red in reds:
        for blue in blues:
            lines.append(Line(red, blue))

    # Find intersections
    new_points = set()
    for i in range(len(lines)):
        for j in range(i+1, len(lines)):
            result = intersection(lines[i], lines[j])
            if not result:
                continue
            p = result[0]
            if not hasattr(p, 'x'):
                continue
            # Check if new (not in reds or existing blues)
            if p not in reds and p not in blues and p not in new_points:
                new_points.add(p)

    return len(blues) + len(new_points), new_points

print("="*70)
print("CONFIGURATION OPTIMIZATION TEST")
print("="*70)
print()

print("QUESTION: Can different configurations give different g(n)?")
print()

# Standard configuration that gives g(1)=8, g(2)=28
standard_reds = [
    Point2D(Rational(0), Rational(0)),
    Point2D(Rational(4), Rational(0)),
    Point2D(Rational(2), Rational(3))
]
standard_blues = [
    Point2D(Rational(1), Rational(1)),
    Point2D(Rational(3), Rational(2))
]

print("STANDARD CONFIGURATION:")
print("-"*70)
print(f"Reds: {[(float(p.x), float(p.y)) for p in standard_reds]}")
print(f"Blues: {[(float(p.x), float(p.y)) for p in standard_blues]}")

total, new = count_new_blues(standard_reds, standard_blues)
print(f"Result: {total} total blues ({len(new)} new)")
print()

# Test: What if we use DIFFERENT points as reds/blues?
print("="*70)
print("TEST 1: Can we get g(1) ≠ 8 with different initial config?")
print("="*70)
print()

# Try some alternative 5-point configurations
alt_configs = [
    # Config 1: Points on a grid
    {
        'reds': [Point2D(0,0), Point2D(1,0), Point2D(0,1)],
        'blues': [Point2D(1,1), Point2D(2,2)],
        'label': 'Grid points'
    },
    # Config 2: Symmetric triangle + 2 blues
    {
        'reds': [Point2D(0,0), Point2D(2,0), Point2D(1,2)],
        'blues': [Point2D(1,1), Point2D(Rational(3,2),Rational(1,2))],
        'label': 'Symmetric triangle'
    },
    # Config 3: Collinear reds
    {
        'reds': [Point2D(0,0), Point2D(1,0), Point2D(2,0)],
        'blues': [Point2D(0,1), Point2D(1,1)],
        'label': 'Collinear reds'
    },
]

results_day1 = []
for config in alt_configs:
    total, new = count_new_blues(config['reds'], config['blues'])
    results_day1.append((config['label'], total))
    print(f"{config['label']:20s}: {total} blues")

print()
print(f"Standard config: 8 blues")
print(f"Max alternative: {max(r[1] for r in results_day1)} blues")

if all(r[1] <= 8 for r in results_day1):
    print("✓ Standard config appears optimal for day 1")
else:
    print("✗ Found better configuration!")

print()
print("="*70)
print("TEST 2: At Day 1, can we RECONFIGURE to get different Day 2?")
print("="*70)
print()

# After day 1, we have 11 total points (3 reds + 8 blues)
# Can we choose DIFFERENT 3 reds + 2 blues to maximize day 2?

print("After Day 1 with standard config, we have 11 points total.")
print("Question: If we RE-CHOOSE which 3 are 'red' and which 2 are 'blue',")
print("can we get MORE than 28 total blues at day 2?")
print()

# First, get all 11 points after day 1
all_points_day1 = set(standard_reds)
total, new_blues = count_new_blues(standard_reds, standard_blues)
all_points_day1.update(standard_blues)
all_points_day1.update(new_blues)
all_points_day1 = list(all_points_day1)

print(f"We have {len(all_points_day1)} points total.")
print(f"Possible reconfigurations: C(11,3) × C(8,2) = {165 * 28} = 4,620")
print()

# This is computationally feasible! Let's try ALL configurations
print("Testing all 4,620 configurations...")
print("(This will take a moment...)")
print()

best_config = None
best_count = 0
tested = 0

for red_indices in combinations(range(len(all_points_day1)), 3):
    reds_new = [all_points_day1[i] for i in red_indices]
    remaining_indices = [i for i in range(len(all_points_day1)) if i not in red_indices]

    for blue_indices in combinations(remaining_indices, 2):
        blues_new = [all_points_day1[i] for i in blue_indices]

        total, _ = count_new_blues(reds_new, blues_new)
        tested += 1

        if total > best_count:
            best_count = total
            best_config = (reds_new, blues_new)

        # Progress indicator
        if tested % 500 == 0:
            print(f"  Tested {tested}/4620... best so far: {best_count}", end='\r')

print()
print()
print(f"Tested {tested} configurations")
print(f"Best result: {best_count} blues")
print(f"Standard evolving config: 28 blues")
print()

if best_count > 28:
    print("✗ FOUND BETTER CONFIGURATION!")
    print()
    print("This means Interpretation B is correct:")
    print("  → Different optimal configurations for different days")
    print("  → g(n) = max over ALL possible configs at day n")
    print()
    print("Best configuration found:")
    print(f"  Reds: {[(float(p.x), float(p.y)) for p in best_config[0]]}")
    print(f"  Blues: {[(float(p.x), float(p.y)) for p in best_config[1]]}")
elif best_count == 28:
    print("? Multiple configurations give 28 blues (including standard)")
    print()
    print("Need to check if all optimal configs give same sequence.")
else:
    print("✓ Standard configuration is optimal")
    print()
    print("This supports Interpretation A:")
    print("  → One fixed optimal configuration evolves over time")

print()
print("="*70)
print("CONCLUSION")
print("="*70)
print()

if best_count != 28:
    print("The answer depends on which interpretation is correct!")
    print()
    print("If DIFFERENT configs optimal each day:")
    print("  → Need completely different approach")
    print("  → Optimization problem at each step")
    print("  → Growth pattern may differ from bilinear recurrence")
else:
    print("Even if we can reconfigure, the maximum is still achieved by")
    print("the standard evolving configuration (or equivalent ones).")
    print()
    print("This suggests the bilinear recurrence may still be valid.")
