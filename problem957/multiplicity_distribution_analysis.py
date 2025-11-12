#!/usr/bin/env python3
"""
MULTIPLICITY DISTRIBUTION ANALYSIS
AI Panel Priority Action (Both GPT-4.1 + Claude Sonnet)

Goal: For days 0-4, compute EXACT multiplicity distribution {m: count}
where m = number of lines passing through each blue point.

If distribution is stable (e.g., "40% mult-2, 60% mult-3"), then:
- avg_multiplicity can be computed
- new_blues(n) = C(lines(n), 2) / avg_multiplicity
- Gives recurrence: g(n) = g(n-1) + new_blues(n)
"""

from sympy import Point, Line, Rational
from typing import Set, List, Dict
from collections import Counter
import time

def create_point(x, y) -> Point:
    """Create SymPy Point with exact Rational coordinates"""
    return Point(Rational(x), Rational(y))

def compute_day_with_multiplicity(reds: List[Point], blues: Set[Point]) -> tuple:
    """
    Compute next day with FULL multiplicity tracking

    Returns:
        (new_blues_set, multiplicity_dict)
        where multiplicity_dict[point] = number of lines through that point
    """
    # Generate all lines
    lines = []
    for red in reds:
        for blue in blues:
            if red != blue:
                lines.append(Line(red, blue))

    # Track which lines pass through which points
    point_to_lines = {}  # point -> list of line indices

    # Compute all pairwise intersections
    existing_points = set(reds).union(blues)

    for i, line1 in enumerate(lines):
        for j, line2 in enumerate(lines):
            if i < j:  # Avoid duplicates
                result = line1.intersection(line2)

                if result:
                    p = result[0]

                    # Ensure intersection is a Point, not a Line (collinear case)
                    if isinstance(p, Point):
                        # Track lines through this point
                        if p not in point_to_lines:
                            point_to_lines[p] = set()
                        point_to_lines[p].add(i)
                        point_to_lines[p].add(j)

    # Extract NEW blues with their multiplicities
    new_blues = set()
    multiplicity_dict = {}

    for p, line_set in point_to_lines.items():
        if p not in existing_points:
            new_blues.add(p)
            multiplicity_dict[p] = len(line_set)

    all_blues = blues.union(new_blues)

    return all_blues, multiplicity_dict

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
print("MULTIPLICITY DISTRIBUTION ANALYSIS")
print("="*80)
print()

print("Configuration:")
print(f"  Reds:  {len(reds)} points")
print(f"  Blues (day 0): {len(blues_day0)} points")
print()

# Track sequence and multiplicity distributions
sequence = [len(blues_day0)]
all_multiplicities = []  # List of Counter objects, one per day
blues = blues_day0

print("="*80)
print("DAY-BY-DAY ANALYSIS")
print("="*80)
print()

for day in range(5):  # Compute g(0) through g(4)
    print(f"Day {day} → Day {day+1}")
    print("-" * 80)

    start = time.time()
    new_blues, mult_dict = compute_day_with_multiplicity(reds, blues)
    elapsed = time.time() - start

    num_lines = 3 * len(blues)  # 3 reds × blues
    num_new = len(new_blues) - len(blues)

    # Multiplicity distribution for NEW blues only
    new_multiplicities = Counter(mult_dict.values())
    all_multiplicities.append(new_multiplicities)

    print(f"  Current blues: g({day}) = {len(blues)}")
    print(f"  Lines drawn: {num_lines}")
    print(f"  Possible intersections: C({num_lines},2) = {num_lines * (num_lines - 1) // 2}")
    print(f"  New blues: {num_new}")
    print(f"  New blues multiplicity distribution:")

    for mult in sorted(new_multiplicities.keys()):
        count = new_multiplicities[mult]
        percentage = 100 * count / num_new if num_new > 0 else 0
        print(f"    Multiplicity {mult}: {count:4d} points ({percentage:5.1f}%)")

    # Average multiplicity
    if mult_dict:
        avg_mult = sum(m * c for m, c in new_multiplicities.items()) / num_new
        print(f"  Average multiplicity: {avg_mult:.2f}")

    print(f"  Time: {elapsed:.2f}s")
    print(f"  Result: g({day+1}) = {len(new_blues)}")
    print()

    blues = new_blues
    sequence.append(len(blues))

    if elapsed > 600:  # 10 minutes
        print(f"⚠️  Computation becoming slow, stopping at day {day+1}")
        break

print("="*80)
print("SUMMARY: MULTIPLICITY PATTERN ANALYSIS")
print("="*80)
print()

print("Sequence:", sequence)
print()

print("Multiplicity Distribution Evolution:")
print()

# Check if distribution is stable
for day, mult_counter in enumerate(all_multiplicities):
    print(f"Day {day}→{day+1}:")
    total = sum(mult_counter.values())
    for mult in sorted(mult_counter.keys()):
        count = mult_counter[mult]
        pct = 100 * count / total
        print(f"  Mult {mult}: {count:4d} / {total:4d} = {pct:5.1f}%")

    avg = sum(m * c for m, c in mult_counter.items()) / total
    print(f"  Average: {avg:.3f}")
    print()

print("="*80)
print("PATTERN ANALYSIS")
print("="*80)
print()

# Check if ratios are stable
print("Checking if multiplicity distribution stabilizes:")
print()

if len(all_multiplicities) >= 2:
    # Compare consecutive distributions
    for day in range(len(all_multiplicities) - 1):
        dist1 = all_multiplicities[day]
        dist2 = all_multiplicities[day + 1]

        print(f"Day {day}→{day+1} vs Day {day+1}→{day+2}:")

        all_mults = set(dist1.keys()).union(set(dist2.keys()))
        for mult in sorted(all_mults):
            total1 = sum(dist1.values())
            total2 = sum(dist2.values())
            pct1 = 100 * dist1.get(mult, 0) / total1
            pct2 = 100 * dist2.get(mult, 0) / total2
            diff = pct2 - pct1
            print(f"  Mult {mult}: {pct1:5.1f}% → {pct2:5.1f}% (Δ {diff:+5.1f}%)")
        print()

print("="*80)
print("RECURRENCE DERIVATION")
print("="*80)
print()

print("If average multiplicity is stable, we can derive:")
print()
print("Formula:")
print("  lines(n) = 3 × g(n-1)")
print("  possible_intersections(n) = C(lines(n), 2)")
print("  new_blues(n) = possible_intersections(n) / avg_multiplicity")
print("  g(n) = g(n-1) + new_blues(n)")
print()

# Try to fit a recurrence
if len(sequence) >= 3:
    print("Testing recurrence with observed average multiplicities:")
    print()

    for day in range(1, len(sequence)):
        if day - 1 < len(all_multiplicities):
            mult_dist = all_multiplicities[day - 1]
            total_new = sum(mult_dist.values())
            avg_mult = sum(m * c for m, c in mult_dist.items()) / total_new if total_new > 0 else 0

            lines = 3 * sequence[day - 1]
            possible = lines * (lines - 1) // 2
            predicted_new = possible / avg_mult if avg_mult > 0 else 0
            predicted_g = sequence[day - 1] + predicted_new
            actual_g = sequence[day]

            print(f"Day {day-1}→{day}:")
            print(f"  g({day-1}) = {sequence[day-1]}")
            print(f"  Avg multiplicity = {avg_mult:.3f}")
            print(f"  Lines = {lines}")
            print(f"  Possible intersections = {possible}")
            print(f"  Predicted new = {possible}/{avg_mult:.3f} = {predicted_new:.1f}")
            print(f"  Predicted g({day}) = {predicted_g:.1f}")
            print(f"  Actual g({day}) = {actual_g}")
            print(f"  Error: {abs(predicted_g - actual_g):.1f} ({100*abs(predicted_g - actual_g)/actual_g:.1f}%)")
            print()

print("="*80)
print("CONCLUSION")
print("="*80)
print()
print("If multiplicity distribution is stable:")
print("  → Can derive exact recurrence for g(n)")
print("  → Can compute g(16) without full simulation")
print()
print("If multiplicity distribution varies:")
print("  → Need to model how distribution evolves")
print("  → May require deeper geometric analysis")
print()
