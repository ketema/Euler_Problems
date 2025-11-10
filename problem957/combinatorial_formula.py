#!/usr/bin/env python3
"""
Key insight: Count DISTINCT LINES, not (red, blue) pairs!

For maximal g(n), we want general position: no unnecessary collinearities.
So each (red, blue) pair creates a distinct line.

Then count distinct intersection points (accounting for concurrency).
"""

from math import comb

def analyze_day_combinatorics(R, B):
    """
    Analyze combinatorics for R reds and B blues in general position.

    Assumptions for maximal configuration:
    - All (red, blue) pairs create distinct lines
    - No three points collinear except as forced by construction
    """
    # Number of (red, blue) pairs = distinct lines in general position
    num_pairs = R * B

    # Each pair creates a line
    num_lines = num_pairs

    # In projective plane: every 2 distinct lines intersect
    max_intersections = comb(num_lines, 2)

    # Forced coincidences at colored points
    coincidences_at_reds = R * comb(B, 2)  # Each red has B lines through it
    coincidences_at_blues = B * comb(R, 2)  # Each blue has R lines through it

    # In GENERAL POSITION: remaining intersections are all at distinct white points
    new_blues_general = max_intersections - coincidences_at_reds - coincidences_at_blues

    return {
        'pairs': num_pairs,
        'lines': num_lines,
        'max_intersections': max_intersections,
        'at_reds': coincidences_at_reds,
        'at_blues': coincidences_at_blues,
        'new_blues_general': new_blues_general
    }


print("="*70)
print("COMBINATORIAL ANALYSIS - DISTINCT LINES IN GENERAL POSITION")
print("="*70)
print()

# Known sequence
g_known = [2, 8, 28, 184]
R = 3

for day in range(len(g_known) - 1):
    B_current = g_known[day]
    B_next = g_known[day + 1]
    actual_new = B_next - B_current

    analysis = analyze_day_combinatorics(R, B_current)

    print(f"Day {day} → {day+1}:")
    print(f"  Blues at start: {B_current}")
    print(f"  (R,B) pairs: {analysis['pairs']}")
    print(f"  Distinct lines (general position): {analysis['lines']}")
    print(f"  Max intersections C({analysis['lines']},2): {analysis['max_intersections']}")
    print(f"  At reds: {analysis['at_reds']}")
    print(f"  At blues: {analysis['at_blues']}")
    print(f"  New blues (general position): {analysis['new_blues_general']}")
    print(f"  Actual new blues: {actual_new}")

    if analysis['new_blues_general'] != actual_new:
        discrepancy = analysis['new_blues_general'] - actual_new
        print(f"  ⚠ Discrepancy: {discrepancy}")
        print(f"    This means {discrepancy} intersections are CONCURRENT at white points")

        # If k lines meet at one point, that's C(k,2) pairwise intersections
        # but only 1 blue point created
        # So we "lose" C(k,2) - 1 potential blues per concurrent point

        # For day 2: 168 expected, 20 actual, discrepancy = 148
        # This suggests heavy concurrency!
    else:
        print(f"  ✓ General position formula matches!")

    print()

print("="*70)
print("KEY INSIGHT")
print("="*70)
print()
print("Day 1: Perfect general position - formula matches exactly!")
print("Day 2+: Heavy concurrency reduces new blues")
print()
print("For MAXIMAL g(n), we need to understand the concurrency pattern.")
print("In projective plane, what forces lines to be concurrent?")
print()
print("Classic theorem: In projective plane, if we have certain configurations,")
print("lines are forced to meet at specific points (Desargues, Pappus, etc.)")
print()

# Let me try to see if there's a pattern in the discrepancies
print("="*70)
print("ANALYZING DISCREPANCY PATTERN")
print("="*70)
print()

discrepancies = []
for day in range(len(g_known) - 1):
    B_current = g_known[day]
    B_next = g_known[day + 1]
    actual_new = B_next - B_current
    analysis = analyze_day_combinatorics(R, B_current)
    disc = analysis['new_blues_general'] - actual_new
    discrepancies.append(disc)

print("Discrepancies (concurrent intersections):", discrepancies)
print()

# Check if discrepancies have a pattern
if len(discrepancies) >= 3:
    print("Ratios:")
    for i in range(1, len(discrepancies)):
        if discrepancies[i-1] > 0:
            ratio = discrepancies[i] / discrepancies[i-1]
            print(f"  disc[{i}] / disc[{i-1}] = {ratio:.4f}")
    print()

# Perhaps the discrepancy is a function of B?
print("Discrepancy vs B:")
for i, disc in enumerate(discrepancies):
    B = g_known[i]
    print(f"  B={B}: discrepancy={disc}, disc/B²={disc/B**2:.4f}")
