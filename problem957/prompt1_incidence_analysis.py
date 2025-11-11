#!/usr/bin/env python3
"""
PROMPT 1: Projective Geometry Incidence Structure Analysis
DETERMINISTIC execution using SymPy exact arithmetic

Objective: Verify simulation and analyze incidence structure for patterns
Tools: SymPy Point/Line/intersection with Rational arithmetic
Output: Verified sequence, growth ratios, structure analysis
"""

from sympy import Point, Line, Rational, intersection
from typing import List, Set, Dict, Tuple
import json

def create_point(x, y) -> Point:
    """Create SymPy Point with exact Rational coordinates"""
    return Point(Rational(x), Rational(y))

def compute_day(reds: List[Point], blues: Set[Point]) -> Tuple[Set[Point], Dict]:
    """
    DETERMINISTIC computation for one day
    Returns: (new_blue_set, analysis_dict)
    """
    # STEP 1: Construct all lines (red to blue)
    lines = []
    for red in reds:
        for blue in blues:
            if red != blue:
                lines.append(Line(red, blue))

    # STEP 2: Compute all pairwise intersections
    existing_points = set(reds).union(blues)
    intersection_points = set()

    for i, line1 in enumerate(lines):
        for line2 in lines[i+1:]:
            result = intersection(line1, line2)
            if result:
                if hasattr(result[0], 'x'):  # It's a Point
                    p = result[0]
                    if p not in existing_points:
                        intersection_points.add(p)

    # STEP 3: New blues = current blues UNION new intersection points
    new_blues = blues.union(intersection_points)

    # STEP 4: Analysis
    analysis = {
        'num_lines': len(lines),
        'num_existing': len(existing_points),
        'num_new_intersections': len(intersection_points),
        'num_total_blues': len(new_blues)
    }

    return new_blues, analysis

def main():
    print("="*80)
    print("PROMPT 1: DETERMINISTIC INCIDENCE STRUCTURE ANALYSIS")
    print("="*80)
    print()

    # STEP 1: Define initial configuration with EXACT Rational coordinates
    reds = [
        create_point(0, 0),
        create_point(4, 0),
        create_point(2, 3)
    ]

    initial_blues = {
        create_point(1, 1),
        create_point(3, 2)
    }

    print("Initial Configuration:")
    print(f"  Reds: {reds}")
    print(f"  Initial Blues: {initial_blues}")
    print()

    # STEP 2: Iterate through days 0-4 (we have verified values)
    sequence = [len(initial_blues)]  # g(0) = 2
    ratios = []
    blues = initial_blues

    print("Day-by-Day Computation:")
    print()

    for day in range(5):  # Days 0→1, 1→2, 2→3, 3→4, 4→5
        print(f"Day {day} → Day {day+1}")
        print(f"  Current blues: g({day}) = {len(blues)}")

        blues, analysis = compute_day(reds, blues)

        print(f"  Lines constructed: {analysis['num_lines']}")
        print(f"  New intersections: {analysis['num_new_intersections']}")
        print(f"  Result: g({day+1}) = {analysis['num_total_blues']}")

        sequence.append(analysis['num_total_blues'])

        if len(sequence) >= 2:
            ratio = sequence[-1] / sequence[-2]
            ratios.append(ratio)
            print(f"  Growth ratio: {ratio:.3f}")

        print()

    # STEP 3: Verification against known values
    print("="*80)
    print("VERIFICATION")
    print("="*80)
    print()

    known_sequence = [2, 8, 28, 184, 1644]
    verified = True

    for i in range(min(5, len(sequence))):
        expected = known_sequence[i]
        actual = sequence[i]
        match = "✓" if expected == actual else "✗"
        if expected != actual:
            verified = False
        print(f"  g({i}): expected={expected}, actual={actual} {match}")

    print()
    if verified:
        print("✓ ALL VALUES VERIFIED CORRECT")
    else:
        print("✗ MISMATCH - CHECK IMPLEMENTATION")

    print()

    # STEP 4: Growth ratio analysis
    print("="*80)
    print("GROWTH RATIO ANALYSIS")
    print("="*80)
    print()

    for i, ratio in enumerate(ratios):
        print(f"  g({i+1})/g({i}) = {ratio:.6f}")

    print()

    # Check for pattern
    if len(ratios) >= 3:
        ratio_diffs = [ratios[i+1] - ratios[i] for i in range(len(ratios)-1)]
        print("Ratio differences (checking for linear/quadratic growth):")
        for i, diff in enumerate(ratio_diffs):
            print(f"  Δratio[{i}] = {diff:.6f}")
        print()

    # STEP 5: Output JSON for chaining
    output = {
        "verified_sequence": sequence,
        "growth_ratios": [float(r) for r in ratios],
        "ratio_pattern": "non-constant, increasing",
        "verification_status": "PASS" if verified else "FAIL",
        "next_prompt": 2 if verified else "fix_implementation"
    }

    print("="*80)
    print("OUTPUT (for chaining to next prompt)")
    print("="*80)
    print()
    print(json.dumps(output, indent=2))
    print()

    # STEP 6: Additional analysis
    print("="*80)
    print("ADDITIONAL ANALYSIS")
    print("="*80)
    print()

    # Check if sequence appears in patterns
    print("Checking against known patterns:")
    print(f"  - Polynomial? Differences analysis:")

    diffs1 = [sequence[i+1] - sequence[i] for i in range(len(sequence)-1)]
    print(f"    Δ¹: {diffs1}")

    if len(diffs1) >= 2:
        diffs2 = [diffs1[i+1] - diffs1[i] for i in range(len(diffs1)-1)]
        print(f"    Δ²: {diffs2}")

    if len(diffs2) >= 2:
        diffs3 = [diffs2[i+1] - diffs2[i] for i in range(len(diffs2)-1)]
        print(f"    Δ³: {diffs3}")

    print()
    print("  - Exponential? Log ratio check:")
    import math
    for i in range(1, len(sequence)):
        if sequence[i] > 0 and sequence[i-1] > 0:
            log_ratio = math.log(sequence[i]) / math.log(sequence[i-1])
            print(f"    log(g({i}))/log(g({i-1})) = {log_ratio:.6f}")

    print()

if __name__ == "__main__":
    main()
