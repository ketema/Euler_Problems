#!/usr/bin/env python3
"""
Symbolic analysis of Problem 957 using SymPy.

Attempt to find closed-form κ_t by:
1. Converting to exact rational coordinates
2. Computing symbolic cross-ratios and projective invariants
3. Analyzing algebraic structure of coincidence patterns
4. Searching for recurrence relations in κ_t
"""

import sys
from sympy import *
from sympy.geometry import Point as SymPoint, Line as SymLine
from fractions import Fraction
from collections import defaultdict

sys.path.insert(0, '.')

# Import our numerical configuration
from src.geometry import Point as NumPoint

# Original numerical configuration
RED_NUMERICAL = [
    (-1.1420985748, -3.1278529420),
    (1.7213348846, -0.8343651343),
    (4.3760906863, 2.3859745813)
]

BLUE_NUMERICAL = [
    (-1.8437265624, 1.4483260402),
    (-1.0486909239, 2.1320688328)
]


def rationalize_config(coords, max_denom=100000):
    """Convert floating point coordinates to exact rationals."""
    print("Converting to exact rational arithmetic...")
    print(f"Maximum denominator: {max_denom:,}")
    print()

    rational_coords = []
    for x, y in coords:
        # Use SymPy's nsimplify with rational option
        # First try with rational=True to get exact representation
        x_rat = nsimplify(x, rational=True)
        y_rat = nsimplify(y, rational=True)

        # Then limit denominator using Rational.limit_denominator
        if x_rat.is_Rational:
            from fractions import Fraction
            x_frac = Fraction(x).limit_denominator(max_denom)
            x_rat = Rational(x_frac.numerator, x_frac.denominator)
        if y_rat.is_Rational:
            from fractions import Fraction
            y_frac = Fraction(y).limit_denominator(max_denom)
            y_rat = Rational(y_frac.numerator, y_frac.denominator)

        # Check error
        x_err = abs(float(x_rat) - x)
        y_err = abs(float(y_rat) - y)

        rational_coords.append((x_rat, y_rat))

        print(f"({x:.10f}, {y:.10f})")
        print(f"  → ({x_rat}, {y_rat})")
        print(f"  Error: x={x_err:.2e}, y={y_err:.2e}")
        print()

    return rational_coords


def analyze_cross_ratios(red_coords, blue_coords):
    """
    Compute cross-ratios - the fundamental projective invariant.

    For 4 collinear points A,B,C,D, the cross-ratio is:
    (A,B;C,D) = (AC/BC) / (AD/BD)

    Cross-ratios are invariant under projective transformations.
    """
    print("="*80)
    print("CROSS-RATIO ANALYSIS")
    print("="*80)
    print()

    # Create symbolic points
    red = [SymPoint(x, y) for x, y in red_coords]
    blue = [SymPoint(x, y) for x, y in blue_coords]

    # For each line, find all points on it and compute cross-ratios
    all_points = red + blue

    # Generate all red-blue lines
    lines_with_points = []
    for r in red:
        for b in blue:
            line = SymLine(r, b)
            points_on_line = [r, b]

            # Check if any other points lie on this line
            for p in all_points:
                if p != r and p != b:
                    if line.contains(p):
                        points_on_line.append(p)

            if len(points_on_line) >= 4:
                lines_with_points.append((line, points_on_line))

    if lines_with_points:
        print(f"Found {len(lines_with_points)} lines with 4+ collinear points:")
        for i, (line, points) in enumerate(lines_with_points):
            print(f"\nLine {i+1}: {line}")
            print(f"  Contains {len(points)} points")

            # Compute cross-ratio for first 4 points
            if len(points) >= 4:
                A, B, C, D = points[:4]
                # Cross ratio: (AC/BC) / (AD/BD)
                AC = A.distance(C)
                BC = B.distance(C)
                AD = A.distance(D)
                BD = B.distance(D)

                cr = simplify((AC/BC) / (AD/BD))
                print(f"  Cross-ratio(p1,p2,p3,p4): {cr}")
    else:
        print("No lines with 4+ collinear points (configuration is in general position)")

    print()


def find_intersection_patterns_symbolic(red_coords, blue_coords, max_day=3):
    """
    Compute first few days symbolically to find patterns.
    """
    print("="*80)
    print("SYMBOLIC INTERSECTION PATTERN ANALYSIS")
    print("="*80)
    print()

    # Create symbolic points
    red = [SymPoint(x, y) for x, y in red_coords]
    blue_initial = [SymPoint(x, y) for x, y in blue_coords]

    print(f"Computing intersections symbolically through day {max_day}...")
    print(f"Red points: {len(red)}")
    print(f"Initial blue points: {len(blue_initial)}")
    print()

    all_blues = list(blue_initial)
    blues_by_level = {0: blue_initial}

    for day in range(1, max_day + 1):
        print(f"Day {day}:")
        print("-" * 60)

        # Generate all red-blue lines
        lines = []
        for r in red:
            for b in all_blues:
                try:
                    line = SymLine(r, b)
                    lines.append(line)
                except:
                    pass

        print(f"  Total lines: {len(lines)}")

        # Find intersections
        intersections = set()
        intersection_multiplicities = defaultdict(int)

        for i in range(len(lines)):
            for j in range(i+1, len(lines)):
                try:
                    pt = lines[i].intersection(lines[j])
                    if pt:
                        # SymPy returns a list
                        pt = pt[0]

                        # Check if it's new (not red, not already blue)
                        is_new = True
                        for r in red:
                            if r.equals(pt):
                                is_new = False
                                break
                        for b in all_blues:
                            if b.equals(pt):
                                is_new = False
                                break

                        if is_new:
                            # Convert to hashable form
                            pt_key = (simplify(pt.x), simplify(pt.y))
                            intersections.add(pt_key)
                            intersection_multiplicities[pt_key] += 1
                except:
                    pass

        new_blues = [SymPoint(x, y) for x, y in intersections]
        blues_by_level[day] = new_blues
        all_blues.extend(new_blues)

        print(f"  New blue points: {len(new_blues)}")
        print(f"  Total blues: {len(all_blues)}")

        # Analyze multiplicity distribution
        mult_counts = defaultdict(int)
        for mult in intersection_multiplicities.values():
            mult_counts[mult] += 1

        if mult_counts:
            print(f"  Multiplicity distribution:")
            for mult in sorted(mult_counts.keys(), reverse=True):
                if mult > 1:
                    count = mult_counts[mult]
                    print(f"    {count} points hit by {mult} line-pairs")

        # Show a few example coordinates
        if len(new_blues) > 0:
            print(f"  Example new points (first 3):")
            for i, pt in enumerate(new_blues[:3]):
                print(f"    ({simplify(pt.x)}, {simplify(pt.y)})")

        print()

    return blues_by_level


def search_for_kappa_pattern():
    """
    Analyze the κ_t sequence for patterns.
    """
    print("="*80)
    print("KAPPA SEQUENCE PATTERN SEARCH")
    print("="*80)
    print()

    # Known values
    kappa_values = [
        Rational(1, 1),          # κ_1 = 1
        Rational(10, 81),        # κ_2 = 10/81 (from earlier analysis)
        Rational(13, 175),       # κ_3 = 13/175 (approximate)
    ]

    m_values = [2, 6, 20, 156, 1462, 17515]
    B_values = [2, 8, 28, 184, 1646, 19161]

    print("Known κ values (exact fractions):")
    for i, k in enumerate(kappa_values, 1):
        print(f"  κ_{i} = {k} = {float(k):.10f}")
    print()

    # Try to find recurrence relation
    print("Testing for recurrence relations:")
    print()

    # Test: κ_t = f(κ_{t-1}, κ_{t-2}, ...)?
    for i in range(1, len(kappa_values)):
        if i > 0:
            ratio = kappa_values[i] / kappa_values[i-1]
            print(f"  κ_{i+1}/κ_{i} = {simplify(ratio)} = {float(ratio):.6f}")
    print()

    # Test: κ_t = f(m_{t-1}, B_{t-2}, ...)?
    print("Testing κ_t vs (m_{t-1}, B_{t-2}):")
    for t in range(1, len(kappa_values) + 1):
        k = kappa_values[t-1]
        m_prev = m_values[t-1]
        B_prev_prev = B_values[t-2] if t >= 2 else 0

        print(f"  Day {t}: κ_{t} = {k}, m_{t-1} = {m_prev}, B_{t-2} = {B_prev_prev}")

        # Try simple relationships
        # κ_t ∝ 1/m_{t-1}?
        if m_prev > 0:
            scaled = k * m_prev
            print(f"    κ_{t} * m_{t-1} = {simplify(scaled)} = {float(scaled):.6f}")

        # κ_t ∝ 1/B_{t-2}?
        if B_prev_prev > 0:
            scaled = k * B_prev_prev
            print(f"    κ_{t} * B_{t-2} = {simplify(scaled)} = {float(scaled):.6f}")

    print()


def main():
    """Run full symbolic analysis."""

    print("="*80)
    print("SYMBOLIC ANALYSIS OF PROJECT EULER 957")
    print("Using SymPy for exact rational arithmetic and projective geometry")
    print("="*80)
    print()

    # Step 1: Convert to rational coordinates
    print("STEP 1: Rationalize Configuration")
    print("="*80)
    red_rational = rationalize_config(RED_NUMERICAL, max_denom=1000000)
    blue_rational = rationalize_config(BLUE_NUMERICAL, max_denom=1000000)
    print()

    # Step 2: Analyze projective invariants
    analyze_cross_ratios(red_rational, blue_rational)

    # Step 3: Compute first few days symbolically
    print("STEP 2: Symbolic Propagation")
    blues_by_level = find_intersection_patterns_symbolic(
        red_rational, blue_rational, max_day=2
    )

    # Step 4: Search for κ pattern
    search_for_kappa_pattern()

    print("="*80)
    print("ANALYSIS COMPLETE")
    print("="*80)
    print()
    print("Next steps if patterns found:")
    print("1. Derive algebraic formula for κ_t from projective structure")
    print("2. Verify formula against known values")
    print("3. Extend to day 16")
    print()


if __name__ == '__main__':
    main()
