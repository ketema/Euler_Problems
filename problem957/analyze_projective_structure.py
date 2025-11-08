#!/usr/bin/env python3
"""
Analyze projective geometry properties of the optimal configuration.

Look for:
1. Concurrency patterns (multiple lines through same point)
2. Collinearity patterns (multiple points on same line)
3. Projective incidence structure
4. Rational relationships between coordinates
"""

import sys
import math
from fractions import Fraction
from collections import defaultdict
sys.path.insert(0, '.')

from src.geometry import Point, Line, intersect, _float_equal

# Optimal configuration
RED = [
    Point(-1.1420985748, -3.1278529420, 'red'),
    Point(1.7213348846, -0.8343651343, 'red'),
    Point(4.3760906863, 2.3859745813, 'red')
]

BLUE_INIT = [
    Point(-1.8437265624, 1.4483260402, 'blue'),
    Point(-1.0486909239, 2.1320688328, 'blue')
]


def analyze_triangle_properties():
    """Analyze geometric properties of the red triangle."""
    print("="*80)
    print("RED TRIANGLE ANALYSIS")
    print("="*80)
    print()

    # Compute side lengths
    r0, r1, r2 = RED
    d01 = math.sqrt((r1.x - r0.x)**2 + (r1.y - r0.y)**2)
    d12 = math.sqrt((r2.x - r1.x)**2 + (r2.y - r1.y)**2)
    d20 = math.sqrt((r0.x - r2.x)**2 + (r0.y - r2.y)**2)

    print(f"Side lengths:")
    print(f"  |R0-R1| = {d01:.10f}")
    print(f"  |R1-R2| = {d12:.10f}")
    print(f"  |R2-R0| = {d20:.10f}")
    print()

    # Check if equilateral
    avg_side = (d01 + d12 + d20) / 3
    max_dev = max(abs(d - avg_side) for d in [d01, d12, d20])
    print(f"Average side length: {avg_side:.10f}")
    print(f"Max deviation from average: {max_dev:.10f} ({100*max_dev/avg_side:.4f}%)")

    if max_dev / avg_side < 0.01:
        print("  → Triangle is approximately equilateral")
    else:
        print("  → Triangle is NOT equilateral")
    print()

    # Compute angles
    def angle(p1, p2, p3):
        """Angle at p2 formed by p1-p2-p3"""
        v1 = (p1.x - p2.x, p1.y - p2.y)
        v2 = (p3.x - p2.x, p3.y - p2.y)
        dot = v1[0]*v2[0] + v1[1]*v2[1]
        mag1 = math.sqrt(v1[0]**2 + v1[1]**2)
        mag2 = math.sqrt(v2[0]**2 + v2[1]**2)
        cos_angle = dot / (mag1 * mag2)
        return math.degrees(math.acos(max(-1, min(1, cos_angle))))

    a0 = angle(r1, r0, r2)
    a1 = angle(r0, r1, r2)
    a2 = angle(r0, r2, r1)

    print(f"Angles:")
    print(f"  ∠R0 = {a0:.6f}°")
    print(f"  ∠R1 = {a1:.6f}°")
    print(f"  ∠R2 = {a2:.6f}°")
    print(f"  Sum = {a0+a1+a2:.6f}° (should be 180°)")
    print()


def analyze_blue_position():
    """Analyze initial blue point positioning relative to reds."""
    print("="*80)
    print("BLUE POINT POSITIONING")
    print("="*80)
    print()

    # Compute centroid of red triangle
    cx = sum(r.x for r in RED) / 3
    cy = sum(r.y for r in RED) / 3
    centroid = Point(cx, cy)

    print(f"Red triangle centroid: ({cx:.10f}, {cy:.10f})")
    print()

    # Distance from each blue to centroid
    for i, b in enumerate(BLUE_INIT):
        dist = math.sqrt((b.x - cx)**2 + (b.y - cy)**2)
        print(f"B{i} to centroid: {dist:.10f}")

    # Distance between the two blues
    dist_bb = math.sqrt((BLUE_INIT[1].x - BLUE_INIT[0].x)**2 +
                        (BLUE_INIT[1].y - BLUE_INIT[0].y)**2)
    print(f"B0 to B1: {dist_bb:.10f}")
    print()

    # Check if blues are collinear with any red
    print("Collinearity checks:")
    for i, r in enumerate(RED):
        for j, b1 in enumerate(BLUE_INIT):
            for k, b2 in enumerate(BLUE_INIT):
                if j >= k:
                    continue
                # Check if r, b1, b2 are collinear
                # Use cross product: if (b1-r) × (b2-r) ≈ 0, they're collinear
                v1 = (b1.x - r.x, b1.y - r.y)
                v2 = (b2.x - r.x, b2.y - r.y)
                cross = v1[0]*v2[1] - v1[1]*v2[0]
                if abs(cross) < 1e-6:
                    print(f"  R{i}, B{j}, B{k} are collinear!")
                else:
                    print(f"  R{i}, B{j}, B{k}: cross product = {cross:.6f} (not collinear)")
    print()


def analyze_line_concurrency():
    """Analyze which lines pass through common points."""
    print("="*80)
    print("LINE CONCURRENCY ANALYSIS (Day 1)")
    print("="*80)
    print()

    # Generate all 6 red-blue lines
    lines = []
    line_names = []
    for i, r in enumerate(RED):
        for j, b in enumerate(BLUE_INIT):
            try:
                line = Line(r, b)
                lines.append(line)
                line_names.append(f"R{i}B{j}")
            except ValueError:
                print(f"Warning: R{i} and B{j} are too close!")

    print(f"Generated {len(lines)} red-blue lines")
    print()

    # Find all pairwise intersections
    intersections = {}
    for i in range(len(lines)):
        for j in range(i+1, len(lines)):
            pt = intersect(lines[i], lines[j])
            if pt is not None:
                key = (pt.x, pt.y)
                if key not in intersections:
                    intersections[key] = []
                intersections[key].append((line_names[i], line_names[j]))

    print(f"Found {len(intersections)} unique intersection points")
    print()

    # Find points with 3+ lines through them (concurrency)
    concurrent_points = []
    for (x, y), pairs in intersections.items():
        # Count how many distinct lines pass through this point
        all_lines = set()
        for (l1, l2) in pairs:
            all_lines.add(l1)
            all_lines.add(l2)

        if len(all_lines) >= 3:
            concurrent_points.append(((x, y), all_lines))

    if concurrent_points:
        print(f"Concurrent points ({len(concurrent_points)} found):")
        for (x, y), lines_through in sorted(concurrent_points,
                                             key=lambda p: -len(p[1])):
            print(f"  ({x:.6f}, {y:.6f}): {len(lines_through)} lines: {sorted(lines_through)}")
    else:
        print("No concurrent points found (all intersections are 2-line only)")
    print()


def check_rational_relationships():
    """Check if coordinates have rational relationships."""
    print("="*80)
    print("RATIONAL RELATIONSHIP ANALYSIS")
    print("="*80)
    print()

    # Try to find simple rational approximations
    all_coords = []
    for r in RED:
        all_coords.append(('R', r.x))
        all_coords.append(('R', r.y))
    for b in BLUE_INIT:
        all_coords.append(('B', b.x))
        all_coords.append(('B', b.y))

    print("Checking for rational approximations (denominator ≤ 1000):")
    for typ, val in all_coords:
        frac = Fraction(val).limit_denominator(1000)
        error = abs(float(frac) - val)
        if error < 1e-8:
            print(f"  {typ} {val:.10f} ≈ {frac} (exact within 1e-8)")
        else:
            print(f"  {typ} {val:.10f} ≈ {frac} (error: {error:.2e})")
    print()

    # Check for algebraic relationships (multiples of sqrt(2), sqrt(3), pi, etc.)
    print("Checking for algebraic constants:")
    constants = {
        'sqrt(2)': math.sqrt(2),
        'sqrt(3)': math.sqrt(3),
        'sqrt(5)': math.sqrt(5),
        'phi': (1 + math.sqrt(5)) / 2,
        'pi': math.pi,
        'e': math.e
    }

    for typ, val in all_coords:
        for const_name, const_val in constants.items():
            if abs(val) < 1e-6:
                continue
            ratio = val / const_val
            frac = Fraction(ratio).limit_denominator(100)
            reconstructed = float(frac) * const_val
            error = abs(reconstructed - val)
            if error < 1e-6:
                print(f"  {typ} {val:.10f} ≈ {frac} * {const_name} (error: {error:.2e})")
    print()


def main():
    """Run all analyses."""
    analyze_triangle_properties()
    analyze_blue_position()
    analyze_line_concurrency()
    check_rational_relationships()

    print("="*80)
    print("CONCLUSION")
    print("="*80)
    print()
    print("To find a closed-form κ_t, we would need:")
    print("1. A projective incidence rule governing which line-pairs coincide")
    print("2. OR an algebraic description of the concurrency structure")
    print("3. OR discovery that this configuration belongs to a known family")
    print()
    print("None of these are evident from the numerical analysis above.")
    print("The configuration appears to be numerically optimized, not algebraically special.")
    print()


if __name__ == '__main__':
    main()
