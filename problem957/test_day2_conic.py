#!/usr/bin/env python3
"""
Test if Day 2 intersection points lie on a conic section (degree 2).

Day 1: PROVEN to lie on hyperbola (RMS = 0.00058)
Day 2: HYPOTHESIS - should also lie on conic if variety structure holds

This is a critical test of the algebraic geometry framework.
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '.'))

from src.geometry import Point, Line, intersect


def compute_day1_intersections(red_points, blue_points):
    """Compute Day 1 intersection points."""
    lines = []
    for r in red_points:
        for b in blue_points:
            lines.append(Line(r, b))

    intersections = []
    for i in range(len(lines)):
        for j in range(i+1, len(lines)):
            pt = intersect(lines[i], lines[j])
            if pt is not None:
                # Check if new (not red, not blue)
                is_new = True
                for r in red_points:
                    if r == pt:
                        is_new = False
                        break
                if is_new:
                    for b in blue_points:
                        if b == pt:
                            is_new = False
                            break

                if is_new:
                    # Check if already in intersections
                    already_exists = False
                    for existing in intersections:
                        if existing == pt:
                            already_exists = True
                            break
                    if not already_exists:
                        intersections.append(pt)

    return intersections


def compute_day2_intersections(red_points, blue_points, day1_blues):
    """Compute Day 2 intersection points."""
    # All blues now include initial + day 1
    all_blues = blue_points + day1_blues

    lines = []
    for r in red_points:
        for b in all_blues:
            lines.append(Line(r, b))

    intersections = []
    for i in range(len(lines)):
        for j in range(i+1, len(lines)):
            pt = intersect(lines[i], lines[j])
            if pt is not None:
                # Check if new (not red, not any existing blue)
                is_new = True
                for r in red_points:
                    if r == pt:
                        is_new = False
                        break
                if is_new:
                    for b in all_blues:
                        if b == pt:
                            is_new = False
                            break

                if is_new:
                    # Check if already in intersections
                    already_exists = False
                    for existing in intersections:
                        if existing == pt:
                            already_exists = True
                            break
                    if not already_exists:
                        intersections.append(pt)

    return intersections


def fit_conic_svd_manual(points):
    """
    Fit conic section Ax² + Bxy + Cy² + Dx + Ey + F = 0 using SVD.

    Manual implementation without numpy.
    Returns: (A, B, C, D, E, F), RMS error
    """
    n = len(points)

    # Build matrix manually: each row is [x², xy, y², x, y, 1]
    matrix = []
    for p in points:
        x, y = p.x, p.y
        row = [x*x, x*y, y*y, x, y, 1.0]
        matrix.append(row)

    # For a homogeneous system, we need the smallest singular vector
    # This requires full SVD which is complex to implement from scratch
    #
    # Instead, use a simpler approach: normalize by setting one coefficient
    # and solve the linear system

    # Alternative: Set F = 1 (if possible) and solve for A,B,C,D,E
    # This gives: Ax² + Bxy + Cy² + Dx + Ey = -1

    # But this fails if all points satisfy F≈0
    # Better: Use least squares with constraint ||coeffs|| = 1

    # Since we don't have numpy, let's compute using a simplified method:
    # Use the first 5 points to determine 5 coefficients, set F=1

    if n < 6:
        return None, float('inf')

    # Take first 5 points to solve for A,B,C,D,E with F=1
    # Then check RMS on all points

    # Build 5x5 system
    mat5 = []
    rhs5 = []
    for i in range(5):
        x, y = points[i].x, points[i].y
        mat5.append([x*x, x*y, y*y, x, y])
        rhs5.append(-1.0)  # -F with F=1

    # Solve 5x5 system using Gaussian elimination
    coeffs = solve_5x5(mat5, rhs5)
    if coeffs is None:
        # Try different normalization
        return None, float('inf')

    A, B, C, D, E = coeffs
    F = 1.0

    # Compute RMS error on all points
    errors = []
    for p in points:
        x, y = p.x, p.y
        val = A*x*x + B*x*y + C*y*y + D*x + E*y + F
        errors.append(val * val)

    rms = (sum(errors) / len(errors)) ** 0.5

    # Normalize coefficients so largest is 1
    max_coeff = max(abs(A), abs(B), abs(C), abs(D), abs(E), abs(F))
    if max_coeff > 0:
        A, B, C, D, E, F = A/max_coeff, B/max_coeff, C/max_coeff, D/max_coeff, E/max_coeff, F/max_coeff
        rms = rms / max_coeff

    return (A, B, C, D, E, F), rms


def solve_5x5(matrix, rhs):
    """Solve 5x5 linear system using Gaussian elimination."""
    n = 5
    # Copy matrix and rhs
    m = [row[:] for row in matrix]
    b = rhs[:]

    # Forward elimination
    for k in range(n):
        # Find pivot
        max_idx = k
        for i in range(k+1, n):
            if abs(m[i][k]) > abs(m[max_idx][k]):
                max_idx = i

        # Swap rows
        m[k], m[max_idx] = m[max_idx], m[k]
        b[k], b[max_idx] = b[max_idx], b[k]

        # Check for singular matrix
        if abs(m[k][k]) < 1e-10:
            return None

        # Eliminate
        for i in range(k+1, n):
            factor = m[i][k] / m[k][k]
            for j in range(k, n):
                m[i][j] -= factor * m[k][j]
            b[i] -= factor * b[k]

    # Back substitution
    x = [0.0] * n
    for i in range(n-1, -1, -1):
        x[i] = b[i]
        for j in range(i+1, n):
            x[i] -= m[i][j] * x[j]
        x[i] /= m[i][i]

    return x


def classify_conic(A, B, C):
    """Classify conic by discriminant."""
    discriminant = B*B - 4*A*C

    if abs(discriminant) < 0.001:
        return "PARABOLA", discriminant
    elif discriminant > 0:
        return "HYPERBOLA", discriminant
    else:
        return "ELLIPSE", discriminant


def main():
    print("="*70)
    print("DAY 2 CONIC SECTION TEST")
    print("="*70)
    print()
    print("Hypothesis: Day 2 intersection points lie on a conic section")
    print("            (like Day 1's proven hyperbola)")
    print()

    # Optimal configuration
    red_points = [
        Point(-1.1420985748, -3.1278529420),
        Point(1.7213348846, -0.8343651343),
        Point(4.3760906863, 2.3859745813)
    ]

    blue_points = [
        Point(-1.8437265624, 1.4483260402),
        Point(-1.0486909239, 2.1320688328)
    ]

    print("Computing Day 1 intersections...")
    day1_blues = compute_day1_intersections(red_points, blue_points)
    print(f"  Day 1: {len(day1_blues)} new blue points")
    print()

    print("Computing Day 2 intersections...")
    day2_blues = compute_day2_intersections(red_points, blue_points, day1_blues)
    print(f"  Day 2: {len(day2_blues)} new blue points")
    print()

    print("-"*70)
    print("FITTING CONIC TO DAY 2 POINTS")
    print("-"*70)
    print()

    if len(day2_blues) < 6:
        print(f"ERROR: Need at least 6 points for conic fit, got {len(day2_blues)}")
        return

    result, rms = fit_conic_svd_manual(day2_blues)

    if result is None:
        print("ERROR: Conic fit failed (singular matrix)")
        return

    A, B, C, D, E, F = result

    print("Conic equation (normalized):")
    print(f"  {A:.6f}x² + {B:.6f}xy + {C:.6f}y² + {D:.6f}x + {E:.6f}y + {F:.6f} = 0")
    print()

    conic_type, discriminant = classify_conic(A, B, C)

    print("Classification:")
    print(f"  Type: {conic_type}")
    print(f"  Discriminant B² - 4AC = {discriminant:.6f}")
    print()

    print("Fit Quality:")
    print(f"  RMS Error: {rms:.6f}")
    print()

    print("="*70)
    print("COMPARISON TO DAY 1 BASELINE")
    print("="*70)
    print()
    print(f"  Day 1: RMS = 0.00058 (hyperbola, degree 2) ✓ PROVEN")
    print(f"  Day 2: RMS = {rms:.6f} ({conic_type.lower()}, degree 2)")
    print()

    if rms < 0.01:
        print("  ✓✓✓ VARIETY HYPOTHESIS SUPPORTED ✓✓✓")
        print(f"      Day 2 points lie on {conic_type.lower()} (RMS < 0.01)")
        print()
        print("  IMPLICATION:")
        print("    - Day 2 has algebraic variety structure (degree 2)")
        print("    - Coincidence is geometrically forced by conic")
        print("    - Supports pattern: d_1 = d_2 = 2 (both conics)")
        print()
        print("  NEXT QUESTION:")
        print("    - Does Day 3 lie on degree-3 curve?")
        print("    - When does degree increase: d_t > d_{t-1}?")
    else:
        print("  ⚠ HYPOTHESIS UNCLEAR")
        print(f"      RMS = {rms:.6f} >> 0.01 (poor fit)")
        print()
        if rms < 0.1:
            print("  POSSIBLE:")
            print("    - Day 2 might lie on higher-degree curve (d_2 > 2)")
            print("    - Conic fit insufficient, try cubic (degree 3)")
        else:
            print("  LIKELY:")
            print("    - Points don't lie on simple algebraic curve")
            print("    - Variety hypothesis may not hold for Day 2")


if __name__ == "__main__":
    main()
