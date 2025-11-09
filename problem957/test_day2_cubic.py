#!/usr/bin/env python3
"""
Test if Day 2 intersection points lie on a CUBIC curve (degree 3).

Result from conic test: Day 2 does NOT lie on conic (RMS = 1.3)
Hypothesis: Day 2 lies on degree-3 curve (cubic)

This tests the degree growth: d_1 = 2 → d_2 = 3?
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
                    already_exists = False
                    for existing in intersections:
                        if existing == pt:
                            already_exists = True
                            break
                    if not already_exists:
                        intersections.append(pt)
    return intersections


def fit_cubic_manual(points):
    """
    Fit cubic curve: Ax³ + Bx²y + Cxy² + Dy³ + Ex² + Fxy + Gy² + Hx + Iy + J = 0

    Degree 3 has 10 terms.
    With 20 points, this is overdetermined (good for fitting).

    Returns: coefficients, RMS error
    """
    n = len(points)

    if n < 10:
        return None, float('inf')

    # Use first 9 points to solve for 9 coefficients, set J=1
    # Then check RMS on all points

    # Build 9x9 system
    mat9 = []
    rhs9 = []
    for i in range(9):
        x, y = points[i].x, points[i].y
        # [x³, x²y, xy², y³, x², xy, y², x, y]
        row = [x**3, x*x*y, x*y*y, y**3, x*x, x*y, y*y, x, y]
        mat9.append(row)
        rhs9.append(-1.0)  # -J with J=1

    # Solve 9x9 system
    coeffs = solve_nxn(mat9, rhs9)
    if coeffs is None:
        return None, float('inf')

    A, B, C, D, E, F, G, H, I = coeffs
    J = 1.0

    # Compute RMS error on all points
    errors = []
    for p in points:
        x, y = p.x, p.y
        val = A*x**3 + B*x*x*y + C*x*y*y + D*y**3 + E*x*x + F*x*y + G*y*y + H*x + I*y + J
        errors.append(val * val)

    rms = (sum(errors) / len(errors)) ** 0.5

    # Normalize
    max_coeff = max(abs(A), abs(B), abs(C), abs(D), abs(E), abs(F), abs(G), abs(H), abs(I), abs(J))
    if max_coeff > 0:
        coeffs_norm = [c/max_coeff for c in [A, B, C, D, E, F, G, H, I, J]]
        rms = rms / max_coeff
    else:
        coeffs_norm = [A, B, C, D, E, F, G, H, I, J]

    return coeffs_norm, rms


def solve_nxn(matrix, rhs):
    """Solve nxn linear system using Gaussian elimination."""
    n = len(matrix)
    # Copy
    m = [row[:] for row in matrix]
    b = rhs[:]

    # Forward elimination
    for k in range(n):
        # Partial pivoting
        max_idx = k
        for i in range(k+1, n):
            if abs(m[i][k]) > abs(m[max_idx][k]):
                max_idx = i
        m[k], m[max_idx] = m[max_idx], m[k]
        b[k], b[max_idx] = b[max_idx], b[k]

        if abs(m[k][k]) < 1e-10:
            return None

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


def main():
    print("="*70)
    print("DAY 2 CUBIC CURVE TEST")
    print("="*70)
    print()
    print("Previous result: Day 2 does NOT lie on conic (RMS = 1.3)")
    print("Hypothesis: Day 2 lies on CUBIC curve (degree 3)")
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
    print(f"  Day 1: {len(day1_blues)} points")

    print("Computing Day 2 intersections...")
    day2_blues = compute_day2_intersections(red_points, blue_points, day1_blues)
    print(f"  Day 2: {len(day2_blues)} points")
    print()

    print("-"*70)
    print("FITTING CUBIC TO DAY 2 POINTS")
    print("-"*70)
    print()

    coeffs, rms = fit_cubic_manual(day2_blues)

    if coeffs is None:
        print("ERROR: Cubic fit failed")
        return

    A, B, C, D, E, F, G, H, I, J = coeffs

    print("Cubic equation (normalized):")
    print(f"  {A:.4f}x³ + {B:.4f}x²y + {C:.4f}xy² + {D:.4f}y³")
    print(f"  + {E:.4f}x² + {F:.4f}xy + {G:.4f}y²")
    print(f"  + {H:.4f}x + {I:.4f}y + {J:.4f} = 0")
    print()

    print("Fit Quality:")
    print(f"  RMS Error: {rms:.6f}")
    print()

    print("="*70)
    print("COMPARISON")
    print("="*70)
    print()
    print(f"  Day 1: d₁ = 2 (conic),  RMS = 0.00058 ✓")
    print(f"  Day 2: d₂ = 2 (conic),  RMS = 1.30     ✗ POOR")
    print(f"  Day 2: d₂ = 3 (cubic),  RMS = {rms:.6f}", end="")

    if rms < 0.01:
        print(" ✓ EXCELLENT")
        print()
        print("  ✓✓✓ DEGREE GROWTH CONFIRMED ✓✓✓")
        print()
        print("  RESULT:")
        print("    d₁ = 2 (hyperbola)")
        print("    d₂ = 3 (cubic curve)")
        print()
        print("  IMPLICATION:")
        print("    - Degree increases: d_t > d_{t-1}")
        print("    - Pattern: d_t = d_{t-1} + 1? (linear growth)")
        print("    - Or: d_t = ?")
        print()
        print("  NEXT TEST:")
        print("    - Compute d₃ to see pattern")
        print("    - Expected: d₃ = 4? or d₃ = 5?")
    elif rms < 0.1:
        print(" ~ MODERATE")
        print()
        print("  Cubic fits better than conic, but not perfect")
        print("  Might need degree 4 or 5")
    else:
        print(" ✗ POOR")
        print()
        print("  Cubic doesn't fit well")
        print("  Need higher degree or variety hypothesis is wrong")


if __name__ == "__main__":
    main()
