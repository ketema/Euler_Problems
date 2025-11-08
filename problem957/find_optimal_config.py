#!/usr/bin/env python3
"""
Find optimal configuration for Project Euler Problem 957.

Search strategy (AI Board recommended):
1. Test geometric hypothesis configurations
2. Verify g(1)=8 and g(2)=28 for each
3. Calculate g(16) for the optimal one
"""

import sys
import math
from typing import Set, Tuple, List
sys.path.insert(0, '/Users/kharri04/projects/my_workspace/prototypes/problem957')

from src.geometry import Point
from src.propagation import propagate_one_day


def calculate_g(red: Set[Point], blue: Set[Point], days: int) -> List[int]:
    """
    Calculate g(1), g(2), ..., g(days).

    Returns list where g[i] = total blue count after day i+1.
    """
    results = []
    current_blue = set(blue)  # Copy initial blues

    for day in range(days):
        new_blues = propagate_one_day(red, current_blue)
        current_blue = current_blue | new_blues
        results.append(len(current_blue))

    return results


def validate_config(red: Set[Point], blue: Set[Point], name: str) -> None:
    """Validate configuration achieves g(1)=8, g(2)=28."""
    # Calculate g(1) and g(2) only initially for speed
    g_values_short = calculate_g(red, blue, 2)

    print(f"{name:50} g(1)={g_values_short[0]:3} g(2)={g_values_short[1]:3}", end="")

    if g_values_short[0] == 8 and g_values_short[1] == 28:
        print(f" ✓✓ OPTIMAL FOUND!")

        # Now calculate full sequence
        g_values = calculate_g(red, blue, 16)

        print(f"\n{'='*60}")
        print(f"🎯 OPTIMAL CONFIGURATION: {name}")
        print(f"{'='*60}")

        # Print configuration
        print(f"\nRed points ({len(red)}):")
        for i, p in enumerate(sorted(red, key=lambda pt: (pt.x, pt.y)), 1):
            print(f"  R{i} = ({p.x:.10f}, {p.y:.10f})")

        print(f"\nBlue points ({len(blue)}):")
        for i, p in enumerate(sorted(blue, key=lambda pt: (pt.x, pt.y)), 1):
            print(f"  B{i} = ({p.x:.10f}, {p.y:.10f})")

        print(f"\nComplete sequence g(1) through g(16):")
        for i, val in enumerate(g_values, 1):
            print(f"  g({i:2}) = {val:4}")

        print(f"\n✓ Answer: g(16) = {g_values[15]}")
        print(f"{'='*60}\n")
    else:
        print()


# ============================================================================
# HYPOTHESIS 1: Regular pentagon (5 points on circle)
# ============================================================================

def test_pentagon_config():
    """
    5 points evenly spaced on unit circle (72° apart).
    Color 3 as red, 2 as blue in various patterns.
    """
    angles = [i * 72 for i in range(5)]  # 0°, 72°, 144°, 216°, 288°
    points = [
        Point(math.cos(math.radians(a)), math.sin(math.radians(a)))
        for a in angles
    ]

    # Try different colorings
    # Pattern A: Adjacent reds
    red = {points[0], points[1], points[2]}
    blue = {points[3], points[4]}
    validate_config(red, blue, "Pentagon - Adjacent Reds (RRRbb)")

    # Pattern B: Alternating (as much as possible)
    red = {points[0], points[2], points[4]}
    blue = {points[1], points[3]}
    validate_config(red, blue, "Pentagon - Alternating (RbRbR)")

    # Pattern C: Two adjacent reds, one separated
    red = {points[0], points[1], points[3]}
    blue = {points[2], points[4]}
    validate_config(red, blue, "Pentagon - Mixed (RRbRb)")


# ============================================================================
# HYPOTHESIS 2: Equilateral triangle + 2 blues at specific positions
# ============================================================================

def test_triangle_variants():
    """
    3 red points form equilateral triangle.
    Test various positions for 2 blue points.
    """
    # Equilateral triangle with unit side length, centered at origin
    h = math.sqrt(3) / 2  # Height from center to vertex
    red = {
        Point(0, 2*h/3),                    # Top vertex
        Point(-0.5, -h/3),                  # Bottom-left
        Point(0.5, -h/3)                    # Bottom-right
    }

    # Variant A: Blues at center and far point
    blue_a = {Point(0, 0), Point(0, -2)}
    validate_config(red, blue_a, "Triangle - Center + Below")

    # Variant B: Blues on opposite sides
    blue_b = {Point(-1, 0), Point(1, 0)}
    validate_config(red, blue_b, "Triangle - Left + Right")

    # Variant C: Blues at midpoints of two sides
    blue_c = {
        Point(-0.25, h/6),   # Midpoint of left edge
        Point(0.25, h/6)     # Midpoint of right edge
    }
    validate_config(red, blue_c, "Triangle - Edge Midpoints")

    # Variant D: Blues inside and outside triangle
    blue_d = {Point(0, 0), Point(0, 3)}
    validate_config(red, blue_d, "Triangle - Center + Above")


# ============================================================================
# HYPOTHESIS 3: Square + 1 point (complete quadrilateral + point)
# ============================================================================

def test_quadrilateral_plus_one():
    """
    4 points forming a square, plus 1 additional point.
    Color strategically as 3 red + 2 blue.
    """
    # Unit square centered at origin
    square = [
        Point(-1, -1),  # Bottom-left
        Point(1, -1),   # Bottom-right
        Point(1, 1),    # Top-right
        Point(-1, 1)    # Top-left
    ]
    center = Point(0, 0)

    # Variant A: 3 square corners + center + 1 corner
    red_a = {square[0], square[1], square[2]}
    blue_a = {square[3], center}
    validate_config(red_a, blue_a, "Square+Center - 3 corners + 1 corner + center")

    # Variant B: Diagonal corners as red
    red_b = {square[0], square[2], center}
    blue_b = {square[1], square[3]}
    validate_config(red_b, blue_b, "Square+Center - Diagonal + center")


# ============================================================================
# HYPOTHESIS 4: Golden ratio configuration
# ============================================================================

def test_golden_ratio_config():
    """
    Use golden ratio φ = (1 + √5)/2 for point positioning.
    Often appears in optimal geometric configurations.
    """
    phi = (1 + math.sqrt(5)) / 2

    # Configuration inspired by pentagonal symmetry
    red = {
        Point(0, 0),
        Point(1, 0),
        Point(phi, phi)
    }
    blue = {
        Point(0.5, math.sqrt(3)/2),
        Point(phi/2, 0)
    }
    validate_config(red, blue, "Golden Ratio - Pentagonal Inspired")


# ============================================================================
# HYPOTHESIS 5: Integer/rational grid points
# ============================================================================

def test_simple_rational_configs():
    """
    Try simple integer or rational coordinate configurations.
    Project Euler often favors these for exact arithmetic.
    """
    # Config A: All points at integer coordinates
    red_a = {Point(0, 0), Point(3, 0), Point(0, 4)}
    blue_a = {Point(1, 1), Point(2, 2)}
    validate_config(red_a, blue_a, "Integer Grid - 3-4-5 Triangle")

    # Config B: Points at (0,0), (1,0), (0,1), (1,1), (2,1)
    red_b = {Point(0, 0), Point(1, 0), Point(0, 1)}
    blue_b = {Point(1, 1), Point(2, 1)}
    validate_config(red_b, blue_b, "Integer Grid - Unit Square + Extension")

    # Config C: Rational coordinates with small denominators
    red_c = {Point(0, 0), Point(2, 0), Point(1, 1)}
    blue_c = {Point(0.5, 0.5), Point(1.5, 0.5)}
    validate_config(red_c, blue_c, "Rational Grid - Halves")


# ============================================================================
# HYPOTHESIS 6: Optimal configuration from differential evolution
# ============================================================================

def test_optimal_from_differential_evolution():
    """
    Optimal configuration found via differential evolution.
    Verified to achieve g(1)=8, g(2)=28, g(3)=184, g(4)=1646, g(5)=19161.
    """
    red = {
        Point(-1.1420985748, -3.1278529420),  # R1
        Point(1.7213348846, -0.8343651343),   # R2
        Point(4.3760906863, 2.3859745813)     # R3
    }

    blue = {
        Point(-1.8437265624, 1.4483260402),   # B1
        Point(-1.0486909239, 2.1320688328)    # B2
    }

    validate_config(red, blue, "Differential Evolution - Optimal Config")


# ============================================================================
# Main execution
# ============================================================================

def main():
    """Run all hypothesis tests."""
    print("PROJECT EULER 957: Optimal Configuration Search")
    print("="*60)
    print("Target: g(1) = 8, g(2) = 28")
    print("Strategy: Test geometric hypotheses systematically\n")

    # Test the known optimal configuration FIRST
    print("Testing known optimal configuration from differential evolution...")
    test_optimal_from_differential_evolution()

    print("\nTesting other hypotheses for comparison...")
    # Run all tests
    test_pentagon_config()
    test_triangle_variants()
    test_quadrilateral_plus_one()
    test_golden_ratio_config()
    test_simple_rational_configs()

    print(f"\n{'='*60}")
    print("Search complete. Check results above for ✓ markers.")
    print(f"{'='*60}\n")


if __name__ == "__main__":
    main()
