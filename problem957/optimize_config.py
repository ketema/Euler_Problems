#!/usr/bin/env python3
"""
Numerical optimization to find optimal configuration for g(1)=8, g(2)=28.

Strategy: Use scipy.optimize.differential_evolution for global optimization.
"""

import sys
import numpy as np
from scipy.optimize import differential_evolution, minimize
sys.path.insert(0, '/Users/kharri04/projects/my_workspace/prototypes/problem957')

from src.geometry import Point
from src.propagation import propagate_one_day


def coords_to_config(x):
    """
    Convert coordinate vector to red/blue point sets.

    x = [r1x, r1y, r2x, r2y, r3x, r3y, b1x, b1y, b2x, b2y]
    """
    red = {
        Point(x[0], x[1], 'red'),
        Point(x[2], x[3], 'red'),
        Point(x[4], x[5], 'red')
    }
    blue = {
        Point(x[6], x[7], 'blue'),
        Point(x[8], x[9], 'blue')
    }
    return red, blue


def objective(x):
    """
    Objective function: minimize |g(1) - 8| + weight * |g(2) - 28|.

    Returns large penalty if invalid configuration.
    """
    try:
        red, blue = coords_to_config(x)

        # Calculate g(1)
        new_blues_1 = propagate_one_day(red, blue)
        current_blue = blue | new_blues_1
        g1 = len(current_blue)

        # Calculate g(2)
        new_blues_2 = propagate_one_day(red, current_blue)
        current_blue = current_blue | new_blues_2
        g2 = len(current_blue)

        # Objective: minimize distance from target
        error = abs(g1 - 8) + 0.5 * abs(g2 - 28)

        return error

    except Exception as e:
        # Invalid configuration (e.g., degenerate points)
        return 1e6


def optimize_differential_evolution():
    """
    Use differential evolution (global optimizer) to find optimal config.
    """
    print("Starting differential evolution optimization...")
    print("Target: g(1)=8, g(2)=28")
    print("Search space: [-5, 5]^10 (10-dimensional)")
    print()

    # Define bounds: each coordinate in [-5, 5]
    bounds = [(-5, 5)] * 10

    # Run optimization
    result = differential_evolution(
        objective,
        bounds,
        strategy='best1bin',
        maxiter=1000,
        popsize=15,
        tol=0.01,
        mutation=(0.5, 1),
        recombination=0.7,
        seed=42,
        workers=1,
        disp=True,
        polish=True,
        init='latinhypercube'
    )

    print("\n" + "="*60)
    print("OPTIMIZATION COMPLETE")
    print("="*60)

    if result.fun < 0.1:  # Near-optimal solution
        print(f"✓ Found near-optimal solution (error={result.fun:.6f})")

        red, blue = coords_to_config(result.x)

        # Verify g(1) and g(2)
        new_blues_1 = propagate_one_day(red, blue)
        current_blue = blue | new_blues_1
        g1 = len(current_blue)

        new_blues_2 = propagate_one_day(red, current_blue)
        current_blue = current_blue | new_blues_2
        g2 = len(current_blue)

        print(f"\nConfiguration achieves: g(1)={g1}, g(2)={g2}")

        # Print coordinates
        print(f"\nRed points:")
        for i, p in enumerate(sorted(red, key=lambda pt: (pt.x, pt.y)), 1):
            print(f"  R{i} = Point({p.x:.10f}, {p.y:.10f})")

        print(f"\nBlue points:")
        for i, p in enumerate(sorted(blue, key=lambda pt: (pt.x, pt.y)), 1):
            print(f"  B{i} = Point({p.x:.10f}, {p.y:.10f})")

        if g1 == 8 and g2 == 28:
            print(f"\n{'='*60}")
            print("🎯 EXACT SOLUTION FOUND! Computing g(16)...")
            print(f"{'='*60}")

            # Compute full sequence
            g_values = []
            current_blue = blue
            for day in range(16):
                new_blues = propagate_one_day(red, current_blue)
                current_blue = current_blue | new_blues
                g_values.append(len(current_blue))

            print(f"\nComplete sequence g(1) through g(16):")
            for i, val in enumerate(g_values, 1):
                print(f"  g({i:2}) = {val:4}")

            print(f"\n✓ Answer: g(16) = {g_values[15]}")
            print(f"{'='*60}\n")
    else:
        print(f"✗ Optimization did not converge (error={result.fun:.6f})")
        print(f"Best configuration found:")
        red, blue = coords_to_config(result.x)

        new_blues_1 = propagate_one_day(red, blue)
        current_blue = blue | new_blues_1
        g1 = len(current_blue)

        new_blues_2 = propagate_one_day(red, current_blue)
        current_blue = current_blue | new_blues_2
        g2 = len(current_blue)

        print(f"  g(1)={g1}, g(2)={g2} (target: g(1)=8, g(2)=28)")
        print(f"\nTry increasing maxiter or adjusting optimization parameters.")


def optimize_local_from_pentagon():
    """
    Use local optimization starting from pentagon configuration.
    """
    print("Starting local optimization from pentagon configuration...")
    print("Target: g(1)=8, g(2)=28")
    print()

    # Pentagon initial guess (alternating pattern)
    import math
    angles = [i * 72 for i in range(5)]
    points = [
        (math.cos(math.radians(a)), math.sin(math.radians(a)))
        for a in angles
    ]

    # Initial: R0, R2, R4, B1, B3
    x0 = [
        points[0][0], points[0][1],  # R1
        points[2][0], points[2][1],  # R2
        points[4][0], points[4][1],  # R3
        points[1][0], points[1][1],  # B1
        points[3][0], points[3][1],  # B2
    ]

    print(f"Starting from pentagon (g(1)=6 typically)")

    result = minimize(
        objective,
        x0,
        method='Nelder-Mead',
        options={'maxiter': 10000, 'disp': True, 'xatol': 1e-6, 'fatol': 1e-6}
    )

    print("\n" + "="*60)
    print("LOCAL OPTIMIZATION COMPLETE")
    print("="*60)

    red, blue = coords_to_config(result.x)

    new_blues_1 = propagate_one_day(red, blue)
    current_blue = blue | new_blues_1
    g1 = len(current_blue)

    new_blues_2 = propagate_one_day(red, current_blue)
    current_blue = current_blue | new_blues_2
    g2 = len(current_blue)

    print(f"\nConfiguration achieves: g(1)={g1}, g(2)={g2}")
    print(f"Objective value: {result.fun:.6f}")


if __name__ == "__main__":
    import sys

    if len(sys.argv) > 1 and sys.argv[1] == '--local':
        optimize_local_from_pentagon()
    else:
        optimize_differential_evolution()
