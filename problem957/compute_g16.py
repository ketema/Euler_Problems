#!/usr/bin/env python3
"""
Compute g(16) for Project Euler Problem 957 using verified optimal configuration.

Optimal configuration (verified for g(1)=8, g(2)=28):
- Red: R1(-1.1420985748, -3.1278529420), R2(1.7213348846, -0.8343651343), R3(4.3760906863, 2.3859745813)
- Blue: B1(-1.8437265624, 1.4483260402), B2(-1.0486909239, 2.1320688328)
"""

import sys
sys.path.insert(0, '/Users/kharri04/projects/my_workspace/prototypes/problem957')

from src.geometry import Point
from src.propagation import propagate_one_day


def main():
    # Verified optimal configuration
    red = {
        Point(-1.1420985748, -3.1278529420),  # R1
        Point(1.7213348846, -0.8343651343),   # R2
        Point(4.3760906863, 2.3859745813)     # R3
    }

    blue = {
        Point(-1.8437265624, 1.4483260402),   # B1
        Point(-1.0486909239, 2.1320688328)    # B2
    }

    print("="*70)
    print("PROJECT EULER 957: Computing g(16) from Verified Optimal Configuration")
    print("="*70)
    print("\nInitial Configuration:")
    print(f"  Red points: {len(red)}")
    for i, p in enumerate(sorted(red, key=lambda pt: (pt.x, pt.y)), 1):
        print(f"    R{i} = ({p.x:.10f}, {p.y:.10f})")
    print(f"  Blue points: {len(blue)}")
    for i, p in enumerate(sorted(blue, key=lambda pt: (pt.x, pt.y)), 1):
        print(f"    B{i} = ({p.x:.10f}, {p.y:.10f})")

    print("\n" + "="*70)
    print("Computing sequence g(1) through g(16)...")
    print("="*70 + "\n")

    # Compute g(1) through g(16)
    g_values = []
    current_blue = set(blue)

    for day in range(1, 17):
        print(f"Day {day}: ", end="", flush=True)

        new_blues = propagate_one_day(red, current_blue)
        g_n = len(new_blues)
        g_values.append(g_n)

        current_blue = current_blue | new_blues
        total_blues = len(current_blue)
        total_lines = 3 * total_blues

        print(f"g({day}) = {g_n:10,} new blues  "
              f"(total blues: {total_blues:10,}, total lines: {total_lines:10,})")

    print("\n" + "="*70)
    print("COMPLETE SEQUENCE:")
    print("="*70)
    for i, val in enumerate(g_values, 1):
        print(f"  g({i:2}) = {val:15,}")

    print("\n" + "="*70)
    print(f"🎯 ANSWER: g(16) = {g_values[15]:,}")
    print("="*70 + "\n")

    # Verification
    print("Verification against known values:")
    known = {1: 8, 2: 28, 3: 184, 4: 1646, 5: 19161}
    all_match = True
    for day, expected in known.items():
        actual = g_values[day - 1]
        match = "✓" if actual == expected else "✗"
        print(f"  g({day}): expected {expected:,}, got {actual:,} {match}")
        if actual != expected:
            all_match = False

    if all_match:
        print("\n✓ All known values verified correctly!")
    else:
        print("\n✗ ERROR: Verification failed! Check configuration or simulation logic.")


if __name__ == "__main__":
    main()
