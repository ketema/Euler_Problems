#!/usr/bin/env python3
"""
Compute ACTUAL Hilbert Gaps Using Proper Problem 957 Geometry

Uses:
  - Optimal configuration (red/blue points from OEIS A189191)
  - Propagation algorithm from src/propagation.py
  - Bidirectional Hilbert curve analysis
  - Locality pattern exploration
"""

import sys
import time
import numpy as np
sys.path.insert(0, '/home/user/Euler_Problems/problem957')

from src.geometry import Point
from src.propagation import propagate_one_day
from hilbert_bidirectional import HilbertCurve

def get_optimal_config():
    """Get the optimal configuration that produces OEIS A189191."""
    red = {
        Point(-1.1420985748, -3.1278529420, 'red'),  # R1
        Point(1.7213348846, -0.8343651343, 'red'),   # R2
        Point(4.3760906863, 2.3859745813, 'red')     # R3
    }
    blue = {
        Point(-1.8437265624, 1.4483260402, 'blue'),  # B1
        Point(-1.0486909239, 2.1320688328, 'blue')   # B2
    }
    return red, blue


def compute_day_t_points(red, initial_blue, t):
    """
    Compute all blue points up to day t.

    Returns:
        all_blues: Set of all blue points up to day t
        new_blues_day_t: Set of blue points generated ON day t
    """
    all_blues = set(initial_blue)

    for day in range(1, t + 1):
        new_blues = propagate_one_day(red, all_blues)

        if day == t:
            new_blues_day_t = new_blues

        all_blues = all_blues.union(new_blues)

        if len(new_blues) == 0:
            print(f"    WARNING: Saturation at day {day}")
            break

    return all_blues, new_blues_day_t


def compute_hilbert_bounds(all_points):
    """Compute bounds for Hilbert encoding from all points."""
    xs = [p.x for p in all_points]
    ys = [p.y for p in all_points]

    min_x, max_x = min(xs), max(xs)
    min_y, max_y = min(ys), max(ys)

    # Add 10% padding
    x_range = max_x - min_x
    y_range = max_y - min_y
    min_x -= 0.1 * x_range
    max_x += 0.1 * x_range
    min_y -= 0.1 * y_range
    max_y += 0.1 * y_range

    return (min_x, max_x, min_y, max_y)


def analyze_hilbert_gaps(points, curve, bounds, verbose=False):
    """Analyze Hilbert gaps for a set of points."""
    # Encode all points
    indices = []
    for p in points:
        idx = curve.encode_point(p, bounds)
        indices.append(idx)

    # Sort indices
    indices_sorted = sorted(indices)

    # Compute gaps
    gaps = []
    for i in range(len(indices_sorted) - 1):
        gap = indices_sorted[i + 1] - indices_sorted[i]
        gaps.append(gap)

    # Statistics
    if len(gaps) > 0:
        avg_gap = np.mean(gaps)
        std_gap = np.std(gaps)
        min_gap = np.min(gaps)
        max_gap = np.max(gaps)
        median_gap = np.median(gaps)

        # Count zero gaps (collisions in Hilbert space)
        zero_gaps = sum(1 for g in gaps if g == 0)
        zero_gap_pct = 100.0 * zero_gaps / len(gaps)
    else:
        avg_gap = std_gap = min_gap = max_gap = median_gap = 0
        zero_gaps = 0
        zero_gap_pct = 0.0

    return {
        'num_points': len(points),
        'num_gaps': len(gaps),
        'avg_gap': avg_gap,
        'std_gap': std_gap,
        'min_gap': min_gap,
        'max_gap': max_gap,
        'median_gap': median_gap,
        'zero_gaps': zero_gaps,
        'zero_gap_pct': zero_gap_pct,
        'indices': indices_sorted,
        'gaps': gaps,
    }


def explore_hilbert_neighbors(day, points, curve, bounds, num_pairs=5):
    """
    Explore geometric meaning of Hilbert neighbors.

    For points that are adjacent in Hilbert space (smallest gaps),
    check if they're also close in Euclidean space.
    """
    print(f"\nDay {day} - Hilbert Neighbor Analysis:")
    print("-"*70)

    # Encode points
    point_list = list(points)
    indexed_points = []
    for p in point_list:
        idx = curve.encode_point(p, bounds)
        indexed_points.append((idx, p))

    # Sort by Hilbert index
    indexed_points.sort(key=lambda x: x[0])

    # Find consecutive pairs with smallest gaps
    pairs_with_gaps = []
    for i in range(len(indexed_points) - 1):
        idx1, p1 = indexed_points[i]
        idx2, p2 = indexed_points[i + 1]
        gap = idx2 - idx1

        # Euclidean distance
        euclidean_dist = np.sqrt((p2.x - p1.x)**2 + (p2.y - p1.y)**2)

        pairs_with_gaps.append((gap, euclidean_dist, p1, p2, idx1, idx2))

    # Sort by gap
    pairs_with_gaps.sort(key=lambda x: x[0])

    print(f"Smallest Hilbert gaps (closest neighbors in Hilbert space):\n")

    for k in range(min(num_pairs, len(pairs_with_gaps))):
        gap, euclidean_dist, p1, p2, idx1, idx2 = pairs_with_gaps[k]

        print(f"  Pair {k+1}:")
        print(f"    Hilbert gap:       {gap:,}")
        print(f"    Euclidean distance: {euclidean_dist:.6f}")
        print(f"    Point 1: ({p1.x:10.4f}, {p1.y:10.4f}) [Hilbert index: {idx1:,}]")
        print(f"    Point 2: ({p2.x:10.4f}, {p2.y:10.4f}) [Hilbert index: {idx2:,}]")

        if gap == 0:
            print(f"    → COLLISION: Same Hilbert cell!")
        elif euclidean_dist < 0.01:
            print(f"    → Very close geometrically ✓")
        print()

    # Correlation analysis
    if len(pairs_with_gaps) > 1:
        gaps = [x[0] for x in pairs_with_gaps]
        euclid_dists = [x[1] for x in pairs_with_gaps]

        # Only consider non-zero gaps for correlation
        non_zero = [(g, e) for g, e in zip(gaps, euclid_dists) if g > 0]
        if len(non_zero) > 1:
            nz_gaps, nz_euclid = zip(*non_zero)
            corr = np.corrcoef(nz_gaps, nz_euclid)[0, 1]
            print(f"Correlation (Hilbert gap vs Euclidean distance, excluding zeros):")
            print(f"  Pearson r = {corr:.4f}")
            if corr > 0.7:
                print(f"  → STRONG positive correlation ✓")
                print(f"  → Hilbert curve preserves locality well!")
            elif corr > 0.3:
                print(f"  → MODERATE positive correlation")
            else:
                print(f"  → WEAK correlation")


def main():
    print("="*70)
    print("COMPUTE ACTUAL HILBERT GAPS - PROPER GEOMETRY")
    print("="*70)
    print()

    # Get optimal configuration
    red, initial_blue = get_optimal_config()
    print(f"Optimal configuration:")
    print(f"  Red points:  {len(red)}")
    print(f"  Blue points: {len(initial_blue)}")
    print()

    # Initialize Hilbert curve
    curve = HilbertCurve(order=16)  # 65536×65536 grid
    print(f"Hilbert curve: order={curve.order}, grid={curve.n}×{curve.n}")
    print()

    # Compute bounds from day 1 points
    print("Computing Day 1...")
    all_blues_day1, new_blues_day1 = compute_day_t_points(red, initial_blue, 1)
    print(f"  Total blue points: {len(all_blues_day1)}")
    print(f"  New points day 1:  {len(new_blues_day1)}")

    # Compute bounds
    all_points_day1 = red.union(all_blues_day1)
    bounds = compute_hilbert_bounds(all_points_day1)
    print(f"  Bounds: x ∈ [{bounds[0]:.2f}, {bounds[1]:.2f}], y ∈ [{bounds[2]:.2f}, {bounds[3]:.2f}]")
    print()

    # Compute gaps for Days 1-7 (or as far as feasible)
    max_day = 10

    print("="*70)
    print("COMPUTING GAPS FOR EACH DAY")
    print("="*70)
    print()

    day_stats = []

    for t in range(1, max_day + 1):
        print(f"Day {t}:")
        start_time = time.time()

        # Compute points
        all_blues, new_blues = compute_day_t_points(red, initial_blue, t)

        # Analyze gaps for NEW blues only (day t intersections)
        stats = analyze_hilbert_gaps(new_blues, curve, bounds)
        stats['day'] = t
        stats['total_blues'] = len(all_blues)
        day_stats.append(stats)

        elapsed = time.time() - start_time

        print(f"  Total blue points: {len(all_blues):,}")
        print(f"  NEW points (day {t}): {stats['num_points']:,}")
        print(f"  Average gap:      {stats['avg_gap']:,.1f}")
        print(f"  Median gap:       {stats['median_gap']:,.1f}")
        print(f"  Min gap:          {stats['min_gap']:,}")
        print(f"  Max gap:          {stats['max_gap']:,}")
        print(f"  Zero gaps:        {stats['zero_gaps']:,} ({stats['zero_gap_pct']:.1f}%)")
        print(f"  Time:             {elapsed:.2f}s")
        print()

        # Stop if taking too long or saturation
        if elapsed > 30:
            print(f"  WARNING: Day {t} took {elapsed:.1f}s, stopping here")
            break

        if len(new_blues) == 0:
            print(f"  Saturation reached at day {t}")
            break

    # Summary table
    print("="*70)
    print("SUMMARY TABLE")
    print("="*70)
    print()
    print(f"{'Day':<5} {'Total Blues':<12} {'New Points':<12} {'Avg Gap':<15} {'Median Gap':<15} {'Zero Gaps %':<12}")
    print("-"*85)

    for stats in day_stats:
        print(f"{stats['day']:<5} {stats['total_blues']:<12,} {stats['num_points']:<12,} "
              f"{stats['avg_gap']:<15,.1f} {stats['median_gap']:<15,.1f} {stats['zero_gap_pct']:<12.1f}")

    print()

    # Explore locality patterns for first few days
    print("="*70)
    print("LOCALITY PATTERN EXPLORATION")
    print("="*70)

    for t in range(1, min(4, len(day_stats) + 1)):
        _, new_blues = compute_day_t_points(red, initial_blue, t)
        if len(new_blues) > 1:
            explore_hilbert_neighbors(t, new_blues, curve, bounds, num_pairs=3)

    # Export data
    print()
    print("="*70)
    print("DATA FOR κ_t MODEL UPDATE")
    print("="*70)
    print()
    print("days = [" + ", ".join(str(s['day']) for s in day_stats) + "]")
    print("hilbert_gaps = [" + ", ".join(f"{s['avg_gap']:.1f}" for s in day_stats) + "]")
    print("m_t = [" + ", ".join(str(s['num_points']) for s in day_stats) + "]  # NEW points each day")
    print("total_blues = [" + ", ".join(str(s['total_blues']) for s in day_stats) + "]")
    print()

    # Save results
    output_file = 'actual_hilbert_gaps_proper.txt'
    with open(output_file, 'w') as f:
        f.write("="*70 + "\n")
        f.write("ACTUAL HILBERT GAPS - PROPER PROBLEM 957 GEOMETRY\n")
        f.write("="*70 + "\n\n")
        f.write("Configuration: Optimal (OEIS A189191)\n\n")

        for stats in day_stats:
            f.write(f"Day {stats['day']}:\n")
            f.write(f"  Total blues:     {stats['total_blues']:,}\n")
            f.write(f"  New points:      {stats['num_points']:,}\n")
            f.write(f"  Average gap:     {stats['avg_gap']:,.1f}\n")
            f.write(f"  Median gap:      {stats['median_gap']:,.1f}\n")
            f.write(f"  Std gap:         {stats['std_gap']:,.1f}\n")
            f.write(f"  Min gap:         {stats['min_gap']:,}\n")
            f.write(f"  Max gap:         {stats['max_gap']:,}\n")
            f.write(f"  Zero gaps:       {stats['zero_gaps']:,} ({stats['zero_gap_pct']:.1f}%)\n")
            f.write("\n")

    print(f"Detailed results saved to: {output_file}")
    print()


if __name__ == "__main__":
    main()
