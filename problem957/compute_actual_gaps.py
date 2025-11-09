#!/usr/bin/env python3
"""
Compute ACTUAL Hilbert Gaps for Days 1-10

No extrapolation - we calculate the real gaps by:
  1. Computing intersection points for each day
  2. Encoding them to Hilbert indices
  3. Measuring actual gaps between consecutive indices

Also explores:
  - Locality patterns (decode neighboring indices)
  - Geometric significance of Hilbert neighbors
  - Spatial distribution analysis
"""

import sys
import time
import math
import numpy as np
from collections import namedtuple, defaultdict
from hilbert_bidirectional import HilbertCurve, Point

# Use existing tribonacci code
sys.path.insert(0, '/home/user/Euler_Problems/problem957')


def tribonacci_triple():
    """Generate Tribonacci triples (a, b, c) where a+b+c=next."""
    a, b, c = 0, 0, 1
    while True:
        yield (a, b, c)
        a, b, c = b, c, a + b + c


def tribonacci_pairs(max_sum):
    """Generate pairs (i, j) where T_i + T_j ≤ max_sum."""
    # Generate Tribonacci numbers up to max_sum
    T = [0, 0, 1]
    while T[-1] < max_sum:
        T.append(T[-1] + T[-2] + T[-3])

    pairs = []
    for i in range(len(T)):
        for j in range(i, len(T)):
            if T[i] + T[j] <= max_sum:
                pairs.append((i, j))

    return pairs


def compute_intersections_day_t(t, verbose=False):
    """
    Compute intersection points for day t.

    Returns:
        List of Point(x, y) where lines intersect
    """
    if verbose:
        print(f"  Computing day {t} intersections...")

    # Get B values
    gen = tribonacci_triple()
    B_vals = [2]  # B_0 = 2
    for _ in range(t):
        a, b, c = next(gen)
        B_vals.append(B_vals[-1] + (a + b + c))

    # For day t, we use B_{t-1} to determine max sum
    B_target = B_vals[t - 1]

    # Generate pairs
    pairs = tribonacci_pairs(B_target)

    # Each pair defines a line
    # Line: T_i·x + T_j·y = B_t
    # We need to find intersection points

    # For efficiency, we'll compute unique intersections
    # Two lines (i1,j1) and (i2,j2) intersect where:
    #   T_i1·x + T_j1·y = B_t
    #   T_i2·x + T_j2·y = B_t

    # Generate Tribonacci numbers
    T = [0, 0, 1]
    while len(T) < 100:  # Enough for our purposes
        T.append(T[-1] + T[-2] + T[-3])

    # Find t value (T_0 + T_t = B_1 = 3, so T_t = 3)
    # This gives us t
    B_t = B_vals[t]

    points = []
    points_set = set()

    # Compute intersections (pairs of lines)
    for idx1 in range(len(pairs)):
        for idx2 in range(idx1 + 1, len(pairs)):
            i1, j1 = pairs[idx1]
            i2, j2 = pairs[idx2]

            # Coefficients
            a1, b1, c1 = T[i1], T[j1], B_t
            a2, b2, c2 = T[i2], T[j2], B_t

            # Solve 2x2 system: a1·x + b1·y = c1
            #                    a2·x + b2·y = c2
            det = a1 * b2 - a2 * b1

            if abs(det) > 1e-10:  # Non-parallel lines
                x = (c1 * b2 - c2 * b1) / det
                y = (a1 * c2 - a2 * c1) / det

                # Round to avoid duplicates from floating point errors
                x_round = round(x, 6)
                y_round = round(y, 6)

                if (x_round, y_round) not in points_set:
                    points_set.add((x_round, y_round))
                    points.append(Point(x, y))

    if verbose:
        print(f"    Found {len(points)} unique intersection points")

    return points


def analyze_hilbert_gaps_day(t, points, curve, bounds, verbose=False):
    """
    Analyze Hilbert gaps for a specific day.

    Returns:
        dict with gap statistics
    """
    if verbose:
        print(f"  Analyzing Hilbert gaps for day {t}...")

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
    else:
        avg_gap = std_gap = min_gap = max_gap = median_gap = 0

    return {
        'day': t,
        'num_points': len(points),
        'num_gaps': len(gaps),
        'avg_gap': avg_gap,
        'std_gap': std_gap,
        'min_gap': min_gap,
        'max_gap': max_gap,
        'median_gap': median_gap,
        'indices': indices_sorted,
        'gaps': gaps,
    }


def explore_locality_patterns(day_stats, curve, bounds, num_neighbors=5):
    """
    Explore locality patterns by decoding neighboring Hilbert indices.

    For each day, find points with smallest gaps and decode their neighbors
    to see if there's geometric significance.
    """
    print()
    print("="*70)
    print("LOCALITY PATTERN EXPLORATION")
    print("="*70)
    print()
    print("Goal: See if Hilbert neighbors have geometric meaning")
    print()

    for stats in day_stats:
        t = stats['day']
        indices = stats['indices']
        gaps = stats['gaps']

        if len(gaps) == 0:
            continue

        print(f"Day {t}:")
        print("-"*70)

        # Find smallest gaps (closest neighbors in Hilbert space)
        gap_idx_pairs = [(gaps[i], i) for i in range(len(gaps))]
        gap_idx_pairs.sort()

        print(f"  Smallest gaps (closest Hilbert neighbors):")
        print()

        for k in range(min(num_neighbors, len(gap_idx_pairs))):
            gap, idx = gap_idx_pairs[k]
            idx1 = indices[idx]
            idx2 = indices[idx + 1]

            # Decode both indices
            p1 = curve.decode_to_bounds(idx1, bounds)
            p2 = curve.decode_to_bounds(idx2, bounds)

            # Euclidean distance
            euclidean_dist = math.sqrt((p2.x - p1.x)**2 + (p2.y - p1.y)**2)

            print(f"    Gap #{k+1}: Hilbert gap = {gap:,}")
            print(f"      Point 1: ({p1.x:10.3f}, {p1.y:10.3f})  [index {idx1:,}]")
            print(f"      Point 2: ({p2.x:10.3f}, {p2.y:10.3f})  [index {idx2:,}]")
            print(f"      Euclidean distance: {euclidean_dist:.6f}")
            print()

        # Also check: Do smallest Hilbert gaps correspond to smallest Euclidean distances?
        print(f"  Correlation: Hilbert gap vs Euclidean distance")

        # Compute Euclidean distances for all consecutive pairs
        euclidean_dists = []
        for i in range(len(indices) - 1):
            p1 = curve.decode_to_bounds(indices[i], bounds)
            p2 = curve.decode_to_bounds(indices[i + 1], bounds)
            dist = math.sqrt((p2.x - p1.x)**2 + (p2.y - p1.y)**2)
            euclidean_dists.append(dist)

        # Correlation
        if len(gaps) > 1:
            corr = np.corrcoef(gaps, euclidean_dists)[0, 1]
            print(f"    Pearson correlation: r = {corr:.4f}")
            if corr > 0.7:
                print(f"    → STRONG positive correlation ✓")
            elif corr > 0.3:
                print(f"    → MODERATE positive correlation")
            else:
                print(f"    → WEAK correlation")
        print()


def main():
    print("="*70)
    print("COMPUTE ACTUAL HILBERT GAPS (Days 1-10)")
    print("="*70)
    print()
    print("No extrapolation - computing real gaps from actual intersection points")
    print()

    # Initialize Hilbert curve
    curve = HilbertCurve(order=16)  # 65536×65536 grid
    print(f"Hilbert curve: order={curve.order}, grid={curve.n}×{curve.n}")
    print()

    # Determine bounds (we'll compute this from Day 1 and use globally)
    print("Computing Day 1 to determine coordinate bounds...")
    day1_points = compute_intersections_day_t(1, verbose=True)

    if len(day1_points) == 0:
        print("ERROR: No points found for Day 1")
        return

    xs = [p.x for p in day1_points]
    ys = [p.y for p in day1_points]
    min_x, max_x = min(xs), max(xs)
    min_y, max_y = min(ys), max(ys)

    # Add 10% padding
    x_range = max_x - min_x
    y_range = max_y - min_y
    min_x -= 0.1 * x_range
    max_x += 0.1 * x_range
    min_y -= 0.1 * y_range
    max_y += 0.1 * y_range

    bounds = (min_x, max_x, min_y, max_y)
    print(f"  Bounds: x ∈ [{min_x:.2f}, {max_x:.2f}], y ∈ [{min_y:.2f}, {max_y:.2f}]")
    print()

    # Compute gaps for Days 1-10 (as far as feasible)
    max_day = 7  # Start with Day 7, can increase if fast enough

    print("="*70)
    print("COMPUTING GAPS")
    print("="*70)
    print()

    day_stats = []

    for t in range(1, max_day + 1):
        print(f"Day {t}:")
        start_time = time.time()

        # Compute intersection points
        points = compute_intersections_day_t(t, verbose=False)
        print(f"  Points: {len(points):,}")

        # Analyze gaps
        stats = analyze_hilbert_gaps_day(t, points, curve, bounds, verbose=False)
        day_stats.append(stats)

        elapsed = time.time() - start_time
        print(f"  Average gap: {stats['avg_gap']:,.1f}")
        print(f"  Median gap:  {stats['median_gap']:,.1f}")
        print(f"  Min gap:     {stats['min_gap']:,}")
        print(f"  Max gap:     {stats['max_gap']:,}")
        print(f"  Time: {elapsed:.2f}s")
        print()

        # Stop if taking too long
        if elapsed > 60:  # More than 1 minute
            print(f"  WARNING: Day {t} took {elapsed:.1f}s, stopping here")
            break

    # Summary table
    print("="*70)
    print("SUMMARY")
    print("="*70)
    print()
    print(f"{'Day':<5} {'Points':<12} {'Avg Gap':<15} {'Median Gap':<15} {'Std Gap':<15}")
    print("-"*70)

    for stats in day_stats:
        print(f"{stats['day']:<5} {stats['num_points']:<12,} "
              f"{stats['avg_gap']:<15,.1f} {stats['median_gap']:<15,.1f} "
              f"{stats['std_gap']:<15,.1f}")

    print()

    # Explore locality patterns
    if len(day_stats) >= 3:
        explore_locality_patterns(day_stats[:3], curve, bounds, num_neighbors=3)

    # Export data for κ_t model update
    print("="*70)
    print("DATA FOR κ_t MODEL UPDATE")
    print("="*70)
    print()
    print("Copy this into your model:")
    print()
    print("days = [" + ", ".join(str(s['day']) for s in day_stats) + "]")
    print("hilbert_gaps = [" + ", ".join(f"{s['avg_gap']:.1f}" for s in day_stats) + "]")
    print("num_points = [" + ", ".join(str(s['num_points']) for s in day_stats) + "]")
    print()

    # Save detailed results
    output_file = 'actual_hilbert_gaps.txt'
    with open(output_file, 'w') as f:
        f.write("="*70 + "\n")
        f.write("ACTUAL HILBERT GAPS (No Extrapolation)\n")
        f.write("="*70 + "\n\n")

        for stats in day_stats:
            f.write(f"Day {stats['day']}:\n")
            f.write(f"  Points: {stats['num_points']:,}\n")
            f.write(f"  Average gap: {stats['avg_gap']:,.1f}\n")
            f.write(f"  Median gap: {stats['median_gap']:,.1f}\n")
            f.write(f"  Std gap: {stats['std_gap']:,.1f}\n")
            f.write(f"  Min gap: {stats['min_gap']:,}\n")
            f.write(f"  Max gap: {stats['max_gap']:,}\n")
            f.write("\n")

    print(f"Detailed results saved to: {output_file}")
    print()


if __name__ == "__main__":
    main()
