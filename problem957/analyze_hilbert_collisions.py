#!/usr/bin/env python3
"""
Analyze Hilbert Space Collisions (Bidirectional Exploration)

KEY DISCOVERY: Starting Day 3, multiple intersection points map to
the same Hilbert index (gap = 0).

This script uses the bidirectional Hilbert curve to:
  1. Find all collision groups (points with same Hilbert index)
  2. Decode the Hilbert index back to see where it maps
  3. Analyze geometric properties of colliding points
  4. Understand if collisions have algebraic significance
"""

import sys
import numpy as np
sys.path.insert(0, '/home/user/Euler_Problems/problem957')

from src.geometry import Point
from src.propagation import propagate_one_day
from hilbert_bidirectional import HilbertCurve


def get_optimal_config():
    """Get the optimal configuration."""
    red = {
        Point(-1.1420985748, -3.1278529420, 'red'),
        Point(1.7213348846, -0.8343651343, 'red'),
        Point(4.3760906863, 2.3859745813, 'red')
    }
    blue = {
        Point(-1.8437265624, 1.4483260402, 'blue'),
        Point(-1.0486909239, 2.1320688328, 'blue')
    }
    return red, blue


def compute_day_t_points(red, initial_blue, t):
    """Compute all blue points up to day t."""
    all_blues = set(initial_blue)

    for day in range(1, t + 1):
        new_blues = propagate_one_day(red, all_blues)

        if day == t:
            new_blues_day_t = new_blues

        all_blues = all_blues.union(new_blues)

        if len(new_blues) == 0:
            break

    return all_blues, new_blues_day_t


def compute_hilbert_bounds(all_points):
    """Compute bounds for Hilbert encoding."""
    xs = [p.x for p in all_points]
    ys = [p.y for p in all_points]

    min_x, max_x = min(xs), max(xs)
    min_y, max_y = min(ys), max(ys)

    x_range = max_x - min_x
    y_range = max_y - min_y
    min_x -= 0.1 * x_range
    max_x += 0.1 * x_range
    min_y -= 0.1 * y_range
    max_y += 0.1 * y_range

    return (min_x, max_x, min_y, max_y)


def find_collision_groups(points, curve, bounds):
    """
    Find groups of points that map to the same Hilbert index.

    Returns:
        dict mapping Hilbert index → list of points
    """
    index_to_points = {}

    for p in points:
        idx = curve.encode_point(p, bounds)

        if idx not in index_to_points:
            index_to_points[idx] = []

        index_to_points[idx].append(p)

    # Filter to only collision groups (size > 1)
    collision_groups = {idx: pts for idx, pts in index_to_points.items() if len(pts) > 1}

    return collision_groups


def analyze_collision_group(hilbert_idx, points, curve, bounds):
    """
    Analyze a specific collision group.

    Returns:
        dict with analysis results
    """
    # Decode the Hilbert index
    decoded_point = curve.decode_to_bounds(hilbert_idx, bounds)

    # Compute geometric properties of the collision group
    xs = [p.x for p in points]
    ys = [p.y for p in points]

    centroid_x = np.mean(xs)
    centroid_y = np.mean(ys)

    # Distances from decoded point
    distances_from_decoded = []
    for p in points:
        dist = np.sqrt((p.x - decoded_point.x)**2 + (p.y - decoded_point.y)**2)
        distances_from_decoded.append(dist)

    # Pairwise distances within group
    pairwise_dists = []
    for i in range(len(points)):
        for j in range(i + 1, len(points)):
            dist = np.sqrt((points[i].x - points[j].x)**2 + (points[i].y - points[j].y)**2)
            pairwise_dists.append(dist)

    return {
        'hilbert_index': hilbert_idx,
        'num_points': len(points),
        'decoded_point': decoded_point,
        'centroid': (centroid_x, centroid_y),
        'points': points,
        'distances_from_decoded': distances_from_decoded,
        'max_dist_from_decoded': max(distances_from_decoded) if distances_from_decoded else 0,
        'avg_dist_from_decoded': np.mean(distances_from_decoded) if distances_from_decoded else 0,
        'pairwise_dists': pairwise_dists,
        'max_pairwise_dist': max(pairwise_dists) if pairwise_dists else 0,
        'avg_pairwise_dist': np.mean(pairwise_dists) if pairwise_dists else 0,
    }


def main():
    print("="*70)
    print("HILBERT COLLISION ANALYSIS (Bidirectional Exploration)")
    print("="*70)
    print()

    # Setup
    red, initial_blue = get_optimal_config()
    curve = HilbertCurve(order=16)

    # Compute bounds
    all_blues_day1, _ = compute_day_t_points(red, initial_blue, 1)
    all_points_day1 = red.union(all_blues_day1)
    bounds = compute_hilbert_bounds(all_points_day1)

    print(f"Hilbert curve: {curve.n}×{curve.n} grid")
    print(f"Bounds: x ∈ [{bounds[0]:.2f}, {bounds[1]:.2f}], y ∈ [{bounds[2]:.2f}, {bounds[3]:.2f}]")
    print()

    # Analyze Days 1-5
    for t in [1, 2, 3, 4, 5]:
        print("="*70)
        print(f"DAY {t} COLLISION ANALYSIS")
        print("="*70)
        print()

        _, new_blues = compute_day_t_points(red, initial_blue, t)

        # Find collision groups
        collision_groups = find_collision_groups(new_blues, curve, bounds)

        total_colliding_points = sum(len(pts) for pts in collision_groups.values())
        collision_pct = 100.0 * total_colliding_points / len(new_blues) if len(new_blues) > 0 else 0

        print(f"Total new points (Day {t}): {len(new_blues):,}")
        print(f"Collision groups:           {len(collision_groups):,}")
        print(f"Points in collisions:       {total_colliding_points:,} ({collision_pct:.1f}%)")
        print()

        if len(collision_groups) == 0:
            print("✓ No collisions - all points have unique Hilbert indices")
            print()
            continue

        # Analyze largest collision groups
        groups_by_size = sorted(collision_groups.items(), key=lambda x: len(x[1]), reverse=True)

        print(f"Largest collision groups:")
        print()

        num_to_show = min(5, len(groups_by_size))

        for k in range(num_to_show):
            hilbert_idx, points = groups_by_size[k]
            analysis = analyze_collision_group(hilbert_idx, points, curve, bounds)

            print(f"Group {k+1}: {analysis['num_points']} points at Hilbert index {hilbert_idx:,}")
            print(f"  Decoded Hilbert index → ({analysis['decoded_point'].x:.6f}, {analysis['decoded_point'].y:.6f})")
            print(f"  Centroid of group:      ({analysis['centroid'][0]:.6f}, {analysis['centroid'][1]:.6f})")
            print(f"  Max distance from decoded point: {analysis['max_dist_from_decoded']:.6f}")
            print(f"  Avg distance from decoded point: {analysis['avg_dist_from_decoded']:.6f}")
            print(f"  Max pairwise distance in group:  {analysis['max_pairwise_dist']:.6f}")
            print(f"  Avg pairwise distance in group:  {analysis['avg_pairwise_dist']:.6f}")
            print()
            print(f"  Points in group:")
            for i, p in enumerate(points[:3]):  # Show first 3
                dist_from_decoded = np.sqrt((p.x - analysis['decoded_point'].x)**2 +
                                           (p.y - analysis['decoded_point'].y)**2)
                print(f"    {i+1}. ({p.x:10.6f}, {p.y:10.6f})  dist={dist_from_decoded:.6f}")
            if len(points) > 3:
                print(f"    ... and {len(points) - 3} more")
            print()

        # Overall statistics
        all_analyses = [analyze_collision_group(idx, pts, curve, bounds)
                       for idx, pts in collision_groups.items()]

        avg_group_size = np.mean([a['num_points'] for a in all_analyses])
        max_group_size = max([a['num_points'] for a in all_analyses])
        avg_max_pairwise = np.mean([a['max_pairwise_dist'] for a in all_analyses])

        print(f"Collision Statistics:")
        print(f"  Average group size:          {avg_group_size:.2f} points/cell")
        print(f"  Maximum group size:          {max_group_size} points/cell")
        print(f"  Avg max pairwise distance:   {avg_max_pairwise:.6f}")
        print()

    # Key insights
    print("="*70)
    print("KEY INSIGHTS FROM BIDIRECTIONAL ANALYSIS")
    print("="*70)
    print()

    print("1. COLLISION MECHANISM:")
    print("   - Multiple algebraically distinct intersection points")
    print("   - Map to same Hilbert cell due to finite grid resolution")
    print("   - Resolution: 65536×65536 grid over coordinate bounds")
    print()

    print("2. GEOMETRIC INTERPRETATION:")
    print("   - Decoded Hilbert index shows cell center")
    print("   - Actual points cluster tightly around cell")
    print("   - Pairwise distances show true geometric proximity")
    print()

    print("3. SPATIAL CLUSTERING:")
    print("   - Collision rate increases: 0% → 0% → 13.5% → 22.3% → 13.3%")
    print("   - Peak at Day 4, then slight decrease")
    print("   - Suggests non-monotonic clustering behavior")
    print()

    print("4. IMPLICATIONS FOR κ_t:")
    print("   - Collisions indicate high spatial density")
    print("   - High density → more line coincidences → lower κ_t")
    print("   - Hilbert gap captures this clustering effect")
    print()


if __name__ == "__main__":
    main()
