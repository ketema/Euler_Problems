#!/usr/bin/env python3
"""
Higher-Dimensional Structure Test for Day 2

Approach 1: Test if Day 2 points lie on 3D surfaces
Approach 2: Hilbert curve mapping to find locality patterns

This explores whether algebraic structure has hidden geometric patterns.
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '.'))

from src.geometry import Point, Line, intersect


def compute_day_intersections(red_points, blue_points, existing_blues=None):
    """Compute day t intersection points."""
    if existing_blues is None:
        all_blues = blue_points
    else:
        all_blues = blue_points + existing_blues

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


def fit_sphere_3d(points, z_func):
    """
    Fit sphere (x-a)² + (y-b)² + (z-c)² = r² where z = z_func(x, y).

    Simplified: try sphere centered at centroid.
    Returns: center, radius, RMS error
    """
    # Compute centroid
    x_coords = [p.x for p in points]
    y_coords = [p.y for p in points]
    z_coords = [z_func(p.x, p.y) for p in points]

    cx = sum(x_coords) / len(points)
    cy = sum(y_coords) / len(points)
    cz = sum(z_coords) / len(points)

    # Compute average radius
    distances = []
    for i, p in enumerate(points):
        dx = p.x - cx
        dy = p.y - cy
        dz = z_coords[i] - cz
        dist = (dx*dx + dy*dy + dz*dz) ** 0.5
        distances.append(dist)

    radius = sum(distances) / len(distances)

    # Compute RMS error
    errors = []
    for d in distances:
        errors.append((d - radius) ** 2)

    rms = (sum(errors) / len(errors)) ** 0.5

    return (cx, cy, cz), radius, rms


def test_3d_embeddings(points, day_name):
    """Test various 3D embeddings."""
    print(f"\n{'='*70}")
    print(f"{day_name}: 3D SHAPE EMBEDDINGS")
    print(f"{'='*70}\n")

    embeddings = [
        ("Planar (z=0)", lambda x, y: 0),
        ("Parabolic (z=x²+y²)", lambda x, y: x*x + y*y),
        ("Hyperbolic (z=x²-y²)", lambda x, y: x*x - y*y),
        ("Saddle (z=xy)", lambda x, y: x*y),
        ("Radial (z=√(x²+y²))", lambda x, y: (x*x + y*y) ** 0.5),
    ]

    results = []

    for name, z_func in embeddings:
        center, radius, rms = fit_sphere_3d(points, z_func)
        results.append((name, rms, radius))

        status = "✓ GOOD" if rms < 0.01 else ("~ OK" if rms < 0.1 else "✗ POOR")
        print(f"  {name:30s} RMS = {rms:.6f}  r = {radius:.4f}  {status}")

    print()
    best = min(results, key=lambda r: r[1])
    print(f"  Best fit: {best[0]} (RMS = {best[1]:.6f})")

    if best[1] < 0.01:
        print(f"\n  ✓✓✓ 3D STRUCTURE FOUND ✓✓✓")
        print(f"      Points lie on sphere in {best[0]} embedding!")
        return True
    else:
        print(f"\n  ✗ No good 3D sphere embedding found")
        return False


def hilbert_encode_2d(x, y, order=16):
    """
    Map 2D point (x, y) to 1D Hilbert curve index.

    Simple implementation for order <= 16.
    Assumes x, y ∈ [0, 2^order - 1] after scaling.
    """
    # Normalize to grid
    n = 2 ** order
    xi = int((x - min_x) / (max_x - min_x) * (n - 1)) if max_x > min_x else 0
    yi = int((y - min_y) / (max_y - min_y) * (n - 1)) if max_y > min_y else 0

    # Ensure within bounds
    xi = max(0, min(n - 1, xi))
    yi = max(0, min(n - 1, yi))

    # Hilbert encoding (simplified rotation-based algorithm)
    d = 0
    s = n // 2

    while s > 0:
        rx = 1 if (xi & s) > 0 else 0
        ry = 1 if (yi & s) > 0 else 0
        d += s * s * ((3 * rx) ^ ry)

        # Rotate
        if ry == 0:
            if rx == 1:
                xi = n - 1 - xi
                yi = n - 1 - yi
            xi, yi = yi, xi

        s //= 2

    return d


def analyze_hilbert_patterns(day_points_list):
    """
    Map each day's points to Hilbert indices and analyze patterns.

    day_points_list: [(day_num, points), ...]
    """
    global min_x, max_x, min_y, max_y

    print(f"\n{'='*70}")
    print("HILBERT CURVE LOCALITY ANALYSIS")
    print(f"{'='*70}\n")

    # Compute global bounding box for consistent scaling
    all_points = []
    for _, points in day_points_list:
        all_points.extend(points)

    min_x = min(p.x for p in all_points)
    max_x = max(p.x for p in all_points)
    min_y = min(p.y for p in all_points)
    max_y = max(p.y for p in all_points)

    print(f"Global bounds: x ∈ [{min_x:.4f}, {max_x:.4f}], y ∈ [{min_y:.4f}, {max_y:.4f}]")
    print()

    # Encode each day
    day_indices = []
    for day, points in day_points_list:
        indices = [hilbert_encode_2d(p.x, p.y) for p in points]
        indices_sorted = sorted(indices)
        day_indices.append((day, indices_sorted, points))

        print(f"Day {day}: {len(points):4d} points")
        print(f"  Hilbert indices: [{indices_sorted[0]:10d}, ..., {indices_sorted[-1]:10d}]")
        print(f"  Span: {indices_sorted[-1] - indices_sorted[0]:10d}")
        print()

    # Analyze locality overlap between consecutive days
    print(f"{'-'*70}")
    print("LOCALITY OVERLAP ANALYSIS")
    print(f"{'-'*70}\n")

    for i in range(len(day_indices) - 1):
        day1, indices1, points1 = day_indices[i]
        day2, indices2, points2 = day_indices[i + 1]

        # Compute index distribution statistics
        set1 = set(indices1)
        set2 = set(indices2)

        overlap = len(set1 & set2)

        # Average gap between consecutive indices (locality measure)
        gaps1 = [indices1[j+1] - indices1[j] for j in range(len(indices1)-1)]
        gaps2 = [indices2[j+1] - indices2[j] for j in range(len(indices2)-1)]

        avg_gap1 = sum(gaps1) / len(gaps1) if gaps1 else 0
        avg_gap2 = sum(gaps2) / len(gaps2) if gaps2 else 0

        print(f"Day {day1} → Day {day2}:")
        print(f"  Index overlap: {overlap} / {len(set1)} = {100*overlap/len(set1):.1f}%")
        print(f"  Avg gap (Day {day1}): {avg_gap1:.1f}")
        print(f"  Avg gap (Day {day2}): {avg_gap2:.1f}")
        print(f"  Gap ratio: {avg_gap2/avg_gap1:.2f}×" if avg_gap1 > 0 else "  Gap ratio: N/A")

        # Check if Day 2 indices are "near" Day 1 indices
        # (within some threshold in Hilbert space)
        threshold = 1000  # arbitrary
        near_count = 0
        for idx2 in indices2:
            if any(abs(idx2 - idx1) < threshold for idx1 in indices1):
                near_count += 1

        print(f"  Points near Day {day1}: {near_count} / {len(indices2)} = {100*near_count/len(indices2):.1f}%")
        print()

    # Summary
    print(f"{'-'*70}")
    print("INTERPRETATION")
    print(f"{'-'*70}\n")

    # Check if average gap decreases (points getting denser in Hilbert space)
    gaps_all = [(day_indices[i][0], sum([day_indices[i][1][j+1] - day_indices[i][1][j]
                                         for j in range(len(day_indices[i][1])-1)]) /
                                        (len(day_indices[i][1])-1) if len(day_indices[i][1]) > 1 else 0)
                for i in range(len(day_indices))]

    print("Average Hilbert gap per day:")
    for day, gap in gaps_all:
        print(f"  Day {day}: {gap:.1f}")

    # Check if gaps decrease monotonically
    gap_decreasing = all(gaps_all[i][1] >= gaps_all[i+1][1] for i in range(len(gaps_all)-1))

    if gap_decreasing:
        print("\n  ✓ Gaps DECREASE monotonically")
        print("    → Points become denser in Hilbert space over time")
        print("    → Suggests locality preservation: new points near old points")
    else:
        print("\n  ✗ Gaps do NOT decrease monotonically")
        print("    → No clear locality pattern in Hilbert space")


def main():
    print("="*70)
    print("HIGHER-DIMENSIONAL STRUCTURE SEARCH")
    print("="*70)
    print()
    print("Goal: Find geometric patterns in Day 2+ points")
    print("Approach 1: 3D shape embeddings (sphere, etc.)")
    print("Approach 2: Hilbert curve locality analysis")
    print()

    # Configuration
    red_points = [
        Point(-1.1420985748, -3.1278529420),
        Point(1.7213348846, -0.8343651343),
        Point(4.3760906863, 2.3859745813)
    ]

    blue_points = [
        Point(-1.8437265624, 1.4483260402),
        Point(-1.0486909239, 2.1320688328)
    ]

    # Compute days 1-3 (or as many as feasible)
    print("Computing intersection points...")

    day1 = compute_day_intersections(red_points, blue_points)
    print(f"  Day 1: {len(day1)} points")

    day2 = compute_day_intersections(red_points, blue_points, day1)
    print(f"  Day 2: {len(day2)} points")

    all_blues_2 = blue_points + day1 + day2
    day3 = compute_day_intersections(red_points, blue_points, all_blues_2)
    print(f"  Day 3: {len(day3)} points")
    print()

    # APPROACH 1: Test 3D embeddings for Day 2
    found_3d = test_3d_embeddings(day2, "Day 2")

    # APPROACH 2: Hilbert curve analysis
    day_data = [
        (1, day1),
        (2, day2),
        (3, day3),
    ]

    analyze_hilbert_patterns(day_data)

    # Final summary
    print("\n" + "="*70)
    print("SUMMARY")
    print("="*70)
    print()

    if found_3d:
        print("✓ 3D STRUCTURE FOUND")
        print("  Day 2 points lie on sphere in specific 3D embedding")
    else:
        print("✗ NO 3D SPHERE STRUCTURE")
        print("  Day 2 points don't lie on simple 3D sphere")

    print()
    print("Hilbert curve analysis complete (see above for patterns)")
    print()
    print("NEXT STEPS:")
    print("  - If 3D structure found: investigate what z=f(x,y) means geometrically")
    print("  - If Hilbert gaps decrease: points preserve locality → algebraic structure")
    print("  - If no patterns: variety degree d_2 > 3 (need higher-dimensional test)")


if __name__ == "__main__":
    main()
