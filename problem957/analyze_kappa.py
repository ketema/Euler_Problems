#!/usr/bin/env python3
"""
Exact coincidence tracking to compute κ_t values for Project Euler 957.

Route 2 from MATHEMATICAL_PROOF.md: Track which blue-pair/red-pair combinations
produce the same geometric point to compute the combinatorial collapse factor.
"""

import sys
from collections import defaultdict
from typing import Set, Tuple, List, Dict
sys.path.insert(0, '.')

from src.geometry import Point, Line, intersect

# Use the verified optimal configuration
OPTIMAL_RED = {
    Point(-1.1420985748, -3.1278529420, 'red'),
    Point(1.7213348846, -0.8343651343, 'red'),
    Point(4.3760906863, 2.3859745813, 'red')
}

OPTIMAL_BLUE_INITIAL = {
    Point(-1.8437265624, 1.4483260402, 'blue'),
    Point(-1.0486909239, 2.1320688328, 'blue')
}


class LeveledBluePoint:
    """Blue point with provenance tracking."""
    def __init__(self, point: Point, level: int, index: int):
        self.point = point
        self.level = level  # Day it was born
        self.index = index  # Index within that day

    def __repr__(self):
        return f"B[{self.level},{self.index}]({self.point.x:.4f},{self.point.y:.4f})"

    def __eq__(self, other):
        return self.point == other.point

    def __hash__(self):
        return hash(self.point)


def propagate_with_tracking(
    red_list: List[Point],
    blue_leveled: List[LeveledBluePoint],
    day: int
) -> Tuple[List[LeveledBluePoint], Dict]:
    """
    Propagate one day with full coincidence tracking.

    Returns:
        - List of new LeveledBluePoints born on this day
        - Statistics dict with coincidence info
    """
    stats = {
        'day': day,
        'blue_pairs_by_level': defaultdict(int),
        'candidate_intersections': 0,
        'unique_intersections': 0,
        'coincidence_groups': defaultdict(int),  # size -> count
        'kappa': 0.0
    }

    # Separate blues by level
    blues_by_level = defaultdict(list)
    for blue in blue_leveled:
        blues_by_level[blue.level].append(blue)

    # Track intersections: point -> list of (r_i, r_j, b_u, b_v) that created it
    intersection_provenance = defaultdict(list)

    # Generate all red pairs (unordered)
    red_list = list(red_list)
    red_pairs = [(red_list[i], red_list[j])
                 for i in range(len(red_list))
                 for j in range(i+1, len(red_list))]

    # For each blue pair where max-level = day - 1
    for level_u in range(day):
        for level_v in range(day):
            max_level = max(level_u, level_v)
            if max_level != day - 1:
                continue  # Not relevant for this day

            # Count this blue-level-pair
            if level_u == level_v:
                stats['blue_pairs_by_level'][f'({level_u},{level_v})'] = \
                    len(blues_by_level[level_u]) * (len(blues_by_level[level_u]) - 1) // 2
            else:
                stats['blue_pairs_by_level'][f'({level_u},{level_v})'] = \
                    len(blues_by_level[level_u]) * len(blues_by_level[level_v])

            # Enumerate concrete blue pairs
            if level_u == level_v:
                blue_pairs = [(blues_by_level[level_u][i], blues_by_level[level_u][j])
                             for i in range(len(blues_by_level[level_u]))
                             for j in range(i+1, len(blues_by_level[level_u]))]
            else:
                blue_pairs = [(b_u, b_v)
                             for b_u in blues_by_level[level_u]
                             for b_v in blues_by_level[level_v]]

            # For each red pair and blue pair, compute cross-line intersections
            for (r_i, r_j) in red_pairs:
                for (b_u, b_v) in blue_pairs:
                    # Two cross lines: (r_i, b_u) × (r_j, b_v) and (r_i, b_v) × (r_j, b_u)
                    try:
                        line1 = Line(r_i, b_u.point)
                        line2 = Line(r_j, b_v.point)
                        int1 = intersect(line1, line2)
                        if int1 is not None:
                            stats['candidate_intersections'] += 1
                            intersection_provenance[int1].append(
                                (r_i, r_j, b_u, b_v, 'cross1')
                            )
                    except ValueError:
                        pass

                    try:
                        line3 = Line(r_i, b_v.point)
                        line4 = Line(r_j, b_u.point)
                        int2 = intersect(line3, line4)
                        if int2 is not None:
                            stats['candidate_intersections'] += 1
                            intersection_provenance[int2].append(
                                (r_i, r_j, b_u, b_v, 'cross2')
                            )
                    except ValueError:
                        pass

    # Filter out points that are already red or blue
    existing_points = set(red_list) | {b.point for b in blue_leveled}
    new_intersections = {pt: provenance
                        for pt, provenance in intersection_provenance.items()
                        if pt not in existing_points}

    stats['unique_intersections'] = len(new_intersections)

    # Analyze coincidence groups
    for point, provenance_list in new_intersections.items():
        group_size = len(provenance_list)
        stats['coincidence_groups'][group_size] += 1

    # Compute kappa
    if stats['candidate_intersections'] > 0:
        stats['kappa'] = stats['unique_intersections'] / stats['candidate_intersections']

    # Create new LeveledBluePoints
    new_blues = [LeveledBluePoint(pt, day, idx)
                 for idx, pt in enumerate(sorted(new_intersections.keys(),
                                                 key=lambda p: (p.x, p.y)))]

    return new_blues, stats


def analyze_kappa_sequence(max_day: int = 10):
    """Compute κ_t for days 1 through max_day with full analysis."""

    print("="*80)
    print("EXACT COINCIDENCE TRACKING - Computing κ_t")
    print("="*80)
    print()

    # Initialize with level-0 blues
    red_list = list(OPTIMAL_RED)
    blue_leveled = [
        LeveledBluePoint(pt, 0, idx)
        for idx, pt in enumerate(sorted(OPTIMAL_BLUE_INITIAL, key=lambda p: (p.x, p.y)))
    ]

    print(f"Initial configuration:")
    print(f"  Red points: {len(red_list)}")
    print(f"  Blue points (level 0): {len(blue_leveled)}")
    print()

    all_stats = []

    for day in range(1, max_day + 1):
        print(f"Day {day}:")
        print("-" * 60)

        new_blues, stats = propagate_with_tracking(red_list, blue_leveled, day)

        # Add new blues to collection
        blue_leveled.extend(new_blues)

        # Display results
        print(f"  Blue pairs (max-level={day-1}): {sum(stats['blue_pairs_by_level'].values())}")
        print(f"  Red pairs: {len(red_list) * (len(red_list) - 1) // 2}")
        print(f"  Candidate intersections: {stats['candidate_intersections']}")
        print(f"  Unique new blue points: {stats['unique_intersections']}")
        print(f"  κ_{day} = {stats['unique_intersections']}/{stats['candidate_intersections']} = {stats['kappa']:.8f}")

        # Show coincidence distribution
        total_coincident = sum(size * count for size, count in stats['coincidence_groups'].items() if size > 1)
        if total_coincident > 0:
            print(f"  Coincidence groups:")
            for size in sorted(stats['coincidence_groups'].keys(), reverse=True):
                count = stats['coincidence_groups'][size]
                if size > 1:
                    print(f"    {count} points hit by {size} line-pairs each (saved {count * (size - 1)} duplicates)")

        print()

        all_stats.append(stats)

        # Memory check
        if len(blue_leveled) > 100000:
            print(f"  ⚠️  Total blues now: {len(blue_leveled):,} - getting large!")
            print()

    # Summary table
    print("="*80)
    print("SUMMARY TABLE")
    print("="*80)
    print()
    print(f"{'Day':>5} {'m_t':>12} {'P_t':>12} {'C_gen':>12} {'κ_t':>12}")
    print("-" * 60)

    m_values = [2]  # m_0
    B_values = [2]  # B_0

    for stats in all_stats:
        day = stats['day']
        m_t = stats['unique_intersections']
        m_values.append(m_t)
        B_values.append(B_values[-1] + m_t)

        # Compute P_t from formula
        if day == 1:
            P_t = 1  # C(2, 2)
        else:
            P_t = (m_values[day-1] * (m_values[day-1] - 1)) // 2 + m_values[day-1] * B_values[day-2]

        C_gen = 6 * P_t
        kappa = stats['kappa']

        print(f"{day:5} {m_t:12,} {P_t:12,} {C_gen:12,} {kappa:12.8f}")

    print()

    # Look for patterns in kappa
    print("="*80)
    print("KAPPA PATTERN ANALYSIS")
    print("="*80)
    print()

    kappa_values = [s['kappa'] for s in all_stats]

    print("Kappa ratios (κ_{t+1}/κ_t):")
    for i in range(len(kappa_values) - 1):
        ratio = kappa_values[i+1] / kappa_values[i] if kappa_values[i] > 0 else float('inf')
        print(f"  κ_{i+2}/κ_{i+1} = {ratio:.6f}")
    print()

    print("Kappa decay rate:")
    for i, kappa in enumerate(kappa_values, 1):
        print(f"  κ_{i} = {kappa:.10f} ≈ 1/{1/kappa:.2f}")
    print()


if __name__ == '__main__':
    try:
        max_day = int(sys.argv[1]) if len(sys.argv) > 1 else 5
        analyze_kappa_sequence(max_day)
    except KeyboardInterrupt:
        print("\n\nInterrupted by user.")
        sys.exit(1)
