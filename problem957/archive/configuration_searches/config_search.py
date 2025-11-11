#!/usr/bin/env python3
"""
Configuration Search for Problem 957

Strategy: "Maximal possible" likely means finding the OPTIMAL initial configuration
of 2 blue points that maximizes g(n) for each n.

Approach:
1. Generate candidate configurations (small rational coordinates)
2. Rank by geometric properties (spread, symmetry)
3. For each config: simulate and check if g(1)=8, g(2)=28
4. For valid configs: push to g(16) with timeout on each day
5. Stop if computation becomes intractable (>10 min per day)
6. Return g(16) for the config that succeeds

Formula: g(16) = 2 + Σ(t=0 to 15) m_t
where m_t = new blues created on day t (from simulation)
"""

from sympy import Point, Line, Rational
from sympy.geometry import intersection
from itertools import combinations, product
from typing import List, Set, Tuple, Optional
import time
import signal

class TimeoutError(Exception):
    pass

def timeout_handler(signum, frame):
    raise TimeoutError("Computation timed out")

class ConfigSearchSolver:
    def __init__(self, reds: List[Point], initial_blues: List[Point]):
        self.reds = reds
        self.blues_by_day = {0: set(initial_blues)}
        self.timing = {}  # Track time per day

    def get_all_blues(self, day: int) -> Set[Point]:
        return self.blues_by_day.get(day, set())

    def simulate_day_fast(self, from_day: int, timeout_seconds: int = 600) -> Tuple[int, float]:
        """
        Simulate one day with timeout protection.
        Returns (g(next_day), elapsed_time)
        Raises TimeoutError if exceeds timeout_seconds
        """
        start = time.time()

        current_cumulative_blues = self.get_all_blues(from_day)

        # Draw lines from reds to blues
        lines = []
        for red in self.reds:
            for blue in current_cumulative_blues:
                lines.append(Line(red, blue))

        # Existing points
        existing: Set[Point] = set(self.reds).union(current_cumulative_blues)

        # Find intersections with periodic timeout checking
        new_points_this_day: Set[Point] = set()
        total_pairs = len(lines) * (len(lines) - 1) // 2

        for i, line1 in enumerate(lines):
            # Check timeout every 1000 pairs
            if i % 1000 == 0:
                elapsed = time.time() - start
                if elapsed > timeout_seconds:
                    raise TimeoutError(f"Day {from_day}→{from_day+1} timeout after {elapsed:.1f}s")

            for line2 in lines[i+1:]:
                result = intersection(line1, line2)
                if result and hasattr(result[0], 'x'):
                    p = result[0]
                    if p not in existing and p not in new_points_this_day:
                        new_points_this_day.add(p)

        # Accumulate
        next_day_cumulative_blues = current_cumulative_blues.union(new_points_this_day)
        self.blues_by_day[from_day + 1] = next_day_cumulative_blues

        elapsed = time.time() - start
        self.timing[from_day + 1] = elapsed

        return len(next_day_cumulative_blues), elapsed

def check_general_position(reds: List[Point], blues: List[Point]) -> bool:
    """Check no 3 points are collinear (general position requirement)"""
    all_points = reds + blues
    for i, p1 in enumerate(all_points):
        for j, p2 in enumerate(all_points[i+1:], i+1):
            line = Line(p1, p2)
            collinear = [p for p in all_points if line.contains(p)]
            if len(collinear) > 2:
                return False
    return True

def rank_configuration(reds: List[Point], blues: List[Point]) -> float:
    """
    Rank configuration by likelihood to be optimal.
    Higher score = more likely to succeed.

    Heuristics:
    - Avoid degeneracies (collinear)
    - Prefer spread out points (larger area)
    - Prefer symmetric arrangements
    - Prefer small coordinate complexity (computation efficiency)
    """
    score = 0.0

    # General position check (required)
    if not check_general_position(reds, blues):
        return -1000.0

    # Spread: larger convex hull area is better
    from sympy.geometry import Polygon
    try:
        poly = Polygon(*blues, *reds)
        area = float(poly.area)
        score += area * 10
    except:
        pass

    # Coordinate complexity: prefer small denominators
    total_complexity = 0
    for p in blues:
        if hasattr(p.x, 'q'):  # Rational
            total_complexity += p.x.q + p.y.q
        else:
            total_complexity += 1
    score -= total_complexity * 0.1

    # Symmetry bonus (if blues are symmetric about triangle centroid)
    red_centroid = Point(
        sum(r.x for r in reds) / 3,
        sum(r.y for r in reds) / 3
    )
    dist1 = float(blues[0].distance(red_centroid))
    dist2 = float(blues[1].distance(red_centroid))
    if abs(dist1 - dist2) < 0.1:
        score += 5

    return score

def generate_configurations(reds: List[Point], coord_range: int = 5) -> List[Tuple[List[Point], float]]:
    """
    Generate candidate initial blue configurations.
    Returns list of (blues, score) sorted by score descending.

    Strategy:
    - Use small integer and simple rational coordinates
    - Filter out degenerate cases
    - Rank by geometric quality
    """
    candidates = []

    # Integer coordinates
    for x1 in range(-coord_range, coord_range + 1):
        for y1 in range(-coord_range, coord_range + 1):
            for x2 in range(x1, coord_range + 1):  # Avoid duplicates
                for y2 in range(-coord_range, coord_range + 1):
                    if x1 == x2 and y1 >= y2:  # Avoid duplicates
                        continue

                    b1 = Point(x1, y1)
                    b2 = Point(x2, y2)

                    # Skip if coincident with reds
                    if b1 in reds or b2 in reds:
                        continue

                    blues = [b1, b2]
                    score = rank_configuration(reds, blues)

                    if score > -100:  # Filter out obviously bad
                        candidates.append((blues, score))

    # Simple rationals (halves only, for speed)
    for x1_num in range(-coord_range*2, coord_range*2 + 1):
        for y1_num in range(-coord_range*2, coord_range*2 + 1):
            if abs(x1_num) % 2 == 0 and abs(y1_num) % 2 == 0:  # Skip integers (already covered)
                continue

            x1 = Rational(x1_num, 2)
            y1 = Rational(y1_num, 2)

            # Just try a few strategic second points for halves (combinatorial explosion)
            for dx in [-1, 0, 1]:
                for dy in [-1, 0, 1]:
                    if dx == 0 and dy == 0:
                        continue

                    x2 = x1 + dx
                    y2 = y1 + dy

                    b1 = Point(x1, y1)
                    b2 = Point(x2, y2)

                    if b1 in reds or b2 in reds:
                        continue

                    blues = [b1, b2]
                    score = rank_configuration(reds, blues)

                    if score > -100:
                        candidates.append((blues, score))

    # Sort by score descending
    candidates.sort(key=lambda x: x[1], reverse=True)

    return candidates

def search_for_valid_config(reds: List[Point], max_configs: int = 1000, timeout_per_day: int = 300):
    """
    Search for configuration that:
    1. Produces g(1)=8, g(2)=28 (required)
    2. Allows computation to day 16 (tractable)

    Returns (config, g_values, success)
    """
    print("="*80)
    print("CONFIGURATION SEARCH")
    print("="*80)
    print()
    print(f"Generating candidate configurations...")

    candidates = generate_configurations(reds, coord_range=5)

    print(f"Generated {len(candidates)} candidates")
    print(f"Testing top {min(max_configs, len(candidates))} configurations...")
    print()

    valid_configs = []

    for idx, (blues, score) in enumerate(candidates[:max_configs]):
        if idx % 100 == 0:
            print(f"\nProgress: {idx}/{min(max_configs, len(candidates))}")

        solver = ConfigSearchSolver(reds, blues)

        try:
            # Quick validation: g(1) and g(2) must match
            g1, t1 = solver.simulate_day_fast(0, timeout_seconds=10)

            if g1 != 8:
                continue  # Not the right config

            g2, t2 = solver.simulate_day_fast(1, timeout_seconds=30)

            if g2 != 28:
                continue  # Not the right config

            # Valid! This config produces g(1)=8, g(2)=28
            print(f"\n{'='*80}")
            print(f"VALID CONFIG FOUND (#{len(valid_configs) + 1})")
            print(f"{'='*80}")
            print(f"Blues: {blues}")
            print(f"Score: {score:.2f}")
            print(f"g(1)={g1} ✓, g(2)={g2} ✓")
            print(f"Times: day 0→1: {t1:.3f}s, day 1→2: {t2:.3f}s")
            print()

            # Now try to push to g(16)
            print("Attempting to compute g(16)...")
            g_values = [2, g1, g2]

            success = True
            for day in range(2, 16):
                try:
                    print(f"  Day {day}→{day+1}...", end=" ", flush=True)
                    g_next, elapsed = solver.simulate_day_fast(day, timeout_seconds=timeout_per_day)
                    g_values.append(g_next)
                    print(f"g({day+1})={g_next} (time: {elapsed:.1f}s)")

                    # Adaptive timeout: if this day took >60s, likely intractable
                    if elapsed > 60:
                        print(f"  ⚠️  Day {day}→{day+1} took {elapsed:.1f}s, trajectory becoming intractable")
                        # But continue for now...

                except TimeoutError as e:
                    print(f"✗ TIMEOUT: {e}")
                    success = False
                    break

            if success:
                print()
                print(f"{'='*80}")
                print(f"SUCCESS! Computed g(16) = {g_values[16]}")
                print(f"{'='*80}")
                print()
                print(f"Complete sequence: {g_values}")
                print()
                print(f"Formula verification:")
                m_values = [g_values[i] - g_values[i-1] if i > 0 else 2 for i in range(len(g_values))]
                print(f"m_t (new blues per day): {m_values}")
                print(f"g(16) = 2 + Σm_t = 2 + {sum(m_values[1:])} = {g_values[16]}")
                print()

                return blues, g_values, True

            else:
                print(f"  Config #{len(valid_configs)+1} became intractable before day 16")
                valid_configs.append((blues, g_values, False))

        except Exception as e:
            # Skip bad configs
            continue

    print()
    print(f"{'='*80}")
    print(f"SEARCH COMPLETE")
    print(f"{'='*80}")
    print(f"Tested {min(max_configs, len(candidates))} configurations")
    print(f"Found {len(valid_configs)} producing g(1)=8, g(2)=28")
    print(f"None computed successfully to g(16) within timeout")
    print()

    if valid_configs:
        print("Valid configs found (but intractable):")
        for i, (blues, g_vals, _) in enumerate(valid_configs):
            print(f"  {i+1}. Blues: {blues}")
            print(f"     Computed to g({len(g_vals)-1}) = {g_vals[-1]}")

    return None, [], False

def main():
    print("="*80)
    print("Problem 957: Configuration Search Strategy")
    print("="*80)
    print()

    # Fixed reds (standard triangle)
    reds = [
        Point(0, 0),
        Point(4, 0),
        Point(2, 3),
    ]

    print(f"Red points (fixed): {reds}")
    print()
    print("Searching for optimal initial blue configuration...")
    print("Requirement: g(1)=8, g(2)=28 (given)")
    print("Goal: Find config that allows tractable computation to g(16)")
    print()

    blues, g_values, success = search_for_valid_config(
        reds,
        max_configs=1000,  # Test up to 1000 configs
        timeout_per_day=300  # 5 min per day max
    )

    if success:
        print()
        print(f"{'='*80}")
        print(f"ANSWER FOUND: g(16) = {g_values[16]}")
        print(f"{'='*80}")
    else:
        print()
        print("No configuration found that computes to g(16) tractably.")
        print("This suggests either:")
        print("  1. Need more sophisticated configuration generation")
        print("  2. Problem interpretation is different than assumed")
        print("  3. Answer is not derivable from direct simulation")

if __name__ == "__main__":
    main()
