#!/usr/bin/env python3
"""
Rapid Configuration Search - Find FAST-COMPUTING configs

Strategy: Only test configs that compute each day in <10 seconds.
If any day takes >=10s, terminate and move to next config.

This finds configs with tractable computation to g(16).
"""

from sympy import Point, Line, Rational
from sympy.geometry import intersection
from typing import List, Set, Tuple, Optional
import time
import random

class RapidSolver:
    def __init__(self, reds: List[Point], initial_blues: List[Point]):
        self.reds = reds
        self.blues_by_day = {0: set(initial_blues)}

    def get_all_blues(self, day: int) -> Set[Point]:
        return self.blues_by_day.get(day, set())

    def simulate_day_rapid(self, from_day: int, max_seconds: float = 10.0) -> Tuple[Optional[int], float]:
        """
        Simulate with strict 10-second timeout per day.
        Returns (g_next, elapsed) or (None, elapsed) if timeout/too large.
        """
        start = time.time()
        current_blues = self.get_all_blues(from_day)

        # Early termination if already too many blues
        if len(current_blues) > 200:
            return None, time.time() - start

        lines = []
        for red in self.reds:
            for blue in current_blues:
                lines.append(Line(red, blue))

        existing = set(self.reds).union(current_blues)
        new_points = set()

        # Check timeout every 50 line pairs
        for i, line1 in enumerate(lines):
            if i % 50 == 0:
                if time.time() - start > max_seconds:
                    return None, time.time() - start

            for line2 in lines[i+1:]:
                result = intersection(line1, line2)
                if result and hasattr(result[0], 'x'):
                    p = result[0]
                    if p not in existing and p not in new_points:
                        new_points.add(p)

        next_day_blues = current_blues.union(new_points)
        self.blues_by_day[from_day + 1] = next_day_blues

        elapsed = time.time() - start
        return len(next_day_blues), elapsed


def generate_fast_configs(reds: List[Point], count: int = 3000) -> List[List[Point]]:
    """
    Generate diverse configs quickly.
    Prioritize smaller coordinates (faster computation).
    """
    configs = []
    random.seed(42)

    # Strategy 1: Small integers [-4, 4]
    for x1 in range(-4, 5):
        for y1 in range(-4, 5):
            for x2 in range(x1, 5):
                for y2 in range(-4, 5):
                    if (x1, y1) >= (x2, y2):
                        continue

                    b1, b2 = Point(x1, y1), Point(x2, y2)
                    if b1 not in reds and b2 not in reds:
                        configs.append([b1, b2])

    # Strategy 2: Random small integers
    for _ in range(count // 2):
        x1, y1 = random.randint(-5, 5), random.randint(-5, 5)
        x2, y2 = random.randint(-5, 5), random.randint(-5, 5)
        b1, b2 = Point(x1, y1), Point(x2, y2)
        if b1 not in reds and b2 not in reds and b1 != b2:
            configs.append([b1, b2])

    # Strategy 3: Simple rationals (halves)
    for x1_num in [-3, -1, 1, 3]:
        for y1_num in [-3, -1, 1, 3]:
            for x2_num in [-3, -1, 1, 3]:
                for y2_num in [-3, -1, 1, 3]:
                    if (x1_num, y1_num) >= (x2_num, y2_num):
                        continue
                    b1 = Point(Rational(x1_num, 2), Rational(y1_num, 2))
                    b2 = Point(Rational(x2_num, 2), Rational(y2_num, 2))
                    if b1 not in reds and b2 not in reds:
                        configs.append([b1, b2])

    # Deduplicate
    seen = set()
    unique = []
    for cfg in configs:
        key = tuple(sorted([(float(p.x), float(p.y)) for p in cfg]))
        if key not in seen:
            seen.add(key)
            unique.append(cfg)

    return unique[:count]


def rapid_search(reds: List[Point], max_configs: int = 3000, max_seconds_per_day: float = 10.0):
    """
    Search for configs that compute to g(16) in <10s per day.
    """
    print("="*80)
    print("RAPID CONFIGURATION SEARCH")
    print("="*80)
    print()
    print(f"Max time per day: {max_seconds_per_day}s")
    print(f"Target: Find ANY config reaching g(16) tractably")
    print()

    configs = generate_fast_configs(reds, count=max_configs)
    print(f"Generated {len(configs)} configurations")
    print()

    start_search = time.time()
    tested = 0
    fast_configs = []  # Configs that completed g(16)

    for idx, blues in enumerate(configs):
        tested += 1

        if idx % 100 == 0 and idx > 0:
            elapsed = time.time() - start_search
            rate = tested / elapsed
            eta = (len(configs) - tested) / rate if rate > 0 else 999
            print(f"Progress: {tested}/{len(configs)} ({100*tested/len(configs):.1f}%) | "
                  f"Rate: {rate:.1f} cfg/s | ETA: {eta:.0f}s | Found: {len(fast_configs)}")

        solver = RapidSolver(reds, blues)

        try:
            g_sequence = [2]
            timings = []
            success = True

            for day in range(16):
                g_next, elapsed = solver.simulate_day_rapid(day, max_seconds=max_seconds_per_day)

                if g_next is None:
                    # Timeout or too large
                    success = False
                    break

                g_sequence.append(g_next)
                timings.append(elapsed)

            if success and len(g_sequence) == 17:
                # Success!
                print()
                print("="*80)
                print(f"FAST CONFIG FOUND #{len(fast_configs) + 1}")
                print("="*80)
                print(f"Blues: {blues}")
                print(f"Sequence: {g_sequence}")
                print(f"g(1)={g_sequence[1]}, g(2)={g_sequence[2]}, g(16)={g_sequence[16]}")
                print(f"Max time per day: {max(timings):.2f}s")
                print(f"Total time: {sum(timings):.1f}s")
                print()

                fast_configs.append((blues, g_sequence, timings))

                # Report first few, then just count
                if len(fast_configs) <= 3:
                    print(f"Continuing search for more configs...")
                    print()

        except Exception as e:
            # Skip bad configs
            continue

    print()
    print("="*80)
    print("SEARCH COMPLETE")
    print("="*80)
    print(f"Tested: {tested} configurations")
    print(f"Found: {len(fast_configs)} fast-computing configs")
    print()

    if fast_configs:
        print("RESULTS:")
        print()
        for i, (blues, g_seq, timings) in enumerate(fast_configs, 1):
            print(f"{i}. Blues: {blues}")
            print(f"   g(1)={g_seq[1]}, g(2)={g_seq[2]}, g(16)={g_seq[16]}")
            print(f"   Max time: {max(timings):.2f}s, Total: {sum(timings):.1f}s")
            print()

        # Recommend the "best" one
        if any(g_seq[1] == 8 and g_seq[2] == 28 for _, g_seq, _ in fast_configs):
            print("⭐ Found config(s) matching example g(1)=8, g(2)=28!")
            matching = [(b, g, t) for b, g, t in fast_configs if g[1] == 8 and g[2] == 28]
            print(f"   g(16) = {matching[0][1][16]}")
        else:
            print("📊 Configs have different g(1), g(2) than example (8, 28)")
            print(f"   Best candidate g(16) = {fast_configs[0][1][16]}")

        return fast_configs

    else:
        print("No fast-computing configs found.")
        print("All configs become intractable (>10s per day) before g(16).")
        return None


def main():
    print("="*80)
    print("Problem 957: Rapid Config Search")
    print("="*80)
    print()

    reds = [Point(0, 0), Point(4, 0), Point(2, 3)]

    print(f"Fixed reds: {reds}")
    print()

    result = rapid_search(reds, max_configs=3000, max_seconds_per_day=10.0)

    if result:
        print()
        print("="*80)
        print(f"SUCCESS: Found {len(result)} tractable configuration(s)")
        print("="*80)
    else:
        print()
        print("No tractable configurations found with 10s timeout.")
        print("May need to expand search space or increase timeout.")


if __name__ == "__main__":
    main()
