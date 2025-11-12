#!/usr/bin/env sage

"""
Optimized solver for PE Problem 957
Uses incremental line generation to avoid O(n^4) complexity
"""

import time
from sage.all import *

def line_intersection(p1, p2, p3, p4):
    """Compute intersection of line (p1,p2) with line (p3,p4)"""
    d1 = p2 - p1
    d2 = p4 - p3
    det = d1[0]*(-d2[1]) - d1[1]*(-d2[0])
    if det == 0:
        return None  # Parallel or coincident
    rhs = p3 - p1
    t = (rhs[0]*(-d2[1]) - rhs[1]*(-d2[0])) / det
    return p1 + t*d1

def compute_to_day_n_optimized(reds, initial_blues, n, verbose=True):
    """
    Optimized version: Only compute intersections involving NEW lines from previous day

    Key insight: On day k, only blues added on day k-1 create NEW lines.
    We only need to intersect these new lines with ALL existing lines.

    OLD approach: C(3m_k, 2) checks where m_k = total blues
    NEW approach: (3 * new_blues) * (3 * m_k) checks

    For day 6: OLD = C(57204, 2) = 1.6B checks
               NEW = (3 * ~20K) * 57204 = 3.4M checks (~500x speedup!)
    """
    blues = list(initial_blues)
    all_points = set([tuple(r) for r in reds] + [tuple(b) for b in blues])

    # Track which blues were added each day for incremental processing
    blues_by_day = {0: list(initial_blues)}

    for day in range(1, n+1):
        start_time = time.time()

        # Previous day's blues (these create the new lines)
        prev_day_blues = blues_by_day.get(day-1, [])

        if not prev_day_blues:
            if verbose:
                print(f"Day {day}: No new blues from day {day-1}, stopping")
            break

        # Generate NEW lines (only involving previous day's blues)
        new_lines = []
        for b in prev_day_blues:
            for r in reds:
                new_lines.append((r, b))
            # Also lines between new blues and ALL existing blues (not just new)
            for old_b in blues:
                if tuple(b) != tuple(old_b):  # Don't create line to self
                    new_lines.append((b, old_b))

        # ALL existing lines (for intersection checking)
        all_lines = []
        for r in reds:
            for b in blues:
                all_lines.append((r, b))

        # Find intersections: new lines × all lines
        new_points = set()
        for (p1, p2) in new_lines:
            for (p3, p4) in all_lines:
                # Skip if same line
                if (tuple(p1) == tuple(p3) and tuple(p2) == tuple(p4)) or \
                   (tuple(p1) == tuple(p4) and tuple(p2) == tuple(p3)):
                    continue

                pt = line_intersection(p1, p2, p3, p4)
                if pt is not None:
                    pt_tuple = tuple(pt)
                    if pt_tuple not in all_points:
                        new_points.add(pt_tuple)

        # Add new blues
        new_blues_list = [vector(QQ, pt) for pt in new_points]
        blues_by_day[day] = new_blues_list
        blues.extend(new_blues_list)
        all_points.update(new_points)

        elapsed = time.time() - start_time

        if verbose:
            print(f"Day {day}: g({day}) = {len(blues)} (+{len(new_blues_list)} new, {elapsed:.1f}s)")
            print(f"  Checked {len(new_lines)} new lines × {len(all_lines)} existing = {len(new_lines)*len(all_lines)} intersections")

    return len(blues)

if __name__ == "__main__":
    # Configuration
    R1 = vector(QQ, [0, 0])
    R2 = vector(QQ, [1, 0])
    R3 = vector(QQ, [0, 1])
    B1 = vector(QQ, [1, 1])
    B2 = vector(QQ, [3, 9])

    reds = [R1, R2, R3]
    initial_blues = [B1, B2]

    print("="*70)
    print("OPTIMIZED SOLVER - INCREMENTAL LINE GENERATION")
    print("="*70)
    print()

    # Test on known values first
    for n in [1, 2, 3, 4, 5]:
        result = compute_to_day_n_optimized(reds, initial_blues, n, verbose=False)
        print(f"g({n}) = {result}")

    print()
    print("Now attempting g(6) with optimization:")
    print()

    result = compute_to_day_n_optimized(reds, initial_blues, 6, verbose=True)

    print()
    print("="*70)
    print(f"RESULT: g(6) = {result}")
    print("="*70)
