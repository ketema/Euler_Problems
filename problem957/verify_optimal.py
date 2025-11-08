#!/usr/bin/env python3
"""
Verify the optimal configuration found by the optimizer.
"""

import sys
sys.path.insert(0, '/Users/kharri04/projects/my_workspace/prototypes/problem957')

from src.geometry import Point
from src.propagation import propagate_one_day


# Optimal configuration from differential evolution
red = {
    Point(-1.1420985748, -3.1278529420, 'red'),
    Point(1.7213348846, -0.8343651343, 'red'),
    Point(4.3760906863, 2.3859745813, 'red')
}

blue = {
    Point(-1.8437265624, 1.4483260402, 'blue'),
    Point(-1.0486909239, 2.1320688328, 'blue')
}

print("Optimal Configuration Verification")
print("="*60)
print(f"\nRed points ({len(red)}):")
for i, p in enumerate(sorted(red, key=lambda pt: (pt.x, pt.y)), 1):
    print(f"  R{i} = Point({p.x:.10f}, {p.y:.10f})")

print(f"\nBlue points ({len(blue)}):")
for i, p in enumerate(sorted(blue, key=lambda pt: (pt.x, pt.y)), 1):
    print(f"  B{i} = Point({p.x:.10f}, {p.y:.10f})")

print(f"\n{'='*60}")
print("Computing g(n) sequence...")
print(f"{'='*60}\n")

# Compute with detailed progress tracking
current_blue = blue
g_values = []

for day in range(1, 17):
    new_blues = propagate_one_day(red, current_blue)
    current_blue = current_blue | new_blues
    g_n = len(current_blue)
    g_values.append(g_n)

    print(f"Day {day:2}: g({day:2}) = {g_n:6}  (added {len(new_blues):4} new blues, total lines={3*len(current_blue):5})")

    # Stop if computation becomes too expensive
    if len(current_blue) > 10000:
        print(f"\n⚠ Stopping at day {day}: blue count exceeds 10,000 (computation becomes intractable)")
        break

print(f"\n{'='*60}")
if len(g_values) >= 16:
    print(f"✓ Answer: g(16) = {g_values[15]}")
else:
    print(f"⚠ Could not compute g(16) - blue count grew too large")
    print(f"  Last computed: g({len(g_values)}) = {g_values[-1]}")
print(f"{'='*60}\n")

# Print summary
print("Summary of g(n) values:")
for i, val in enumerate(g_values, 1):
    print(f"  g({i:2}) = {val:6}")
