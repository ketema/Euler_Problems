#!/usr/bin/env python3
"""
Test hypothesis: g(n) = max over all initial blue configurations

If true, then:
- My config gives one sequence: 2, 8, 28, 184, 1644, ...
- Other configs might give: 2, 8, 28, X, Y, ... where later values differ
- We need to find which config maximizes SPECIFICALLY g(16)
"""

from sympy import Point, Line, Rational
from sympy.geometry import intersection
from itertools import combinations
import time

def simulate_day(reds, blues):
    """Simulate one day and return new blues set."""
    # Draw all red-to-blue lines
    lines = []
    for r in reds:
        for b in blues:
            try:
                line = Line(r, b)
                lines.append(line)
            except:
                pass

    # Find intersections
    new_blues = set()
    for p in blues:
        new_blues.add((Rational(p.x), Rational(p.y)))

    for i, line1 in enumerate(lines):
        for line2 in lines[i+1:]:
            if line1 != line2:
                result = intersection(line1, line2)
                if result:
                    p = result[0]
                    if hasattr(p, 'x'):
                        new_blues.add((Rational(p.x), Rational(p.y)))

    return [Point(x, y) for x, y in new_blues]

# Fixed 3 reds (problem says these are fixed)
reds = [Point(0, 0), Point(4, 0), Point(2, 3)]

print("="*70)
print("TESTING MAXIMAL HYPOTHESIS")
print("="*70)
print(f"\nFixed reds: {reds}")

# Test a few different initial blue configurations
configs = [
    # My original config
    ([Point(1, 1), Point(3, 2)], "Original (1,1), (3,2)"),
    # Try blues closer together
    ([Point(1, 1), Point(1, 2)], "Close (1,1), (1,2)"),
    # Try blues farther apart
    ([Point(1, 1), Point(3, 1)], "Far (1,1), (3,1)"),
    # Try different positions
    ([Point(2, 1), Point(3, 2)], "Alt (2,1), (3,2)"),
]

results = {}

for blues_init, name in configs:
    print(f"\n{'='*70}")
    print(f"Config: {name}")
    print(f"{'='*70}")

    blues = blues_init
    sequence = [len(blues)]

    for day in range(1, 5):
        blues = simulate_day(reds, blues)
        g_n = len(blues)
        sequence.append(g_n)
        print(f"  g({day}) = {g_n}")

        if day == 1 and g_n != 8:
            print(f"    ⚠️  This config does NOT give g(1)=8!")
            break
        if day == 2 and g_n != 28:
            print(f"    ⚠️  This config does NOT give g(2)=28!")
            break

    results[name] = sequence

print(f"\n{'='*70}")
print(f"SUMMARY")
print(f"{'='*70}")

for name, seq in results.items():
    print(f"{name:30s}: {seq}")

print(f"\n{'='*70}")
print(f"CONCLUSION")
print(f"{'='*70}")

# Check if all configs that give g(1)=8, g(2)=28 also give same g(3), g(4)
valid_configs = {name: seq for name, seq in results.items() if len(seq) >= 3 and seq[1] == 8 and seq[2] == 28}

if len(valid_configs) == 0:
    print("No configs gave g(1)=8 and g(2)=28!")
elif len(valid_configs) == 1:
    print("Only ONE config gives g(1)=8 and g(2)=28!")
    print("This means the problem has a UNIQUE configuration.")
else:
    # Check if they all give same later values
    sequences = list(valid_configs.values())
    all_same = all(seq == sequences[0] for seq in sequences)

    if all_same:
        print(f"All {len(valid_configs)} configs give the SAME sequence!")
        print("This means there's a unique growth pattern.")
    else:
        print(f"Found {len(valid_configs)} configs with g(1)=8, g(2)=28")
        print("But they give DIFFERENT values for g(3), g(4)!")
        print()
        for name, seq in valid_configs.items():
            print(f"  {name}: {seq}")
        print()
        print("⚠️  This confirms the MAXIMAL HYPOTHESIS!")
        print("We need to find which config maximizes g(16) specifically!")
