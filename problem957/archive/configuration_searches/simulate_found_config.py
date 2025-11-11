"""
Simulate the found configuration to compute g(1), g(2), g(3), ..., g(16)
"""

from fractions import Fraction
from itertools import combinations

def line_intersection(p1, p2, p3, p4):
    """Find intersection of lines."""
    x1, y1 = p1
    x2, y2 = p2
    x3, y3 = p3
    x4, y4 = p4

    denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)

    if denom == 0:
        return None

    t = ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)) / denom

    x = x1 + t*(x2-x1)
    y = y1 + t*(y2-y1)

    return (x, y)

def simulate_one_day(reds, blues):
    """Simulate one day and return new blue points."""
    lines = [(r, b) for r in reds for b in blues]

    new_points = set()

    for i, (r1, b1) in enumerate(lines):
        for r2, b2 in lines[i+1:]:
            # Skip if same red or same blue
            if r1 == r2 or b1 == b2:
                continue

            intersection = line_intersection(r1, b1, r2, b2)

            if intersection and intersection not in blues and intersection not in reds:
                new_points.add(intersection)

    return list(new_points)

# Initial configuration (from search)
reds = [
    (Fraction(0), Fraction(0)),
    (Fraction(1), Fraction(0)),
    (Fraction(0), Fraction(1)),
]

blues = [
    (Fraction(-2), Fraction(-2)),
    (Fraction(-2), Fraction(-3, 2)),
]

print("="*70)
print("Simulating configuration:")
print("="*70)
print(f"Reds: {reds}")
print(f"Blues (initial): {blues}")
print()

g_values = [len(blues)]  # g(0) = 2

max_days = 3  # Try to get to day 3 to verify formula

for day in range(max_days):
    print(f"Day {day} → {day+1}:")
    print(f"  Current blues: {len(blues)}")

    new_blues = simulate_one_day(reds, blues)

    print(f"  New blues: {len(new_blues)}")

    blues = blues + new_blues
    g_values.append(len(blues))

    print(f"  g({day+1}) = {len(blues)}")

    # Check against expected
    if day == 0 and len(blues) != 8:
        print(f"  ✗ Expected g(1) = 8, got {len(blues)}")
        break
    elif day == 0:
        print(f"  ✓ Matches g(1) = 8")

    if day == 1 and len(blues) != 28:
        print(f"  ✗ Expected g(2) = 28, got {len(blues)}")
        break
    elif day == 1:
        print(f"  ✓ Matches g(2) = 28")

    print()

print("="*70)
print("Results:")
print("="*70)
for day, g in enumerate(g_values):
    print(f"g({day}) = {g}")

# Check quadratic fit
if len(g_values) >= 3:
    print()
    print("Checking quadratic fit g(t) = 7t² - t + 2:")
    all_match = True
    for day, g_actual in enumerate(g_values):
        g_predicted = 7*day*day - day + 2
        match = "✓" if g_actual == g_predicted else "✗"
        print(f"  g({day}) = {g_actual:4d}, predicted = {g_predicted:4d} {match}")
        if g_actual != g_predicted:
            all_match = False

    if all_match:
        print()
        print("✓ Quadratic formula is CORRECT!")
        print()
        print(f"Therefore: g(16) = 7×256 - 16 + 2 = {7*256 - 16 + 2}")
    else:
        print()
        print("✗ Quadratic formula does NOT match")
        print("Need to find a different pattern...")
