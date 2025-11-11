"""
Compute more terms to find the pattern
"""

from fractions import Fraction
from itertools import combinations

def line_intersection(p1, p2, p3, p4):
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
    lines = [(r, b) for r in reds for b in blues]
    new_points = set()

    for i, (r1, b1) in enumerate(lines):
        for r2, b2 in lines[i+1:]:
            if r1 == r2 or b1 == b2:
                continue

            intersection = line_intersection(r1, b1, r2, b2)

            if intersection and intersection not in blues and intersection not in reds:
                new_points.add(intersection)

    return list(new_points)

reds = [
    (Fraction(0), Fraction(0)),
    (Fraction(1), Fraction(0)),
    (Fraction(0), Fraction(1)),
]

blues = [
    (Fraction(-2), Fraction(-2)),
    (Fraction(-2), Fraction(-3, 2)),
]

g_values = [len(blues)]

print("Computing g(t) for t = 0 to 5...")
print()

for day in range(5):
    print(f"Day {day}: g({day}) = {len(blues)}", end="")

    new_blues = simulate_one_day(reds, blues)

    if len(new_blues) > 0:
        blues = blues + new_blues
        g_values.append(len(blues))
        print(f" → adding {len(new_blues)} → g({day+1}) = {len(blues)}")
    else:
        print(f" → no new blues, stopping")
        break

    # Stop if too many points (will take too long)
    if len(blues) > 300:
        print(f"  (stopping early, too many points)")
        break

print()
print("="*70)
print("Sequence:")
print("="*70)
for t, g in enumerate(g_values):
    print(f"g({t}) = {g}")

print()
print("Differences:")
for t in range(len(g_values) - 1):
    m = g_values[t+1] - g_values[t]
    print(f"m({t}) = g({t+1}) - g({t}) = {m}")

print()
print("Ratios:")
for t in range(len(g_values) - 1):
    m = g_values[t+1] - g_values[t]
    if t > 0:
        m_prev = g_values[t] - g_values[t-1]
        ratio = m / m_prev if m_prev > 0 else 0
        print(f"m({t})/m({t-1}) = {m}/{m_prev} = {ratio:.3f}")

print()
print("Looking for pattern...")

# Check if it's a recurrence
if len(g_values) >= 4:
    # Try g(n) = a*g(n-1) + b*g(n-2) + c
    # g(2) = a*g(1) + b*g(0) + c
    # g(3) = a*g(2) + b*g(1) + c

    # 28 = 8a + 2b + c
    # 183 = 28a + 8b + c

    # Subtract: 155 = 20a + 6b
    # So: 155 = 20a + 6b

    # Need another equation. If g(4) is available:
    if len(g_values) >= 5:
        # g(4) = a*g(3) + b*g(2) + c
        g4 = g_values[4]
        # g4 = 183a + 28b + c

        print(f"Trying linear recurrence g(n) = a·g(n-1) + b·g(n-2) + c")
        print(f"  28 = 8a + 2b + c")
        print(f"  183 = 28a + 8b + c")
        print(f"  {g4} = 183a + 28b + c")
