"""
Exhaustive search through small rational coordinates
Find configuration giving g(1)=8, g(2)=28
"""

from fractions import Fraction
from itertools import product, combinations

def line_intersection(p1, p2, p3, p4):
    x1, y1 = p1
    x2, y2 = p2
    x3, y3 = p3
    x4, y4 = p4
    denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
    if denom == 0:
        return None
    t = ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)) / denom
    return (x1 + t*(x2-x1), y1 + t*(y2-y1))

def simulate_days(reds, blues, max_days=3):
    g = [len(blues)]
    for day in range(max_days):
        lines = [(r, b) for r in reds for b in blues]
        new = set()
        for i, (r1, b1) in enumerate(lines):
            for r2, b2 in lines[i+1:]:
                if r1 == r2 or b1 == b2:
                    continue
                pt = line_intersection(r1, b1, r2, b2)
                if pt and pt not in blues and pt not in reds:
                    new.add(pt)
        if len(new) == 0:
            return None
        blues = blues + list(new)
        g.append(len(blues))
        if len(blues) > 100:  # Explodes
            return None
    return g

# Fix reds at simple positions
reds_fixed = [
    (Fraction(0), Fraction(0)),
    (Fraction(1), Fraction(0)),
    (Fraction(0), Fraction(1)),
]

# Search blues in small rational coordinates
coords = [Fraction(i, 4) for i in range(-16, 17)]  # -4 to 4 in quarters

print("Searching through rational coordinates...")
print(f"Testing {len(coords)} x {len(coords)} = {len(coords)**4} combinations...")
print()

found = []
tested = 0

for x1, y1, x2, y2 in product(coords, repeat=4):
    blues = [(x1, y1), (x2, y2)]

    # Skip invalid
    if blues[0] == blues[1]:
        continue
    if blues[0] in reds_fixed or blues[1] in reds_fixed:
        continue

    tested += 1
    if tested % 100000 == 0:
        print(f"  Tested {tested:,} configurations, found {len(found)}...")

    g = simulate_days(reds_fixed, blues, max_days=3)

    if g and len(g) >= 3 and g[1] == 8 and g[2] == 28:
        print(f"\n✓✓✓ FOUND: Blues at {blues[0]}, {blues[1]}")
        print(f"    g = {g}")
        found.append((blues, g))

        if len(found) >= 5:
            print("\n  Found 5 configs, stopping search...")
            break

print(f"\n{'='*70}")
print(f"Search complete: tested {tested:,} configurations")
print(f"Found {len(found)} matching configurations")
print(f"{'='*70}")

if found:
    print("\nAnalyzing found configurations:")
    for blues, g in found:
        print(f"\n  Blues: {blues}")
        print(f"  g = {g}")

        if len(g) >= 4:
            # Estimate g(16)
            ratios = [g[i]/g[i-1] for i in range(2, len(g))]
            avg_ratio = sum(ratios) / len(ratios)
            est_g16 = g[2] * (avg_ratio ** 14)
            print(f"  Avg growth: {avg_ratio:.3f}x")
            print(f"  Est g(16): ~{est_g16:.1e}")

            if est_g16 < 1e5:
                print(f"  ✓ TRACTABLE!")
else:
    print("\n✗ No configurations found")
    print("\nMaybe the configuration has:")
    print("  - Irrational coordinates (√2, √3, etc.)?")
    print("  - Larger integer coordinates?")
    print("  - Different red positions?")
