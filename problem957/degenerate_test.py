"""
Test if the solution involves degenerate configurations
where multiple lines meet at points
"""

from fractions import Fraction

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

def simulate_with_detail(reds, blues, days=2):
    """Simulate and show details."""
    print(f"Reds: {reds}")
    print(f"Blues (initial): {blues}")
    print(f"g(0) = {len(blues)}")
    print()

    for day in range(days):
        print(f"Day {day} → {day+1}:")
        lines = [(r, b, f"({r},{b})") for r in reds for b in blues]

        intersections = {}  # point -> list of line pairs

        for i, (r1, b1, l1) in enumerate(lines):
            for r2, b2, l2 in lines[i+1:]:
                if r1 == r2 or b1 == b2:
                    continue

                pt = line_intersection(r1, b1, r2, b2)
                if pt:
                    if pt not in intersections:
                        intersections[pt] = []
                    intersections[pt].append((l1, l2))

        # Count new blues
        new_blues = []
        for pt, line_pairs in intersections.items():
            if pt not in blues and pt not in reds:
                new_blues.append(pt)

        print(f"  {len(new_blues)} new blues")

        # Show multiplicity
        mult_counts = {}
        for pt, line_pairs in intersections.items():
            mult = len(line_pairs)
            if mult > 1:
                mult_counts[mult] = mult_counts.get(mult, 0) + 1

        if mult_counts:
            print(f"  Points with multiple line pairs: {mult_counts}")

        blues = blues + new_blues
        print(f"  g({day+1}) = {len(blues)}")
        print()

    return len(blues)

# Test some special configurations

print("="*70)
print("TEST 1: COLLINEAR REDS (degener acy)")
print("="*70)
reds1 = [(Fraction(0), Fraction(0)), (Fraction(1), Fraction(0)), (Fraction(2), Fraction(0))]
blues1 = [(Fraction(0), Fraction(1)), (Fraction(1), Fraction(1))]
simulate_with_detail(reds1, blues1, days=2)

print("\n" + "="*70)
print("TEST 2: BLUES ON LINE THROUGH ONE RED")
print("="*70)
reds2 = [(Fraction(0), Fraction(0)), (Fraction(1), Fraction(0)), (Fraction(0), Fraction(1))]
blues2 = [(Fraction(1), Fraction(1)), (Fraction(2), Fraction(2))]  # on line through origin
simulate_with_detail(reds2, blues2, days=2)

print("\n" + "="*70)
print("TEST 3: ALL 5 COLLINEAR")
print("="*70)
reds3 = [(Fraction(0), Fraction(0)), (Fraction(1), Fraction(0)), (Fraction(2), Fraction(0))]
blues3 = [(Fraction(3), Fraction(0)), (Fraction(4), Fraction(0))]
simulate_with_detail(reds3, blues3, days=2)

print("\n" + "="*70)
print("TEST 4: REDS AT INFINITY (very spread out)")
print("="*70)
reds4 = [(Fraction(0), Fraction(0)), (Fraction(100), Fraction(0)), (Fraction(0), Fraction(100))]
blues4 = [(Fraction(1), Fraction(1)), (Fraction(2), Fraction(2))]
g4 = simulate_with_detail(reds4, blues4, days=2)
