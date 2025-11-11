"""
Debug which crossings we get
"""

from fractions import Fraction

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

# Configuration
reds = [
    (Fraction(0), Fraction(0)),
    (Fraction(1), Fraction(0)),
    (Fraction(0), Fraction(1)),
]

blues = [
    (Fraction(2), Fraction(2)),
    (Fraction(3), Fraction(1)),
]

r1, r2, r3 = reds
b1, b2 = blues

print("Testing all 6 potential crossings:")
print()

tests = [
    ((r1, b1), (r2, b2), "r1-b1 × r2-b2"),
    ((r1, b1), (r3, b2), "r1-b1 × r3-b2"),
    ((r1, b2), (r2, b1), "r1-b2 × r2-b1"),
    ((r1, b2), (r3, b1), "r1-b2 × r3-b1"),
    ((r2, b1), (r3, b2), "r2-b1 × r3-b2"),
    ((r2, b2), (r3, b1), "r2-b2 × r3-b1"),
]

for (p1, p2), (p3, p4), label in tests:
    intersection = line_intersection(p1, p2, p3, p4)
    print(f"{label}: {intersection}")

    # Check if it's an existing point
    if intersection in reds:
        print(f"  → coincides with red point!")
    elif intersection in blues:
        print(f"  → coincides with blue point!")
    print()

print("\nLet me try: 3 reds at vertices of triangle, 2 blues inside")
print("Reds: (0,0), (3,0), (0,3)")
print("Blues: (1,1), (2,1)")
print()

reds2 = [
    (Fraction(0), Fraction(0)),
    (Fraction(3), Fraction(0)),
    (Fraction(0), Fraction(3)),
]

blues2 = [
    (Fraction(1), Fraction(1)),
    (Fraction(2), Fraction(1)),
]

r1, r2, r3 = reds2
b1, b2 = blues2

new_points = set()
for (p1, p2), (p3, p4), label in tests:
    intersection = line_intersection(p1, p2, p3, p4)
    if intersection and intersection not in reds2 and intersection not in blues2:
        new_points.add(intersection)
        print(f"{label}: {intersection} ✓")
    else:
        print(f"{label}: {intersection} ✗ (existing or None)")

print(f"\nTotal new points: {len(new_points)}")
print(f"g(1) = {len(blues2) + len(new_points)}")
