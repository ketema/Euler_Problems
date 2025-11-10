#!/usr/bin/env python3
"""
Continue simulation to find actual pattern.
Use integer coordinates to avoid overflow at higher iterations.
"""

from math import gcd as math_gcd
from itertools import combinations

class IntPoint:
    """Point with integer homogeneous coords [x:y:z]."""
    def __init__(self, x, y, z):
        # Normalize by GCD
        g = math_gcd(math_gcd(abs(x), abs(y)), abs(z))
        if g > 0:
            x, y, z = x//g, y//g, z//g

        # Canonical form: first non-zero positive
        if x < 0 or (x == 0 and y < 0) or (x == 0 and y == 0 and z < 0):
            x, y, z = -x, -y, -z

        self.x, self.y, self.z = x, y, z

    def __eq__(self, other):
        # Check if proportional
        return (self.x * other.y == self.y * other.x and
                self.y * other.z == self.z * other.y and
                self.x * other.z == self.z * other.x)

    def __hash__(self):
        return hash((self.x, self.y, self.z))

    def __repr__(self):
        return f"[{self.x}:{self.y}:{self.z}]"


class IntLine:
    """Line [a:b:c]."""
    def __init__(self, p1, p2):
        # Cross product
        a = p1.y * p2.z - p1.z * p2.y
        b = p1.z * p2.x - p1.x * p2.z
        c = p1.x * p2.y - p1.y * p2.x

        # Normalize
        g = math_gcd(math_gcd(abs(a), abs(b)), abs(c))
        if g > 0:
            a, b, c = a//g, b//g, c//g

        if a < 0 or (a == 0 and b < 0) or (a == 0 and b == 0 and c < 0):
            a, b, c = -a, -b, -c

        self.a, self.b, self.c = a, b, c

    def intersect(self, other):
        x = self.b * other.c - self.c * other.b
        y = self.c * other.a - self.a * other.c
        z = self.a * other.b - self.b * other.a

        if x == 0 and y == 0 and z == 0:
            return None

        return IntPoint(x, y, z)

    def __eq__(self, other):
        return (self.a * other.b == self.b * other.a and
                self.b * other.c == self.c * other.b and
                self.a * other.c == self.c * other.a)

    def __hash__(self):
        return hash((self.a, self.b, self.c))


def simulate_day(reds, blues):
    lines = set()
    for r in reds:
        for b in blues:
            try:
                lines.add(IntLine(r, b))
            except:
                pass

    new_blues = set(blues)
    all_colored = reds | blues

    for l1, l2 in combinations(lines, 2):
        pt = l1.intersect(l2)
        if pt and pt not in all_colored:
            new_blues.add(pt)

    return new_blues, len(lines)


# Start with integer coords in general position
reds = {
    IntPoint(0, 0, 1),
    IntPoint(1, 0, 1),
    IntPoint(0, 1, 1)
}

blues = {
    IntPoint(1, 1, 1),
    IntPoint(2, 3, 1)
}

print("Computing sequence with integer arithmetic:")
print(f"g(0) = {len(blues)}")

seq = [len(blues)]
lines_count = []

for day in range(1, 11):
    blues, num_lines = simulate_day(reds, blues)
    seq.append(len(blues))
    lines_count.append(num_lines)

    print(f"g({day}) = {len(blues):>10} (from {num_lines} distinct lines)")

    if len(blues) > 500000:
        print(f"\nStopping - too many points")
        break

print()
print("Sequence:", seq)
print()
print("Now looking for recurrence pattern...")

# Check various patterns
if len(seq) >= 5:
    print("\nChecking if new_blues(n) follows a pattern:")
    for i in range(1, len(seq)):
        new = seq[i] - seq[i-1]
        B_prev = seq[i-1]
        L = lines_count[i-1] if i <= len(lines_count) else 0
        print(f"Day {i}: new={new:>8}, B={B_prev:>6}, L={L:>4}")
