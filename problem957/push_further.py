#!/usr/bin/env python3
"""
Optimize simulation to push further. Use integer arithmetic only, aggressive pruning.
"""

from math import gcd as math_gcd
from itertools import combinations

class P:
    """Minimal point representation."""
    __slots__ = ('x', 'y', 'z')

    def __init__(self, x, y, z):
        g = math_gcd(math_gcd(abs(x), abs(y)), abs(z))
        if g > 1:
            x, y, z = x//g, y//g, z//g
        if x < 0 or (x == 0 and y < 0) or (x == 0 and y == 0 and z < 0):
            x, y, z = -x, -y, -z
        self.x, self.y, self.z = x, y, z

    def __eq__(self, o):
        return self.x*o.y == self.y*o.x and self.y*o.z == self.z*o.y and self.x*o.z == self.z*o.x

    def __hash__(self):
        return hash((self.x, self.y, self.z))


class L:
    """Minimal line representation."""
    __slots__ = ('a', 'b', 'c')

    def __init__(self, p1, p2):
        a = p1.y*p2.z - p1.z*p2.y
        b = p1.z*p2.x - p1.x*p2.z
        c = p1.x*p2.y - p1.y*p2.x
        g = math_gcd(math_gcd(abs(a), abs(b)), abs(c))
        if g > 1:
            a, b, c = a//g, b//g, c//g
        if a < 0 or (a == 0 and b < 0) or (a == 0 and b == 0 and c < 0):
            a, b, c = -a, -b, -c
        self.a, self.b, self.c = a, b, c

    def meet(self, o):
        x = self.b*o.c - self.c*o.b
        y = self.c*o.a - self.a*o.c
        z = self.a*o.b - self.b*o.a
        if x == 0 and y == 0 and z == 0:
            return None
        return P(x, y, z)

    def __eq__(self, o):
        return self.a*o.b == self.b*o.a and self.b*o.c == self.c*o.b and self.a*o.c == self.c*o.a

    def __hash__(self):
        return hash((self.a, self.b, self.c))


reds = {P(0,0,1), P(1,0,1), P(0,1,1)}
blues = {P(1,1,1), P(2,3,1)}

seq = [len(blues)]
print(f"g(0) = {seq[0]}")

for day in range(1, 12):
    lines = set()
    for r in reds:
        for b in blues:
            lines.add(L(r, b))

    new_blues = set(blues)
    all_col = reds | blues

    for l1, l2 in combinations(lines, 2):
        pt = l1.meet(l2)
        if pt and pt not in all_col:
            new_blues.add(pt)

    blues = new_blues
    seq.append(len(blues))

    print(f"g({day}) = {seq[day]} (from {len(lines)} lines)")

    if len(blues) > 10000000:
        print(f"Stopping - {len(blues)} points too large")
        break

print()
print(f"Computed sequence: {seq}")
