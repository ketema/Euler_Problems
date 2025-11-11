"""
Analyze the initial configuration to discover special structure.

Initial points:
r₁ = (0, 0)
r₂ = (1, 0)
r₃ = (0, 1)
b₁ = (1, 1)
b₂ = (3, 2)

Question: Do these 5 points lie on a conic? If so, what kind?
"""

from fractions import Fraction

points = [
    (0, 0, "r1"),
    (1, 0, "r2"),
    (0, 1, "r3"),
    (1, 1, "b1"),
    (3, 2, "b2"),
]

# A conic in ℝ² has equation: ax² + bxy + cy² + dx + ey + f = 0
# 5 points determine it uniquely (up to scaling)

# Set up system of equations
print("Testing if 5 points lie on a conic: ax² + bxy + cy² + dx + ey + f = 0\n")

equations = []
for (x, y, label) in points:
    # ax² + bxy + cy² + dx + ey + f = 0
    coeffs = [x*x, x*y, y*y, x, y, 1]
    equations.append((coeffs, label))
    print(f"{label} = ({x},{y}): {x*x}a + {x*y}b + {y*y}c + {x}d + {y}e + f = 0")

print("\nSolving:")
print("(0,0): f = 0")
print("(1,0): a + d + f = 0  →  a + d = 0  →  d = -a")
print("(0,1): c + e + f = 0  →  c + e = 0  →  e = -c")
print("(1,1): a + b + c + d + e + f = 0")
print("       → a + b + c - a - c = 0  →  b = 0")
print("(3,2): 9a + 6b + 4c + 3d + 2e + f = 0")
print("       → 9a + 0 + 4c - 3a - 2c = 0")
print("       → 6a + 2c = 0  →  c = -3a")

print("\n" + "="*70)
print("SOLUTION (choosing a=1 for normalization):")
print("a = 1, b = 0, c = -3, d = -1, e = 3, f = 0")
print("\nConic equation: x² - 3y² - x + 3y = 0")
print("  or: x(x-1) - 3y(y-1) = 0")
print("  or: x(x-1) = 3y(y-1)")
print("="*70)

# Verify all points satisfy this
a, b, c, d, e, f = 1, 0, -3, -1, 3, 0

print("\nVerification:")
for (x, y, label) in points:
    result = a*x*x + b*x*y + c*y*y + d*x + e*y + f
    check = "✓" if result == 0 else "✗"
    print(f"{label} = ({x},{y}): {result} {check}")

# Canonical form of conic
print("\n" + "="*70)
print("CANONICAL FORM:")
print("x² - 3y² - x + 3y = 0")
print("x² - x - 3y² + 3y = 0")
print("(x² - x + 1/4) - 3(y² - y + 1/4) = 1/4 - 3/4")
print("(x - 1/2)² - 3(y - 1/2)² = -1/2")
print("\nMultiply by -2:")
print("3(y - 1/2)² - (x - 1/2)² = 1")
print("\nDivide:")
print("(y - 1/2)²/(1/3) - (x - 1/2)²/1 = 1")
print("="*70)

print("\n" + "="*70)
print("INTERPRETATION:")
print("This is a HYPERBOLA with:")
print("  • Center: (1/2, 1/2)")
print("  • Vertical transverse axis (opens up/down)")
print("  • a² = 1/3,  b² = 1")
print("  • c² = a² + b² = 1/3 + 1 = 4/3")
print("  • c = 2/√3 ≈ 1.155")
print("  • Foci at (1/2, 1/2 ± 2/√3)")
print("="*70)

import math
c_val = math.sqrt(4/3)
focus1 = (0.5, 0.5 + c_val)
focus2 = (0.5, 0.5 - c_val)
print(f"\nFoci (approximate):")
print(f"  F₁ ≈ {focus1}")
print(f"  F₂ ≈ {focus2}")

print("\n" + "="*70)
print("KEY DISCOVERY:")
print("All 5 initial points lie on a HYPERBOLA!")
print("\nThis explains the rich Pappus structure:")
print("  • Pascal's theorem applies to 6 points on a conic")
print("  • We have 5 points, so 'almost Pascal'")
print("  • New intersections may also lie on the conic")
print("  • Creates cascading collinearities")
print("="*70)

# Check asymptotes
print("\n" + "="*70)
print("ASYMPTOTES:")
print("For hyperbola (y-k)²/a² - (x-h)²/b² = 1:")
print("  y - k = ±(a/b)(x - h)")
print(f"\nHere: a² = 1/3, b² = 1, so a/b = 1/√3")
print(f"  y - 1/2 = ±(1/√3)(x - 1/2)")
print(f"  y = 1/2 ± (x - 1/2)/√3")
print("\nAsymptote 1: y = 1/2 + (x-1/2)/√3")
print(f"             y ≈ {0.5 + 0.5/math.sqrt(3)} + {1/math.sqrt(3)}x")
print("Asymptote 2: y = 1/2 - (x-1/2)/√3")
print(f"             y ≈ {0.5 - 0.5/math.sqrt(3)} - {1/math.sqrt(3)}x")
print("="*70)

# Test if new points might lie on conic
print("\n" + "="*70)
print("TESTING: Do day-1 intersection points lie on the conic?")
print("="*70)

# Some day-1 lines and their intersections
# Line through r1=(0,0) and b1=(1,1): y = x
# Line through r2=(1,0) and b1=(1,1): x = 1
# Intersection: (1, 1) = b1 (already know it's on conic)

# Line through r1=(0,0) and b1=(1,1): y = x
# Line through r3=(0,1) and b2=(3,2): slope = (2-1)/(3-0) = 1/3, through (0,1) → y = x/3 + 1
# Intersection: x = x/3 + 1 → 2x/3 = 1 → x = 3/2, y = 3/2
test_point = (Fraction(3, 2), Fraction(3, 2))
x, y = test_point
result = x*(x-1) - 3*y*(y-1)
print(f"\nIntersection of ℓ(r1,b1) and ℓ(r3,b2): {test_point}")
print(f"  Conic test: {float(x)}({float(x)}-1) - 3·{float(y)}({float(y)}-1) = {float(result)}")
if result == 0:
    print("  ✓ This point LIES ON THE CONIC!")
else:
    print(f"  ✗ Not on conic (error = {float(result)})")

# Another test: line through r2=(1,0) and b2=(3,2)
# Slope = (2-0)/(3-1) = 1, through (1,0) → y = x - 1
# Line through r3=(0,1) and b1=(1,1): y = 1
# Intersection: 1 = x - 1 → x = 2, y = 1
test_point2 = (2, 1)
x, y = test_point2
result2 = x*(x-1) - 3*y*(y-1)
print(f"\nIntersection of ℓ(r2,b2) and ℓ(r3,b1): {test_point2}")
print(f"  Conic test: {x}({x}-1) - 3·{y}({y}-1) = {result2}")
if result2 == 0:
    print("  ✓ This point LIES ON THE CONIC!")
else:
    print(f"  ✗ Not on conic (error = {result2})")

print("\n" + "="*70)
print("CONCLUSION:")
print("The puzzle starts with 5 points on a hyperbola.")
print("This is the 'special structure' that makes g(16) tractable!")
print("="*70)
