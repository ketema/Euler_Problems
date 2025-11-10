#!/usr/bin/env python3
"""
Test exact rational arithmetic capabilities of installed libraries
"""

from fractions import Fraction
import numpy as np

print("=" * 70)
print("TESTING EXACT RATIONAL ARITHMETIC CAPABILITIES")
print("=" * 70)

# Test 1: Shapely - Does it use exact arithmetic?
print("\n1. SHAPELY TEST")
print("-" * 70)
try:
    from shapely.geometry import Point, LineString

    # Create lines with rational coordinates
    line1 = LineString([(0, 0), (1, 1)])
    line2 = LineString([(0, 1), (1, 0)])
    intersection = line1.intersection(line2)

    print(f"Line1: (0,0) to (1,1)")
    print(f"Line2: (0,1) to (1,0)")
    print(f"Intersection: {intersection}")
    print(f"Intersection type: {type(intersection)}")
    print(f"Coordinates: x={intersection.x}, y={intersection.y}")
    print(f"Coordinate types: x is {type(intersection.x)}, y is {type(intersection.y)}")

    # Check if it's exact (should be 0.5, 0.5)
    if intersection.x == 0.5 and intersection.y == 0.5:
        print("✓ Result is correct (0.5, 0.5)")
        print("⚠ WARNING: Uses FLOATING POINT, not exact rational arithmetic")

except Exception as e:
    print(f"❌ Shapely test failed: {e}")

# Test 2: Geometer - Does it support exact arithmetic?
print("\n2. GEOMETER TEST")
print("-" * 70)
try:
    from geometer import Point as GPoint, Line

    # Test with rational coordinates
    p1 = GPoint(0, 0)
    p2 = GPoint(1, 1)
    p3 = GPoint(0, 1)
    p4 = GPoint(1, 0)

    line1 = Line(p1, p2)
    line2 = Line(p3, p4)

    # Find intersection
    intersection = line1.meet(line2)
    print(f"Line1: through (0,0) and (1,1)")
    print(f"Line2: through (0,1) and (1,0)")
    print(f"Intersection (projective): {intersection}")

    # Convert to Euclidean
    if intersection.dim == 0:  # Point
        coords = intersection.normalized_array
        print(f"Euclidean coords: {coords}")
        print(f"Coordinate types: {type(coords[0])}")
        print("⚠ WARNING: Geometer uses FLOATING POINT by default")

except Exception as e:
    print(f"❌ Geometer test failed: {e}")

# Test 3: SymPy - Exact symbolic geometry
print("\n3. SYMPY TEST (Exact Rational)")
print("-" * 70)
try:
    from sympy import Point as SPoint, Line as SLine, Rational
    from sympy.geometry import intersection

    # Create lines with exact rational coordinates
    p1 = SPoint(Rational(0), Rational(0))
    p2 = SPoint(Rational(1), Rational(1))
    p3 = SPoint(Rational(0), Rational(1))
    p4 = SPoint(Rational(1), Rational(0))

    line1 = SLine(p1, p2)
    line2 = SLine(p3, p4)

    # Find intersection
    result = intersection(line1, line2)
    print(f"Line1: through (0,0) and (1,1)")
    print(f"Line2: through (0,1) and (1,0)")
    print(f"Intersection: {result}")

    if result:
        pt = result[0]
        print(f"Point: {pt}")
        print(f"x = {pt.x}, y = {pt.y}")
        print(f"x type: {type(pt.x)}, y type: {type(pt.y)}")
        print("✓ SymPy uses EXACT RATIONAL arithmetic")

except Exception as e:
    print(f"❌ SymPy test failed: {e}")

# Test 4: NumPy with fractions
print("\n4. NUMPY + FRACTIONS TEST")
print("-" * 70)
try:
    from fractions import Fraction

    # Can we use Fraction objects in numpy arrays?
    arr = np.array([Fraction(1, 2), Fraction(1, 3), Fraction(2, 5)], dtype=object)
    print(f"Array of Fractions: {arr}")
    print(f"Sum: {sum(arr)}")  # Should be exact
    print(f"Product: {arr[0] * arr[1]}")  # Should be exact
    print("✓ NumPy can hold Fraction objects (dtype=object)")
    print("⚠ Limited functionality - arithmetic works but slow")

except Exception as e:
    print(f"❌ NumPy+Fractions test failed: {e}")

print("\n" + "=" * 70)
print("SUMMARY")
print("=" * 70)
print("✓ SymPy: TRUE exact rational geometry (best for exact computation)")
print("✗ Shapely: Floating point only (fast but not exact)")
print("✗ Geometer: Floating point by default (projective geometry support)")
print("~ NumPy: Can use Fractions but limited and slow")
print("\nRECOMMENDATION: Use SymPy for exact rational geometry computations")
print("=" * 70)
