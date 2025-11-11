#!/usr/bin/env python3
"""
Validation Test: SymPy Point Auto-Normalization

Recommended by AI Panel to verify that Point objects properly handle
canonicalization in sets, avoiding the deduplication bug.

Expected behavior:
- Point(Rational(2,4), Rational(3,6)) should equal Point(Rational(1,2), Rational(1,2))
- Points with equivalent rational coordinates should deduplicate in sets
- Point.__hash__ should return same value for equivalent points
"""

from sympy import Point, Rational

print("="*70)
print("VALIDATION TEST: SymPy Point Auto-Normalization")
print("="*70)
print()

# Test 1: Equality with non-canonical rationals
print("TEST 1: Point Equality with Non-Canonical Rationals")
print("-"*70)

p1 = Point(Rational(2, 4), Rational(3, 6))  # Non-canonical
p2 = Point(Rational(1, 2), Rational(1, 2))  # Canonical
p3 = Point(Rational(4, 8), Rational(5, 10)) # Non-canonical, same values

print(f"p1 = Point(Rational(2,4), Rational(3,6)) = {p1}")
print(f"p2 = Point(Rational(1,2), Rational(1,2)) = {p2}")
print(f"p3 = Point(Rational(4,8), Rational(5,10)) = {p3}")
print()

print(f"p1 == p2: {p1 == p2} (expected: True)")
print(f"p1 == p3: {p1 == p3} (expected: True)")
print(f"p2 == p3: {p2 == p3} (expected: True)")
print()

if p1 == p2 and p1 == p3 and p2 == p3:
    print("✓ TEST 1 PASSED: Point.__eq__ properly normalizes rationals")
else:
    print("✗ TEST 1 FAILED: Point equality doesn't normalize!")
    exit(1)

print()

# Test 2: Hash consistency
print("TEST 2: Point Hashing Consistency")
print("-"*70)

hash1 = hash(p1)
hash2 = hash(p2)
hash3 = hash(p3)

print(f"hash(p1) = {hash1}")
print(f"hash(p2) = {hash2}")
print(f"hash(p3) = {hash3}")
print()

print(f"hash(p1) == hash(p2): {hash1 == hash2} (expected: True)")
print(f"hash(p1) == hash(p3): {hash1 == hash3} (expected: True)")
print()

if hash1 == hash2 and hash1 == hash3:
    print("✓ TEST 2 PASSED: Point.__hash__ consistent for equivalent points")
else:
    print("✗ TEST 2 FAILED: Point hashing inconsistent!")
    exit(1)

print()

# Test 3: Set deduplication
print("TEST 3: Set Deduplication")
print("-"*70)

point_set = set()
point_set.add(p1)
point_set.add(p2)
point_set.add(p3)

print(f"Added p1, p2, p3 to set")
print(f"Set size: {len(point_set)} (expected: 1)")
print(f"Set contents: {point_set}")
print()

if len(point_set) == 1:
    print("✓ TEST 3 PASSED: Set properly deduplicates equivalent Points")
else:
    print("✗ TEST 3 FAILED: Set contains duplicates!")
    exit(1)

print()

# Test 4: Realistic geometry scenario
print("TEST 4: Realistic Geometry Scenario")
print("-"*70)

# Simulate line intersection that produces non-canonical rational
from sympy import Line

# Lines that intersect at (1/2, 1/2)
l1 = Line(Point(0, 0), Point(1, 1))  # y = x
l2 = Line(Point(0, 1), Point(1, 0))  # y = -x + 1

intersection_point = l1.intersection(l2)[0]
print(f"Intersection of y=x and y=-x+1: {intersection_point}")
print(f"Expected: Point(1/2, 1/2)")
print()

canonical = Point(Rational(1, 2), Rational(1, 2))
print(f"intersection_point == Point(1/2, 1/2): {intersection_point == canonical}")
print()

if intersection_point == canonical:
    print("✓ TEST 4 PASSED: Line intersections produce properly normalized Points")
else:
    print("✗ TEST 4 FAILED: Intersection point not normalized!")
    exit(1)

print()

# Test 5: Set membership with computed points
print("TEST 5: Set Membership with Computed Points")
print("-"*70)

existing_points = {Point(Rational(1, 2), Rational(1, 2))}
computed_point = intersection_point

print(f"Existing set: {existing_points}")
print(f"Computed point: {computed_point}")
print(f"computed_point in existing_points: {computed_point in existing_points} (expected: True)")
print()

if computed_point in existing_points:
    print("✓ TEST 5 PASSED: Set membership works correctly with computed Points")
else:
    print("✗ TEST 5 FAILED: Set membership check failed!")
    exit(1)

print()

# Summary
print("="*70)
print("ALL VALIDATION TESTS PASSED ✓")
print("="*70)
print()
print("Summary:")
print("• Point.__eq__ properly normalizes rational coordinates")
print("• Point.__hash__ is consistent for equivalent points")
print("• set() deduplication works correctly with Point objects")
print("• Line intersections produce properly normalized Points")
print("• Set membership checks work with computed Points")
print()
print("CONCLUSION: Using Point objects directly in sets is correct.")
print("The previous point_to_tuple() approach was introducing bugs.")
print()
