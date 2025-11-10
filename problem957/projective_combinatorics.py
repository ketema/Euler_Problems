#!/usr/bin/env python3
"""
Analyze the combinatorics of projective plane point generation.

In projective plane:
- Every 2 distinct lines intersect at exactly 1 point
- We draw lines through (red, blue) pairs
- Intersections of these lines create new blues

Question: Is there a formula for g(n) based on combinatorics?
"""

def analyze_day(num_reds, num_blues_start):
    """
    Analyze what happens on a single day.

    - R red points, B blue points
    - Create R × B lines (one per red-blue pair)
    - In projective plane, C(R×B, 2) potential intersections
    - But some intersections are at existing points
    """
    num_lines = num_reds * num_blues_start

    # Maximum intersections (if all lines distinct)
    max_intersections = num_lines * (num_lines - 1) // 2

    print(f"Day with {num_reds} reds, {num_blues_start} blues:")
    print(f"  Lines created: {num_lines}")
    print(f"  Max intersections: C({num_lines},2) = {max_intersections}")
    print()

    return max_intersections


print("="*70)
print("PROJECTIVE PLANE COMBINATORICS")
print("="*70)
print()

# Day 1: Start with 3 reds, 2 blues
max_day1 = analyze_day(3, 2)
print(f"If all intersections were new: {max_day1} new blues")
print(f"Actual g(1) = 8 (so 8 - 2 = 6 new blues)")
print(f"Coincidences: {max_day1 - 6} intersections at existing points")
print()

# This shows that not all intersections create new points
# Some lines pass through existing red/blue points

print("="*70)
print("KEY INSIGHT:")
print("="*70)
print()
print("In optimal configuration, we want to MAXIMIZE new blues.")
print("This means minimizing coincidences where:")
print("  - 3+ lines pass through same point")
print("  - Lines pass through existing red points")
print()
print("In projective geometry, this relates to 'general position':")
print("  - No 3 lines concurrent (except as necessary)")
print("  - Points spread out to avoid unnecessary coincidences")
print()

# Let me think about the recurrence
print("="*70)
print("RECURRENCE RELATION")
print("="*70)
print()

print("After day n, we have B(n) blue points (and always 3 reds)")
print("On day n+1:")
print("  - Create 3 × B(n) lines")
print("  - These lines have C(3×B(n), 2) potential intersections")
print("  - Subtract coincidences to get m(n+1) new blues")
print()

# For optimal config, what are the coincidences?
print("Coincidences come from:")
print("  1. Lines passing through the 3 red points")
print("  2. Lines being identical (red-blue pairs with same line)")
print("  3. Multiple lines meeting at same white point (reduces new blues)")
print()

print("The key is finding the configuration that minimizes type 3 coincidences!")
print()

# Check if there's a formula
print("="*70)
print("SEARCHING FOR FORMULA")
print("="*70)
print()

# From the sequence: 2, 8, 28, 184, 1646, 19161
g = [2, 8, 28, 184, 1646, 19161]

print("Known values:")
for i, val in enumerate(g):
    print(f"g({i}) = {val}")

print()
print("Looking for pattern...")

# Check if it's related to powers or factorials
for i in range(1, len(g)):
    ratio = g[i] / g[i-1]
    print(f"g({i})/g({i-1}) = {ratio:.4f}")

print()

# Maybe it's related to C(3*B, 2)?
print("Checking if related to C(3*B(n-1), 2):")
for i in range(1, len(g)):
    lines = 3 * g[i-1]
    max_int = lines * (lines - 1) // 2
    print(f"Day {i}: C(3×{g[i-1]}, 2) = {max_int}, actual new = {g[i] - g[i-1]}")
    if max_int > 0:
        print(f"  Efficiency: {(g[i] - g[i-1]) / max_int * 100:.2f}%")

print()
print("The efficiency is decreasing - more coincidences at higher days.")
