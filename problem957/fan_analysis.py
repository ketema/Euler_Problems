"""
3-Fan Drawing Analysis for Problem 957

Given: g(1) = 8, g(2) = 28

Structure:
- 3 red vertices (fan centers): r1, r2, r3
- Blue vertices (growing each day)
- Each day: draw segments from each red to all current blues
- New blues = crossings between segments from DIFFERENT fans

Key insight: Two segments cross iff endpoints INTERLEAVE in cyclic orders
"""

from itertools import combinations

def count_crossings_between_fans(order_i, order_j, blues):
    """
    Count crossings between fan i and fan j.

    order_i: cyclic order of blues around red vertex i (list of blue labels)
    order_j: cyclic order of blues around red vertex j (list of blue labels)
    blues: list of blue labels

    Two segments (ri, b1) and (rj, b2) cross iff:
    - b1 and b2 interleave in the cyclic orders around ri and rj
    """
    # Create position maps
    n = len(blues)
    pos_i = {b: idx for idx, b in enumerate(order_i)}
    pos_j = {b: idx for idx, b in enumerate(order_j)}

    crossings = 0
    # Check all pairs of blues
    for b1, b2 in combinations(blues, 2):
        # Get positions in both orders
        pi1, pi2 = pos_i[b1], pos_i[b2]
        pj1, pj2 = pos_j[b1], pos_j[b2]

        # Check if they interleave
        # In cyclic order i: b1 comes before b2 (say, b1 < b2)
        # In cyclic order j: they should be in opposite order for crossing
        order_same_i = (pi1 < pi2)
        order_same_j = (pj1 < pj2)

        if order_same_i != order_same_j:
            crossings += 1

    return crossings

def total_new_points(cyclic_orders, blues):
    """
    Count total new points created on a day.

    cyclic_orders: dict mapping red vertex -> cyclic order of blues
    blues: list of current blue labels
    """
    reds = list(cyclic_orders.keys())
    total = 0

    # For each pair of red vertices
    for r1, r2 in combinations(reds, 2):
        crossings = count_crossings_between_fans(
            cyclic_orders[r1],
            cyclic_orders[r2],
            blues
        )
        total += crossings
        print(f"  Crossings between fan({r1}) and fan({r2}): {crossings}")

    return total

# Work backwards from g(1) = 8, g(2) = 28
print("="*70)
print("Working backwards from g(1) = 8, g(2) = 28")
print("="*70)

# Try g(0) = 2
g0 = 2
g1 = 8
g2 = 28

m0 = g1 - g0  # new points day 0->1
m1 = g2 - g1  # new points day 1->2

print(f"\nAssuming g(0) = {g0}:")
print(f"  Day 0→1: added {m0} new blues (g(1) = {g1})")
print(f"  Day 1→2: added {m1} new blues (g(2) = {g2})")

print("\n" + "="*70)
print("Day 0→1 Analysis")
print("="*70)
print(f"Starting with {g0} blues: b1, b2")
print(f"3 red vertices: r1, r2, r3")
print(f"Each red connects to all {g0} blues")
print(f"Total segments: 3 × {g0} = {3*g0}")
print(f"Pairs of reds: C(3,2) = 3")
print(f"\nNeed to add {m0} new points via crossings")

# For 2 blues and 3 reds, maximum crossings per pair = C(2,2) = 1
print(f"\nMaximum crossings per red pair: C({g0},2) = {g0*(g0-1)//2} = 1")
print(f"Maximum total crossings: 3 × 1 = 3")
print(f"But we need {m0} crossings!")

if m0 > 3:
    print(f"\n⚠️  ERROR: Can't get {m0} crossings with only {g0} starting blues!")
    print(f"Need different g(0)...")

# Try different g(0) values
print("\n" + "="*70)
print("Testing different initial configurations")
print("="*70)

for test_g0 in range(1, 10):
    test_m0 = g1 - test_g0
    max_crossings_day1 = 3 * (test_g0 * (test_g0 - 1) // 2)

    print(f"\ng(0) = {test_g0}:")
    print(f"  Need {test_m0} crossings on day 1")
    print(f"  Max possible: 3 × C({test_g0},2) = {max_crossings_day1}")

    if test_m0 <= max_crossings_day1:
        print(f"  ✓ Feasible!")

        # Check day 2
        test_g1 = g1
        test_m1 = g2 - g1
        max_crossings_day2 = 3 * (test_g1 * (test_g1 - 1) // 2)
        print(f"  Day 2: need {test_m1} crossings, max = 3 × C({test_g1},2) = {max_crossings_day2}")

        if test_m1 <= max_crossings_day2:
            print(f"  ✓ Day 2 also feasible!")
        else:
            print(f"  ✗ Day 2 not feasible")
    else:
        print(f"  ✗ Not feasible")

print("\n" + "="*70)
print("Conclusion")
print("="*70)
print("The key is finding the right cyclic orders that give EXACTLY")
print("the right number of crossings at each step.")
print("\nThis is a combinatorial constraint satisfaction problem!")
