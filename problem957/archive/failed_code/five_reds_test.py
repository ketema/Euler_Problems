"""
Testing with 5 red vertices instead of 3
"""

from itertools import combinations, permutations

def count_all_crossings(n_reds, blues, orders):
    """Count total crossings given cyclic orders."""
    pos_maps = {}
    for r in range(1, n_reds + 1):
        pos_maps[r] = {b: idx for idx, b in enumerate(orders[r])}

    total = 0
    for r1, r2 in combinations(range(1, n_reds + 1), 2):
        for b1, b2 in combinations(blues, 2):
            p1_r1 = pos_maps[r1][b1]
            p2_r1 = pos_maps[r1][b2]
            p1_r2 = pos_maps[r2][b1]
            p2_r2 = pos_maps[r2][b2]

            order_same_r1 = (p1_r1 < p2_r1)
            order_same_r2 = (p1_r2 < p2_r2)

            if order_same_r1 != order_same_r2:
                total += 1

    return total

print("Testing with 5 red vertices, g(0) = 2")
print("="*70)

# With 5 reds and 2 blues, maximum crossings = C(5,2) × C(2,2) = 10 × 1 = 10
print(f"Maximum possible crossings: C(5,2) × C(2,2) = 10 × 1 = 10")
print(f"Need: 6 crossings to get g(1) = 8")
print()

# Try configuration where some pairs interleave, others don't
test_orders = {
    1: [1, 2],
    2: [2, 1],  # interleaves with r1
    3: [2, 1],  # interleaves with r1
    4: [2, 1],  # interleaves with r1
    5: [1, 2],  # same as r1
}

blues = [1, 2]
crossings = count_all_crossings(5, blues, test_orders)
print(f"Config: r1:[1,2], r2-4:[2,1], r5:[1,2]")
print(f"  Total crossings: {crossings}")
if crossings == 6:
    print("  ✓ MATCHES g(1) = 8!")

# Count by pair
print("\n  Breakdown by red pair:")
pos_maps = {}
for r in range(1, 6):
    pos_maps[r] = {b: idx for idx, b in enumerate(test_orders[r])}

for r1, r2 in combinations(range(1, 6), 2):
    pair_crossings = 0
    for b1, b2 in combinations(blues, 2):
        p1_r1 = pos_maps[r1][b1]
        p2_r1 = pos_maps[r1][b2]
        p1_r2 = pos_maps[r2][b1]
        p2_r2 = pos_maps[r2][b2]

        order_same_r1 = (p1_r1 < p2_r1)
        order_same_r2 = (p1_r2 < p2_r2)

        if order_same_r1 != order_same_r2:
            pair_crossings += 1

    print(f"    r{r1} × r{r2}: {pair_crossings}")

# If that works, try a full simulation
if crossings == 6:
    print("\n" + "="*70)
    print("Simulating days 0→1→2 with this configuration")
    print("="*70)

    # Day 0: 2 blues
    current_blues = [1, 2]
    current_orders = test_orders.copy()

    print(f"\nDay 0: g(0) = {len(current_blues)}")

    # Day 0→1
    new_on_day_1 = crossings
    current_blues.extend(range(3, 3 + new_on_day_1))
    print(f"Day 1: g(1) = {len(current_blues)} (added {new_on_day_1})")

    # Need to determine cyclic orders for new blues around each red
    # This is the key challenge!
    print(f"  New blues: {list(range(3, 3 + new_on_day_1))}")
    print(f"  Need to assign cyclic positions...")

    # For now, let's just check if we CAN get 20 more with 8 blues
    max_crossings_day2 = 10 * (8 * 7 // 2)  # C(5,2) × C(8,2)
    print(f"\nDay 1→2:")
    print(f"  Max possible crossings: C(5,2) × C(8,2) = 10 × 28 = {max_crossings_day2}")
    print(f"  Need: 20 crossings to get g(2) = 28")
    print(f"  ✓ Feasible!" if 20 <= max_crossings_day2 else "  ✗ Not feasible")
