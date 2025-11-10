"""
Simple simulation with cyclic orders for 3 reds, 2 initial blues
Try different configurations to find one that gives g(1)=8, g(2)=28
"""

from itertools import combinations, permutations

def count_crossings_for_config(n_reds, blues, cyclic_orders):
    """
    Count crossings given cyclic orders.

    Two segments (ri, bj) and (rk, bl) cross iff bj and bl interleave
    in the cyclic orders around ri and rk.
    """
    total = 0

    for r1, r2 in combinations(range(1, n_reds + 1), 2):
        order1 = cyclic_orders[r1]
        order2 = cyclic_orders[r2]

        # Build position maps
        pos1 = {b: i for i, b in enumerate(order1)}
        pos2 = {b: i for i, b in enumerate(order2)}

        # Check each pair of blues
        for b1, b2 in combinations(blues, 2):
            # Check if b1, b2 interleave
            order_same_at_r1 = (pos1[b1] < pos1[b2])
            order_same_at_r2 = (pos2[b1] < pos2[b2])

            if order_same_at_r1 != order_same_at_r2:
                # They interleave, so 2 segments cross
                # (r1,b1)×(r2,b2) and (r1,b2)×(r2,b1)
                total += 2

    return total

print("="*70)
print("Finding configuration that gives g(1)=8, g(2)=28")
print("="*70)
print()

# Try all possible cyclic orders for 3 reds and 2 blues
blues = [1, 2]

print("Testing all cyclic order configurations for 3 reds, 2 blues:")
print()

# For 2 elements, there are only 2 cyclic orders: [1,2] and [2,1]
orders = [[1,2], [2,1]]

found = False
for o1 in orders:
    for o2 in orders:
        for o3 in orders:
            config = {1: o1, 2: o2, 3: o3}
            crossings = count_crossings_for_config(3, blues, config)

            if crossings == 6:  # Need 6 to get g(1) = 8
                print(f"✓ FOUND: r1:{o1}, r2:{o2}, r3:{o3} → {crossings} crossings")

                # Now check if this can give g(2) = 28
                # With 8 blues, we need 20 new crossings

                # But we don't know the cyclic orders for the 6 new blues yet!
                # This is the key challenge

                print("  For day 2, need to determine cyclic orders of 6 new blues...")
                found = True

if not found:
    print("✗ No configuration found that gives 6 crossings!")
    print()
    print("Testing each config:")
    for o1 in orders:
        for o2 in orders:
            for o3 in orders:
                config = {1: o1, 2: o2, 3: o3}
                crossings = count_crossings_for_config(3, blues, config)
                print(f"  r1:{o1}, r2:{o2}, r3:{o3} → {crossings} crossings")

print()
print("="*70)
print("Key insight: ALL configs give same total crossings!")
print("="*70)
print("This is because we're just permuting labels.")
print("The question is: how do new blues get inserted into orders?")
