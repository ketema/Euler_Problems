"""
Cyclic Order Simulator for 3-Fan Drawing

Model:
- 3 red vertices (r1, r2, r3) - fixed fan centers
- Blue vertices growing each day
- Cyclic order of blues around each red determines crossings

Two segments (ri, b_a) and (rj, b_b) cross iff b_a and b_b interleave
in the cyclic orders around ri and rj.
"""

from itertools import combinations

class FanConfiguration:
    """Represents the cyclic ordering state."""

    def __init__(self, n_reds, initial_blues, cyclic_orders):
        """
        n_reds: number of red vertices
        initial_blues: list of initial blue labels
        cyclic_orders: dict {red_label: [ordered blues]}
        """
        self.n_reds = n_reds
        self.blues = list(initial_blues)
        self.orders = cyclic_orders
        self.day = 0
        self.next_blue_id = max(initial_blues) + 1 if initial_blues else 1

    def count_crossings(self, red_i, red_j):
        """Count crossings between fan(red_i) and fan(red_j)."""
        order_i = self.orders[red_i]
        order_j = self.orders[red_j]

        # Position maps
        pos_i = {b: idx for idx, b in enumerate(order_i)}
        pos_j = {b: idx for idx, b in enumerate(order_j)}

        crossings = 0
        for b1, b2 in combinations(self.blues, 2):
            # Check if b1, b2 interleave
            # Interleave means: order differs in the two cyclic sequences
            order_same_i = (pos_i[b1] < pos_i[b2])
            order_same_j = (pos_j[b1] < pos_j[b2])

            if order_same_i != order_same_j:
                crossings += 1

        return crossings

    def step(self, insert_strategy=None):
        """
        Execute one day step.

        insert_strategy: function(new_blue_id, current_orders) -> updated_orders
        If None, uses default: append to end of each order
        """
        print(f"\n{'='*70}")
        print(f"Day {self.day} → {self.day + 1}")
        print(f"{'='*70}")
        print(f"Current blues: {len(self.blues)} = {self.blues}")

        # Count crossings for each pair of reds
        total_new = 0
        for red_i, red_j in combinations(range(1, self.n_reds + 1), 2):
            c = self.count_crossings(red_i, red_j)
            print(f"  Fan(r{red_i}) × Fan(r{red_j}): {c} crossings")
            total_new += c

        print(f"\nTotal new blues: {total_new}")
        print(f"g({self.day + 1}) = {len(self.blues) + total_new}")

        # Add new blues and update orders
        new_blues = list(range(self.next_blue_id, self.next_blue_id + total_new))

        if insert_strategy:
            self.orders = insert_strategy(new_blues, self.orders)
        else:
            # Default: append all new blues to end of each order
            for red in range(1, self.n_reds + 1):
                self.orders[red].extend(new_blues)

        self.blues.extend(new_blues)
        self.next_blue_id += total_new
        self.day += 1

        return total_new

# Test case: Find configuration that gives g(1)=8, g(2)=28
print("="*70)
print("Finding configuration for g(1) = 8, g(2) = 28")
print("="*70)

# Try g(0) = 2 with "alternating" cyclic orders
print("\n" + "="*70)
print("Attempt: g(0) = 2, alternating orders")
print("="*70)

config = FanConfiguration(
    n_reds=3,
    initial_blues=[1, 2],
    cyclic_orders={
        1: [1, 2],  # r1 sees b1, b2 in that order
        2: [2, 1],  # r2 sees b2, b1 (reversed)
        3: [1, 2],  # r3 sees b1, b2 (same as r1)
    }
)

print(f"Initial configuration:")
print(f"  g(0) = {len(config.blues)}")
for red in range(1, 4):
    print(f"  Cyclic order around r{red}: {config.orders[red]}")

m0 = config.step()
if len(config.blues) == 8:
    print(f"\n✓ Matches g(1) = 8!")
else:
    print(f"\n✗ Got g(1) = {len(config.blues)}, expected 8")

# Try another config if that doesn't work
if len(config.blues) != 8:
    print("\n" + "="*70)
    print("Attempt: g(0) = 3")
    print("="*70)

    config2 = FanConfiguration(
        n_reds=3,
        initial_blues=[1, 2, 3],
        cyclic_orders={
            1: [1, 2, 3],
            2: [1, 3, 2],  # b2 and b3 swapped
            3: [1, 2, 3],
        }
    )

    print(f"Initial configuration:")
    print(f"  g(0) = {len(config2.blues)}")
    for red in range(1, 4):
        print(f"  Cyclic order around r{red}: {config2.orders[red]}")

    m0 = config2.step()
    if len(config2.blues) == 8:
        print(f"\n✓ Matches g(1) = 8!")
    else:
        print(f"\n✗ Got g(1) = {len(config2.blues)}, expected 8")

# Enumerate all possible cyclic order configurations for small g(0)
print("\n" + "="*70)
print("Systematic search for g(0) = 2")
print("="*70)

from itertools import permutations

# For 2 blues, there are only 2 distinct cyclic orders: [1,2] and [2,1]
# For 3 reds, we need to assign one of these to each
# But cyclic orders are equivalent up to rotation, so:
# [1,2] is the "same" as [2,1] if we think cyclically...

# Actually for 2 elements, cyclic order is trivial (only one up to rotation/reflection)
# The key is the RELATIVE order between different fans!

print("\nFor 2 blues, testing all relative orderings:")
for o2 in [[1,2], [2,1]]:
    for o3 in [[1,2], [2,1]]:
        test_config = FanConfiguration(
            n_reds=3,
            initial_blues=[1, 2],
            cyclic_orders={1: [1, 2], 2: o2, 3: o3}
        )

        # Count total crossings
        total = 0
        for ri, rj in combinations(range(1, 4), 2):
            c = test_config.count_crossings(ri, rj)
            total += c

        if total == 6:  # Need 6 new blues to get g(1) = 8
            print(f"  ✓ FOUND: r1:[1,2], r2:{o2}, r3:{o3} → {total} crossings")
