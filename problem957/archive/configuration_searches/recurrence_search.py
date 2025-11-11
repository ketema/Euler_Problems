"""
Search for recurrence relation using crossing count formula

Key insight from user: "maintain canonical representation (e.g., the circular
permutations of blue labels around each red), and update it day-by-day. That
gives a provable recurrence or exact count without geometry."

Strategy: Try different insertion strategies and see which gives g(2) = 28
"""

from itertools import combinations

def count_crossings_between_fans(r1_order, r2_order):
    """Count crossings between two fans given their cyclic orders."""
    blues = r1_order  # Assume both have same set of blues
    pos1 = {b: i for i, b in enumerate(r1_order)}
    pos2 = {b: i for i, b in enumerate(r2_order)}

    crossings = 0
    for b1, b2 in combinations(blues, 2):
        if (pos1[b1] < pos1[b2]) != (pos2[b1] < pos2[b2]):
            crossings += 1

    return crossings

def total_crossings(orders):
    """Count total crossings given all fan orders."""
    reds = list(orders.keys())
    total = 0
    for i, r1 in enumerate(reds):
        for r2 in reds[i+1:]:
            c = count_crossings_between_fans(orders[r1], orders[r2])
            total += c
    return total

# Starting configuration
initial_orders = {
    1: [1, 2],
    2: [2, 1],
    3: [2, 1],
    4: [2, 1],
    5: [1, 2],
}

# After day 1, we have blues [1, 2, 3, 4, 5, 6, 7, 8]
# New blues 3-8 were created by specific crossings

# Let me try: append all new blues to the end (preserving relative order)
print("Strategy 1: Append all new blues to end")
print("="*70)

day1_orders_v1 = {
    1: [1, 2, 3, 4, 5, 6, 7, 8],
    2: [2, 1, 3, 4, 5, 6, 7, 8],
    3: [2, 1, 3, 4, 5, 6, 7, 8],
    4: [2, 1, 3, 4, 5, 6, 7, 8],
    5: [1, 2, 3, 4, 5, 6, 7, 8],
}

c1 = total_crossings(day1_orders_v1)
print(f"Total crossings: {c1}")
print(f"Expected: 20 (to get g(2) = 28)")
print(f"Match: {c1 == 20}")

# Try: reverse the new blues for the "reversed" fans
print("\n" + "="*70)
print("Strategy 2: Reverse new blues for reversed fans")
print("="*70)

day1_orders_v2 = {
    1: [1, 2, 3, 4, 5, 6, 7, 8],
    2: [2, 1, 8, 7, 6, 5, 4, 3],  # reversed new blues
    3: [2, 1, 8, 7, 6, 5, 4, 3],
    4: [2, 1, 8, 7, 6, 5, 4, 3],
    5: [1, 2, 3, 4, 5, 6, 7, 8],
}

c2 = total_crossings(day1_orders_v2)
print(f"Total crossings: {c2}")
print(f"Expected: 20")
print(f"Match: {c2 == 20}")

# Let's think about this differently: maybe there's a formula
print("\n" + "="*70)
print("Formula approach")
print("="*70)

# With 5 reds and 2 blues initially:
# Number of "disagreeing" pairs of reds
# r1 vs r2,r3,r4: 3 pairs (all disagree)
# r5 vs r2,r3,r4: 3 pairs (all disagree)
# Total disagreeing pairs: 6

n_disagree = 6
n_agree = 10 - 6  # C(5,2) = 10 total pairs

print(f"Red pairs that disagree on [1,2] order: {n_disagree}")
print(f"Red pairs that agree: {n_agree}")
print(f"Crossings on day 1: {n_disagree} × C(2,2) = {n_disagree}")

# On day 2: 8 blues
# Crossings = n_disagree × (# of disagreeing blue pairs)
# If new blues maintain the disagreement pattern:

print("\nHypothesis: New blues maintain disagreement pattern")
print("If all 8 blues are ordered consistently within each 'group':")
print("  Group A (r1, r5): [1,2,3,4,5,6,7,8]")
print("  Group B (r2,r3,r4): [2,1,?,?,?,?,?,?]")
print()
print("Any blue from old set + any from new set will disagree if groups disagree")
print("Any two new blues: depends on their ordering")

# Let me compute different scenarios
print("\nScenario: Group B has [2,1,3,4,5,6,7,8]")
# Disagreeing pairs:
# Old blues: (1,2) - 1 pair
# Old vs new: 2×6 = 12 pairs (depends on positioning)
# New blues among themselves: 0 pairs (same order)

# Actually this is getting complicated. Let me try a different approach.

print("\n" + "="*70)
print("Pattern recognition from known values")
print("="*70)

# Known: g(0)=2, g(1)=8, g(2)=28
# Differences: m(0)=6, m(1)=20

print("g(0) = 2")
print("g(1) = 8  (added 6)")
print("g(2) = 28 (added 20)")
print()

# Look at second differences
print("Δg: 6, 20")
print("Δ²g: 14")
print()

# Try quadratic
# g(t) = at² + bt + c
# g(0) = c = 2
# g(1) = a + b + 2 = 8 → a + b = 6
# g(2) = 4a + 2b + 2 = 28 → 4a + 2b = 26 → 2a + b = 13

a = 13 - 6
b = 6 - a
c = 2

print(f"Quadratic fit: g(t) = {a}t² + {b}t + {c}")
print(f"Verify: g(0) = {c}, g(1) = {a+b+c}, g(2) = {4*a+2*b+c}")

if a+b+c == 8 and 4*a+2*b+c == 28:
    print("✓ Perfect fit!")
    print(f"\ng(16) = {a}*256 + {b}*16 + {c} = {a*256 + b*16 + c}")
else:
    print("✗ Doesn't fit")
