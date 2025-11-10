"""
Double-checking the formula and looking for alternatives
"""

print("Given constraints:")
print("  3 red points (stay red forever)")
print("  2 blue points initially")
print("  g(1) = 8")
print("  g(2) = 28")
print()

# The quadratic formula
print("="*70)
print("Standard quadratic fit:")
print("="*70)
a, b, c = 7, -1, 2
print(f"g(t) = {a}t² + ({b})t + {c}")
print()

for t in range(17):
    g_t = a*t*t + b*t + c
    print(f"g({t:2d}) = {g_t:4d}", end="")
    if t % 4 == 3:
        print()
    else:
        print("  ", end="")
print()
print()
print(f"Answer: g(16) = {a*256 + b*16 + c}")
print()

print("="*70)
print("But PE says this is WRONG!")
print("="*70)
print()

# Let me think about what else could give these values
print("Alternative approaches:")
print()

# What if there's an off-by-one in how days are counted?
print("1. Maybe 'after n days' means something different?")
print("   If we call the initial state 'day 1' instead of 'day 0':")
print(f"   Then g(1)=2, g(2)=8, g(3)=28")
print(f"   Fitting: g(t) = 7t² - 15t + 10")
print(f"   g(17) = 7×289 - 15×17 + 10 = 2023 - 255 + 10 = 1778")
print(f"   Same answer!")
print()

# What if it's not quadratic at all?
print("2. Maybe it's not quadratic?")
print("   Need g(3) to check. If quadratic: g(3) = 62")
print("   If g(3) ≠ 62, then formula is wrong")
print()

# What if max crossings formula is wrong?
print("3. Rechecking the crossing count for day 1:")
print("   3 reds, 2 blues → 6 lines")
print("   Pairs with different red AND different blue:")
print("     C(3,2) × 2! = 3 × 2 = 6 intersections")
print("   This gives g(1) = 2 + 6 = 8 ✓")
print()

print("4. Day 2 collapse rate:")
print("   3 reds, 8 blues → 24 lines")
print("   Maximum crossings: C(3,2) × C(8,2) × 2 = 3 × 28 × 2 = 168")
print("   Actual: 20 new blues")
print("   Collapse: 88%!")
print()

# Maybe there's a formula based on the collapse pattern
print("5. What if collapse rate follows a pattern?")
print()
print("   Let m(t) be new blues on day t→t+1")
print("   Let max(t) = C(3,2) × C(g(t),2) × 2 = 3 × g(t)(g(t)-1)")
print()

for t in range(5):
    g_t = a*t*t + b*t + c
    g_t1 = a*(t+1)*(t+1) + b*(t+1) + c
    m_t = g_t1 - g_t
    max_t = 3 * g_t * (g_t - 1)
    if max_t > 0:
        collapse = 1 - m_t/max_t
        print(f"   Day {t}: g={g_t:3d}, m={m_t:3d}, max={max_t:4d}, collapse={collapse:.1%}")

print()
print("   Collapse increases each day! 0% → 76% → 93% → ...")
print()

print("="*70)
print("Hypothesis: Maybe the problem has a special structure")
print("where the collapse follows a specific pattern?")
print("="*70)
print()
print("Without being able to compute g(3) directly, I cannot")
print("determine if the quadratic formula is correct.")
print()
print("Current best answer: g(16) = 1778")
print("But this is apparently incorrect.")
