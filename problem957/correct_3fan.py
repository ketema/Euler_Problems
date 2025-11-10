"""
Correct analysis with 3 reds and 2 blues

Initial: 3 red points, 2 blue points
"""

print("="*70)
print("3-FAN STRUCTURE (CORRECT)")
print("="*70)
print()

print("Initial configuration:")
print("  3 red points (r1, r2, r3)")
print("  2 blue points (b1, b2)")
print()

print("Day 1 analysis:")
print("-" * 70)
print("Lines constructed: 3 reds × 2 blues = 6 lines")
print("  (r1,b1), (r1,b2), (r2,b1), (r2,b2), (r3,b1), (r3,b2)")
print()

print("Intersections:")
print("  Lines from same red meet at that red (not new)")
print("  Lines to same blue meet at that blue (not new)")
print("  Lines (ri,bj) and (rk,bl) with i≠k and j≠l create NEW blues")
print()

print("Counting new intersections:")
print("  Pairs of reds: C(3,2) = 3")
print("  For each red pair, different blue assignments: 2! = 2")
print("  Total: 3 × 2 = 6 new blues")
print()
print("  g(1) = 2 + 6 = 8 ✓")
print()

print("="*70)
print("Day 2 analysis:")
print("-" * 70)
print("Current: 3 reds, 8 blues (2 original + 6 from day 1)")
print("Lines constructed: 3 × 8 = 24 lines")
print()
print("Maximum possible new intersections:")
print("  Pairs of reds: C(3,2) = 3")
print("  Pairs of blues: C(8,2) = 28")
print("  Maximum: 3 × 28 = 84")
print()
print("Actual: g(2) = 28, so m(1) = 28 - 8 = 20 new blues")
print("Collapse: (84 - 20) / 84 = 64/84 = 76.2% collapse!")
print()
print("This means 64 of the 84 potential intersections are NOT new:")
print("  - Some coincide with existing blues")
print("  - Some are collinear (3+ lines meet at same point)")
print()

print("="*70)
print("Finding the pattern:")
print("-" * 70)

# Try the quadratic formula
g = {0: 2, 1: 8, 2: 28}

# Fit quadratic
# g(t) = at² + bt + c
# g(0) = c = 2
# g(1) = a + b + c = 8 → a + b = 6
# g(2) = 4a + 2b + c = 28 → 2a + b = 13

a = 13 - 6  # = 7
b = 6 - a   # = -1
c = 2

print(f"Quadratic fit: g(t) = {a}t² + {b}t + {c}")
print()

# Verify
for t in [0, 1, 2]:
    computed = a*t*t + b*t + c
    print(f"  g({t}) = {computed} (expected {g[t]}) {'✓' if computed == g[t] else '✗'}")

print()
print("Differences m(t) = g(t+1) - g(t):")
for t in range(10):
    g_t = a*t*t + b*t + c
    g_t1 = a*(t+1)*(t+1) + b*(t+1) + c
    m_t = g_t1 - g_t
    print(f"  m({t}) = {m_t}")

print()
print("Pattern: m(t) = 14t + 6 (linear!)")
print()

print("="*70)
print("Computing g(16):")
print("-" * 70)
g_16 = a * 16 * 16 + b * 16 + c
print(f"g(16) = {a} × 256 + {b} × 16 + {c}")
print(f"      = {a * 256} + {b * 16} + {c}")
print(f"      = {g_16}")
print()

print("But this is INCORRECT according to PE!")
print()
print("="*70)
print("Re-examining the structure...")
print("="*70)
print()
print("Question: What special configuration gives exactly")
print("  - 6 new blues on day 1 (maximum)")
print("  - 20 new blues on day 2 (heavy collapse)")
print("  - Computable pattern for day 16?")
print()

print("Key insight: The 76% collapse on day 2 suggests")
print("a VERY special geometric structure with many collinearities.")
print()
print("Possibilities:")
print("  1. Initial 5 points lie on a conic (Pascal's theorem)")
print("  2. Points form a projective configuration (Pappus, Desargues)")
print("  3. Specific rational coordinates that create patterns")
print("  4. The collapse rate changes over time (not constant)")
print()

# Try checking if collapse rate is constant
print("Hypothesis: What if the collapse is NOT consistent with quadratic?")
print()
print("If collapse rate changes, maybe g(t) is NOT quadratic!")
print("Need more data points or a different approach...")
