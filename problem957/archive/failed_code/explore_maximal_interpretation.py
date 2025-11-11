#!/usr/bin/env python3
"""
Deep analysis of "maximal possible" language.

What if "maximal" doesn't mean maximum count, but something else?
"""

print("="*70)
print("EXPLORING 'MAXIMAL POSSIBLE' INTERPRETATIONS")
print("="*70)
print()

# Given: g(1) = 8, g(2) = 28

print("GIVEN VALUES:")
print("  g(1) = 8")
print("  g(2) = 28")
print()

# ============================================================================
# INTERPRETATION 1: "Maximal" as in graph theory
# ============================================================================

print("="*70)
print("INTERPRETATION 1: GRAPH-THEORETIC 'MAXIMAL'")
print("="*70)
print()

print("In graph theory, 'maximal' ≠ 'maximum':")
print("  • Maximal = cannot be extended")
print("  • Maximum = largest possible")
print()

print("Example: Maximal independent set vs maximum independent set")
print()

print("Could 'maximal possible' mean:")
print("  'The configuration that cannot be extended further'")
print("  NOT 'the configuration with most blues'")
print()

# ============================================================================
# INTERPRETATION 2: "After n days" = cumulative definition
# ============================================================================

print("="*70)
print("INTERPRETATION 2: 'AFTER N DAYS' ENCODING")
print("="*70)
print()

print("What if 'after n days' encodes a recursive relationship?")
print()

# g(1) = 8, g(2) = 28
# Is there a relation like g(n) = f(g(n-1), g(n-2))?

print("Testing recursive patterns:")
print()

# g(2) = a*g(1) + b
# 28 = a*8 + b
# If a=3: b = 4, so g(2) = 3*8 + 4 = 28 ✓

print("Pattern: g(n) = 3*g(n-1) + 4")
print(f"  g(1) = 8 (given)")
print(f"  g(2) = 3*8 + 4 = {3*8 + 4} ✓")
g = [0, 8, 28]
for n in range(3, 17):
    g_n = 3*g[n-1] + 4
    g.append(g_n)
    if n <= 5:
        print(f"  g({n}) = 3*{g[n-1]} + 4 = {g_n}")

print(f"  ...")
print(f"  g(16) = {g[16]:,}")
print()

# Another pattern: g(n) = a*g(n-1) + b*g(n-2)
# g(2) = a*g(1) + b*g(0)
# If g(0) = 0: 28 = a*8, so a = 3.5? Not integer

print("Pattern: g(n) = 3*g(n-1) + g(n-2)")
print("  Need g(0)...")
# 28 = 3*8 + g(0) → g(0) = 4
print(f"  If g(0) = 4:")
g2 = [4, 8, 28]
for n in range(3, 17):
    g_n = 3*g2[n-1] + g2[n-2]
    g2.append(g_n)
    if n <= 5:
        print(f"    g({n}) = 3*{g2[n-1]} + {g2[n-2]} = {g_n}")
print(f"    ...")
print(f"    g(16) = {g2[16]:,}")
print()

# ============================================================================
# INTERPRETATION 3: g(1)=8 and g(2)=28 are CONSTRAINTS not examples
# ============================================================================

print("="*70)
print("INTERPRETATION 3: GIVEN VALUES AS CONSTRAINTS")
print("="*70)
print()

print("What if g(1)=8 and g(2)=28 are NOT examples to verify,")
print("but DEFINING CONSTRAINTS that determine g(16)?")
print()

print("Analogy: 'Find x where f(1)=8 and f(2)=28' defines f uniquely")
print()

# If we have exactly 2 constraints, we can fit:
# - Linear function (2 parameters)
# - Or define recursive relation

print("This leads to g(n) = 20n - 12 (linear)")
print(f"  g(16) = 20*16 - 12 = {20*16 - 12}")
print()

# ============================================================================
# INTERPRETATION 4: "Days" as exponential
# ============================================================================

print("="*70)
print("INTERPRETATION 4: EXPONENTIAL/POWER INTERPRETATION")
print("="*70)
print()

print("What if 'n days' means something grows exponentially?")
print()

# g(1) = 8 = 2³
# g(2) = 28 = 4*7
# Pattern?

print("g(1) = 8 = 2³")
print("g(2) = 28 = 4*7 = 2²*7")
print()
print("Is there a pattern with powers of 2?")
print("  g(1) involves 2³")
print("  g(2) involves 2²")
print("  g(16) involves 2^? ?")
print()

# Maybe g(n) = 2^(something(n)) * (other thing)

# ============================================================================
# INTERPRETATION 5: Binary/Bitwise
# ============================================================================

print("="*70)
print("INTERPRETATION 5: BINARY ENCODING")
print("="*70)
print()

print("g(1) = 8  = 0b1000")
print("g(2) = 28 = 0b11100")
print()

print("Looking at binary:")
print("  8  = 1000")
print("  28 = 11100")
print()

print("Pattern: append '100' to previous?")
print("  1000 → 11000 = 24? No...")
print()

print("Or: shift and add?")
print("  8 << 2 = 32, 32 - 4 = 28? Close...")
print()

# ============================================================================
# SUMMARY
# ============================================================================

print("="*70)
print("MOST PROMISING RECURSIVE PATTERNS")
print("="*70)
print()

print("1. g(n) = 3*g(n-1) + 4, g(1)=8")
print(f"   → g(16) = {g[16]:,}")
print()

print("2. g(n) = 3*g(n-1) + g(n-2), g(0)=4, g(1)=8")
print(f"   → g(16) = {g2[16]:,}")
print()

print("3. g(n) = 20n - 12 (linear)")
print(f"   → g(16) = 308")
print()

print("CANDIDATES TO TRY:")
print(f"  • {g[16]:,} (recursive: 3*g(n-1) + 4)")
print(f"  • {g2[16]:,} (recursive: 3*g(n-1) + g(n-2))")
print(f"  • 308 (linear: 20n - 12)")
print()
