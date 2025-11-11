#!/usr/bin/env python3
"""
Deep analysis of remaining candidates: 828, 74, 256, 124, 112

After 152 rejected, explore mathematical AND linguistic basis for each.
Look for connections between:
- Mathematical properties (divisibility, sequences, geometry)
- Linguistic encoding (words, letters, problem statement)
- Problem structure (3 reds, 2 blues, n days)
"""

import math
from sympy import factorint, isprime, divisors

print("="*80)
print("DEEP ANALYSIS: REMAINING LINGUISTIC CANDIDATES")
print("="*80)
print()

candidates = [828, 74, 256, 124, 112]

# Known values
g1 = 8
g2 = 28
n = 16

print("Context:")
print(f"  g(1) = {g1}")
print(f"  g(2) = {g2}")
print(f"  Target: g({n})")
print()
print(f"  152 = 'POINT GENESIS' letter sum → REJECTED ✗")
print()

# ============================================================================
# CANDIDATE 1: 828 (Concatenation 8||28)
# ============================================================================

print("="*80)
print("CANDIDATE 1: 828 = Concatenation 8||28")
print("="*80)
print()

val = 828

print("LINGUISTIC BASIS:")
print("  Direct concatenation of g(1)||g(2) = 8||28")
print("  Simple wordplay: 'put them together'")
print()

print("MATHEMATICAL PROPERTIES:")
print(f"  Prime factorization: {factorint(val)}")
factors = factorint(val)
print(f"  828 = {' × '.join(f'{p}^{e}' if e > 1 else str(p) for p, e in factors.items())}")
print(f"  828 = 2² × 3² × 23")
print()

print("Divisibility:")
print(f"  828 / 2 = {val / 2}")
print(f"  828 / 3 = {val / 3}")
print(f"  828 / 4 = {val / 4}")
print(f"  828 / 8 = {val / 8} (g(1))")
print(f"  828 / 28 = {val / 28} (g(2))")
print(f"  828 / 16 = {val / 16} (target n)")
print()

print("Relation to problem parameters:")
print(f"  828 / (3+2) = {val / 5} (total initial points)")
print(f"  828 / (3×2) = {val / 6} (initial lines)")
print()

print("As geometric quantity:")
print(f"  If total points after 16 days = 828")
print(f"  Then g(16) = 828 - 3 = {val - 3} blues")
print(f"  OR g(16) = 828 directly")
print()

print("Connection to C(8,1)=8, C(8,2)=28:")
c82 = math.comb(8, 2)
print(f"  828 / C(8,2) = 828 / {c82} = {val / c82}")
print(f"  828 - C(8,2) = 828 - {c82} = {val - c82}")
print()

# ============================================================================
# CANDIDATE 2: 74 (POINT word sum)
# ============================================================================

print("="*80)
print("CANDIDATE 2: 74 = 'POINT' Letter Sum")
print("="*80)
print()

val = 74

print("LINGUISTIC BASIS:")
print("  P(16) + O(15) + I(9) + N(14) + T(20) = 74")
print("  First word of title 'Point Genesis'")
print("  Focus on POINT (the things being counted)")
print()

print("MATHEMATICAL PROPERTIES:")
print(f"  Prime factorization: {factorint(val)}")
print(f"  74 = 2 × 37")
print(f"  37 is prime")
print()

print("Relation to given values:")
print(f"  74 - 8 = {val - g1}")
print(f"  74 - 28 = {val - g2}")
print(f"  8 + 28 + 74 = {g1 + g2 + val}")
print(f"  (74 - 8) / (28 - 8) = {(val - g1) / (g2 - g1)}")
print()

print("As arithmetic pattern:")
print(f"  g(1) = 8")
print(f"  g(2) = 28 (diff = 20)")
print(f"  g(3) = 74? (diff = 46)")
print(f"  Pattern: 8, 28, 74, ... (diffs: 20, 46, ...)")
print(f"  Second differences: 46-20 = 26")
print()

if (val - g1) % (g2 - g1) == 0:
    k = (val - g1) // (g2 - g1)
    print(f"Linear check: 74 = 8 + {k}×20")
    print(f"  If g(n) = 8 + 20(n-1), then 74 = g({k+1})")
else:
    print(f"Not a simple linear pattern from g(1), g(2)")
print()

print("Connection to problem:")
print(f"  Problem has: 3 reds, 2 blues, lines, plane, days")
print(f"  'POINT' appears in problem statement")
print(f"  Is 74 counting something specific?")
print()

# ============================================================================
# CANDIDATE 3: 256 (2^8 = 16^2)
# ============================================================================

print("="*80)
print("CANDIDATE 3: 256 = 2^8 = 16^2")
print("="*80)
print()

val = 256

print("LINGUISTIC BASIS:")
print("  Weaker: relates to P=16 and g(1)=8")
print("  No direct word encoding")
print()

print("MATHEMATICAL PROPERTIES:")
print(f"  256 = 2^8 (g(1) as exponent)")
print(f"  256 = 16^2 (target n as base)")
print(f"  256 = 4^4")
print(f"  Perfect power with multiple representations")
print()

print("Relation to problem structure:")
print(f"  Binary: 256 = 0b{bin(val)[2:]}")
print(f"  Powers of 2: 1, 2, 4, 8, 16, 32, 64, 128, 256")
print(f"  Position: 8th power of 2 (matching g(1)=8)")
print()

print("As growth formula:")
print(f"  If g(n) = 2^(n+2) for n≥1:")
print(f"    g(1) = 2^3 = 8 ✓")
print(f"    g(2) = 2^4 = 16 ✗ (need 28)")
print(f"  Doesn't fit exponential pattern")
print()

print("As finite field bound:")
print(f"  256 = 16^2 = 2^8")
print(f"  PG(2,15) has 15^2+15+1 = {15**2+15+1} points")
print(f"  PG(2,16) has 16^2+16+1 = {16**2+16+1} points")
print(f"  256 is close but not exact finite field formula")
print()

print("Connection to combinatorics:")
print(f"  C(16,2) = {math.comb(16,2)}")
print(f"  C(256,2) = {math.comb(256,2)}")
print(f"  Not obvious binomial connection")
print()

# ============================================================================
# CANDIDATE 4: 124 ('SIXTEENTH' letter sum)
# ============================================================================

print("="*80)
print("CANDIDATE 4: 124 = 'SIXTEENTH' Letter Sum")
print("="*80)
print()

val = 124

print("LINGUISTIC BASIS:")
print("  S(19) + I(9) + X(24) + T(20) + E(5) + E(5) + N(14) + T(20) + H(8) = 124")
print("  Direct ordinal for '16th'")
print("  Word explicitly states the index")
print()

print("MATHEMATICAL PROPERTIES:")
print(f"  Prime factorization: {factorint(val)}")
print(f"  124 = 2² × 31")
print(f"  31 is prime")
print()

print("Relation to given values:")
print(f"  124 / 8 = {val / g1}")
print(f"  124 / 28 = {val / g2}")
print(f"  124 - 8 = {val - g1}")
print(f"  124 - 28 = {val - g2}")
print()

print("As pattern:")
print(f"  If letter sums were the pattern:")
print(f"    'FIRST' = 72 (but g(1)=8)")
print(f"    'SECOND' = 60 (but g(2)=28)")
print(f"  Pattern doesn't match known values")
print()

print("But consider:")
print(f"  What if g(16) is special?")
print(f"  Only g(16) = letter sum of ordinal?")
print(f"  While g(1), g(2) have different encoding?")
print()

print("Relation to problem:")
print(f"  124 / 4 = 31 (4 = 2 blues + 2?)")
print(f"  124 = 4 × 31")
print(f"  Not obvious connection to 3 reds, 2 blues")
print()

# ============================================================================
# CANDIDATE 5: 112 (P×G initials)
# ============================================================================

print("="*80)
print("CANDIDATE 5: 112 = P(16) × G(7)")
print("="*80)
print()

val = 112

print("LINGUISTIC BASIS:")
print("  Product of initials: P × G = 16 × 7 = 112")
print("  'Point Genesis' → P.G.")
print("  Cryptic encoding")
print()

print("MATHEMATICAL PROPERTIES:")
print(f"  Prime factorization: {factorint(val)}")
print(f"  112 = 2^4 × 7")
print(f"  112 = 16 × 7 (target n × G)")
print()

print("Relation to given values:")
print(f"  112 / 8 = {val / g1}")
print(f"  112 / 28 = {val / g2}")
print(f"  112 - 8 = {val - g1}")
print(f"  112 - 28 = {val - g2}")
print()

print("Connection to problem structure:")
print(f"  112 / (3+2) = {val / 5}")
print(f"  112 / (3×2) = {val / 6}")
print(f"  112 / 16 = 7 (equals G!)")
print()

print("As formula:")
print(f"  If g(n) involves product with 7:")
print(f"    g(16) = 16 × 7 = 112")
print(f"    g(1) = 1 × 7 = 7 (but need 8)")
print(f"    g(2) = 2 × 7 = 14 (but need 28)")
print(f"  Simple product doesn't work")
print()

print("Alternative:")
print(f"  If g(n) = n × 7 + offset:")
print(f"    g(1) = 1×7 + 1 = 8 ✓")
print(f"    g(2) = 2×7 + 14 = 28 ✓")
print(f"    g(16) = 16×7 + ? = 112 + ?")
print()

# Find offset pattern
offset_1 = g1 - 1*7
offset_2 = g2 - 2*7
print(f"  Offsets: g(1)→{offset_1}, g(2)→{offset_2}")
print(f"  Pattern: offsets are 1, 14, ...")
print(f"  14 = 14×1, so offset(n) = 14(n-1)?")
print(f"  Check: g(1) = 1×7 + 14×0 = 7 ✗ (need 8)")
print()

print(f"  Actually: g(1) = 7 + 1, g(2) = 14 + 14")
print(f"  Let's try: g(n) = 7n + f(n)")
for n in [1, 2]:
    f_n = (g1 if n==1 else g2) - 7*n
    print(f"    f({n}) = {f_n}")

print(f"  f(1)=1, f(2)=14")
print(f"  f(16) = ? (can't determine without pattern)")
print()

# ============================================================================
# SYNTHESIS
# ============================================================================

print("="*80)
print("SYNTHESIS: CROSS-CONNECTIONS")
print("="*80)
print()

print("Looking for hybrid mathematical-linguistic patterns...")
print()

print("1. CONCATENATION (828):")
print("   • Strongest wordplay: direct combination")
print("   • Math: 828 = 2²×3²×23, divisible by both 8 and 28")
print("   • Problem: Could represent 'merging' g(1) and g(2)")
print()

print("2. POINT SUM (74):")
print("   • Medium wordplay: first word of title")
print("   • Math: 74 = 2×37, forms arithmetic progression")
print("   • Problem: 'POINT' is central concept")
print()

print("3. POWER (256):")
print("   • Weak wordplay: only via P=16, H=8 connection")
print("   • Math: Perfect power 2^8 = 16^2, elegant")
print("   • Problem: Could represent geometric growth bound")
print()

print("4. SIXTEENTH (124):")
print("   • Strong wordplay: explicit ordinal")
print("   • Math: 124 = 4×31, special for n=16 only?")
print("   • Problem: Directly names the index")
print()

print("5. INITIALS (112):")
print("   • Medium wordplay: cryptic P×G")
print("   • Math: 112 = 16×7, formula g(n)=7n+f(n)?")
print("   • Problem: Initials encode multiplication?")
print()

print("="*80)
print("RECOMMENDATION ORDER")
print("="*80)
print()

print("After 152 rejected, try in order:")
print()
print("1. 828 - Strongest wordplay (concatenation)")
print("   Mathematical bonus: divisible by both 8 and 28")
print()
print("2. 74 - Medium wordplay ('POINT')")
print("   Mathematical bonus: forms arithmetic sequence")
print()
print("3. 256 - Weakest wordplay, strongest math (2^8 = 16^2)")
print("   But mathematical elegance matters in PE")
print()
print("4. 124 - Strong wordplay ('SIXTEENTH')")
print("   But ordinals didn't match g(1), g(2)")
print()
print("5. 112 - Medium wordplay (P×G)")
print("   Interesting formula g(n) = 7n + f(n) possibility")
print()
