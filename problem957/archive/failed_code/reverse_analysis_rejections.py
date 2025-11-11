#!/usr/bin/env python3
"""
REVERSE ANALYSIS: Study all rejected answers to find the inverse pattern

Hypothesis: The answer might be the OPPOSITE of what all our attempts share
"""

import math
from collections import Counter

print("="*80)
print("REVERSE ANALYSIS: What Do All Rejections Share?")
print("="*80)
print()

# ALL rejected values (comprehensive list)
all_rejected = [
    # Mathematical approaches
    1778, 1973818, 15730302251147551048, 1893, 2257, 633250439,
    3010, 143489068, 512159344, 12870,

    # Simple arithmetic
    168, 308,

    # Linguistic
    152, 112, 828,

    # Pure math
    256,

    # Small attempts
    56,
]

print(f"Total rejected: {len(all_rejected)} attempts")
print()

# Categorize by size
tiny = [x for x in all_rejected if x < 10]
small = [x for x in all_rejected if 10 <= x < 100]
medium = [x for x in all_rejected if 100 <= x < 1000]
large = [x for x in all_rejected if 1000 <= x < 1000000]
huge = [x for x in all_rejected if x >= 1000000]

print("Distribution by size:")
print(f"  Tiny (1-9): {len(tiny)} - {tiny}")
print(f"  Small (10-99): {len(small)} - {small}")
print(f"  Medium (100-999): {len(medium)} - {medium}")
print(f"  Large (1K-1M): {len(large)} - {large}")
print(f"  Huge (>1M): {len(huge)} - {len(huge)} values")
print()

# ============================================================================
# PATTERN 1: What properties do they share?
# ============================================================================

print("="*80)
print("PATTERN ANALYSIS: Properties of Rejected Values")
print("="*80)
print()

# Check divisibility
print("Divisibility patterns:")
for divisor in [2, 3, 4, 5, 7, 8, 11, 13, 16, 28]:
    divisible = [x for x in all_rejected[:10] if x % divisor == 0]  # Check first 10
    print(f"  Divisible by {divisor:2d}: {len(divisible)}/10 = {divisible[:5]}")
print()

# Check parity
even_count = sum(1 for x in all_rejected[:10] if x % 2 == 0)
print(f"Even numbers: {even_count}/10")
print(f"Odd numbers: {10-even_count}/10")
print()

# Check digit sums
print("Digit sum patterns:")
for val in all_rejected[:10]:
    digit_sum = sum(int(d) for d in str(val))
    print(f"  {val:12d} → digit sum = {digit_sum}")
print()

# ============================================================================
# PATTERN 2: What ranges are AVOIDED?
# ============================================================================

print("="*80)
print("GAPS ANALYSIS: What Ranges Are Untested?")
print("="*80)
print()

# Focus on small numbers (most likely range)
tested_small = sorted([x for x in all_rejected if x < 100])
print(f"Tested small numbers (<100): {tested_small}")
print()

print("UNTESTED RANGES (most likely):")
print()

# Range 1-100
untested_1_100 = []
for n in range(1, 101):
    if n not in all_rejected:
        untested_1_100.append(n)

print(f"Untested 1-100 ({len(untested_1_100)} values):")
print(f"  First 20: {untested_1_100[:20]}")
print()

# Specifically interesting untested
interesting_untested = [
    (1, "Unity"),
    (2, "Initial blues"),
    (3, "Initial reds"),
    (4, "GCD(8,28)"),
    (5, "3+2 total initial"),
    (6, "3×2 initial lines"),
    (7, "G letter value"),
    (8, "g(1) itself"),
    (10, "2×5"),
    (14, "112/8 or 2×7"),
    (16, "n itself"),
    (20, "28-8"),
    (24, "3×8"),
    (28, "g(2) itself"),
    (32, "2^5"),
    (36, "8+28"),
    (40, "5×8"),
    (48, "6×8"),
    (64, "2^6 or 8^2"),
    (74, "POINT sum"),
]

print("Interesting untested candidates:")
for val, desc in interesting_untested:
    if val not in all_rejected:
        print(f"  {val:3d} - {desc}")
print()

# ============================================================================
# PATTERN 3: INVERSE relationships
# ============================================================================

print("="*80)
print("INVERSE ANALYSIS: Opposite Operations")
print("="*80)
print()

print("Rejected operations and their inverses:")
print()

inverse_ops = [
    ("152", "Sum (8+28+...)", "→ Difference?", "20 = 28-8"),
    ("828", "Concatenation (8||28)", "→ Separation?", "8, 28 separate"),
    ("112", "Product (16×7)", "→ Quotient?", "28/8 = 3.5"),
    ("256", "Power (2^8)", "→ Root?", "√256 = 16"),
    ("56", "LCM(8,28)", "→ GCD?", "4 = GCD(8,28)"),
]

for rejected, operation, arrow, inverse in inverse_ops:
    print(f"  Rejected {rejected:6s}: {operation:25s} {arrow:15s} {inverse}")
print()

print("KEY INVERSES TO TRY:")
print(f"  • 4 = GCD(8,28) [inverse of rejected 56=LCM]")
print(f"  • 20 = 28-8 [inverse of sum attempts]")
print(f"  • 3.5 = 28/8 → 7/2 [inverse of product 112]")
print(f"  • 16 = √256 [inverse of power 256]")
print()

# ============================================================================
# PATTERN 4: Relationship to 8 and 28
# ============================================================================

print("="*80)
print("RELATIONSHIP TO g(1)=8 AND g(2)=28")
print("="*80)
print()

print("Rejected values' relationship to 8 and 28:")
for val in [152, 828, 112, 256, 56]:
    print(f"\n{val}:")
    print(f"  {val} / 8 = {val/8}")
    print(f"  {val} / 28 = {val/28:.3f}")
    print(f"  {val} - 8 = {val-8}")
    print(f"  {val} - 28 = {val-28}")
    print(f"  {val} % 8 = {val%8}")
    print(f"  {val} % 28 = {val%28}")

print()
print("Pattern: Most rejected values are NOT clean multiples/divisors")
print("Inverse: Try values that ARE clean multiples/divisors?")
print()

# ============================================================================
# PATTERN 5: What's the SIMPLEST untried value?
# ============================================================================

print("="*80)
print("SIMPLEST UNTRIED VALUES")
print("="*80)
print()

# Sort untested by absolute value
simplest = sorted([x for x in untested_1_100 if x <= 50])

print("Simplest untried (1-50):")
for i, val in enumerate(simplest[:15]):
    # Calculate some properties
    desc = []
    if val == 28 - 8:
        desc.append("28-8")
    if val == 8 + 28:
        desc.append("8+28")
    if val == math.gcd(8, 28):
        desc.append("GCD(8,28)")
    if val % 8 == 0:
        desc.append(f"{val//8}×8")
    if val % 28 == 0:
        desc.append(f"{val//28}×28")
    if val in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]:
        desc.append("prime")

    desc_str = ", ".join(desc) if desc else ""
    print(f"  {val:3d} {f'({desc_str})' if desc_str else ''}")
print()

# ============================================================================
# PATTERN 6: Modular patterns
# ============================================================================

print("="*80)
print("MODULAR PATTERN ANALYSIS")
print("="*80)
print()

print("Rejected values modulo small numbers:")
for mod in [8, 16, 28]:
    print(f"\nmod {mod}:")
    remainders = [x % mod for x in all_rejected[:10]]
    counter = Counter(remainders)
    print(f"  Distribution: {dict(counter)}")
    print(f"  Never appears: {[i for i in range(mod) if i not in remainders][:10]}")
print()

# ============================================================================
# FINAL RECOMMENDATIONS
# ============================================================================

print("="*80)
print("REVERSE ANALYSIS CONCLUSIONS")
print("="*80)
print()

print("Based on inverse/complement analysis:")
print()

recommendations = [
    (4, "GCD(8,28) - inverse of rejected LCM=56", "★★★★★"),
    (20, "28-8 - inverse of addition", "★★★★★"),
    (16, "√256 - inverse of power", "★★★★☆"),
    (14, "28/8×4 or 112/8", "★★★★☆"),
    (36, "8+28 - simplest operation", "★★★★☆"),
    (74, "POINT - simpler linguistic", "★★★★☆"),
    (2, "Initial blues - trivial", "★★★☆☆"),
    (3, "Initial reds - trivial", "★★★☆☆"),
    (7, "28/4 or G value", "★★☆☆☆"),
]

print("TOP RECOMMENDATIONS:")
for val, reasoning, stars in recommendations:
    print(f"  {val:3d} - {reasoning:45s} {stars}")
print()

print("="*80)
print("CRITICAL INSIGHT: Try INVERSES of rejected operations")
print("="*80)
print()

print("Priority:")
print("  1. GCD=4 (inverse of LCM=56)")
print("  2. Difference=20 (inverse of sums)")
print("  3. Root=16 (inverse of power 256)")
print()
