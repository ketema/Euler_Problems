#!/usr/bin/env python3
"""
Deep analysis of the specific numbers g(1)=8 and g(2)=28.

What's special about these numbers?
What patterns or relationships could lead to g(16)?
"""

import math

print("="*70)
print("ANALYZING g(1)=8 AND g(2)=28")
print("="*70)
print()

# ============================================================================
# NUMBER PROPERTIES
# ============================================================================

print("="*70)
print("PROPERTIES OF 8 AND 28")
print("="*70)
print()

print("8:")
print(f"  • 2³ = {2**3}")
print(f"  • 2 × 4 = {2*4}")
print(f"  • Perfect cube")
print(f"  • Fibonacci number F₆ = {8}")
print(f"  • Even")
print()

print("28:")
print(f"  • 4 × 7 = {4*7}")
print(f"  • 2² × 7 = {2**2 * 7}")
print(f"  • PERFECT NUMBER (sum of proper divisors)")
divisors_28 = [1, 2, 4, 7, 14]
print(f"  • Divisors: {divisors_28}")
print(f"  • Sum of divisors: {sum(divisors_28)} = 28 ✓")
print(f"  • Triangular number: T₇ = 1+2+3+4+5+6+7 = {sum(range(1,8))}")
print(f"  • Even")
print()

# ============================================================================
# PERFECT NUMBER INSIGHT
# ============================================================================

print("="*70)
print("PERFECT NUMBER INSIGHT!")
print("="*70)
print()

print("28 is the 2nd PERFECT NUMBER!")
print("Perfect numbers: 6, 28, 496, 8128, ...")
print()

print("Perfect numbers have form: 2^(p-1) × (2^p - 1)")
print("where 2^p - 1 is a Mersenne prime")
print()

print("  6 = 2¹ × (2² - 1) = 2 × 3")
print(" 28 = 2² × (2³ - 1) = 4 × 7")
print("496 = 2⁴ × (2⁵ - 1) = 16 × 31")
print()

print("Could g(n) be related to perfect numbers?")
print()

# But 8 is NOT a perfect number...
print("But 8 is NOT a perfect number")
print("  8's divisors: 1, 2, 4")
print(f"  Sum: {1+2+4} ≠ 8")
print()

# ============================================================================
# TRIANGULAR NUMBERS
# ============================================================================

print("="*70)
print("TRIANGULAR NUMBERS")
print("="*70)
print()

print("28 is a triangular number: T₇ = 1+2+...+7 = 28")
print()

# Is 8 triangular?
# T_n = n(n+1)/2
# 8 = n(n+1)/2 → n² + n - 16 = 0
# n = (-1 + √65)/2 ≈ 3.53... (not integer)

print("Is 8 triangular?")
print("  8 = n(n+1)/2 → n ≈ 3.53")
print("  ✗ Not a triangular number")
print()

print("But:")
print("  T₃ = 6")
print("  T₄ = 10")
print("  8 is between T₃ and T₄")
print()

# ============================================================================
# RELATIONSHIP BETWEEN 8 AND 28
# ============================================================================

print("="*70)
print("RELATIONSHIPS")
print("="*70)
print()

print(f"28 / 8 = {28/8} = 3.5")
print(f"28 - 8 = {28-8} = 20")
print(f"28 + 8 = {28+8} = 36 = 6²")
print(f"28 × 8 = {28*8} = 224 = 2⁵ × 7")
print(f"lcm(8, 28) = {math.lcm(8, 28)} = 56")
print(f"gcd(8, 28) = {math.gcd(8, 28)} = 4")
print()

# ============================================================================
# SEQUENCE EXPLORATION
# ============================================================================

print("="*70)
print("WHAT IF g(1), g(2) ARE IN A SPECIAL SEQUENCE?")
print("="*70)
print()

# Perfect numbers sequence
perfect_nums = [6, 28, 496, 8128]
print(f"Perfect numbers: {perfect_nums}")
print(f"  → g(16) = 8128? (4th perfect number)")
print()

# Triangular numbers
triangular = [n*(n+1)//2 for n in range(1, 20)]
print(f"Triangular numbers: {triangular[:10]}")
print(f"  T₇ = 28 ✓")
print(f"  T₁₆ = {16*17//2} = 136")
print()

# Powers of 2
powers_of_2 = [2**n for n in range(1, 20)]
print(f"Powers of 2: {powers_of_2[:10]}")
print(f"  2³ = 8 ✓")
print(f"  2¹⁶ = {2**16} = 65,536")
print()

# Fibonacci
fib = [1, 1]
for i in range(2, 20):
    fib.append(fib[-1] + fib[-2])
print(f"Fibonacci: {fib[:12]}")
print(f"  F₆ = 8 ✓")
print(f"  F₁₆ = {fib[15]} = 987")
print()

# ============================================================================
# COMBINATORIAL INTERPRETATION
# ============================================================================

print("="*70)
print("COMBINATORIAL INTERPRETATION")
print("="*70)
print()

# C(n, k) = n! / (k! × (n-k)!)
print("Binomial coefficients:")
for n in range(1, 10):
    for k in range(n+1):
        val = math.comb(n, k)
        if val == 8:
            print(f"  C({n},{k}) = 8 ← MATCH g(1)!")
        if val == 28:
            print(f"  C({n},{k}) = 28 ← MATCH g(2)!")
print()

# C(8,5) = 56, C(8,3) = 56
# C(8,4) = 70
# C(7,3) = 35
# C(7,4) = 35
# Hmm...

print("C(16, k) values:")
for k in range(17):
    val = math.comb(16, k)
    if k <= 8:
        print(f"  C(16, {k}) = {val}")
print()

# ============================================================================
# KEY INSIGHT: PERFECT NUMBERS
# ============================================================================

print("="*70)
print("KEY INSIGHT")
print("="*70)
print()

print("28 is the 2nd perfect number.")
print("Perfect numbers are RARE and SPECIAL.")
print()

print("Could the problem encode perfect numbers?")
print("  g(1) = 8 (not perfect, but 2³)")
print("  g(2) = 28 (2nd perfect number)")
print("  g(16) = ??? (related to 16th something?)")
print()

print("Perfect number sequence:")
print(f"  1st: 6")
print(f"  2nd: 28 ← g(2)")
print(f"  3rd: 496")
print(f"  4th: 8128")
print()

print("Is g(16) = 8128? (4th perfect number)")
print()

# ============================================================================
# OEIS SEARCH
# ============================================================================

print("="*70)
print("SEQUENCES WITH 8, 28")
print("="*70)
print()

print("Common sequences starting 8, 28:")
print("  • 8, 28, 56, 92, ... (not clear)")
print("  • Need to search OEIS for '8, 28'")
print()

print("CANDIDATES FOR g(16):")
print("-"*70)
print("  1. 8128 (4th perfect number)")
print("  2. 987 (F₁₆, Fibonacci)")
print("  3. 136 (T₁₆, triangular)")
print("  4. 65536 (2¹⁶, power of 2)")
print()
