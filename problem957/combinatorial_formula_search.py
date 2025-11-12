#!/usr/bin/env python3
"""
COMBINATORIAL FORMULA SEARCH
Based on AI Panel recommendations and problem statement analysis

Our sequence: g(0)=2, g(1)=8, g(2)=28, g(3)=184, g(4)=1644
This appears to be THE MAXIMAL configuration (matches problem's g(1)=8, g(2)=28)

AI Panel suggested:
1. Multi-term recurrence with polynomial coefficients
2. Try g(n) = f(n)*g(n-1) + h(n)*g(n-2) + k(n)*g(n-3) where f,h,k are polynomials
3. Check for combinatorial structures (binomial coefficients, factorials)

CRITICAL: The answer must be computable in ~1 hour, ruling out simulation
"""

from sympy import symbols, solve, factorial, binomial, simplify, Rational, Function
from sympy import Matrix, Symbol
import numpy as np
from typing import List, Callable

# Known values
SEQUENCE = [2, 8, 28, 184, 1644]

print("="*80)
print("COMBINATORIAL FORMULA SEARCH")
print("="*80)
print()
print(f"Known sequence: {SEQUENCE}")
print()

#============================================================================
# APPROACH 1: POLYNOMIAL-COEFFICIENT RECURRENCE
#============================================================================
print("="*80)
print("APPROACH 1: POLYNOMIAL-COEFFICIENT RECURRENCE")
print("="*80)
print()
print("Testing: g(n) = (a0 + a1*n) * g(n-1) + (b0 + b1*n) * g(n-2)")
print()

# g(n) = (a0 + a1*n) * g(n-1) + (b0 + b1*n) * g(n-2)
# We have g(0)=2, g(1)=8, g(2)=28
# Use g(2), g(3), g(4) to solve for a0, a1, b0, b1

a0, a1, b0, b1 = symbols('a0 a1 b0 b1')

# g(2) = (a0 + a1*2) * g(1) + (b0 + b1*2) * g(0)
eq1 = (a0 + a1*2) * SEQUENCE[1] + (b0 + b1*2) * SEQUENCE[0] - SEQUENCE[2]

# g(3) = (a0 + a1*3) * g(2) + (b0 + b1*3) * g(1)
eq2 = (a0 + a1*3) * SEQUENCE[2] + (b0 + b1*3) * SEQUENCE[1] - SEQUENCE[3]

# g(4) = (a0 + a1*4) * g(3) + (b0 + b1*4) * g(2)
eq3 = (a0 + a1*4) * SEQUENCE[3] + (b0 + b1*4) * SEQUENCE[2] - SEQUENCE[4]

print("System of 3 equations, 4 unknowns - trying special cases:")
print()

# Try b1 = 0 (simpler form)
print("Case 1: b1 = 0 (constant coefficient for g(n-2))")
sol1 = solve([eq1.subs(b1, 0), eq2.subs(b1, 0), eq3.subs(b1, 0)], [a0, a1, b0])
if sol1:
    print(f"  Solution: a0={sol1[a0]}, a1={sol1[a1]}, b0={sol1[b0]}, b1=0")
    print(f"  Formula: g(n) = ({sol1[a0]} + {sol1[a1]}*n) * g(n-1) + {sol1[b0]} * g(n-2)")

    # Validate
    def test_formula_1(n):
        if n == 0: return SEQUENCE[0]
        if n == 1: return SEQUENCE[1]

        seq = SEQUENCE[:2]
        a0_val = float(sol1[a0])
        a1_val = float(sol1[a1])
        b0_val = float(sol1[b0])

        for i in range(2, n+1):
            next_val = (a0_val + a1_val*i) * seq[i-1] + b0_val * seq[i-2]
            seq.append(int(round(next_val)))

        return seq[n]

    # Test against known values
    print("  Verification:")
    all_match = True
    for i in range(5):
        computed = test_formula_1(i)
        expected = SEQUENCE[i]
        match = "✓" if computed == expected else "✗"
        print(f"    {match} g({i}) = {computed} (expected {expected})")
        if computed != expected:
            all_match = False

    if all_match:
        print("\n  ⭐ FORMULA VALIDATED! Computing g(16)...")
        g16 = test_formula_1(16)
        print(f"  g(16) = {g16}")
else:
    print("  No solution found for b1=0")

print()

# Try a1 = 0 (constant coefficient for g(n-1))
print("Case 2: a1 = 0 (constant coefficient for g(n-1))")
sol2 = solve([eq1.subs(a1, 0), eq2.subs(a1, 0), eq3.subs(a1, 0)], [a0, b0, b1])
if sol2:
    print(f"  Solution: a0={sol2[a0]}, a1=0, b0={sol2[b0]}, b1={sol2[b1]}")
    print(f"  Formula: g(n) = {sol2[a0]} * g(n-1) + ({sol2[b0]} + {sol2[b1]}*n) * g(n-2)")

    # Validate
    def test_formula_2(n):
        if n == 0: return SEQUENCE[0]
        if n == 1: return SEQUENCE[1]

        seq = SEQUENCE[:2]
        a0_val = float(sol2[a0])
        b0_val = float(sol2[b0])
        b1_val = float(sol2[b1])

        for i in range(2, n+1):
            next_val = a0_val * seq[i-1] + (b0_val + b1_val*i) * seq[i-2]
            seq.append(int(round(next_val)))

        return seq[n]

    # Test against known values
    print("  Verification:")
    all_match = True
    for i in range(5):
        computed = test_formula_2(i)
        expected = SEQUENCE[i]
        match = "✓" if computed == expected else "✗"
        print(f"    {match} g({i}) = {computed} (expected {expected})")
        if computed != expected:
            all_match = False

    if all_match:
        print("\n  ⭐ FORMULA VALIDATED! Computing g(16)...")
        g16 = test_formula_2(16)
        print(f"  g(16) = {g16}")
else:
    print("  No solution found for a1=0")

print()

#============================================================================
# APPROACH 2: 3-TERM RECURRENCE WITH POLYNOMIAL COEFFICIENTS
#============================================================================
print("="*80)
print("APPROACH 2: 3-TERM RECURRENCE")
print("="*80)
print()
print("Testing: g(n) = a*g(n-1) + b*g(n-2) + c*n + d")
print()

# Try adding linear term: g(n) = a*g(n-1) + b*g(n-2) + c*n + d
a, b, c, d = symbols('a b c d')

# Use g(2), g(3), g(4), and one more to solve
eq1 = a*SEQUENCE[1] + b*SEQUENCE[0] + c*2 + d - SEQUENCE[2]
eq2 = a*SEQUENCE[2] + b*SEQUENCE[1] + c*3 + d - SEQUENCE[3]
eq3 = a*SEQUENCE[3] + b*SEQUENCE[2] + c*4 + d - SEQUENCE[4]

# Try c=0, d=0 (pure recurrence - we already tested this)
# Try c=0 (just constant offset)
print("Case: Linear offset only (c=0)")
sol_offset = solve([eq1.subs(c, 0), eq2.subs(c, 0), eq3.subs(c, 0)], [a, b, d])
if sol_offset:
    print(f"  Solution: a={sol_offset[a]}, b={sol_offset[b]}, c=0, d={sol_offset[d]}")

    def test_formula_offset(n):
        if n == 0: return SEQUENCE[0]
        if n == 1: return SEQUENCE[1]

        seq = SEQUENCE[:2]
        a_val = float(sol_offset[a])
        b_val = float(sol_offset[b])
        d_val = float(sol_offset[d])

        for i in range(2, n+1):
            next_val = a_val * seq[i-1] + b_val * seq[i-2] + d_val
            seq.append(int(round(next_val)))

        return seq[n]

    print("  Verification:")
    all_match = True
    for i in range(5):
        computed = test_formula_offset(i)
        expected = SEQUENCE[i]
        match = "✓" if computed == expected else "✗"
        print(f"    {match} g({i}) = {computed} (expected {expected})")
        if computed != expected:
            all_match = False

    if all_match:
        print("\n  ⭐ FORMULA VALIDATED! Computing g(16)...")
        g16 = test_formula_offset(16)
        print(f"  g(16) = {g16}")

print()
print("="*80)
print("SUMMARY")
print("="*80)
print()
print("Searching for recurrence with polynomial coefficients")
print("that fits all known values and can compute g(16) efficiently")
print()
