#!/usr/bin/env python3
"""
SYSTEMATIC PATTERN SEARCH
AI Panel Recommendation: Test recurrences and combinatorial formulas using ALL known values

Known sequence: g(0)=2, g(1)=8, g(2)=28, g(3)=184, g(4)=1644
OEIS search: NO MATCH - sequence is unique to Project Euler 957

Test-first validation: Any formula MUST reproduce all 5 known values
"""

from sympy import symbols, solve, factorial, binomial, simplify, Rational, lambdify
from sympy import Function, rsolve, Eq
import numpy as np
from typing import List, Callable, Optional

# Known values
SEQUENCE = [2, 8, 28, 184, 1644]

def validate_formula(formula_func: Callable[[int], int], name: str) -> bool:
    """Test formula against all known values"""
    print(f"\nTesting: {name}")
    errors = []
    for n, expected in enumerate(SEQUENCE):
        try:
            computed = formula_func(n)
            if isinstance(computed, (int, float)):
                computed = int(round(computed))

            if computed == expected:
                print(f"  ✓ g({n}) = {computed} (correct)")
            else:
                print(f"  ✗ g({n}) = {computed}, expected {expected} (error: {abs(computed-expected)})")
                errors.append(abs(computed - expected))
        except Exception as e:
            print(f"  ✗ g({n}): Error - {e}")
            return False

    if not errors:
        print(f"  ✅ PERFECT MATCH - All values correct!")
        return True
    else:
        print(f"  ❌ Total error: {sum(errors)}")
        return False

#============================================================================
# TEST 1: LINEAR RECURRENCES (Higher Order)
#============================================================================
print("="*80)
print("TEST 1: LINEAR RECURRENCES")
print("="*80)

# Order-3 recurrence: g(n) = A*g(n-1) + B*g(n-2) + C*g(n-3)
print("\nOrder-3 Linear Recurrence: g(n) = A*g(n-1) + B*g(n-2) + C*g(n-3)")

if len(SEQUENCE) >= 4:
    # Use g(0), g(1), g(2) as initial, solve for A,B,C using g(3), g(4)
    A, B, C = symbols('A B C')

    # g(3) = A*g(2) + B*g(1) + C*g(0)
    # g(4) = A*g(3) + B*g(2) + C*g(1)

    # Only 2 equations, 3 unknowns - try assuming C=0 first
    eq1 = Eq(SEQUENCE[3], A*SEQUENCE[2] + B*SEQUENCE[1] + C*SEQUENCE[0])
    eq2 = Eq(SEQUENCE[4], A*SEQUENCE[3] + B*SEQUENCE[2] + C*SEQUENCE[1])

    # Try C=0
    print("\nAssuming C=0:")
    sol_c0 = solve([eq1.subs(C, 0), eq2.subs(C, 0)], [A, B])
    if sol_c0:
        print(f"  Solution: A={sol_c0[A]}, B={sol_c0[B]}, C=0")

        # Validate
        def recurrence_order2_c0(n):
            if n == 0: return SEQUENCE[0]
            if n == 1: return SEQUENCE[1]
            if n == 2: return SEQUENCE[2]

            a_val = float(sol_c0[A])
            b_val = float(sol_c0[B])

            seq = SEQUENCE[:3]
            for i in range(3, n+1):
                next_val = a_val * seq[i-1] + b_val * seq[i-2]
                seq.append(int(round(next_val)))
            return seq[n]

        if validate_formula(recurrence_order2_c0, f"g(n) = {sol_c0[A]}*g(n-1) + {sol_c0[B]}*g(n-2)"):
            print("\n⭐ RECURRENCE FOUND!")
            print(f"   g(n) = {sol_c0[A]}*g(n-1) + {sol_c0[B]}*g(n-2)")
            print(f"   with g(0)={SEQUENCE[0]}, g(1)={SEQUENCE[1]}, g(2)={SEQUENCE[2]}")

# Try general order-3
print("\nGeneral order-3 (need more constraints):")
# We have 5 values, need 3 for initial conditions, leaves 2 equations for 3 unknowns
# Try various assumptions

#============================================================================
# TEST 2: CLOSED-FORM POLYNOMIAL (Already tested - failed for g(16))
#============================================================================
print("\n" + "="*80)
print("TEST 2: POLYNOMIAL CLOSED FORM")
print("="*80)
print("Already tested: degree-4 polynomial fits g(0)-g(4) exactly")
print("BUT: g(16) = 1,973,818 was REJECTED by Project Euler")
print("Conclusion: Polynomial doesn't hold beyond fitted points")

#============================================================================
# TEST 3: COMBINATORIAL PATTERNS (Binomial coefficients, factorials)
#============================================================================
print("\n" + "="*80)
print("TEST 3: COMBINATORIAL PATTERNS")
print("="*80)

# Test if g(n) = A * C(B*n + C, 2) + D
print("\nPattern: g(n) = A * C(k*n + m, 2) + D")

# g(n) = A * (k*n + m)*(k*n + m - 1)/2 + D
# With 4 unknowns, use first 4 values to solve
from sympy import Symbol
k_sym, m_sym, A_sym, D_sym = symbols('k m A D')

# Build equations
eqs_comb = []
for n, g_n in enumerate(SEQUENCE[:4]):
    expr = A_sym * (k_sym*n + m_sym)*(k_sym*n + m_sym - 1)/2 + D_sym
    eqs_comb.append(Eq(expr, g_n))

print("  Solving system for A, k, m, D...")
try:
    sol_comb = solve(eqs_comb, [A_sym, k_sym, m_sym, D_sym])
    if sol_comb:
        for sol in (sol_comb if isinstance(sol_comb, list) else [sol_comb]):
            print(f"\n  Solution: A={sol[A_sym]}, k={sol[k_sym]}, m={sol[m_sym]}, D={sol[D_sym]}")

            def comb_pattern(n):
                A_val = float(sol[A_sym])
                k_val = float(sol[k_sym])
                m_val = float(sol[m_sym])
                D_val = float(sol[D_sym])

                return int(round(A_val * (k_val*n + m_val)*(k_val*n + m_val - 1)/2 + D_val))

            if validate_formula(comb_pattern, f"g(n) = {sol[A_sym]}*C({sol[k_sym]}n+{sol[m_sym]},2) + {sol[D_sym]}"):
                print("\n⭐ COMBINATORIAL FORMULA FOUND!")
    else:
        print("  No solution found")
except Exception as e:
    print(f"  Solving failed: {e}")

#============================================================================
# TEST 4: DIFFERENCES AND RATIOS ANALYSIS
#============================================================================
print("\n" + "="*80)
print("TEST 4: SEQUENCE ANALYSIS")
print("="*80)

print("\nSequence:", SEQUENCE)

# First differences
diffs1 = [SEQUENCE[i+1] - SEQUENCE[i] for i in range(len(SEQUENCE)-1)]
print(f"1st differences: {diffs1}")

# Second differences
diffs2 = [diffs1[i+1] - diffs1[i] for i in range(len(diffs1)-1)]
print(f"2nd differences: {diffs2}")

# Third differences
diffs3 = [diffs2[i+1] - diffs2[i] for i in range(len(diffs2)-1)]
print(f"3rd differences: {diffs3}")

# Ratios
ratios = [SEQUENCE[i+1] / SEQUENCE[i] for i in range(len(SEQUENCE)-1)]
print(f"\nRatios g(n+1)/g(n): {[f'{r:.4f}' for r in ratios]}")

# Check for geometric growth
ratio_diffs = [ratios[i+1] - ratios[i] for i in range(len(ratios)-1)]
print(f"Ratio changes: {[f'{r:.4f}' for r in ratio_diffs]}")

#============================================================================
# TEST 5: FACTORIZATIONS (Look for structure)
#============================================================================
print("\n" + "="*80)
print("TEST 5: FACTORIZATIONS")
print("="*80)

from sympy import factorint

for n, val in enumerate(SEQUENCE):
    factors = factorint(val)
    print(f"g({n}) = {val} = {factors}")

print()
print("="*80)
print("SUMMARY")
print("="*80)
print()
print("If no formula found:")
print("  → Sequence requires deeper geometric/combinatorial insight")
print("  → May need to analyze the STRUCTURE of line intersections")
print("  → Consider: degeneracy patterns, multiplicity distributions")
print("  → Review Project Euler 957 problem statement for hidden constraints")
print()
