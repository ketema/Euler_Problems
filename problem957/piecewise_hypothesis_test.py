#!/usr/bin/env python3
"""
PIECEWISE RECURRENCE HYPOTHESIS TEST

AI Panel insight: "99% degeneracy at day 3→4 suggests recurrence might CHANGE at n=3 or n=4"

Multiplicity evidence:
- Day 0→1: avg = 2.00
- Day 1→2: avg = 3.30
- Day 2→3: avg = 4.62
- Day 3→4: avg = 10.32 (MORE THAN DOUBLES!)

HYPOTHESIS: Different formulas for early (n≤3) vs late (n≥4) phases

TEST: Fit formulas using ONLY g(0), g(1), g(2), g(3)
      Then check which one predicts g(4) = 1644
"""

from sympy import symbols, solve, Rational, simplify
from typing import Callable

# Known values
EARLY_PHASE = [2, 8, 28, 184]  # g(0), g(1), g(2), g(3)
TARGET = 1644  # g(4) - the first value in "late phase"

print("="*80)
print("PIECEWISE RECURRENCE HYPOTHESIS TEST")
print("="*80)
print()
print(f"Early phase (n≤3): {EARLY_PHASE}")
print(f"Target g(4) = {TARGET}")
print()

def test_recurrence(name: str, formula_func: Callable, coeffs: dict):
    """Test a recurrence formula against g(4)"""
    print(f"\nTesting: {name}")
    print(f"  Coefficients: {coeffs}")

    # Verify it fits early phase
    errors = []
    for n in range(len(EARLY_PHASE)):
        computed = formula_func(n)
        expected = EARLY_PHASE[n]
        if abs(computed - expected) > 1e-6:
            errors.append((n, computed, expected))

    if errors:
        print(f"  ✗ Doesn't fit early phase!")
        for n, comp, exp in errors:
            print(f"    g({n}) = {comp}, expected {exp}")
        return False

    print(f"  ✓ Fits early phase g(0)-g(3)")

    # Test prediction for g(4)
    g4_pred = formula_func(4)
    error = abs(g4_pred - TARGET)
    error_pct = 100 * error / TARGET

    print(f"  Prediction: g(4) = {g4_pred}")
    print(f"  Target:     g(4) = {TARGET}")
    print(f"  Error: {error} ({error_pct:.2f}%)")

    if error < 1:
        print(f"  ⭐ EXACT MATCH!")
        return True
    elif error_pct < 5:
        print(f"  ✓ Good match (< 5% error)")
        return True
    else:
        print(f"  ✗ Poor match")
        return False

#============================================================================
# TEST 1: Linear recurrence g(n) = A*g(n-1) + B*g(n-2)
#============================================================================
print("="*80)
print("TEST 1: LINEAR RECURRENCE (2nd order)")
print("="*80)

# Use g(2), g(3) to solve for A, B
# g(2) = A*g(1) + B*g(0)
# g(3) = A*g(2) + B*g(1)

A, B = symbols('A B')
eq1 = A*EARLY_PHASE[1] + B*EARLY_PHASE[0] - EARLY_PHASE[2]
eq2 = A*EARLY_PHASE[2] + B*EARLY_PHASE[1] - EARLY_PHASE[3]

sol = solve([eq1, eq2], [A, B])
if sol:
    A_val = float(sol[A])
    B_val = float(sol[B])

    def linear_rec(n):
        if n == 0: return EARLY_PHASE[0]
        if n == 1: return EARLY_PHASE[1]

        seq = EARLY_PHASE[:2]
        for i in range(2, n+1):
            next_val = A_val * seq[i-1] + B_val * seq[i-2]
            seq.append(next_val)
        return seq[n]

    test_recurrence(
        f"g(n) = {sol[A]}*g(n-1) + {sol[B]}*g(n-2)",
        linear_rec,
        {'A': sol[A], 'B': sol[B]}
    )

#============================================================================
# TEST 2: Polynomial-coefficient g(n) = (a0 + a1*n)*g(n-1) + b0*g(n-2)
#============================================================================
print()
print("="*80)
print("TEST 2: POLYNOMIAL-COEFFICIENT (b1=0)")
print("="*80)

# 3 unknowns, 2 equations - try setting one coefficient to integer
a0, a1, b0 = symbols('a0 a1 b0')

# g(2) = (a0 + a1*2)*g(1) + b0*g(0)
# g(3) = (a0 + a1*3)*g(2) + b0*g(1)

eq1 = (a0 + a1*2)*EARLY_PHASE[1] + b0*EARLY_PHASE[0] - EARLY_PHASE[2]
eq2 = (a0 + a1*3)*EARLY_PHASE[2] + b0*EARLY_PHASE[1] - EARLY_PHASE[3]

# Try b0 = 0 (no g(n-2) term)
print("\nCase: b0 = 0 (no g(n-2) term)")
sol_no_b = solve([eq1.subs(b0, 0), eq2.subs(b0, 0)], [a0, a1])
if sol_no_b:
    a0_val = float(sol_no_b[a0])
    a1_val = float(sol_no_b[a1])

    def poly_rec_no_b(n):
        if n == 0: return EARLY_PHASE[0]
        if n == 1: return EARLY_PHASE[1]

        seq = EARLY_PHASE[:2]
        for i in range(2, n+1):
            next_val = (a0_val + a1_val*i) * seq[i-1]
            seq.append(next_val)
        return seq[n]

    test_recurrence(
        f"g(n) = ({sol_no_b[a0]} + {sol_no_b[a1]}*n)*g(n-1)",
        poly_rec_no_b,
        {'a0': sol_no_b[a0], 'a1': sol_no_b[a1], 'b0': 0}
    )

# Try various integer values for b0
for b0_test in [-2, -1, 1, 2, 3, 4]:
    print(f"\nCase: b0 = {b0_test}")
    sol_b = solve([eq1.subs(b0, b0_test), eq2.subs(b0, b0_test)], [a0, a1])
    if sol_b:
        a0_val = float(sol_b[a0])
        a1_val = float(sol_b[a1])

        def make_poly_rec(a0v, a1v, b0v):
            def poly_rec(n):
                if n == 0: return EARLY_PHASE[0]
                if n == 1: return EARLY_PHASE[1]

                seq = EARLY_PHASE[:2]
                for i in range(2, n+1):
                    next_val = (a0v + a1v*i) * seq[i-1] + b0v * seq[i-2]
                    seq.append(next_val)
                return seq[n]
            return poly_rec

        test_recurrence(
            f"g(n) = ({sol_b[a0]} + {sol_b[a1]}*n)*g(n-1) + {b0_test}*g(n-2)",
            make_poly_rec(a0_val, a1_val, b0_test),
            {'a0': sol_b[a0], 'a1': sol_b[a1], 'b0': b0_test}
        )

#============================================================================
# TEST 3: Ratio-based (geometric with varying ratio)
#============================================================================
print()
print("="*80)
print("TEST 3: RATIO ANALYSIS")
print("="*80)

ratios = [EARLY_PHASE[i+1] / EARLY_PHASE[i] for i in range(len(EARLY_PHASE)-1)]
print(f"\nRatios g(n+1)/g(n): {[f'{r:.4f}' for r in ratios]}")

# Check if ratios follow linear pattern: r(n) = a + b*n
from sympy import Symbol
n_sym = Symbol('n')
a_ratio, b_ratio = symbols('a_ratio b_ratio')

# r(0) = g(1)/g(0) = ratios[0]
# r(1) = g(2)/g(1) = ratios[1]
# r(2) = g(3)/g(2) = ratios[2]

# r(n) = a + b*n
eq1_r = a_ratio + b_ratio*0 - ratios[0]
eq2_r = a_ratio + b_ratio*1 - ratios[1]

sol_r = solve([eq1_r, eq2_r], [a_ratio, b_ratio])
if sol_r:
    print(f"\nLinear ratio formula: r(n) = {sol_r[a_ratio]} + {sol_r[b_ratio]}*n")

    # Predict r(3) and g(4)
    r3_pred = float(sol_r[a_ratio] + sol_r[b_ratio]*3)
    g4_from_ratio = EARLY_PHASE[3] * r3_pred

    print(f"  Predicted r(3) = {r3_pred:.4f}")
    print(f"  Actual r(3) = {ratios[2]:.4f}" if len(ratios) > 2 else "  (Unknown r(3))")
    print(f"  Predicted g(4) = g(3) * r(3) = {EARLY_PHASE[3]} * {r3_pred:.4f} = {g4_from_ratio:.0f}")
    print(f"  Target g(4) = {TARGET}")
    print(f"  Error: {abs(g4_from_ratio - TARGET):.0f}")

print()
print("="*80)
print("SUMMARY")
print("="*80)
print()
print("Testing if recurrence fitted to g(0)-g(3) ONLY can predict g(4)")
print("If one formula succeeds, it may represent the 'early phase' pattern")
print("that changes at n=4 due to the multiplicity explosion")
print()
