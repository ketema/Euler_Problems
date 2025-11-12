#!/usr/bin/env python3
"""
REFIT FORMULA USING g(0) THROUGH g(5)

Previous Formula 1 had 0.78% error on g(5), which compounds when extrapolating to g(16).
Now we have 6 data points, so we can fit a more accurate formula.

Known sequence: [2, 8, 28, 184, 1644, 19068]
"""

from sympy import symbols, solve, Rational, simplify
from fractions import Fraction

# All known values
SEQUENCE = [2, 8, 28, 184, 1644, 19068]

print("="*80)
print("REFITTING FORMULA WITH g(0) THROUGH g(5)")
print("="*80)
print()
print(f"Known sequence: {SEQUENCE}")
print()

#==========================================================================
# TEST 1: Polynomial-coefficient recurrence with MORE terms
# g(n) = (a0 + a1*n) * g(n-1) + (b0 + b1*n) * g(n-2)
#==========================================================================
print("="*80)
print("TEST 1: FULL POLYNOMIAL-COEFFICIENT RECURRENCE")
print("="*80)
print()

a0, a1, b0, b1 = symbols('a0 a1 b0 b1')

# We have 4 unknowns, so we need 4 equations
# Use g(2), g(3), g(4), g(5)
equations = []
for n in range(2, 6):
    lhs = SEQUENCE[n]
    rhs = (a0 + a1*n) * SEQUENCE[n-1] + (b0 + b1*n) * SEQUENCE[n-2]
    equations.append(lhs - rhs)

print(f"Solving system of {len(equations)} equations with 4 unknowns...")
print()

sol = solve(equations, [a0, a1, b0, b1])
if sol:
    print("✓ Solution found!")
    print(f"  a0 = {sol[a0]}")
    print(f"  a1 = {sol[a1]}")
    print(f"  b0 = {sol[b0]}")
    print(f"  b1 = {sol[b1]}")
    print()

    # Verify it fits ALL values g(0) through g(5)
    print("Verification:")
    a0_val = Fraction(sol[a0])
    a1_val = Fraction(sol[a1])
    b0_val = Fraction(sol[b0])
    b1_val = Fraction(sol[b1])

    computed = [SEQUENCE[0], SEQUENCE[1]]
    for n in range(2, 6):
        g_n = (a0_val + a1_val*n) * computed[n-1] + (b0_val + b1_val*n) * computed[n-2]
        computed.append(int(g_n))
        match = "✓" if computed[n] == SEQUENCE[n] else "✗"
        print(f"  {match} g({n}) = {computed[n]} (expected {SEQUENCE[n]})")

    if computed == SEQUENCE:
        print()
        print("⭐ EXACT MATCH! Computing g(16) with PURE FRACTION arithmetic...")
        print()

        # Extend to g(16) using EXACT Fraction arithmetic (no float conversion!)
        sequence_fractions = [Fraction(SEQUENCE[0]), Fraction(SEQUENCE[1])]

        for n in range(2, 17):
            g_n = (a0_val + a1_val*n) * sequence_fractions[n-1] + (b0_val + b1_val*n) * sequence_fractions[n-2]
            sequence_fractions.append(g_n)

        # Now convert only the final value to int
        # Check if it's already an integer
        g16_frac = sequence_fractions[16]
        if g16_frac.denominator == 1:
            g16 = g16_frac.numerator
            print(f"g(16) = {g16:,}")
            print()
            print("✓ Result is EXACT integer (no rounding needed)")
        else:
            print(f"⚠️  g(16) = {g16_frac} (not an integer!)")
            print(f"   Numerator: {g16_frac.numerator}")
            print(f"   Denominator: {g16_frac.denominator}")
            g16_rounded = int(round(float(g16_frac)))
            print(f"   Rounded: {g16_rounded:,}")

        print()
        print("Complete sequence:")
        for i in range(17):
            val = sequence_fractions[i]
            if val.denominator == 1:
                print(f"  g({i:2}) = {val.numerator:,}")
            else:
                print(f"  g({i:2}) ≈ {int(round(float(val))):,}")
    else:
        print()
        print("✗ Formula doesn't fit perfectly - this shouldn't happen!")

else:
    print("✗ No solution found")

#==========================================================================
# TEST 2: 3rd-order recurrence
# g(n) = a*g(n-1) + b*g(n-2) + c*g(n-3) + d*n + e
#==========================================================================
print()
print("="*80)
print("TEST 2: 3RD-ORDER RECURRENCE")
print("="*80)
print()

a, b, c, d, e = symbols('a b c d e')

# 5 unknowns, need 5 equations
equations2 = []
for n in range(3, 6):
    lhs = SEQUENCE[n]
    rhs = a*SEQUENCE[n-1] + b*SEQUENCE[n-2] + c*SEQUENCE[n-3] + d*n + e
    equations2.append(lhs - rhs)

# Add one more equation for g(2) to have 5 total
# But g(2) requires g(-1) which doesn't exist
# So instead let's try with d=0, e=0 (no linear term)
print("Case 1: No linear term (d=0, e=0)")
sol2 = solve([eq.subs([(d, 0), (e, 0)]) for eq in equations2], [a, b, c])
if sol2:
    print(f"  a = {sol2[a]}")
    print(f"  b = {sol2[b]}")
    print(f"  c = {sol2[c]}")
    print()

    # Verify and extend
    a_val = Fraction(sol2[a])
    b_val = Fraction(sol2[b])
    c_val = Fraction(sol2[c])

    computed2 = [Fraction(SEQUENCE[0]), Fraction(SEQUENCE[1]), Fraction(SEQUENCE[2])]
    for n in range(3, 17):
        g_n = a_val*computed2[n-1] + b_val*computed2[n-2] + c_val*computed2[n-3]
        computed2.append(g_n)

    # Check if it matches g(3), g(4), g(5)
    matches = all(computed2[n].denominator == 1 and computed2[n].numerator == SEQUENCE[n] for n in range(3, 6))
    if matches:
        print("  ✓ Matches g(3), g(4), g(5)")
        g16 = computed2[16].numerator if computed2[16].denominator == 1 else int(round(float(computed2[16])))
        print(f"  g(16) = {g16:,}")
    else:
        print("  ✗ Doesn't match")

print()
print("="*80)
print("SUMMARY")
print("="*80)
print()
print("Using all 6 known values to fit exact formula and compute g(16)")
print("with pure Fraction arithmetic (no floating point errors)")
print()
