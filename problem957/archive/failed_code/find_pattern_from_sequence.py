#!/usr/bin/env python3
"""
Find pattern from sequence: 2, 8, 28, 184
"""

from sympy import symbols, solve, simplify, factorial, binomial, Rational
from itertools import combinations

sequence = [2, 8, 28, 184]

print("Sequence:", sequence)
print("\nDifferences:")
diffs = [sequence[i+1] - sequence[i] for i in range(len(sequence)-1)]
print(f"  Δg: {diffs}")

second_diffs = [diffs[i+1] - diffs[i] for i in range(len(diffs)-1)]
print(f"  Δ²g: {second_diffs}")

third_diffs = [second_diffs[i+1] - second_diffs[i] for i in range(len(second_diffs)-1)]
print(f"  Δ³g: {third_diffs}")

print("\nRatios:")
ratios = [sequence[i+1] / sequence[i] for i in range(len(sequence)-1)]
print(f"  g(n+1)/g(n): {[f'{r:.4f}' for r in ratios]}")

diff_ratios = [diffs[i+1] / diffs[i] for i in range(len(diffs)-1)]
print(f"  Δg(n+1)/Δg(n): {[f'{r:.4f}' for r in diff_ratios]}")

# Try to search OEIS
print("\n" + "="*70)
print("Searching OEIS for sequence...")
print("="*70)

try:
    from oeis import oeis
    results = oeis.search(sequence)
    if results:
        print(f"Found {len(results)} matches in OEIS:")
        for i, entry in enumerate(results[:3]):
            print(f"\n{i+1}. {entry.name}")
            print(f"   ID: {entry.id}")
            if hasattr(entry, 'formulas') and entry.formulas:
                print(f"   Formula: {entry.formulas[0] if entry.formulas else 'None'}")
    else:
        print("No matches found in OEIS")
except Exception as e:
    print(f"OEIS search failed: {e}")

# Try polynomial fits of increasing degree
print("\n" + "="*70)
print("Trying polynomial fits...")
print("="*70)

t = symbols('t')

# Try cubic
print("\nTrying cubic: g(t) = a·t³ + b·t² + c·t + d")
a, b, c, d = symbols('a b c d')
equations = []
for i, val in enumerate(sequence):
    eq = a*i**3 + b*i**2 + c*i + d - val
    equations.append(eq)

solution = solve(equations, [a, b, c, d])
if solution:
    print(f"  Found: a={solution[a]}, b={solution[b]}, c={solution[c]}, d={solution[d]}")
    formula = solution[a]*t**3 + solution[b]*t**2 + solution[c]*t + solution[d]
    print(f"  g(t) = {formula}")

    # Verify
    print("\n  Verification:")
    for i in range(len(sequence)):
        pred = formula.subs(t, i)
        print(f"    g({i}) = {pred} {'✓' if pred == sequence[i] else '✗'}")

    # Predict next values
    print("\n  Predictions:")
    for i in range(4, 8):
        pred = formula.subs(t, i)
        print(f"    g({i}) = {pred}")

# Try quartic if needed
if not solution or solution[a] != 0:
    print("\nTrying quartic: g(t) = a·t⁴ + b·t³ + c·t² + d·t + e")
    # Would need more data points

# Check for recurrence relation
print("\n" + "="*70)
print("Checking for linear recurrence...")
print("="*70)

# Try: g(n) = a·g(n-1) + b·g(n-2) + c
if len(sequence) >= 4:
    # g(2) = a·g(1) + b·g(0) + c
    # g(3) = a·g(2) + b·g(1) + c
    # Two equations, three unknowns - try different values

    # Let's try c=0 first
    # 28 = a·8 + b·2
    # 184 = a·28 + b·8

    # From first: b = (28 - 8a)/2 = 14 - 4a
    # Substitute: 184 = 28a + 8(14-4a) = 28a + 112 - 32a = 112 - 4a
    # => 4a = 112 - 184 = -72 => a = -18

    # Check: a=-18, b=14-4(-18)=14+72=86
    a_val = -18
    b_val = 86
    print(f"Trying: g(n) = {a_val}·g(n-1) + {b_val}·g(n-2)")

    # Verify
    for i in range(2, len(sequence)):
        pred = a_val * sequence[i-1] + b_val * sequence[i-2]
        print(f"  g({i}) = {a_val}·{sequence[i-1]} + {b_val}·{sequence[i-2]} = {pred} (actual: {sequence[i]}) {'✓' if pred == sequence[i] else '✗'}")
