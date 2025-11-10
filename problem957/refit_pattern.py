#!/usr/bin/env python3
"""
Refit pattern with actual data: 2, 8, 28, 184, 1644
"""

from sympy import symbols, solve, simplify, Rational

sequence = [2, 8, 28, 184, 1644]

print("="*70)
print("Actual sequence:", sequence)
print("="*70)

print("\nDifferences:")
diffs = [sequence[i+1] - sequence[i] for i in range(len(sequence)-1)]
print(f"  Δg: {diffs}")

ratios = [sequence[i+1] / sequence[i] for i in range(len(sequence)-1)]
print(f"\nRatios g(n+1)/g(n): {[f'{r:.4f}' for r in ratios]}")

# Try quartic fit
print("\n" + "="*70)
print("Trying quartic: g(t) = a·t⁴ + b·t³ + c·t² + d·t + e")
print("="*70)

t = symbols('t')
a, b, c, d, e = symbols('a b c d e')

equations = []
for i, val in enumerate(sequence):
    eq = a*i**4 + b*i**3 + c*i**2 + d*i + e - val
    equations.append(eq)

solution = solve(equations, [a, b, c, d, e])

if solution:
    print(f"\nFound coefficients:")
    print(f"  a = {solution[a]}")
    print(f"  b = {solution[b]}")
    print(f"  c = {solution[c]}")
    print(f"  d = {solution[d]}")
    print(f"  e = {solution[e]}")

    formula = solution[a]*t**4 + solution[b]*t**3 + solution[c]*t**2 + solution[d]*t + solution[e]
    print(f"\ng(t) = {formula}")

    # Simplify
    formula_simplified = simplify(formula)
    print(f"\nSimplified: g(t) = {formula_simplified}")

    # Verify
    print("\nVerification:")
    for i in range(len(sequence)):
        pred = formula.subs(t, i)
        print(f"  g({i}) = {pred} (actual: {sequence[i]}) {'✓' if pred == sequence[i] else '✗'}")

    # Predict g(5) through g(16)
    print("\n" + "="*70)
    print("PREDICTIONS")
    print("="*70)

    predictions = []
    for i in range(len(sequence), 17):
        pred = formula.subs(t, i)
        predictions.append(int(pred))
        print(f"  g({i:2d}) = {int(pred)}")

    print("\n" + "="*70)
    print(f"ANSWER: g(16) = {predictions[-1]}")
    print("="*70)

    # Check growth rate
    print("\nGrowth analysis:")
    all_vals = sequence + predictions
    for i in range(1, min(10, len(all_vals))):
        ratio = all_vals[i] / all_vals[i-1]
        print(f"  g({i})/g({i-1}) = {ratio:.4f}")
else:
    print("No quartic solution found!")

# Try to find exponential-like pattern
print("\n" + "="*70)
print("Checking for exponential growth")
print("="*70)

# If g(n) ≈ C · r^n, then log(g(n)) ≈ log(C) + n·log(r)
import math
log_vals = [math.log(v) for v in sequence]
print("\nlog(g(n)):", [f'{v:.4f}' for v in log_vals])

# Check if log values are approximately linear
log_diffs = [log_vals[i+1] - log_vals[i] for i in range(len(log_vals)-1)]
print("Δlog(g(n)):", [f'{d:.4f}' for d in log_diffs])
print("(If constant → exponential growth)")
