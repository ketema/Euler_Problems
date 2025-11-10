#!/usr/bin/env python3
"""
Test if git history sequence fits a recurrence
"""

from sympy import symbols, solve

# Git history sequence
git_seq = [2, 8, 28, 184, 1644, 19068]

print("="*70)
print("TESTING GIT HISTORY SEQUENCE")
print("="*70)
print(f"\nSequence: {git_seq}")

# Test bilinear recurrence: g(n+1) = a·g(n) + b·g(n)·g(n-1) + c
print("\n" + "="*70)
print("Testing bilinear: g(n+1) = a·g(n) + b·g(n)·g(n-1) + c")
print("="*70)

a, b, c = symbols('a b c')
equations = []

# Use g(1)→g(2), g(2)→g(3), g(3)→g(4)
for i in range(1, 4):
    g_n_minus_1 = git_seq[i - 1]
    g_n = git_seq[i]
    g_n1 = git_seq[i + 1]
    eq = a * g_n + b * g_n * g_n_minus_1 + c - g_n1
    equations.append(eq)
    print(f"  g({i}) → g({i+1}): {a}·{g_n} + {b}·{g_n}·{g_n_minus_1} + {c} = {g_n1}")

solution = solve(equations, [a, b, c])

if solution:
    print(f"\n  Solution: a={solution[a]}, b={solution[b]}, c={solution[c]}")

    # Verify with g(4)→g(5)
    g4_to_5_pred = solution[a] * git_seq[4] + solution[b] * git_seq[4] * git_seq[3] + solution[c]
    print(f"\n  Verification g(4)→g(5):")
    print(f"    Predicted: {g4_to_5_pred}")
    print(f"    Actual: {git_seq[5]}")
    print(f"    Match: {'✓' if int(g4_to_5_pred) == git_seq[5] else '✗'}")

else:
    print("\n  ✗ No bilinear recurrence found!")

# Check growth ratios
print("\n" + "="*70)
print("GROWTH RATIOS")
print("="*70)
for i in range(1, len(git_seq)):
    ratio = git_seq[i] / git_seq[i-1]
    print(f"  g({i})/g({i-1}) = {ratio:.4f}")

# Compare to my recurrence prediction
my_seq = [2, 8, 28, 184, 1644, 33791]
print("\n" + "="*70)
print("COMPARISON TO MY RECURRENCE")
print("="*70)
print(f"Git g(5) = {git_seq[5]}")
print(f"My g(5)  = {my_seq[5]}")
print(f"Ratio: {git_seq[5] / my_seq[5]:.4f}")
