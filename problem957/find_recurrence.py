#!/usr/bin/env python3
"""
Systematically search for recurrence relation using SymPy
"""

from sympy import symbols, solve, simplify, Rational
from typing import List, Tuple, Optional

# Known data: g(0)=2, g(1)=8, g(2)=28, g(3)=184, g(4)=1644
data = [2, 8, 28, 184, 1644]

print("="*70)
print("SYSTEMATIC RECURRENCE RELATION SEARCH")
print("="*70)
print(f"\nKnown sequence: {data}")
print("\nTesting various recurrence forms...\n")

def test_recurrence(form_name: str, equations: List, variables: List, data: List) -> Optional[dict]:
    """
    Test a recurrence form by solving system of equations.
    Returns solution dict if found and verified, None otherwise.
    """
    print(f"\n{'='*70}")
    print(f"Testing: {form_name}")
    print(f"{'='*70}")

    try:
        solution = solve(equations, variables)

        if not solution:
            print("  ❌ No solution found")
            return None

        # Handle multiple solutions
        if isinstance(solution, list):
            solution = solution[0]

        print(f"  ✓ Found coefficients:")
        for var in variables:
            val = solution[var]
            print(f"    {var} = {val}")

        return solution

    except Exception as e:
        print(f"  ❌ Error: {e}")
        return None


# Form 1: g(n+1) = a·g(n)² + b·g(n) + c
print("\n" + "="*70)
print("FORM 1: g(n+1) = a·g(n)² + b·g(n) + c")
print("="*70)

a, b, c = symbols('a b c')
equations = []
for i in range(len(data) - 1):
    g_n = data[i]
    g_n1 = data[i + 1]
    eq = a * g_n**2 + b * g_n + c - g_n1
    equations.append(eq)

# Use first 3 equations (3 unknowns)
solution1 = test_recurrence("g(n+1) = a·g(n)² + b·g(n) + c",
                            equations[:3], [a, b, c], data)

if solution1:
    # Verify with remaining data points
    print("\n  Verification:")
    valid = True
    for i in range(len(data) - 1):
        g_n = data[i]
        predicted = solution1[a] * g_n**2 + solution1[b] * g_n + solution1[c]
        actual = data[i + 1]
        match = (predicted == actual)
        print(f"    g({i+1}) = {predicted} (actual: {actual}) {'✓' if match else '✗'}")
        if not match:
            valid = False

    if valid:
        print("\n  🎯 VALID RECURRENCE FOUND!")
        # Compute g(16)
        g_vals = data[:1]  # Start with g(0)
        for n in range(len(data) - 1, 16):
            g_n = g_vals[-1]
            g_next = solution1[a] * g_n**2 + solution1[b] * g_n + solution1[c]
            g_vals.append(int(g_next))

        print(f"\n  Computing g(5) through g(16):")
        for i in range(5, 17):
            print(f"    g({i:2d}) = {g_vals[i]}")

        print(f"\n  {'='*70}")
        print(f"  ANSWER: g(16) = {g_vals[16]}")
        print(f"  {'='*70}")


# Form 2: g(n+1) = a·g(n) + b·g(n)·g(n-1) + c
print("\n" + "="*70)
print("FORM 2: g(n+1) = a·g(n) + b·g(n)·g(n-1) + c")
print("="*70)

a2, b2, c2 = symbols('a2 b2 c2')
equations2 = []
for i in range(1, len(data) - 1):  # Start from i=1 (need g(n-1))
    g_n_minus_1 = data[i - 1]
    g_n = data[i]
    g_n1 = data[i + 1]
    eq = a2 * g_n + b2 * g_n * g_n_minus_1 + c2 - g_n1
    equations2.append(eq)

solution2 = test_recurrence("g(n+1) = a·g(n) + b·g(n)·g(n-1) + c",
                            equations2[:3], [a2, b2, c2], data)

if solution2:
    print("\n  Verification:")
    valid = True
    for i in range(1, len(data) - 1):
        g_n_minus_1 = data[i - 1]
        g_n = data[i]
        predicted = solution2[a2] * g_n + solution2[b2] * g_n * g_n_minus_1 + solution2[c2]
        actual = data[i + 1]
        match = (predicted == actual)
        print(f"    g({i+1}) = {predicted} (actual: {actual}) {'✓' if match else '✗'}")
        if not match:
            valid = False

    if valid:
        print("\n  🎯 VALID RECURRENCE FOUND!")
        # Compute g(16)
        g_vals = data[:2]  # Start with g(0), g(1)
        for n in range(2, 17):
            if n < len(data):
                g_vals.append(data[n])
            else:
                g_n_minus_1 = g_vals[-2]
                g_n = g_vals[-1]
                g_next = solution2[a2] * g_n + solution2[b2] * g_n * g_n_minus_1 + solution2[c2]
                g_vals.append(int(g_next))

        print(f"\n  Computing g(5) through g(16):")
        for i in range(5, 17):
            print(f"    g({i:2d}) = {g_vals[i]}")

        print(f"\n  {'='*70}")
        print(f"  ANSWER: g(16) = {g_vals[16]}")
        print(f"  {'='*70}")


# Form 3: g(n+1) = g(n) + a·g(n)² + b·g(n) + c (additive form)
print("\n" + "="*70)
print("FORM 3: g(n+1) = g(n) + a·g(n)² + b·g(n) + c")
print("="*70)

a3, b3, c3 = symbols('a3 b3 c3')
equations3 = []
for i in range(len(data) - 1):
    g_n = data[i]
    g_n1 = data[i + 1]
    # g(n+1) - g(n) = a·g(n)² + b·g(n) + c
    delta = g_n1 - g_n
    eq = a3 * g_n**2 + b3 * g_n + c3 - delta
    equations3.append(eq)

solution3 = test_recurrence("g(n+1) = g(n) + a·g(n)² + b·g(n) + c",
                            equations3[:3], [a3, b3, c3], data)

if solution3:
    print("\n  Verification:")
    valid = True
    for i in range(len(data) - 1):
        g_n = data[i]
        delta_pred = solution3[a3] * g_n**2 + solution3[b3] * g_n + solution3[c3]
        predicted = g_n + delta_pred
        actual = data[i + 1]
        match = (predicted == actual)
        print(f"    g({i+1}) = {predicted} (actual: {actual}) {'✓' if match else '✗'}")
        if not match:
            valid = False

    if valid:
        print("\n  🎯 VALID RECURRENCE FOUND!")
        # Compute g(16)
        g_vals = data[:1]
        for n in range(len(data) - 1, 16):
            g_n = g_vals[-1]
            delta = solution3[a3] * g_n**2 + solution3[b3] * g_n + solution3[c3]
            g_next = g_n + delta
            g_vals.append(int(g_next))

        print(f"\n  Computing g(5) through g(16):")
        for i in range(5, 17):
            print(f"    g({i:2d}) = {g_vals[i]}")

        print(f"\n  {'='*70}")
        print(f"  ANSWER: g(16) = {g_vals[16]}")
        print(f"  {'='*70}")


print("\n" + "="*70)
print("SEARCH COMPLETE")
print("="*70)
