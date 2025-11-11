#!/usr/bin/env python3
"""
PROMPT 2: Algebraic Polynomial/Recurrence Fitting (EXACT ARITHMETIC VERSION)
DETERMINISTIC execution using SymPy Rational arithmetic ONLY

Objective: Find closed-form expression or recurrence for g(n)
Tools: SymPy interpolate, rsolve, solve with Rational arithmetic
Output: Exact formulas/recurrences that fit all known values

AI Panel Critique Applied:
- ✅ Replaced NumPy polyfit with SymPy interpolate
- ✅ Replaced NumPy lstsq with SymPy solve
- ✅ All validation uses exact arithmetic (no float tolerances)
- ✅ Removed non-deterministic PySR
"""

from sympy import (
    symbols, Rational, simplify, solve, rsolve, Function,
    interpolate, lambdify, Integer, Poly
)
from sympy.abc import n as n_sym, x
import json
from typing import List, Dict, Optional

# Verified data from PROMPT 1 - converted to SymPy Rational
VERIFIED_SEQUENCE = [Rational(2), Rational(8), Rational(28), Rational(184), Rational(1644)]
N_VALUES = [Rational(i) for i in range(5)]

def print_section(title: str):
    """Print formatted section header"""
    print("=" * 80)
    print(title)
    print("=" * 80)
    print()

def test_sympy_polynomial_fits() -> Dict:
    """Test polynomial fits using SymPy exact Lagrange interpolation"""
    print_section("STEP 1: EXACT POLYNOMIAL FITTING (SymPy)")

    results = {}

    # SymPy interpolate uses Lagrange interpolation (exact for n points, degree n-1)
    # We have 5 points, so degree 4 polynomial is exact

    print("Using SymPy Lagrange interpolation for exact polynomial fit:")
    print()

    # Create interpolating polynomial
    data_points = list(zip(N_VALUES, VERIFIED_SEQUENCE))
    poly_expr = interpolate(data_points, x)

    print(f"Interpolating polynomial P(x):")
    print(f"  {poly_expr}")
    print()

    # Verify on known values
    print("Verification on known values:")
    all_exact = True
    for i, (n_val, g_val) in enumerate(data_points):
        predicted = poly_expr.subs(x, n_val)
        match = "✓" if predicted == g_val else "✗"
        if predicted != g_val:
            all_exact = False
        print(f"  P({n_val}) = {predicted}, expected {g_val} {match}")
    print()

    if all_exact:
        print("✓ Polynomial fits all known values EXACTLY")
    else:
        print("✗ Polynomial does not fit exactly (unexpected)")
    print()

    # Evaluate at n=5..16
    print("Extended predictions (n=5..16):")
    extended_predictions = {}
    all_integers = True
    all_positive = True

    for i in range(5, 17):
        val = poly_expr.subs(x, Rational(i))
        val_simplified = simplify(val)

        # Check if it's an integer (Rational with denominator=1)
        is_int = val_simplified.is_integer if hasattr(val_simplified, 'is_integer') else False
        is_pos = val_simplified > 0 if hasattr(val_simplified, '__gt__') else True

        if not is_int:
            all_integers = False
        if not is_pos:
            all_positive = False

        extended_predictions[i] = {
            "value": val_simplified,
            "is_integer": is_int,
            "is_positive": is_pos
        }

        print(f"  P({i}) = {val_simplified} {'✓' if is_int and is_pos else '✗'}")

    print()

    # Extract g(16)
    g_16 = extended_predictions.get(16, {}).get("value")
    g_16_int = int(g_16) if g_16 and g_16.is_integer else None

    results["lagrange_interpolation"] = {
        "polynomial": str(poly_expr),
        "exact_fit": all_exact,
        "all_integers_5_to_16": all_integers,
        "all_positive": all_positive,
        "g_16": g_16_int,
        "extended_predictions": {str(k): str(v["value"]) for k, v in extended_predictions.items()}
    }

    print(f"Result summary:")
    if all_integers and all_positive and g_16_int:
        print(f"  ✓ Valid integer solution: g(16) = {g_16_int}")
    else:
        print(f"  ✗ Not all values are positive integers")
    print()

    return results

def test_sympy_recurrence_relations() -> Dict:
    """Test linear recurrence relations using SymPy exact solve"""
    print_section("STEP 2: EXACT RECURRENCE RELATIONS (SymPy)")

    results = {}

    # Define symbolic variables for coefficients
    # g(n) = c0*g(n-1) + c1*g(n-2) + ... + c_{k-1}*g(n-k) + d

    for order in range(2, 5):
        print(f"Testing order-{order} recurrence:")
        print(f"  g(n) = c₀·g(n-1) + c₁·g(n-2) + ... + c_{order-1}·g(n-{order}) + d")
        print()

        if len(VERIFIED_SEQUENCE) < order + 2:
            print(f"  ✗ Not enough data points (need {order+2}, have {len(VERIFIED_SEQUENCE)})")
            print()
            continue

        # Create symbolic coefficients
        coeffs = symbols(f'c0:{order}') if order > 1 else (symbols('c0'),)
        d = symbols('d')

        # Set up equations from known values
        equations = []
        for i in range(order, len(VERIFIED_SEQUENCE)):
            # g(i) = c0*g(i-1) + c1*g(i-2) + ... + c_{k-1}*g(i-k) + d
            lhs = VERIFIED_SEQUENCE[i]
            rhs = sum(coeffs[j] * VERIFIED_SEQUENCE[i - j - 1] for j in range(order)) + d
            equations.append(lhs - rhs)

        print(f"  System of {len(equations)} equations in {order + 1} unknowns")

        try:
            # Solve for coefficients (exact rational solution)
            if order == 1:
                solution = solve(equations, [coeffs[0], d], dict=True)
            else:
                solution = solve(equations, list(coeffs) + [d], dict=True)

            if not solution:
                print(f"  ✗ No solution found")
                results[f"recurrence_order{order}"] = {"status": "no_solution"}
                print()
                continue

            # Take first solution if multiple
            sol = solution[0]
            print(f"  ✓ Found exact rational coefficients:")
            for var, val in sol.items():
                print(f"    {var} = {val}")
            print()

            # Verify on known values
            print("  Verification on known values:")
            max_error = Rational(0)
            for i in range(order, len(VERIFIED_SEQUENCE)):
                predicted = sum(sol[coeffs[j]] * VERIFIED_SEQUENCE[i - j - 1] for j in range(order)) + sol[d]
                error = abs(predicted - VERIFIED_SEQUENCE[i])
                max_error = max(max_error, error)
                match = "✓" if error == 0 else "✗"
                print(f"    g({i}): predicted={predicted}, actual={VERIFIED_SEQUENCE[i]} {match}")
            print()

            if max_error == 0:
                print("  ✓ Recurrence reproduces all known values EXACTLY")
            else:
                print(f"  ✗ Max error: {max_error}")
                results[f"recurrence_order{order}"] = {"status": "inexact", "max_error": str(max_error)}
                print()
                continue

            # Generate extended sequence
            print("  Extended predictions (n=5..16):")
            extended_seq = list(VERIFIED_SEQUENCE)
            all_integers = True
            all_positive = True

            for i in range(len(VERIFIED_SEQUENCE), 17):
                next_val = sum(sol[coeffs[j]] * extended_seq[i - j - 1] for j in range(order)) + sol[d]
                next_val = simplify(next_val)

                is_int = next_val.is_integer if hasattr(next_val, 'is_integer') else False
                is_pos = next_val > 0 if hasattr(next_val, '__gt__') else True

                if not is_int:
                    all_integers = False
                if not is_pos:
                    all_positive = False

                extended_seq.append(next_val)
                if i >= 5:
                    print(f"    g({i}) = {next_val} {'✓' if is_int and is_pos else '✗'}")

            print()

            g_16 = extended_seq[16]
            g_16_int = int(g_16) if g_16.is_integer else None

            results[f"recurrence_order{order}"] = {
                "coefficients": {str(k): str(v) for k, v in sol.items()},
                "exact_fit": (max_error == 0),
                "all_integers": all_integers,
                "all_positive": all_positive,
                "g_16": g_16_int,
                "extended_sequence": [str(v) for v in extended_seq[:17]]
            }

            print(f"  Result summary:")
            if all_integers and all_positive and g_16_int:
                print(f"    ✓ Valid integer solution: g(16) = {g_16_int}")
            else:
                print(f"    ✗ Not all values are positive integers")
            print()

        except Exception as e:
            print(f"  ✗ Error solving system: {e}")
            results[f"recurrence_order{order}"] = {"status": "error", "message": str(e)}
            print()

    return results

def analyze_differences() -> Dict:
    """Analyze finite differences (exact arithmetic)"""
    print_section("STEP 3: EXACT DIFFERENCE ANALYSIS")

    print("Computing finite differences (exact Rational arithmetic):")
    print()

    # Convert to regular ints for cleaner display
    seq_ints = [int(v) for v in VERIFIED_SEQUENCE]

    diffs = [VERIFIED_SEQUENCE]
    level = 0

    while len(diffs[level]) > 1 and level < 5:
        next_diff = [diffs[level][i+1] - diffs[level][i] for i in range(len(diffs[level])-1)]
        level += 1
        diffs.append(next_diff)

        # Convert to ints for display
        diff_ints = [int(d) for d in next_diff]
        print(f"Δ^{level}: {diff_ints}")

        # Check if constant (all equal)
        if len(set(next_diff)) == 1:
            print(f"  → Constant at level {level}! Sequence is degree-{level} polynomial")
            print()
            break

        # Check if geometric (all ratios equal)
        if len(next_diff) >= 2:
            ratios = []
            for i in range(len(next_diff)-1):
                if next_diff[i] != 0:
                    ratio = next_diff[i+1] / next_diff[i]
                    ratios.append(ratio)

            if ratios and len(set(ratios)) == 1:
                print(f"  → Geometric with exact ratio {ratios[0]}! Exponential component detected")
                print()
                break

    print()

    # Growth ratio analysis (exact)
    print("Growth ratio analysis (exact Rational):")
    ratios = [VERIFIED_SEQUENCE[i+1] / VERIFIED_SEQUENCE[i] for i in range(len(VERIFIED_SEQUENCE)-1)]
    for i, ratio in enumerate(ratios):
        print(f"  g({i+1})/g({i}) = {ratio} = {float(ratio):.6f}")

    print()

    # Ratio differences
    ratio_diffs = [ratios[i+1] - ratios[i] for i in range(len(ratios)-1)]
    print(f"Ratio differences (exact):")
    for i, diff in enumerate(ratio_diffs):
        print(f"  Δratio[{i}] = {diff} ≈ {float(diff):.6f}")
    print()

    return {
        "differences": [[str(d) for d in diff] for diff in diffs],
        "ratios": [str(r) for r in ratios],
        "ratio_differences": [str(d) for d in ratio_diffs]
    }

def main():
    print_section("PROMPT 2: EXACT ARITHMETIC FORMULA FITTING")

    print("Input data from PROMPT 1 (SymPy Rational):")
    print(f"  n values: {[int(v) for v in N_VALUES]}")
    print(f"  g(n) values: {[int(v) for v in VERIFIED_SEQUENCE]}")
    print()

    # Run all tests with exact arithmetic
    poly_results = test_sympy_polynomial_fits()
    recurrence_results = test_sympy_recurrence_relations()
    diff_analysis = analyze_differences()

    # Compile results
    print_section("SUMMARY AND OUTPUT")

    all_results = {
        "polynomial_fits": poly_results,
        "recurrence_relations": recurrence_results,
        "difference_analysis": diff_analysis
    }

    # Find valid candidates
    candidates = []

    # Check polynomial fit
    if poly_results.get("lagrange_interpolation", {}).get("all_integers_5_to_16"):
        g_16 = poly_results["lagrange_interpolation"]["g_16"]
        if g_16:
            candidates.append({
                "method": "lagrange_polynomial",
                "g_16": g_16,
                "confidence": "high"
            })

    # Check recurrence relations
    for key, value in recurrence_results.items():
        if isinstance(value, dict) and value.get("all_integers") and value.get("all_positive"):
            g_16 = value.get("g_16")
            if g_16:
                candidates.append({
                    "method": key,
                    "g_16": g_16,
                    "confidence": "high" if value.get("exact_fit") else "medium"
                })

    print("Valid candidates for g(16):")
    if candidates:
        # Group by g(16) value
        by_value = {}
        for c in candidates:
            val = c["g_16"]
            if val not in by_value:
                by_value[val] = []
            by_value[val].append(c["method"])

        for g_16, methods in sorted(by_value.items()):
            print(f"  g(16) = {g_16}")
            print(f"    Methods: {', '.join(methods)}")
        print()

        # Check consensus
        if len(by_value) == 1:
            g_16_consensus = list(by_value.keys())[0]
            print(f"✓ CONSENSUS: All methods agree on g(16) = {g_16_consensus}")
            print()
    else:
        print("  ✗ No valid integer candidates found")
        print()

    # Output JSON for chaining
    output = {
        "verified_input": [int(v) for v in VERIFIED_SEQUENCE],
        "arithmetic_mode": "exact_sympy_rational",
        "results": all_results,
        "candidates": candidates,
        "next_prompt": 3 if candidates else "investigate_further"
    }

    print("JSON output for PROMPT 3:")
    print(json.dumps(output, indent=2, default=str))
    print()

if __name__ == "__main__":
    main()
