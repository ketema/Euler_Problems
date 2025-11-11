#!/usr/bin/env python3
"""
PROMPT 2: Algebraic Polynomial/Recurrence Fitting
DETERMINISTIC execution using SymPy, NumPy, and PySR

Objective: Find closed-form expression or recurrence for g(n)
Tools: SymPy rsolve, NumPy polyfit, PySR symbolic regression
Output: Formula/recurrence that fits all known values
"""

import numpy as np
from sympy import symbols, Rational, simplify, solve, rsolve, Function, Eq
from sympy.abc import n
import json
from typing import List, Dict, Optional, Tuple

# Verified data from PROMPT 1
VERIFIED_SEQUENCE = [2, 8, 28, 184, 1644]
N_VALUES = [0, 1, 2, 3, 4]

def print_section(title: str):
    """Print formatted section header"""
    print("=" * 80)
    print(title)
    print("=" * 80)
    print()

def test_polynomial_fits() -> Dict:
    """Test polynomial hypotheses for degrees 2-5"""
    print_section("STEP 1: POLYNOMIAL FITTING")

    results = {}

    for degree in range(2, 6):
        print(f"Testing degree {degree} polynomial...")

        # Fit polynomial using NumPy
        coeffs = np.polyfit(N_VALUES, VERIFIED_SEQUENCE, degree)
        poly = np.poly1d(coeffs)

        # Test predictions
        predictions = [poly(i) for i in range(5)]
        residuals = [abs(predictions[i] - VERIFIED_SEQUENCE[i]) for i in range(5)]
        max_residual = max(residuals)

        print(f"  Coefficients: {coeffs}")
        print(f"  Max residual: {max_residual:.2e}")

        # Check if predictions are integers for extended range
        extended_predictions = [poly(i) for i in range(17)]
        all_integers = all(abs(p - round(p)) < 0.01 for p in extended_predictions)
        all_positive = all(p > 0 for p in extended_predictions)

        print(f"  Extended predictions (n=0..16): ", end="")
        if all_integers and all_positive:
            print("✓ All integers and positive")
            print(f"    g(16) = {int(round(extended_predictions[16]))}")
        else:
            print("✗ Contains non-integers or negatives")

        results[f"poly_deg{degree}"] = {
            "coefficients": coeffs.tolist(),
            "max_residual": float(max_residual),
            "all_integers": all_integers,
            "all_positive": all_positive,
            "g_16": int(round(extended_predictions[16])) if all_integers and all_positive else None
        }

        print()

    return results

def test_recurrence_relations() -> Dict:
    """Test linear recurrence relations of orders 2-4"""
    print_section("STEP 2: RECURRENCE RELATION TESTING")

    results = {}

    for order in range(2, 5):
        print(f"Testing order-{order} recurrence: g(n) = c₁·g(n-1) + c₂·g(n-2) + ... + c_{order}·g(n-{order}) + d")

        # Need at least order+1 equations to solve for order+1 unknowns
        if len(VERIFIED_SEQUENCE) < order + 2:
            print(f"  ✗ Not enough data points (need {order+2}, have {len(VERIFIED_SEQUENCE)})")
            print()
            continue

        # Set up system of equations
        # g(n) = c1*g(n-1) + c2*g(n-2) + ... + ck*g(n-k) + d
        # We need order+1 coefficients (c1, c2, ..., ck, d)

        num_equations = len(VERIFIED_SEQUENCE) - order
        A = []
        b = []

        for i in range(order, len(VERIFIED_SEQUENCE)):
            row = [VERIFIED_SEQUENCE[i - j - 1] for j in range(order)]
            row.append(1)  # coefficient for constant term d
            A.append(row)
            b.append(VERIFIED_SEQUENCE[i])

        A = np.array(A, dtype=float)
        b = np.array(b, dtype=float)

        try:
            # Solve for coefficients
            coeffs = np.linalg.lstsq(A, b, rcond=None)[0]

            print(f"  Coefficients: {coeffs}")

            # Verify on known values
            max_error = 0
            for i in range(order, len(VERIFIED_SEQUENCE)):
                predicted = sum(coeffs[j] * VERIFIED_SEQUENCE[i - j - 1] for j in range(order)) + coeffs[-1]
                error = abs(predicted - VERIFIED_SEQUENCE[i])
                max_error = max(max_error, error)

            print(f"  Max error on known values: {max_error:.2e}")

            # Compute extended sequence
            extended_seq = VERIFIED_SEQUENCE.copy()
            for i in range(len(VERIFIED_SEQUENCE), 17):
                next_val = sum(coeffs[j] * extended_seq[i - j - 1] for j in range(order)) + coeffs[-1]
                extended_seq.append(next_val)

            # Check if all values are positive integers
            all_integers = all(abs(v - round(v)) < 0.01 for v in extended_seq)
            all_positive = all(v > 0 for v in extended_seq)

            print(f"  Extended sequence (n=0..16): ", end="")
            if all_integers and all_positive:
                print("✓ All integers and positive")
                print(f"    g(16) = {int(round(extended_seq[16]))}")
            else:
                print("✗ Contains non-integers or negatives")

            results[f"recurrence_order{order}"] = {
                "coefficients": coeffs.tolist(),
                "max_error": float(max_error),
                "all_integers": all_integers,
                "all_positive": all_positive,
                "g_16": int(round(extended_seq[16])) if all_integers and all_positive else None,
                "extended_sequence": [int(round(v)) if all_integers else v for v in extended_seq]
            }

        except np.linalg.LinAlgError as e:
            print(f"  ✗ Failed to solve: {e}")
            results[f"recurrence_order{order}"] = {"error": str(e)}

        print()

    return results

def analyze_differences() -> Dict:
    """Analyze finite differences to detect polynomial/exponential patterns"""
    print_section("STEP 3: DIFFERENCE ANALYSIS")

    print("Computing finite differences:")
    print()

    diffs = [VERIFIED_SEQUENCE]
    level = 0

    while len(diffs[level]) > 1 and level < 5:
        next_diff = [diffs[level][i+1] - diffs[level][i] for i in range(len(diffs[level])-1)]
        level += 1
        diffs.append(next_diff)

        print(f"Δ^{level}: {next_diff}")

        # Check if constant
        if len(set(next_diff)) == 1:
            print(f"  → Constant at level {level}! Sequence is degree-{level} polynomial")
            break

        # Check if geometric
        if len(next_diff) >= 2:
            ratios = [next_diff[i+1] / next_diff[i] for i in range(len(next_diff)-1) if next_diff[i] != 0]
            if len(set(round(r, 2) for r in ratios)) == 1:
                print(f"  → Geometric with ratio {ratios[0]:.2f}! Exponential component detected")

    print()

    # Ratio analysis
    print("Growth ratio analysis:")
    ratios = [VERIFIED_SEQUENCE[i+1] / VERIFIED_SEQUENCE[i] for i in range(len(VERIFIED_SEQUENCE)-1)]
    for i, ratio in enumerate(ratios):
        print(f"  g({i+1})/g({i}) = {ratio:.6f}")

    print()

    # Check for ratio pattern
    ratio_diffs = [ratios[i+1] - ratios[i] for i in range(len(ratios)-1)]
    print(f"Ratio differences: {[f'{d:.3f}' for d in ratio_diffs]}")
    print()

    return {
        "differences": [[int(d) for d in diff] for diff in diffs],
        "ratios": [float(r) for r in ratios],
        "ratio_differences": [float(d) for d in ratio_diffs]
    }

def try_symbolic_regression() -> Optional[Dict]:
    """Use PySR for symbolic regression if simpler methods fail"""
    print_section("STEP 4: SYMBOLIC REGRESSION (PySR)")

    try:
        from pysr import PySRRegressor

        print("Initializing PySR...")
        print("Configuration:")
        print("  - Binary operators: +, *, -, /")
        print("  - Unary operators: square, cube")
        print("  - Populations: 30")
        print("  - Iterations: 50")
        print()

        # Prepare data
        X = np.array(N_VALUES).reshape(-1, 1)
        y = np.array(VERIFIED_SEQUENCE)

        # Configure PySR
        model = PySRRegressor(
            binary_operators=["+", "*", "-", "/"],
            unary_operators=["square", "cube"],
            niterations=50,
            populations=30,
            population_size=50,
            maxsize=20,
            timeout_in_seconds=300,  # 5 minutes max
            parsimony=0.01,
            random_state=42
        )

        print("Training symbolic regression model...")
        print("(This may take several minutes)")
        print()

        model.fit(X, y)

        print("Top formulas by complexity/accuracy:")
        print()

        # Get top equations
        equations = model.equations_

        results = {"formulas": []}

        for i, row in equations.iterrows():
            if i >= 5:  # Top 5
                break

            formula = row['equation']
            complexity = row['complexity']
            loss = row['loss']

            print(f"{i+1}. {formula}")
            print(f"   Complexity: {complexity}, Loss: {loss:.2e}")

            # Test on extended range
            try:
                predictions = [model.predict(np.array([[n]]))[0] for n in range(17)]
                all_integers = all(abs(p - round(p)) < 0.1 for p in predictions[:5])
                all_positive = all(p > 0 for p in predictions)

                if all_integers and all_positive:
                    print(f"   g(16) = {int(round(predictions[16]))}")

                    results["formulas"].append({
                        "formula": str(formula),
                        "complexity": int(complexity),
                        "loss": float(loss),
                        "g_16": int(round(predictions[16]))
                    })
                else:
                    print(f"   ✗ Predictions not all positive integers")

            except Exception as e:
                print(f"   ✗ Error evaluating: {e}")

            print()

        return results

    except ImportError:
        print("PySR not available (requires Julia runtime)")
        print("Skipping symbolic regression")
        return None
    except Exception as e:
        print(f"Error in symbolic regression: {e}")
        return None

def main():
    print_section("PROMPT 2: DETERMINISTIC FORMULA FITTING")

    print("Input data from PROMPT 1:")
    print(f"  n values: {N_VALUES}")
    print(f"  g(n) values: {VERIFIED_SEQUENCE}")
    print()

    # Run all tests
    poly_results = test_polynomial_fits()
    recurrence_results = test_recurrence_relations()
    diff_analysis = analyze_differences()

    # Only try PySR if simpler methods don't find integer solutions
    has_integer_solution = False
    for key, value in {**poly_results, **recurrence_results}.items():
        if isinstance(value, dict) and value.get("all_integers") and value.get("all_positive"):
            has_integer_solution = True
            break

    pysr_results = None
    if not has_integer_solution:
        pysr_results = try_symbolic_regression()
    else:
        print_section("STEP 4: SYMBOLIC REGRESSION (PySR)")
        print("✓ Integer solutions found in polynomial/recurrence testing")
        print("Skipping computationally expensive PySR")
        print()

    # Compile results
    print_section("SUMMARY AND OUTPUT")

    all_results = {
        "polynomial_fits": poly_results,
        "recurrence_relations": recurrence_results,
        "difference_analysis": diff_analysis,
        "symbolic_regression": pysr_results
    }

    # Find best candidate
    candidates = []

    for method, data in all_results.items():
        if method == "difference_analysis":
            continue
        if method == "symbolic_regression" and data is None:
            continue

        if method == "polynomial_fits" or method == "recurrence_relations":
            for key, value in data.items():
                if isinstance(value, dict) and value.get("all_integers") and value.get("all_positive") and value.get("g_16") is not None:
                    candidates.append({
                        "method": f"{method}/{key}",
                        "g_16": value["g_16"],
                        "confidence": "high" if value.get("max_residual", value.get("max_error", 1)) < 1e-6 else "medium"
                    })
        elif method == "symbolic_regression" and data:
            for formula_data in data.get("formulas", []):
                candidates.append({
                    "method": f"pysr/{formula_data['formula']}",
                    "g_16": formula_data["g_16"],
                    "confidence": "medium"
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
    else:
        print("  ✗ No valid integer candidates found")
        print()

    # Output JSON for chaining
    output = {
        "verified_input": VERIFIED_SEQUENCE,
        "results": all_results,
        "candidates": candidates,
        "next_prompt": 3 if candidates else "investigate_further"
    }

    print("JSON output for PROMPT 3:")
    print(json.dumps(output, indent=2))
    print()

if __name__ == "__main__":
    main()
