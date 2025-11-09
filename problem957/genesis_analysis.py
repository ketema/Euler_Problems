#!/usr/bin/env python3
"""
Point Genesis Analysis - Looking for Generation Pattern

The problem name "Point Genesis" suggests the solution involves:
  - Generating functions
  - Growth/recurrence relations
  - Combinatorial generation theory

Let's analyze the m_t sequence for patterns that might reveal κ_t formula.
"""

import numpy as np
import math


def analyze_growth_patterns():
    """Analyze m_t growth pattern for recurrence relations."""

    print("="*70)
    print("POINT GENESIS - GENERATION PATTERN ANALYSIS")
    print("="*70)
    print()

    # OEIS A189191
    m = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]

    print("m_t sequence (OEIS A189191):")
    for t, val in enumerate(m, 1):
        print(f"  m_{t} = {val:,}")
    print()

    # Growth ratios
    print("Growth Ratios (m_t / m_{t-1}):")
    ratios = []
    for t in range(1, len(m)):
        ratio = m[t] / m[t-1]
        ratios.append(ratio)
        print(f"  m_{t+1}/m_{t} = {ratio:.6f}")
    print()

    # Second-order differences in growth
    print("Ratio Growth (ratio_t / ratio_{t-1}):")
    for t in range(1, len(ratios)):
        ratio_growth = ratios[t] / ratios[t-1]
        print(f"  r_{t+1}/r_{t} = {ratio_growth:.6f}")
    print()

    # Test for linear recurrence: m_t = a*m_{t-1} + b*m_{t-2} + c*m_{t-3}
    print("Testing Linear Recurrence: m_t = a*m_{t-1} + b*m_{t-2} + c*m_{t-3}")
    print()

    # Use first 6 terms to solve for a, b, c
    # Then test on remaining terms

    # Build system: [m_3, m_4, m_5] = [a, b, c] * [[m_2, m_1, m_0], ...]
    if len(m) >= 6:
        # Use m_0 = ?, m_1 = 6, m_2 = 20, m_3 = 156 to find m_0
        # Or use m_4, m_5, m_6 to solve for a, b, c

        # System of equations:
        # m_4 = a*m_3 + b*m_2 + c*m_1
        # m_5 = a*m_4 + b*m_3 + c*m_2
        # m_6 = a*m_5 + b*m_4 + c*m_3

        A_matrix = np.array([
            [m[2], m[1], m[0]],  # [156, 20, 6]
            [m[3], m[2], m[1]],  # [1462, 156, 20]
            [m[4], m[3], m[2]]   # [17515, 1462, 156]
        ])

        b_vector = np.array([m[3], m[4], m[5]])

        try:
            coeffs = np.linalg.solve(A_matrix, b_vector)
            a, b, c = coeffs

            print(f"  Fitted coefficients:")
            print(f"    a = {a:.10f}")
            print(f"    b = {b:.10f}")
            print(f"    c = {c:.10f}")
            print()

            # Test on all terms
            print("  Verification:")
            errors = []
            for t in range(3, len(m)):
                predicted = a * m[t-1] + b * m[t-2] + c * m[t-3]
                actual = m[t]
                error = abs(predicted - actual) / actual * 100
                errors.append(error)

                marker = " ✓" if error < 0.001 else (" ~" if error < 1 else " ✗")
                print(f"    m_{t+1}: predicted={predicted:,.0f}, actual={actual:,}, error={error:.6f}%{marker}")

            avg_error = sum(errors) / len(errors)
            print()
            print(f"  Average error: {avg_error:.6f}%")

            if avg_error < 0.001:
                print("  ✓✓✓ EXACT LINEAR RECURRENCE FOUND! ✓✓✓")
                print()
                print(f"  m_t = {a:.10f} * m_{{t-1}} + {b:.10f} * m_{{t-2}} + {c:.10f} * m_{{t-3}}")

        except np.linalg.LinAlgError:
            print("  Matrix is singular, cannot solve for coefficients")

    print()

    # Characteristic equation for recurrence
    print("="*70)
    print("CHARACTERISTIC EQUATION ANALYSIS")
    print("="*70)
    print()

    print("If m_t = a*m_{t-1} + b*m_{t-2} + c*m_{t-3}, then:")
    print("Characteristic equation: x³ = a*x² + b*x + c")
    print()

    if 'a' in locals():
        # Solve characteristic equation
        # x³ - a*x² - b*x - c = 0
        poly_coeffs = [1, -a, -b, -c]
        roots = np.roots(poly_coeffs)

        print("Characteristic roots:")
        for i, root in enumerate(roots, 1):
            if np.isreal(root):
                print(f"  λ_{i} = {np.real(root):.10f}")
            else:
                print(f"  λ_{i} = {root:.10f}")
        print()

        # Largest root determines asymptotic growth
        largest_root = max(abs(r) for r in roots)
        print(f"Largest root (magnitude): {largest_root:.10f}")
        print(f"This determines the asymptotic growth rate of m_t")
        print()

        # Compare to Tribonacci roots
        print("For comparison, Tribonacci characteristic equation: x³ = x² + x + 1")
        trib_roots = np.roots([1, -1, -1, -1])
        print("Tribonacci roots:")
        for i, root in enumerate(trib_roots, 1):
            if np.isreal(root):
                print(f"  τ_{i} = {np.real(root):.10f}")
            else:
                print(f"  τ_{i} = {root:.10f}")

        trib_largest = max(abs(r) for r in trib_roots)
        print(f"Largest Tribonacci root: {trib_largest:.10f}")
        print()


def analyze_kappa_pattern():
    """Analyze κ_t for generation patterns."""

    print("="*70)
    print("κ_t GENERATION PATTERN")
    print("="*70)
    print()

    m = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]

    # Compute κ_t from m_t
    B = [2]
    for i in range(len(m) - 1):
        B.append(B[-1] + m[i])

    kappa = []
    for t in range(1, len(m) + 1):
        if t == 1:
            P_t = 1
        else:
            m_prev = m[t-2]
            B_prev2 = B[t-2] if t >= 2 else 0
            P_t = m_prev * (m_prev - 1) // 2 + m_prev * B_prev2

        kappa_t = m[t-1] / (6 * P_t)
        kappa.append(kappa_t)

    print("κ_t values:")
    for t, k in enumerate(kappa, 1):
        print(f"  κ_{t} = {k:.10f}")
    print()

    # Look for patterns
    print("κ_t Ratios:")
    for t in range(1, len(kappa)):
        ratio = kappa[t] / kappa[t-1]
        print(f"  κ_{t+1}/κ_{t} = {ratio:.10f}")
    print()

    # Inverse κ_t (might have simpler pattern)
    print("Inverse κ_t (1/κ_t):")
    inv_kappa = [1/k for k in kappa]
    for t, inv_k in enumerate(inv_kappa, 1):
        print(f"  1/κ_{t} = {inv_k:.10f}")
    print()

    # Growth of inverse
    print("Inverse κ_t Ratios:")
    for t in range(1, len(inv_kappa)):
        ratio = inv_kappa[t] / inv_kappa[t-1]
        print(f"  (1/κ_{t+1})/(1/κ_{t}) = {ratio:.10f}")
    print()

    # Log scale analysis
    print("Log(κ_t) - looking for exponential pattern:")
    log_kappa = [math.log(k) for k in kappa]
    for t, lk in enumerate(log_kappa, 1):
        print(f"  log(κ_{t}) = {lk:.10f}")
    print()

    # Differences in log space (exponential growth manifests as linear)
    print("Differences in log(κ_t):")
    for t in range(1, len(log_kappa)):
        diff = log_kappa[t] - log_kappa[t-1]
        print(f"  Δlog(κ_{t}) = {diff:.10f}")
    print()


def test_generating_function():
    """Test if there's a generating function pattern."""

    print("="*70)
    print("GENERATING FUNCTION EXPLORATION")
    print("="*70)
    print()

    m = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]

    print("For sequence a_n, generating function G(x) = Σ a_n * x^n")
    print()
    print("If G(x) = P(x) / Q(x) where Q(x) = 1 - c_1*x - c_2*x² - c_3*x³")
    print("then a_n satisfies: a_n = c_1*a_{n-1} + c_2*a_{n-2} + c_3*a_{n-3}")
    print()
    print("This connects generating functions to recurrence relations!")
    print()

    # If we found a recurrence, we can write the generating function
    if True:  # Assume we found recurrence from previous analysis
        print("Next step: Use the recurrence coefficients to write explicit")
        print("generating function for m_t")
        print()


def main():
    analyze_growth_patterns()
    print("\n")
    analyze_kappa_pattern()
    print("\n")
    test_generating_function()

    print("="*70)
    print("CONCLUSIONS")
    print("="*70)
    print()
    print("The 'Genesis' clue likely points to:")
    print("  1. Finding the exact recurrence relation for m_t")
    print("  2. Using that to derive a formula for κ_t")
    print("  3. Or finding κ_t has its own generation pattern")
    print()
    print("If m_t has exact linear recurrence, we can:")
    print("  - Write closed-form solution using characteristic roots")
    print("  - Compute m_16 and κ_16 directly")
    print("  - Solve for g(16) without computing all intersection points")
    print()


if __name__ == "__main__":
    main()
