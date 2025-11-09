#!/usr/bin/env python3
"""
P_t Genesis Pattern Analysis

P_t = number of candidate intersection points
P_t = C(m_{t-1}, 2) + m_{t-1} * B_{t-2}

where:
- C(m_{t-1}, 2) = m_{t-1} * (m_{t-1} - 1) / 2  (lines from red-blue pairs within day t-1)
- m_{t-1} * B_{t-2} = lines from new blues crossing old blues

The "genesis" might be in how P_t grows relative to m_t!
"""

import numpy as np


def compute_all_values():
    """Compute B_t, P_t, κ_t for Days 1-10."""

    m = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]

    # B_t = cumulative sum of m
    B = [2]  # B_0 = 2 (initial blues)
    for mt in m:
        B.append(B[-1] + mt)

    # P_t
    P = []
    for t in range(1, len(m) + 1):
        if t == 1:
            P_t = 1  # Special case
        else:
            m_prev = m[t-2]
            B_prev2 = B[t-2]
            P_t = m_prev * (m_prev - 1) // 2 + m_prev * B_prev2

        P.append(P_t)

    # κ_t = m_t / (6 * P_t)
    kappa = [m[t-1] / (6 * P[t-1]) for t in range(1, len(m) + 1)]

    return m, B, P, kappa


def analyze_p_t_pattern():
    """Look for pattern in P_t."""

    print("="*70)
    print("P_t (CANDIDATE COUNT) GENESIS PATTERN")
    print("="*70)
    print()

    m, B, P, kappa = compute_all_values()

    print(f"{'t':<3} {'m_t':<15} {'B_t':<15} {'P_t':<20} {'κ_t':<15}")
    print("-"*70)
    for t in range(len(m)):
        print(f"{t+1:<3} {m[t]:<15,} {B[t+1]:<15,} {P[t]:<20,} {kappa[t]:<15.10f}")

    print()

    # P_t growth rate
    print("P_t Growth Ratios:")
    for t in range(1, len(P)):
        ratio = P[t] / P[t-1]
        print(f"  P_{t+1}/P_{t} = {ratio:.6f}")
    print()

    # Relationship between P_t and m_t
    print("P_t / m_t Ratios:")
    for t in range(len(P)):
        ratio = P[t] / m[t]
        print(f"  P_{t+1}/m_{t+1} = {ratio:.6f}")
    print()

    # m_t / P_t (related to κ_t)
    print("m_t / P_t (proportional to κ_t):")
    for t in range(len(P)):
        ratio = m[t] / P[t]
        print(f"  m_{t+1}/P_{t+1} = {ratio:.10f}  (κ_{t+1} = {kappa[t]:.10f})")
    print()


def analyze_combinatorial_formula():
    """Analyze the combinatorial structure of P_t."""

    print("="*70)
    print("COMBINATORIAL STRUCTURE OF P_t")
    print("="*70)
    print()

    m, B, P, kappa = compute_all_values()

    print("P_t = C(m_{t-1}, 2) + m_{t-1} * B_{t-2}")
    print()
    print("Breaking down into components:")
    print()

    for t in range(1, min(len(m) + 1, 8)):
        if t == 1:
            term1 = 0
            term2 = 1
            P_t = 1
        else:
            m_prev = m[t-2]
            B_prev2 = B[t-2]
            term1 = m_prev * (m_prev - 1) // 2
            term2 = m_prev * B_prev2
            P_t = P[t-1]

        print(f"Day {t}:")
        print(f"  C(m_{t-1}, 2) = {term1:,}")
        print(f"  m_{t-1} * B_{t-2} = {term2:,}")
        print(f"  P_{t} = {P_t:,}")
        print()

    # Check if there's a simpler pattern
    print("Hypothesis: Is P_t related to B_t²?")
    print()

    for t in range(len(P)):
        ratio = P[t] / (B[t] ** 2) if B[t] > 0 else 0
        print(f"  P_{t+1} / B_{t}² = {ratio:.10f}")
    print()


def test_inverse_kappa_polynomial():
    """Test if 1/κ_t follows a polynomial pattern."""

    print("="*70)
    print("POLYNOMIAL PATTERN IN 1/κ_t")
    print("="*70)
    print()

    m, B, P, kappa = compute_all_values()

    inv_kappa = [1.0 / k for k in kappa]

    print("Inverse κ_t values:")
    for t in range(len(inv_kappa)):
        print(f"  1/κ_{t+1} = {inv_kappa[t]:,.6f}")
    print()

    # Test polynomial fits
    t_vals = np.array(range(1, len(inv_kappa) + 1))
    inv_k_vals = np.array(inv_kappa)

    # Try different polynomial degrees
    for degree in [2, 3, 4, 5]:
        coeffs = np.polyfit(t_vals, inv_k_vals, degree)
        poly = np.poly1d(coeffs)

        print(f"Degree {degree} polynomial fit:")
        print(f"  {poly}")

        # Compute errors
        errors = []
        for t in range(len(inv_kappa)):
            predicted = poly(t + 1)
            actual = inv_kappa[t]
            error = abs(predicted - actual) / actual * 100
            errors.append(error)

        avg_error = sum(errors) / len(errors)
        print(f"  Average error: {avg_error:.2f}%")
        print()

        if avg_error < 1:
            print(f"  ✓✓✓ EXCELLENT FIT! ✓✓✓")
            print()
            print(f"  This means: 1/κ_t ≈ polynomial of degree {degree}")
            print(f"  Therefore: κ_t ≈ 1 / (polynomial)")
            print()

            # Predict κ_16
            inv_k_16 = poly(16)
            k_16 = 1.0 / inv_k_16
            print(f"  Predicted 1/κ_16 = {inv_k_16:,.6f}")
            print(f"  Predicted κ_16 = {k_16:.15e}")
            print()
            break


def main():
    analyze_p_t_pattern()
    print()
    analyze_combinatorial_formula()
    print()
    test_inverse_kappa_polynomial()

    print("="*70)
    print("GENESIS INSIGHT")
    print("="*70)
    print()
    print("The 'Point Genesis' name suggests we should look at:")
    print("  1. How P_t (candidates) are GENERATED from m_t and B_t")
    print("  2. How the GROWTH of κ_t relates to combinatorial generation")
    print("  3. Whether 1/κ_t has a simple closed form (polynomial, etc.)")
    print()
    print("If we find 1/κ_t = f(t), then:")
    print("  κ_t = 1/f(t)")
    print("  We can compute κ_16 directly!")
    print("  Then g(16) = total_blues_16 = Σ m_i from i=1 to 16")
    print()


if __name__ == "__main__":
    main()
