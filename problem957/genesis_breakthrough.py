#!/usr/bin/env python3
"""
GENESIS BREAKTHROUGH: P_t ≈ B_t² / 2

This is the key insight from "Point Genesis"!

P_t / B_t² → 0.5

Therefore:
- P_t ≈ B_t² / 2
- κ_t = m_t / (6 * P_t) ≈ m_t / (3 * B_t²)

This gives us a direct formula involving CUMULATIVE growth (B_t)!

Since B_t = 2 + Σm_i from i=1 to t, we can potentially:
1. Find pattern for B_t
2. Use it to compute κ_t directly
3. Solve for g(16) without full simulation!
"""

import numpy as np
import math


def test_approximation():
    """Test the approximation κ_t ≈ m_t / (3 * B_t²)."""

    print("="*70)
    print("GENESIS APPROXIMATION: κ_t ≈ m_t / (3 * B_t²)")
    print("="*70)
    print()

    m = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]

    # Compute B_t
    B = [2]
    for mt in m:
        B.append(B[-1] + mt)

    # Compute actual κ_t
    P = []
    for t in range(1, len(m) + 1):
        if t == 1:
            P_t = 1
        else:
            m_prev = m[t-2]
            B_prev2 = B[t-2]
            P_t = m_prev * (m_prev - 1) // 2 + m_prev * B_prev2
        P.append(P_t)

    kappa_actual = [m[t-1] / (6 * P[t-1]) for t in range(1, len(m) + 1)]

    # Compute approximation
    kappa_approx = [m[t-1] / (3 * B[t-1]**2) for t in range(1, len(m) + 1)]

    print(f"{'t':<3} {'κ_t (actual)':<20} {'κ_t (approx)':<20} {'Error %'}")
    print("-"*70)

    errors = []
    for t in range(len(m)):
        actual = kappa_actual[t]
        approx = kappa_approx[t]
        error = abs(actual - approx) / actual * 100 if actual > 0 else 0
        errors.append(error)

        marker = " ✓" if error < 5 else (" ~" if error < 20 else " ✗")
        print(f"{t+1:<3} {actual:<20.10e} {approx:<20.10e} {error:>6.2f}%{marker}")

    avg_error = sum(errors) / len(errors)
    print()
    print(f"Average error: {avg_error:.2f}%")
    print()

    if avg_error < 10:
        print("✓✓✓ EXCELLENT APPROXIMATION! ✓✓✓")
        print()
        print("This means we can use: κ_t ≈ m_t / (3 * B_t²)")
        print("where B_t = 2 + Σm_i from i=1 to t-1")
        print()


def analyze_b_t_growth():
    """Analyze B_t growth pattern."""

    print("="*70)
    print("B_t GROWTH PATTERN")
    print("="*70)
    print()

    m = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]

    B = [2]
    for mt in m:
        B.append(B[-1] + mt)

    print("B_t values:")
    for t in range(len(B)):
        print(f"  B_{t} = {B[t]:,}")
    print()

    print("B_t / m_t Ratios:")
    for t in range(1, len(B)):
        if t <= len(m):
            ratio = B[t] / m[t-1]
            print(f"  B_{t}/m_{t} = {ratio:.10f}")
    print()

    # B_t growth rate
    print("B_t Growth Ratios:")
    for t in range(1, len(B)):
        ratio = B[t] / B[t-1]
        print(f"  B_{t}/B_{t-1} = {ratio:.10f}")
    print()

    print("Observation: B_t+1 / B_t approaches the growth rate of m_t")
    print("Because: B_{t+1} = B_t + m_t")
    print("So: B_{t+1}/B_t = 1 + m_t/B_t")
    print()

    # For large t, if m_t/B_t → const, then B_t and m_t grow at same rate
    print("m_t / B_t Ratios:")
    for t in range(len(m)):
        ratio = m[t] / B[t+1]
        print(f"  m_{t+1}/B_{t+1} = {ratio:.10f}")
    print()


def derive_asymptotic_formula():
    """Derive asymptotic behavior."""

    print("="*70)
    print("ASYMPTOTIC ANALYSIS")
    print("="*70)
    print()

    m = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]
    B = [2]
    for mt in m:
        B.append(B[-1] + mt)

    print("For large t:")
    print()
    print("1. m_t grows exponentially with ratio λ ≈ 20 (observed)")
    print()
    print("2. B_t = 2 + Σm_i ≈ m_t * (1 + 1/λ + 1/λ² + ...)")
    print("         = m_t * λ/(λ-1)  [geometric series]")
    print()
    print("3. Therefore: B_t ≈ const * m_t")
    print()
    print("4. κ_t = m_t / (3 * B_t²)")
    print("       ≈ m_t / (3 * (const * m_t)²)")
    print("       = 1 / (3 * const² * m_t)")
    print("       ∝ 1 / m_t")
    print()
    print("So asymptotically: κ_t ∝ 1/m_t")
    print()

    # Test this
    print("Testing: κ_t * m_t should be roughly constant for large t")
    print()

    P = []
    for t in range(1, len(m) + 1):
        if t == 1:
            P_t = 1
        else:
            m_prev = m[t-2]
            B_prev2 = B[t-2]
            P_t = m_prev * (m_prev - 1) // 2 + m_prev * B_prev2
        P.append(P_t)

    kappa_actual = [m[t-1] / (6 * P[t-1]) for t in range(1, len(m) + 1)]

    print(f"{'t':<3} {'κ_t * m_t':<20}")
    print("-"*30)
    for t in range(len(m)):
        product = kappa_actual[t] * m[t]
        print(f"{t+1:<3} {product:<20.10e}")

    print()
    print("Observation: κ_t * m_t is NOT constant, it's decreasing")
    print("This means: κ_t decreases FASTER than 1/m_t")
    print()


def refine_with_correction_factor():
    """Add correction factor to approximation."""

    print("="*70)
    print("REFINED APPROXIMATION WITH CORRECTION")
    print("="*70)
    print()

    m = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]

    B = [2]
    for mt in m:
        B.append(B[-1] + mt)

    # Exact P_t
    P = []
    for t in range(1, len(m) + 1):
        if t == 1:
            P_t = 1
        else:
            m_prev = m[t-2]
            B_prev2 = B[t-2]
            P_t = m_prev * (m_prev - 1) // 2 + m_prev * B_prev2
        P.append(P_t)

    print("Observed: P_t / B_t² → 0.5")
    print()
    print("Let's model: P_t = α(t) * B_t² / 2")
    print("where α(t) is a correction factor")
    print()

    alpha_vals = [2 * P[t-1] / (B[t-1]**2) for t in range(1, len(m) + 1)]

    print(f"{'t':<3} {'α(t)':<15}")
    print("-"*20)
    for t in range(len(m)):
        print(f"{t+1:<3} {alpha_vals[t]:<15.10f}")

    print()

    # Fit α(t)
    t_vals = np.array(range(1, len(alpha_vals) + 1))
    alpha_arr = np.array(alpha_vals)

    # Test if α(t) approaches 1
    print("α(t) → 1 as t → ∞")
    print()
    print("We can model: α(t) = 1 - c/t^p")
    print()

    # Fit 1 - α(t) ≈ c * t^(-p)
    inv_alpha = [1 - a for a in alpha_vals[3:]]  # Use later terms
    log_t = [math.log(t) for t in range(4, len(alpha_vals) + 1)]
    log_inv = [math.log(ia) if ia > 0 else -20 for ia in inv_alpha]

    # Linear fit in log-log space
    if len(log_t) > 1:
        coeffs = np.polyfit(log_t, log_inv, 1)
        p_fit = coeffs[0]
        log_c = coeffs[1]
        c_fit = math.exp(log_c)

        print(f"Fitted: α(t) ≈ 1 - {c_fit:.6f} * t^{p_fit:.4f}")
        print()

        # Use this to predict κ_16
        alpha_16 = 1 - c_fit * (16 ** p_fit)
        print(f"Predicted α(16) = {alpha_16:.10f}")
        print()


def main():
    test_approximation()
    print()
    analyze_b_t_growth()
    print()
    derive_asymptotic_formula()
    print()
    refine_with_correction_factor()

    print("="*70)
    print("GENESIS BREAKTHROUGH SUMMARY")
    print("="*70)
    print()
    print("✓ DISCOVERED: P_t ≈ B_t² / 2  (with error < 1% for t ≥ 6)")
    print()
    print("This gives us:")
    print("  κ_t = m_t / (6 * P_t)")
    print("      ≈ m_t / (3 * B_t²)")
    print()
    print("Where B_t = 2 + Σm_i from i=1 to t-1")
    print()
    print("This is the 'Point Genesis' insight:")
    print("  - κ_t depends on CUMULATIVE generation (B_t)")
    print("  - B_t² represents the combinatorial explosion of candidates")
    print("  - m_t / B_t² gives the 'survival rate' of unique points")
    print()
    print("Next step: If we can find exact formula for m_t,")
    print("we can compute B_16, then κ_16, then g(16)!")
    print()


if __name__ == "__main__":
    main()
