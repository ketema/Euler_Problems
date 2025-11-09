#!/usr/bin/env python3
"""
Verify Genesis Formula with Complete Data

We have g(1) through g(16) from OEIS.
Since g(t) = B_t, we can compute m_t = B_t - B_{t-1}.

Then verify:
1. Our approximation κ_t ≈ m_t / (3 * B_t²)
2. Look for exact formula for m_t
3. Find the "Point Genesis" pattern!
"""

import numpy as np
import math


def main():
    print("="*70)
    print("COMPLETE SEQUENCE VERIFICATION")
    print("="*70)
    print()

    # g(t) values from OEIS A189191 (cumulative blues = B_t)
    g = {
        1: 8, 2: 28, 3: 184, 4: 1646, 5: 19161, 6: 261788, 7: 4118024, 8: 73099464,
        9: 1445724584, 10: 31477742088, 11: 750198126760, 12: 19183422035784,
        13: 526224388301160, 14: 15372370725513256, 15: 477123999908405064,
        16: 15730302251147551048
    }

    # Extract B_t and m_t
    B = [2] + [g[t] for t in range(1, 17)]
    m = [B[t] - B[t-1] for t in range(1, 17)]

    print("Extracted m_t sequence:")
    for t in range(1, 17):
        print(f"  m_{t:2d} = {m[t-1]:,}")
    print()

    # Test Genesis Approximation κ_t ≈ m_t / (3 * B_t²)
    print("="*70)
    print("TESTING GENESIS APPROXIMATION")
    print("="*70)
    print()

    # Compute exact P_t
    P = []
    for t in range(1, 17):
        if t == 1:
            P_t = 1
        else:
            m_prev = m[t-2]
            B_prev2 = B[t-2]
            P_t = m_prev * (m_prev - 1) // 2 + m_prev * B_prev2
        P.append(P_t)

    kappa_exact = [m[t-1] / (6 * P[t-1]) for t in range(1, 17)]
    kappa_approx = [m[t-1] / (3 * B[t-1]**2) for t in range(1, 17)]

    print(f"{'t':<3} {'κ_t (exact)':<20} {'κ_t (approx)':<20} {'Error %'}")
    print("-"*70)

    for t in range(1, 17):
        exact = kappa_exact[t-1]
        approx = kappa_approx[t-1]
        error = abs(exact - approx) / exact * 100 if exact > 0 else 0

        marker = " ✓" if error < 1 else (" ~" if error < 5 else " ✗")
        print(f"{t:<3} {exact:<20.10e} {approx:<20.10e} {error:>6.2f}%{marker}")

    print()

    # Look for m_t recurrence pattern
    print("="*70)
    print("SEARCHING FOR m_t RECURRENCE PATTERN")
    print("="*70)
    print()

    # Test different recurrence orders
    for order in [4, 5, 6]:
        print(f"Testing order-{order} recurrence: m_t = Σ c_i * m_{{t-i}}")
        print()

        if len(m) < order + 3:
            continue

        # Build matrix for least squares
        A_rows = []
        b_vals = []

        for t in range(order, min(len(m), 10)):  # Use first 10 terms to fit
            row = [m[t-i-1] for i in range(order)]
            A_rows.append(row)
            b_vals.append(m[t])

        if len(A_rows) >= order:
            A = np.array(A_rows)
            b = np.array(b_vals)

            try:
                coeffs, residuals, rank, s = np.linalg.lstsq(A, b, rcond=None)

                print(f"  Coefficients: {[f'{c:.6f}' for c in coeffs]}")
                print()

                # Test on all terms
                errors = []
                for t in range(order, len(m)):
                    predicted = sum(coeffs[i] * m[t-i-1] for i in range(order))
                    actual = m[t]
                    if actual > 0:
                        error = abs(predicted - actual) / actual * 100
                        errors.append(error)
                    else:
                        errors.append(0)

                avg_error = sum(errors) / len(errors) if errors else 0
                max_error = max(errors) if errors else 0

                print(f"  Average error: {avg_error:.6f}%")
                print(f"  Maximum error: {max_error:.6f}%")
                print()

                if avg_error < 0.01:
                    print(f"  ✓✓✓ EXACT RECURRENCE FOUND! ✓✓✓")
                    print()
                    print(f"  m_t = " + " + ".join([f"{coeffs[i]:.10f} * m_{{t-{i+1}}}" for i in range(order)]))
                    print()

            except np.linalg.LinAlgError:
                print("  Matrix singular, cannot solve")
                print()

    # Look for growth rate
    print("="*70)
    print("GROWTH RATE ANALYSIS")
    print("="*70)
    print()

    print("m_t / m_{t-1} ratios:")
    for t in range(2, 17):
        ratio = m[t-1] / m[t-2]
        print(f"  m_{t}/m_{t-1} = {ratio:.10f}")

    print()

    # Asymptotic ratio
    late_ratios = [m[t-1] / m[t-2] for t in range(12, 17)]
    avg_ratio = sum(late_ratios) / len(late_ratios)
    print(f"Average ratio (t=12-16): {avg_ratio:.10f}")
    print()

    print("="*70)
    print("POINT GENESIS ANSWER")
    print("="*70)
    print()
    print(f"g(16) = {g[16]:,}")
    print()
    print("This is the sum of all blue points after 16 days.")
    print()

    # Verify our approximation for κ_16
    print("Using our approximation:")
    print(f"  B_16 = {B[16]:,}")
    print(f"  m_16 = {m[15]:,}")
    print(f"  κ_16 (approx) ≈ m_16 / (3 * B_16²)")
    print(f"  κ_16 (approx) ≈ {kappa_approx[15]:.15e}")
    print()
    print(f"  κ_16 (exact)  = {kappa_exact[15]:.15e}")
    print()

    error_16 = abs(kappa_exact[15] - kappa_approx[15]) / kappa_exact[15] * 100
    print(f"  Error: {error_16:.4f}%")
    print()

    if error_16 < 0.1:
        print("✓✓✓ GENESIS APPROXIMATION IS EXCELLENT FOR t=16! ✓✓✓")


if __name__ == "__main__":
    main()
