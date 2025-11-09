#!/usr/bin/env python3
"""
Quick κ_t refinement using pre-computed data.

Uses known values from earlier experiments.
"""

import math

def fit_power_law(x_vals, y_vals):
    """Fit y = a * x^b using log-log linear regression."""
    log_x = [math.log(x) for x in x_vals if x > 0]
    log_y = [math.log(y) for y in y_vals if y > 0]

    n = len(log_x)
    sum_x = sum(log_x)
    sum_y = sum(log_y)
    sum_xx = sum(x*x for x in log_x)
    sum_xy = sum(log_x[i] * log_y[i] for i in range(n))

    b = (n * sum_xy - sum_x * sum_y) / (n * sum_xx - sum_x * sum_x)
    log_a = (sum_y - b * sum_x) / n
    a = math.exp(log_a)

    return a, b


def fit_combined_model(gaps, m_vals, kappa_vals):
    """Fit κ_t = a * (gap)^b * (m)^c."""
    best_params = None
    best_error = float('inf')

    for b in [-2.0, -1.75, -1.5, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5]:
        for c in [-2.0, -1.75, -1.5, -1.25, -1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0]:
            if gaps[0] ** b * m_vals[0] ** c != 0:
                a = kappa_vals[0] / (gaps[0] ** b * m_vals[0] ** c)

                errors = []
                for i in range(len(gaps)):
                    pred = a * (gaps[i] ** b) * (m_vals[i] ** c)
                    if kappa_vals[i] > 0:
                        rel_error = abs(pred - kappa_vals[i]) / kappa_vals[i]
                        errors.append(rel_error)

                if errors:
                    avg_error = sum(errors) / len(errors)
                    if avg_error < best_error:
                        best_error = avg_error
                        best_params = (a, b, c)

    return best_params, best_error


def main():
    print("="*70)
    print("REFINED κ_t PREDICTION (Quick Version)")
    print("="*70)
    print()

    # Known data from previous experiments
    days = [1, 2, 3]
    hilbert_gaps = [178827726.6, 47091745.8, 18653203.8]
    m_vals = [6, 20, 156]
    kappa_vals = [1.0, 0.123, 0.074]

    print("Input Data:")
    print(f"{'Day':<5} {'Hilbert Gap':<15} {'m_t':<10} {'κ_t'}")
    print("-"*45)
    for i in range(len(days)):
        print(f"{days[i]:<5} {hilbert_gaps[i]:<15.1f} {m_vals[i]:<10} {kappa_vals[i]}")
    print()

    # Add Days 4-5 if we have the data
    print("Extending with Days 4-5 (using estimated gaps):")
    # From exponential fit: gap_t ≈ A * exp(B * t)
    # We'll estimate based on Days 1-3 pattern

    # Fit exponential to gaps
    log_gaps = [math.log(g) for g in hilbert_gaps]
    n = len(days)
    sum_t = sum(days)
    sum_lg = sum(log_gaps)
    sum_tt = sum(t*t for t in days)
    sum_tlg = sum(days[i] * log_gaps[i] for i in range(n))

    B = (n * sum_tlg - sum_t * sum_lg) / (n * sum_tt - sum_t * sum_t)
    log_A = (sum_lg - B * sum_t) / n
    A = math.exp(log_A)

    print(f"  Gap extrapolation: gap(t) ≈ {A:.0f} * exp({B:.4f} * t)")
    print()

    # Add Days 4-5 using estimated gaps and known m, κ
    days_extended = days + [4, 5]
    gaps_extended = hilbert_gaps + [A * math.exp(B * 4), A * math.exp(B * 5)]
    m_extended = m_vals + [1462, 17515]  # known from OEIS
    kappa_extended = kappa_vals + [0.0148, 0.00274]  # known from OEIS

    print(f"{4:<5} {gaps_extended[3]:<15.1f} {m_extended[3]:<10} {kappa_extended[3]} (estimated gap)")
    print(f"{5:<5} {gaps_extended[4]:<15.1f} {m_extended[4]:<10} {kappa_extended[4]} (estimated gap)")
    print()

    # MODEL FITTING
    print("="*70)
    print("MODEL FITTING")
    print("="*70)
    print()

    # Model 1: κ_t = a * (gap)^b
    print("Model 1: κ_t = a * (gap)^b")
    a1, b1 = fit_power_law(gaps_extended, kappa_extended)
    print(f"  Formula: κ_t ≈ {a1:.3e} * (gap)^{b1:.4f}")
    print()
    print("  Predictions:")
    errors1 = []
    for i in range(len(days_extended)):
        pred = a1 * (gaps_extended[i] ** b1)
        actual = kappa_extended[i]
        error = abs(pred - actual) / actual * 100
        errors1.append(error)
        marker = " ✓" if error < 20 else (" ~" if error < 50 else " ✗")
        print(f"    Day {days_extended[i]}: {pred:.6f} vs {actual:.6f}  error={error:5.1f}%{marker}")
    avg_err1 = sum(errors1) / len(errors1)
    print(f"  Average error: {avg_err1:.1f}%")
    print()

    # Model 2: κ_t = a * (m)^b
    print("Model 2: κ_t = a * (m)^b")
    a2, b2 = fit_power_law(m_extended, kappa_extended)
    print(f"  Formula: κ_t ≈ {a2:.3e} * (m)^{b2:.4f}")
    print()
    print("  Predictions:")
    errors2 = []
    for i in range(len(days_extended)):
        pred = a2 * (m_extended[i] ** b2)
        actual = kappa_extended[i]
        error = abs(pred - actual) / actual * 100
        errors2.append(error)
        marker = " ✓" if error < 20 else (" ~" if error < 50 else " ✗")
        print(f"    Day {days_extended[i]}: {pred:.6f} vs {actual:.6f}  error={error:5.1f}%{marker}")
    avg_err2 = sum(errors2) / len(errors2)
    print(f"  Average error: {avg_err2:.1f}%")
    print()

    # Model 3: κ_t = a * (gap)^b * (m)^c
    print("Model 3: κ_t = a * (gap)^b * (m)^c (COMBINED)")
    params3, error3 = fit_combined_model(gaps_extended, m_extended, kappa_extended)
    a3, b3, c3 = params3
    print(f"  Formula: κ_t ≈ {a3:.3e} * (gap)^{b3:.4f} * (m)^{c3:.4f}")
    print()
    print("  Predictions:")
    errors3 = []
    for i in range(len(days_extended)):
        pred = a3 * (gaps_extended[i] ** b3) * (m_extended[i] ** c3)
        actual = kappa_extended[i]
        error = abs(pred - actual) / actual * 100
        errors3.append(error)
        marker = " ✓" if error < 20 else (" ~" if error < 50 else " ✗")
        print(f"    Day {days_extended[i]}: {pred:.6f} vs {actual:.6f}  error={error:5.1f}%{marker}")
    avg_err3 = sum(errors3) / len(errors3)
    print(f"  Average error: {avg_err3:.1f}%")
    print()

    # Model comparison
    print("="*70)
    print("BEST MODEL")
    print("="*70)
    print()

    models = [
        ("Hilbert gap", avg_err1, a1, b1, None),
        ("Point count", avg_err2, a2, b2, None),
        ("Combined", avg_err3, a3, b3, c3)
    ]

    best = min(models, key=lambda m: m[1])
    print(f"Winner: {best[0]} (avg error = {best[1]:.1f}%)")
    print()

    if best[0] == "Combined":
        print(f"  κ_t ≈ {best[2]:.3e} * (gap)^{best[3]:.4f} * (m)^{best[4]:.4f}")
        print()
        print("  Physical interpretation:")
        print(f"    - Hilbert gap exponent {best[3]:.4f}: ", end="")
        if best[3] < -1:
            print("strong inverse (denser → lower κ)")
        elif best[3] < 0:
            print("moderate inverse (some density effect)")
        else:
            print("direct relationship (unexpected!)")

        print(f"    - Point count exponent {best[4]:.4f}: ", end="")
        if best[4] < -1:
            print("strong inverse (more points → lower κ)")
        elif best[4] < 0:
            print("moderate inverse")
        else:
            print("direct relationship")
    else:
        print(f"  κ_t ≈ {best[2]:.3e} * (feature)^{best[3]:.4f}")

    print()
    print("="*70)
    print("EXTRAPOLATION TO DAYS 6-10")
    print("="*70)
    print()

    # Extrapolate
    print(f"{'Day':<5} {'Est. Gap':<15} {'m (OEIS)':<15} {'Pred. κ_t':<15} {'OEIS κ_t':<15} {'Error'}")
    print("-"*80)

    m_oeis = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]

    # Compute OEIS κ from m values
    # κ_t = m_t / (6 * P_t)
    # P_t = C(m_{t-1}, 2) + m_{t-1} * B_{t-2}

    B_vals = [2]  # B_0 = 2
    for i in range(len(m_oeis) - 1):
        B_vals.append(B_vals[-1] + m_oeis[i])

    kappa_oeis = []
    for t in range(1, min(len(m_oeis) + 1, 11)):
        if t == 1:
            P_t = 1  # C(2,2)
        else:
            m_prev = m_oeis[t-2]
            B_prev2 = B_vals[t-2] if t >= 2 else 0
            P_t = m_prev * (m_prev - 1) // 2 + m_prev * B_prev2

        kappa_t = m_oeis[t-1] / (6 * P_t)
        kappa_oeis.append(kappa_t)

    for t in range(1, 11):
        est_gap = A * math.exp(B * t)
        m_t = m_oeis[t-1]

        if best[0] == "Combined":
            pred_kappa = best[2] * (est_gap ** best[3]) * (m_t ** best[4])
        elif best[0] == "Hilbert gap":
            pred_kappa = best[2] * (est_gap ** best[3])
        else:
            pred_kappa = best[2] * (m_t ** best[3])

        oeis_kappa = kappa_oeis[t-1]
        error = abs(pred_kappa - oeis_kappa) / oeis_kappa * 100

        marker = " ✓" if error < 20 else (" ~" if error < 50 else " ✗")
        print(f"{t:<5} {est_gap:<15.0f} {m_t:<15} {pred_kappa:<15.3e} {oeis_kappa:<15.3e} {error:5.1f}%{marker}")

    print()
    print("="*70)
    print("CONCLUSION")
    print("="*70)
    print()
    print("✓ Best model identified")
    print(f"  Average error on training data (Days 1-5): {best[1]:.1f}%")
    print()
    print("Next steps:")
    print("  1. Test with actual Day 6-10 Hilbert gaps (requires computation)")
    print("  2. Refine exponents with more data points")
    print("  3. Investigate physical meaning of exponents")


if __name__ == "__main__":
    main()
