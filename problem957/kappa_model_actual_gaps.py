#!/usr/bin/env python3
"""
κ_t Prediction Model with ACTUAL Hilbert Gaps

Uses real gap measurements (no extrapolation) from Days 1-5.

Key Discovery: Hilbert collisions start at Day 3!
  - Zero gap means multiple points in same Hilbert cell
  - This is direct evidence of spatial clustering
"""

import numpy as np
import math


def fit_power_law(x_vals, y_vals):
    """Fit y = a * x^b using log-log linear regression."""
    # Filter out zeros
    pairs = [(x, y) for x, y in zip(x_vals, y_vals) if x > 0 and y > 0]
    if len(pairs) < 2:
        return None, None

    x_filt, y_filt = zip(*pairs)

    log_x = [math.log(x) for x in x_filt]
    log_y = [math.log(y) for y in y_filt]

    n = len(log_x)
    sum_x = sum(log_x)
    sum_y = sum(log_y)
    sum_xx = sum(x*x for x in log_x)
    sum_xy = sum(log_x[i] * log_y[i] for i in range(n))

    denom = (n * sum_xx - sum_x * sum_x)
    if abs(denom) < 1e-10:
        return None, None

    b = (n * sum_xy - sum_x * sum_y) / denom
    log_a = (sum_y - b * sum_x) / n
    a = math.exp(log_a)

    return a, b


def compute_kappa_oeis(m_vals):
    """Compute κ_t from OEIS sequence using the formula."""
    B_vals = [2]
    for i in range(len(m_vals) - 1):
        B_vals.append(B_vals[-1] + m_vals[i])

    kappa_vals = []
    for t in range(1, len(m_vals) + 1):
        if t == 1:
            P_t = 1
        else:
            m_prev = m_vals[t-2]
            B_prev2 = B_vals[t-2] if t >= 2 else 0
            P_t = m_prev * (m_prev - 1) // 2 + m_prev * B_prev2
        kappa_t = m_vals[t-1] / (6 * P_t)
        kappa_vals.append(kappa_t)

    return kappa_vals


def main():
    print("="*70)
    print("κ_t PREDICTION MODEL - ACTUAL HILBERT GAPS")
    print("="*70)
    print()

    # ACTUAL measured data from compute_gaps_properly.py
    days = [1, 2, 3, 4, 5]
    hilbert_gaps = [134116398.2, 44651235.1, 22932911.2, 2878725.4, 245230.5]
    m_t = [6, 20, 156, 1462, 17515]  # NEW points each day
    zero_gap_pct = [0.0, 0.0, 13.5, 22.3, 13.3]  # Collision rate

    # OEIS data
    m_oeis = [6, 20, 156, 1462, 17515, 242627, 3867126, 69812473, 1399763210, 30876369555]
    kappa_oeis = compute_kappa_oeis(m_oeis)

    print("MEASURED DATA (Days 1-5):")
    print("-"*70)
    print(f"{'Day':<5} {'Hilbert Gap':<20} {'m_t':<15} {'κ_t (OEIS)':<15} {'Collisions %'}")
    print("-"*70)

    for i in range(len(days)):
        print(f"{days[i]:<5} {hilbert_gaps[i]:<20,.1f} {m_t[i]:<15,} "
              f"{kappa_oeis[i]:<15.6f} {zero_gap_pct[i]:<10.1f}")

    print()
    print("KEY OBSERVATION: Zero gaps (collisions) appear starting Day 3!")
    print("  → Multiple points map to same Hilbert cell")
    print("  → Direct evidence of spatial clustering")
    print()

    # Fit κ_t ~ gap relationship on actual data
    print("="*70)
    print("MODEL FITTING: κ_t = a · gap^b")
    print("="*70)
    print()

    kappa_actual = [kappa_oeis[i] for i in range(len(days))]
    a, b = fit_power_law(hilbert_gaps, kappa_actual)

    print(f"Fitted Parameters (Days 1-5 with ACTUAL gaps):")
    print(f"  a = {a:.3e}")
    print(f"  b = {b:.4f}")
    print()
    print(f"Formula: κ_t ≈ {a:.3e} · (Hilbert gap)^{b:.4f}")
    print()

    # Validate on Days 1-5
    print("Validation on Training Data:")
    print(f"{'Day':<5} {'Actual Gap':<20} {'Predicted κ':<15} {'Actual κ':<15} {'Error %'}")
    print("-"*70)

    errors = []
    for i in range(len(days)):
        pred = a * (hilbert_gaps[i] ** b)
        actual = kappa_actual[i]
        error = abs(pred - actual) / actual * 100
        errors.append(error)

        marker = " ✓" if error < 20 else (" ~" if error < 50 else " ✗")
        print(f"{days[i]:<5} {hilbert_gaps[i]:<20,.1f} {pred:<15.6f} {actual:<15.6f} {error:>6.1f}%{marker}")

    avg_error = sum(errors) / len(errors)
    print()
    print(f"Average error: {avg_error:.1f}%")
    print()

    # Compare to previous extrapolated model
    print("="*70)
    print("COMPARISON: ACTUAL vs EXTRAPOLATED GAPS")
    print("="*70)
    print()

    # Previous extrapolated gaps (exponential decay assumption)
    gap_extrap_exp = lambda t: 517280111 * math.exp(-1.1302 * t)

    print(f"{'Day':<5} {'Actual Gap':<20} {'Extrapolated Gap':<20} {'Difference %'}")
    print("-"*70)

    for t in range(1, 6):
        actual_gap = hilbert_gaps[t-1]
        extrap_gap = gap_extrap_exp(t)
        diff_pct = abs(actual_gap - extrap_gap) / actual_gap * 100

        marker = " ✓" if diff_pct < 20 else (" ~" if diff_pct < 50 else " ✗")
        print(f"{t:<5} {actual_gap:<20,.0f} {extrap_gap:<20,.0f} {diff_pct:>6.1f}%{marker}")

    print()
    print("Observation: Exponential extrapolation is reasonable for Days 1-3,")
    print("             but diverges significantly for Days 4-5")
    print()

    # Refit gap extrapolation with Days 1-5
    print("="*70)
    print("UPDATED GAP EXTRAPOLATION (Days 1-5 actual data)")
    print("="*70)
    print()

    a_gap_exp, b_gap_exp = fit_power_law(days, hilbert_gaps)
    print(f"Power law fit: gap(t) = {a_gap_exp:.3e} · t^{b_gap_exp:.4f}")
    print()

    # Try exponential fit
    log_gaps = [math.log(g) for g in hilbert_gaps]
    n = len(days)
    sum_t = sum(days)
    sum_lg = sum(log_gaps)
    sum_tt = sum(t*t for t in days)
    sum_tlg = sum(days[i] * log_gaps[i] for i in range(n))

    B_exp = (n * sum_tlg - sum_t * sum_lg) / (n * sum_tt - sum_t * sum_t)
    log_A_exp = (sum_lg - B_exp * sum_t) / n
    A_exp = math.exp(log_A_exp)

    print(f"Exponential fit: gap(t) = {A_exp:.3e} · exp({B_exp:.4f}·t)")
    print()

    # Test both on Days 1-5
    print("Test on training data:")
    print(f"{'Day':<5} {'Actual':<20} {'Power Law':<20} {'Exponential':<20}")
    print("-"*70)

    for t in range(1, 6):
        actual = hilbert_gaps[t-1]
        pred_pow = a_gap_exp * (t ** b_gap_exp)
        pred_exp = A_exp * math.exp(B_exp * t)

        print(f"{t:<5} {actual:<20,.0f} {pred_pow:<20,.0f} {pred_exp:<20,.0f}")

    print()

    # Extrapolate to Days 6-10 and predict κ
    print("="*70)
    print("PREDICTIONS FOR DAYS 6-10")
    print("="*70)
    print()

    print("Using updated gap extrapolation + κ model:")
    print()
    print(f"{'Day':<5} {'Gap (Power)':<20} {'Gap (Exp)':<20} {'Pred κ (Pow)':<15} {'Pred κ (Exp)':<15} {'OEIS κ':<15} {'Best Error %'}")
    print("-"*120)

    for t in range(1, 11):
        gap_pow = a_gap_exp * (t ** b_gap_exp) if t <= 10 else 0
        gap_exp = A_exp * math.exp(B_exp * t) if t <= 10 else 0

        # Predict κ using both gap estimates
        kappa_pred_pow = a * (gap_pow ** b) if gap_pow > 0 else 0
        kappa_pred_exp = a * (gap_exp ** b) if gap_exp > 0 else 0

        kappa_actual = kappa_oeis[t-1]

        error_pow = abs(kappa_pred_pow - kappa_actual) / kappa_actual * 100 if kappa_pred_pow > 0 else 1000
        error_exp = abs(kappa_pred_exp - kappa_actual) / kappa_actual * 100 if kappa_pred_exp > 0 else 1000

        best_error = min(error_pow, error_exp)
        marker = " ✓" if best_error < 20 else (" ~" if best_error < 50 else " ✗")

        print(f"{t:<5} {gap_pow:<20,.0f} {gap_exp:<20,.0f} {kappa_pred_pow:<15.3e} {kappa_pred_exp:<15.3e} {kappa_actual:<15.3e} {best_error:>6.1f}%{marker}")

    print()

    # Summary
    print("="*70)
    print("SUMMARY")
    print("="*70)
    print()

    print("✓ ACTUAL GAPS MEASURED for Days 1-5 (no extrapolation)")
    print(f"  κ_t model: κ = {a:.3e} · gap^{b:.4f}")
    print(f"  Training error (Days 1-5): {avg_error:.1f}%")
    print()

    print("✓ HILBERT COLLISIONS DISCOVERED")
    print("  Starting Day 3: 13-22% of consecutive points have zero gap")
    print("  → Multiple intersection points in same Hilbert cell")
    print("  → Direct evidence of spatial clustering")
    print()

    print("✓ GAP EXTRAPOLATION UPDATED with Days 1-5 data")
    print(f"  Power law: gap(t) = {a_gap_exp:.3e} · t^{b_gap_exp:.4f}")
    print(f"  Exponential: gap(t) = {A_exp:.3e} · exp({B_exp:.4f}·t)")
    print()

    print("NEXT STEPS:")
    print("  1. Investigate Hilbert collision patterns (decode neighboring indices)")
    print("  2. Analyze geometric significance of collision points")
    print("  3. Compute Day 6+ if computationally feasible")
    print()


if __name__ == "__main__":
    main()
