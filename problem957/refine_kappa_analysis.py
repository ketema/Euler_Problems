#!/usr/bin/env python3
"""
Deep Analysis of κ_t ~ gap Relationship

The confusion: Document says "inverse" but fit shows "direct" correlation.

Let's analyze the actual data systematically to understand what's happening.
"""

import math


def main():
    print("="*70)
    print("SYSTEMATIC ANALYSIS OF κ_t ~ gap RELATIONSHIP")
    print("="*70)
    print()

    # Known data
    days = [1, 2, 3]
    gaps = [178827726.6, 47091745.8, 18653203.8]
    kappa = [1.0, 0.123, 0.074]

    print("OBSERVATION 1: BOTH GAP AND κ DECREASE")
    print("="*70)
    print()
    print(f"{'Day':<5} {'Gap':<20} {'κ_t':<15} {'Gap Direction':<15} {'κ Direction'}")
    print("-"*75)
    for i in range(len(days)):
        if i == 0:
            gap_dir = "—"
            kappa_dir = "—"
        else:
            gap_dir = "↓ DECREASE" if gaps[i] < gaps[i-1] else "↑ INCREASE"
            kappa_dir = "↓ DECREASE" if kappa[i] < kappa[i-1] else "↑ INCREASE"
        print(f"{days[i]:<5} {gaps[i]:<20.0f} {kappa[i]:<15.6f} {gap_dir:<15} {kappa_dir}")

    print()
    print("✓ Both gap and κ decrease together → POSITIVE CORRELATION")
    print()

    print("OBSERVATION 2: CORRELATION TEST")
    print("="*70)
    print()

    # Compute correlation coefficient
    mean_gap = sum(gaps) / len(gaps)
    mean_kappa = sum(kappa) / len(kappa)

    cov = sum((gaps[i] - mean_gap) * (kappa[i] - mean_kappa) for i in range(len(gaps)))
    var_gap = sum((g - mean_gap)**2 for g in gaps)
    var_kappa = sum((k - mean_kappa)**2 for k in kappa)

    corr = cov / (var_gap * var_kappa) ** 0.5

    print(f"Pearson correlation: r = {corr:.4f}")
    if corr > 0.9:
        print("  → STRONG POSITIVE correlation")
    elif corr > 0.5:
        print("  → MODERATE POSITIVE correlation")
    elif corr < -0.9:
        print("  → STRONG NEGATIVE (inverse) correlation")
    else:
        print("  → OTHER")
    print()

    print("OBSERVATION 3: PHYSICAL MECHANISM")
    print("="*70)
    print()
    print("Gap decreases → Density increases → Points cluster")
    print("Clustered points → More lines intersect at same location")
    print("More coincidence → κ_t DECREASES")
    print()
    print("Therefore:")
    print("  Gap ↓ → Density ↑ → Coincidence ↑ → κ ↓")
    print("  Gap ↑ → Density ↓ → Coincidence ↓ → κ ↑")
    print()
    print("✓ This matches our data: gap ↓ and κ ↓ move together")
    print()

    print("OBSERVATION 4: MATHEMATICAL FORM")
    print("="*70)
    print()
    print("Since gap and κ both decrease together, we can model as:")
    print("  Option A: κ_t = a · gap^b  where b > 0  (power law)")
    print("  Option B: κ_t = a · exp(b·gap)  where b > 0  (exponential)")
    print("  Option C: κ_t = a / (c - gap)  where c < min(gap)  (rational)")
    print()

    # Fit power law
    log_gaps = [math.log(g) for g in gaps]
    log_kappas = [math.log(k) for k in kappa]

    n = len(gaps)
    sum_lg = sum(log_gaps)
    sum_lk = sum(log_kappas)
    sum_lglg = sum(lg*lg for lg in log_gaps)
    sum_lglk = sum(log_gaps[i] * log_kappas[i] for i in range(n))

    b = (n * sum_lglk - sum_lg * sum_lk) / (n * sum_lglg - sum_lg * sum_lg)
    log_a = (sum_lk - b * sum_lg) / n
    a = math.exp(log_a)

    print(f"Power law fit: κ_t = {a:.3e} · gap^{b:.4f}")
    print()

    print("Predictions:")
    errors = []
    for i in range(len(gaps)):
        pred = a * (gaps[i] ** b)
        actual = kappa[i]
        error = abs(pred - actual) / actual * 100
        errors.append(error)
        print(f"  Day {i+1}: {pred:.6f} vs {actual:.6f}  ({error:.1f}% error)")

    avg_err = sum(errors) / len(errors)
    print(f"\nAverage error: {avg_err:.1f}%")
    print()

    print("="*70)
    print("THE REAL ISSUE: EXTRAPOLATION")
    print("="*70)
    print()

    print("The model κ_t = a · gap^b works well for Days 1-3 (where we")
    print("have ACTUAL gap measurements from Hilbert curve analysis).")
    print()
    print("The problem occurs when we EXTRAPOLATE gaps to Days 4-10")
    print("using gap(t) = A · exp(B·t).")
    print()

    # Fit exponential to gaps
    sum_t = sum(days)
    sum_lglg = sum(log_gaps)
    sum_tt = sum(t*t for t in days)
    sum_tlg = sum(days[i] * log_gaps[i] for i in range(n))

    B = (n * sum_tlg - sum_t * sum_lglg) / (n * sum_tt - sum_t * sum_t)
    log_A = (sum_lglg - B * sum_t) / n
    A = math.exp(log_A)

    print(f"Gap extrapolation: gap(t) = {A:.0f} · exp({B:.4f}·t)")
    print()
    print("This assumes gap continues to decrease EXPONENTIALLY forever.")
    print("But reality might be different:")
    print("  - Gap might level off (reach a floor)")
    print("  - Gap decay might be power law instead: gap(t) = A · t^α")
    print("  - Gap behavior might change after Day 5 (regime change)")
    print()

    print("="*70)
    print("DIAGNOSIS: TWO SEPARATE ISSUES")
    print("="*70)
    print()

    print("ISSUE 1: Functional form κ_t = f(gap)")
    print("  Status: ✓ SOLVED")
    print("  Solution: κ_t = a · gap^b with b ≈ 1.18 (positive exponent)")
    print("  Performance: 26% average error on Days 1-3")
    print()

    print("ISSUE 2: Gap extrapolation gap(t) = g(t)")
    print("  Status: ✗ UNSOLVED")
    print("  Problem: Exponential extrapolation fails for Days 6-10")
    print("  Evidence: Model works well on Days 1-5, catastrophic on Days 6-10")
    print()

    print("="*70)
    print("CONCLUSION")
    print("="*70)
    print()

    print("The relationship κ_t ∝ gap^1.18 is CORRECT for the data we have.")
    print()
    print("The phrase 'inverse relationship' in the documentation was misleading.")
    print("What was meant: κ and gap both DECREASE (they co-vary), but the")
    print("CAUSAL chain involves density = 1/gap.")
    print()
    print("The mathematical form should be:")
    print(f"  κ_t = {a:.3e} · gap^{b:.4f}  (direct power law)")
    print()
    print("NOT:")
    print("  κ_t = a · (1/gap)^b  (this gives wrong predictions)")
    print()
    print("To improve predictions for Days 6-10, we need:")
    print("  1. Compute ACTUAL Hilbert gaps for Days 4-10 (no extrapolation)")
    print("  2. OR: Use better gap extrapolation (power law, piecewise, etc.)")
    print("  3. OR: Include additional features (variety degree d_t, point count m_t)")
    print()


if __name__ == "__main__":
    main()
