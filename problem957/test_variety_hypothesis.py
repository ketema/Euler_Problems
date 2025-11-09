#!/usr/bin/env python3
"""
Test the Algebraic Variety Hypothesis for Days 1-5

Hypothesis: Day t intersection points lie on an algebraic variety V_t of degree d_t.

Method:
1. Compute Day t points using existing simulation
2. For increasing degree d = 2, 3, 4, ..., fit polynomial
3. Measure residual error (RMS)
4. Use AIC to select minimal degree
5. Compare to Day 1 baseline (known degree 2, RMS = 0.00058)
"""

import numpy as np
from scipy.optimize import least_squares
from scipy.linalg import lstsq
import sys
import os

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '.'))

from src.geometry import Point, Line
from src.propagation import propagate_one_day


def polynomial_terms_2d(x, y, degree):
    """
    Generate all polynomial terms up to given degree.

    For degree 2: [1, x, y, x², xy, y²]
    For degree 3: [1, x, y, x², xy, y², x³, x²y, xy², y³]
    etc.

    Returns: numpy array of shape (n_points, n_terms)
    """
    terms = []
    for total_deg in range(degree + 1):
        for x_pow in range(total_deg + 1):
            y_pow = total_deg - x_pow
            terms.append((x ** x_pow) * (y ** y_pow))

    return np.column_stack(terms)


def fit_polynomial_curve(points, degree):
    """
    Fit polynomial of given degree to points.

    Returns:
        coeffs: Polynomial coefficients
        rms_error: RMS residual error
        n_terms: Number of polynomial terms
    """
    # Extract coordinates
    coords = np.array([(p.x, p.y) for p in points])
    x = coords[:, 0]
    y = coords[:, 1]

    # Build design matrix
    X = polynomial_terms_2d(x, y, degree)
    n_terms = X.shape[1]

    # We want f(x,y) = 0, so we'll normalize and solve
    # Use SVD to find nullspace (smallest singular value direction)
    U, s, Vt = np.linalg.svd(X, full_matrices=False)

    # Smallest singular value corresponds to best-fit polynomial
    coeffs = Vt[-1, :]

    # Compute residual
    residuals = X @ coeffs
    rms_error = np.sqrt(np.mean(residuals ** 2))

    # Normalize coefficients so largest is 1
    max_coeff = np.max(np.abs(coeffs))
    if max_coeff > 0:
        coeffs = coeffs / max_coeff
        residuals = residuals / max_coeff
        rms_error = rms_error / max_coeff

    return coeffs, rms_error, n_terms


def compute_aic(rms_error, n_points, n_params):
    """
    Compute AIC (Akaike Information Criterion) for model selection.

    Lower AIC = better model (balances fit quality vs complexity)
    """
    # Assume Gaussian errors
    if rms_error == 0:
        return float('inf')  # Perfect fit, but suspicious

    # Log-likelihood for Gaussian with variance σ²
    sigma_sq = rms_error ** 2
    log_likelihood = -n_points / 2 * (np.log(2 * np.pi * sigma_sq) + 1)

    # AIC = 2k - 2ln(L)
    aic = 2 * n_params - 2 * log_likelihood

    return aic


def test_day_t_variety(day, points):
    """Test which polynomial degree best fits day t points."""

    n_points = len(points)

    print(f"\n{'='*70}")
    print(f"Day {day}: Testing {n_points} points")
    print(f"{'='*70}")

    results = []

    # Test degrees 2 through min(10, n_points // 2)
    max_degree = min(10, n_points // 2)

    for degree in range(2, max_degree + 1):
        # Count terms
        n_terms = (degree + 1) * (degree + 2) // 2

        if n_terms > n_points:
            print(f"  Degree {degree}: SKIP (underdetermined: {n_terms} params > {n_points} points)")
            break

        # Fit polynomial
        coeffs, rms_error, actual_n_terms = fit_polynomial_curve(points, degree)

        # Compute AIC
        aic = compute_aic(rms_error, n_points, actual_n_terms)

        results.append({
            'degree': degree,
            'n_terms': actual_n_terms,
            'rms_error': rms_error,
            'aic': aic,
            'coeffs': coeffs
        })

        print(f"  Degree {degree}: {actual_n_terms:2d} terms, RMS = {rms_error:.6f}, AIC = {aic:.2f}")

    # Select best by AIC
    best = min(results, key=lambda r: r['aic'])

    print(f"\n  BEST FIT: Degree {best['degree']} (AIC = {best['aic']:.2f}, RMS = {best['rms_error']:.6f})")

    # Compare to Day 1 baseline
    if day == 1:
        print(f"  Day 1 baseline established: degree {best['degree']}, RMS = {best['rms_error']:.6f}")
    else:
        print(f"  Day 1 baseline: degree 2, RMS = 0.00058")
        if best['rms_error'] < 0.01:
            print(f"  ✓ HYPOTHESIS SUPPORTED: Points lie on degree-{best['degree']} curve (RMS < 0.01)")
        else:
            print(f"  ✗ HYPOTHESIS REJECTED: Poor fit (RMS = {best['rms_error']:.6f} >> 0.01)")

    return best


def main():
    """Test variety hypothesis for days 1-5."""

    print("="*70)
    print("ALGEBRAIC VARIETY HYPOTHESIS TEST")
    print("="*70)
    print("\nHypothesis: Day t intersection points lie on algebraic variety V_t")
    print("of degree d_t, where d_t increases with t.")
    print("\nMethod: Polynomial fitting with AIC model selection")
    print("Baseline: Day 1 known to be degree 2 (hyperbola, RMS = 0.00058)")

    # Initial configuration (from OEIS optimal config)
    red_points = [
        Point(-1.1420985748, -3.1278529420),
        Point(1.7213348846, -0.8343651343),
        Point(4.3760906863, 2.3859745813)
    ]

    blue_points = [
        Point(-1.8437265624, 1.4483260402),
        Point(-1.0486909239, 2.1320688328)
    ]

    # Test days 1-5
    all_results = {}

    for day in range(1, 6):
        # Propagate one day
        new_blues = propagate_one_day(red_points, blue_points)

        # Test variety hypothesis
        result = test_day_t_variety(day, new_blues)
        all_results[day] = result

        # Update blue points for next day
        blue_points = blue_points + new_blues

    # Summary
    print(f"\n{'='*70}")
    print("SUMMARY: Variety Degrees d_t")
    print(f"{'='*70}")
    print(f"{'Day':<5} {'Points':<8} {'Degree d_t':<12} {'RMS Error':<12} {'Status':<15}")
    print("-"*70)

    for day in range(1, 6):
        r = all_results[day]
        status = "✓ Supported" if r['rms_error'] < 0.01 else "✗ Rejected"
        print(f"{day:<5} {len(blue_points):<8} {r['degree']:<12} {r['rms_error']:<12.6f} {status:<15}")

    # Check if d_t is increasing
    degrees = [all_results[day]['degree'] for day in range(1, 6)]
    is_increasing = all(degrees[i] <= degrees[i+1] for i in range(len(degrees)-1))

    print(f"\nDegree sequence: {degrees}")
    print(f"Monotonically increasing: {is_increasing}")

    if is_increasing and all(all_results[day]['rms_error'] < 0.01 for day in range(1, 6)):
        print("\n✓✓✓ VARIETY HYPOTHESIS STRONGLY SUPPORTED ✓✓✓")
        print("    Days 1-5 lie on algebraic curves with increasing degree")
    else:
        print("\n⚠ HYPOTHESIS INCONCLUSIVE")
        print("  Further investigation needed")


if __name__ == "__main__":
    main()
