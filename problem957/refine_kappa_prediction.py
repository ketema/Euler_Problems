#!/usr/bin/env python3
"""
Refine κ_t Prediction Using Multiple Geometric Features

We have multiple signals:
1. Hilbert gap (spatial density)
2. Point count m_t
3. Variety degree d_t (partially known)

Test different models to predict κ_t.
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '.'))

from src.geometry import Point, Line, intersect


def compute_day_intersections(red_points, blue_points, existing_blues=None):
    """Compute day t intersection points."""
    if existing_blues is None:
        all_blues = blue_points
    else:
        all_blues = blue_points + existing_blues

    lines = []
    for r in red_points:
        for b in all_blues:
            lines.append(Line(r, b))

    intersections = []
    for i in range(len(lines)):
        for j in range(i+1, len(lines)):
            pt = intersect(lines[i], lines[j])
            if pt is not None:
                is_new = True
                for r in red_points:
                    if r == pt:
                        is_new = False
                        break
                if is_new:
                    for b in all_blues:
                        if b == pt:
                            is_new = False
                            break
                if is_new:
                    already_exists = False
                    for existing in intersections:
                        if existing == pt:
                            already_exists = True
                            break
                    if not already_exists:
                        intersections.append(pt)
    return intersections


def hilbert_encode_2d(x, y, order=16):
    """Map 2D point to 1D Hilbert curve index."""
    global min_x, max_x, min_y, max_y

    n = 2 ** order
    xi = int((x - min_x) / (max_x - min_x) * (n - 1)) if max_x > min_x else 0
    yi = int((y - min_y) / (max_y - min_y) * (n - 1)) if max_y > min_y else 0

    xi = max(0, min(n - 1, xi))
    yi = max(0, min(n - 1, yi))

    d = 0
    s = n // 2

    while s > 0:
        rx = 1 if (xi & s) > 0 else 0
        ry = 1 if (yi & s) > 0 else 0
        d += s * s * ((3 * rx) ^ ry)

        if ry == 0:
            if rx == 1:
                xi = n - 1 - xi
                yi = n - 1 - yi
            xi, yi = yi, xi

        s //= 2

    return d


def compute_hilbert_gap(points):
    """Compute average Hilbert gap for points."""
    indices = [hilbert_encode_2d(p.x, p.y) for p in points]
    indices_sorted = sorted(indices)

    if len(indices_sorted) < 2:
        return 0

    gaps = [indices_sorted[i+1] - indices_sorted[i] for i in range(len(indices_sorted)-1)]
    avg_gap = sum(gaps) / len(gaps)

    return avg_gap


def fit_power_law(x_vals, y_vals):
    """
    Fit y = a * x^b using log-log linear regression.

    log(y) = log(a) + b*log(x)
    """
    if len(x_vals) < 2:
        return None, None

    # Take logs
    log_x = [__import__('math').log(x) for x in x_vals if x > 0]
    log_y = [__import__('math').log(y) for y in y_vals if y > 0]

    if len(log_x) < 2:
        return None, None

    # Linear regression on log-log
    n = len(log_x)
    sum_x = sum(log_x)
    sum_y = sum(log_y)
    sum_xx = sum(x*x for x in log_x)
    sum_xy = sum(log_x[i] * log_y[i] for i in range(n))

    # Solve for b (slope)
    b = (n * sum_xy - sum_x * sum_y) / (n * sum_xx - sum_x * sum_x)

    # Solve for log(a) (intercept)
    log_a = (sum_y - b * sum_x) / n
    a = __import__('math').exp(log_a)

    return a, b


def predict_kappa(gap, m, model_type, params):
    """Predict κ_t using different models."""

    if model_type == "hilbert_power":
        # κ_t = a * (gap)^b
        a, b = params
        return a * (gap ** b)

    elif model_type == "point_power":
        # κ_t = a * (m)^b
        a, b = params
        return a * (m ** b)

    elif model_type == "combined_power":
        # κ_t = a * (gap)^b * (m)^c
        a, b, c = params
        return a * (gap ** b) * (m ** c)

    elif model_type == "ratio":
        # κ_t = a * gap / m^b
        a, b = params
        return a * gap / (m ** b)

    return None


def fit_combined_model(gaps, m_vals, kappa_vals):
    """
    Fit κ_t = a * (gap)^b * (m)^c using iterative search.

    This is nonlinear, so use simple grid search.
    """
    best_params = None
    best_error = float('inf')

    # Grid search over exponents
    for b in [-2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0]:
        for c in [-2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0]:
            # Compute a from first data point
            if gaps[0] ** b * m_vals[0] ** c != 0:
                a = kappa_vals[0] / (gaps[0] ** b * m_vals[0] ** c)

                # Compute error
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
    print("REFINING κ_t PREDICTION")
    print("="*70)
    print()
    print("Goal: Find best model to predict κ_t from geometric features")
    print()

    # Configuration
    red_points = [
        Point(-1.1420985748, -3.1278529420),
        Point(1.7213348846, -0.8343651343),
        Point(4.3760906863, 2.3859745813)
    ]

    blue_points = [
        Point(-1.8437265624, 1.4483260402),
        Point(-1.0486909239, 2.1320688328)
    ]

    # Compute days 1-5
    print("Computing days 1-5...")

    all_blues = blue_points[:]
    day_data = []

    for t in range(1, 6):
        new_blues = compute_day_intersections(red_points, blue_points,
                                              all_blues if t > 1 else None)
        all_blues = blue_points + new_blues if t == 1 else all_blues + new_blues
        day_data.append((t, new_blues))
        print(f"  Day {t}: {len(new_blues)} points")

    print()

    # Compute global bounds for Hilbert
    global min_x, max_x, min_y, max_y
    all_points = []
    for _, points in day_data:
        all_points.extend(points)

    min_x = min(p.x for p in all_points)
    max_x = max(p.x for p in all_points)
    min_y = min(p.y for p in all_points)
    max_y = max(p.y for p in all_points)

    # Compute features for each day
    print("Computing features...")
    print()

    features = []
    for day, points in day_data:
        gap = compute_hilbert_gap(points)
        m = len(points)
        features.append((day, gap, m))
        print(f"  Day {day}: gap = {gap:.0f}, m = {m}")

    print()

    # Known κ_t values (from OEIS)
    kappa_known = {
        1: 1.0,
        2: 0.123,
        3: 0.074,
        4: 0.0148,
        5: 0.00274
    }

    # Extract training data
    gaps = [f[1] for f in features]
    m_vals = [f[2] for f in features]
    kappa_vals = [kappa_known[f[0]] for f in features]

    print("="*70)
    print("MODEL FITTING")
    print("="*70)
    print()

    # Model 1: κ_t = a * (gap)^b
    print("Model 1: κ_t = a * (gap)^b")
    a1, b1 = fit_power_law(gaps, kappa_vals)
    if a1 and b1:
        print(f"  Parameters: a = {a1:.6f}, b = {b1:.4f}")
        print(f"  Formula: κ_t ≈ {a1:.6f} * (gap)^{b1:.4f}")
        print()
        print("  Predictions:")
        for i, (day, gap, m) in enumerate(features):
            pred = a1 * (gap ** b1)
            actual = kappa_vals[i]
            error = abs(pred - actual) / actual * 100
            print(f"    Day {day}: pred = {pred:.6f}, actual = {actual:.6f}, error = {error:.1f}%")
    print()

    # Model 2: κ_t = a * (m)^b
    print("Model 2: κ_t = a * (m)^b")
    a2, b2 = fit_power_law(m_vals, kappa_vals)
    if a2 and b2:
        print(f"  Parameters: a = {a2:.6f}, b = {b2:.4f}")
        print(f"  Formula: κ_t ≈ {a2:.6f} * (m)^{b2:.4f}")
        print()
        print("  Predictions:")
        for i, (day, gap, m) in enumerate(features):
            pred = a2 * (m ** b2)
            actual = kappa_vals[i]
            error = abs(pred - actual) / actual * 100
            print(f"    Day {day}: pred = {pred:.6f}, actual = {actual:.6f}, error = {error:.1f}%")
    print()

    # Model 3: κ_t = a * (gap)^b * (m)^c
    print("Model 3: κ_t = a * (gap)^b * (m)^c (combined)")
    params3, error3 = fit_combined_model(gaps, m_vals, kappa_vals)
    if params3:
        a3, b3, c3 = params3
        print(f"  Parameters: a = {a3:.6f}, b = {b3:.4f}, c = {c3:.4f}")
        print(f"  Formula: κ_t ≈ {a3:.6f} * (gap)^{b3:.4f} * (m)^{c3:.4f}")
        print(f"  Average relative error: {error3*100:.1f}%")
        print()
        print("  Predictions:")
        for i, (day, gap, m) in enumerate(features):
            pred = a3 * (gap ** b3) * (m ** c3)
            actual = kappa_vals[i]
            error = abs(pred - actual) / actual * 100
            print(f"    Day {day}: pred = {pred:.6f}, actual = {actual:.6f}, error = {error:.1f}%")
    print()

    # Compare models
    print("="*70)
    print("MODEL COMPARISON")
    print("="*70)
    print()

    models = [
        ("Hilbert gap only", a1, b1, None),
        ("Point count only", a2, b2, None),
        ("Combined", a3, b3, c3)
    ]

    print(f"{'Model':<20} {'Avg Error':<15} {'Best For'}")
    print("-"*70)

    for model_name, *params in models:
        errors = []
        for i, (day, gap, m) in enumerate(features):
            if model_name == "Hilbert gap only":
                pred = params[0] * (gap ** params[1])
            elif model_name == "Point count only":
                pred = params[0] * (m ** params[1])
            else:
                pred = params[0] * (gap ** params[1]) * (m ** params[2])

            actual = kappa_vals[i]
            rel_error = abs(pred - actual) / actual
            errors.append(rel_error)

        avg_error = sum(errors) / len(errors) * 100
        best_day = features[errors.index(min(errors))][0]
        print(f"{model_name:<20} {avg_error:<15.1f}% Day {best_day}")

    print()
    print("="*70)
    print("BEST MODEL SELECTED")
    print("="*70)
    print()

    # Select best model (lowest average error)
    if params3 and error3 < 0.5:  # If combined model < 50% error
        print("✓ Combined model: κ_t = a * (gap)^b * (m)^c")
        print(f"  a = {a3:.6f}")
        print(f"  b = {b3:.4f}")
        print(f"  c = {c3:.4f}")
        print()
        print(f"  Average error: {error3*100:.1f}%")
        best_model = (a3, b3, c3)
        best_type = "combined"
    elif a1 and b1:
        print("✓ Hilbert gap model: κ_t = a * (gap)^b")
        print(f"  a = {a1:.6f}")
        print(f"  b = {b1:.4f}")
        best_model = (a1, b1)
        best_type = "hilbert"
    else:
        print("✗ No good model found")
        return

    print()
    print("="*70)
    print("EXTRAPOLATION TO FUTURE DAYS")
    print("="*70)
    print()

    print("Note: This requires estimating Hilbert gaps for Days 6+")
    print("      Using exponential fit to gap sequence")
    print()

    # Fit exponential to gaps: gap_t = A * exp(B * t)
    days = [f[0] for f in features]
    import math
    log_gaps = [math.log(g) for g in gaps]

    # Linear regression on semi-log
    n = len(days)
    sum_t = sum(days)
    sum_lg = sum(log_gaps)
    sum_tt = sum(t*t for t in days)
    sum_tlg = sum(days[i] * log_gaps[i] for i in range(n))

    B = (n * sum_tlg - sum_t * sum_lg) / (n * sum_tt - sum_t * sum_t)
    log_A = (sum_lg - B * sum_t) / n
    A = math.exp(log_A)

    print(f"Gap extrapolation: gap(t) ≈ {A:.0f} * exp({B:.4f} * t)")
    print()

    # Predict κ_t for days 6-10
    print(f"{'Day':<5} {'Est. Gap':<15} {'Est. m':<10} {'Pred. κ_t':<15} {'OEIS κ_t':<15} {'Error'}")
    print("-"*70)

    # Known values for comparison (from OEIS)
    kappa_oeis = {
        1: 1.0,
        2: 0.123,
        3: 0.074,
        4: 0.0148,
        5: 0.00274,
        # Days 6+ would need to be computed from OEIS m_t values
        # For now, show prediction only
    }

    # Known m_t from OEIS
    m_oeis = {
        1: 6,
        2: 20,
        3: 156,
        4: 1462,
        5: 17515,
        6: 242627,
        7: 3867126,
        8: 69812473,
        9: 1399763210,
        10: 30876369555
    }

    for day in range(1, 11):
        # Estimate gap
        est_gap = A * math.exp(B * day)

        # Use actual m if known
        est_m = m_oeis.get(day, m_vals[-1] * (day / days[-1])**2)  # rough extrapolation

        # Predict κ
        if best_type == "combined":
            pred_kappa = best_model[0] * (est_gap ** best_model[1]) * (est_m ** best_model[2])
        else:
            pred_kappa = best_model[0] * (est_gap ** best_model[1])

        # Compare to OEIS
        oeis_kappa = kappa_oeis.get(day, None)

        if oeis_kappa:
            error = abs(pred_kappa - oeis_kappa) / oeis_kappa * 100
            print(f"{day:<5} {est_gap:<15.0f} {est_m:<10} {pred_kappa:<15.6f} {oeis_kappa:<15.6f} {error:.1f}%")
        else:
            print(f"{day:<5} {est_gap:<15.0f} {est_m:<10} {pred_kappa:<15.6f} {'(unknown)':<15}")


if __name__ == "__main__":
    main()
