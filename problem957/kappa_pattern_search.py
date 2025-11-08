#!/usr/bin/env python3
"""
Deep pattern analysis of κ_t sequence.

Try various approaches to find a formula:
1. Polynomial/exponential fitting
2. Recurrence relation search
3. Generating function analysis
4. Combinatorial formula search
"""

import sys
import numpy as np
from sympy import *
from scipy.optimize import curve_fit
from fractions import Fraction

# Known empirical values from exact coincidence tracking
kappa_empirical = {
    1: 1.0,
    2: 0.08547008547008547,  # 20/234
    3: 0.05098039215686274,  # 156/3060
    4: 0.011692115652840668,  # 1461/124956
    5: 0.001819830827067669,  # 17516/9625068
}

m_values = [2, 6, 20, 156, 1462, 17515, 242627]
B_values = [2]
for m in m_values[1:]:
    B_values.append(B_values[-1] + m)


def analyze_log_log_relationship():
    """Check if κ_t follows a power law: κ_t ~ t^α"""
    print("="*80)
    print("POWER LAW ANALYSIS: κ_t ~ t^α")
    print("="*80)
    print()

    days = np.array([1, 2, 3, 4, 5])
    kappas = np.array([kappa_empirical[d] for d in days])

    # Skip κ_1 = 1 as it's exact
    days_fit = days[1:]
    kappas_fit = kappas[1:]

    # Log-log fit
    log_days = np.log(days_fit)
    log_kappas = np.log(kappas_fit)

    # Linear fit in log space: log(κ) = α*log(t) + log(c)
    coeffs = np.polyfit(log_days, log_kappas, 1)
    alpha, log_c = coeffs
    c = np.exp(log_c)

    print(f"Best fit: κ_t ≈ {c:.6f} * t^({alpha:.6f})")
    print()

    # Test fit quality
    print("Actual vs Predicted:")
    for d in days:
        actual = kappa_empirical[d]
        predicted = c * (d ** alpha) if d > 1 else 1.0
        error = abs(actual - predicted) / actual
        print(f"  Day {d}: actual={actual:.8f}, predicted={predicted:.8f}, error={100*error:.2f}%")
    print()


def analyze_exponential_relationship():
    """Check if κ_t follows exponential decay: κ_t ~ exp(-βt)"""
    print("="*80)
    print("EXPONENTIAL DECAY ANALYSIS: κ_t ~ exp(-βt)")
    print("="*80)
    print()

    days = np.array([1, 2, 3, 4, 5])
    kappas = np.array([kappa_empirical[d] for d in days])

    # Skip κ_1 = 1
    days_fit = days[1:]
    kappas_fit = kappas[1:]

    # Semi-log fit: log(κ) = -βt + log(c)
    coeffs = np.polyfit(days_fit, np.log(kappas_fit), 1)
    neg_beta, log_c = coeffs
    c = np.exp(log_c)
    beta = -neg_beta

    print(f"Best fit: κ_t ≈ {c:.6f} * exp(-{beta:.6f} * t)")
    print()

    # Test fit quality
    print("Actual vs Predicted:")
    for d in days:
        actual = kappa_empirical[d]
        predicted = c * np.exp(-beta * d) if d > 1 else 1.0
        error = abs(actual - predicted) / actual if actual > 0 else 0
        print(f"  Day {d}: actual={actual:.8f}, predicted={predicted:.8f}, error={100*error:.2f}%")
    print()


def analyze_combinatorial_relationship():
    """Check if κ_t relates to m_{t-1}, B_{t-2} combinatorially"""
    print("="*80)
    print("COMBINATORIAL FORMULA SEARCH")
    print("="*80)
    print()

    print("Testing various functional forms of κ_t(m_{t-1}, B_{t-2}):")
    print()

    # Compute P_t for each day
    P_values = []
    for t in range(1, 6):
        if t == 1:
            P_t = 1
        else:
            m_prev = m_values[t-1]
            B_prev_prev = B_values[t-2]
            P_t = (m_prev * (m_prev - 1)) // 2 + m_prev * B_prev_prev
        P_values.append(P_t)

    data = []
    for t in range(1, 6):
        kappa = kappa_empirical[t]
        m_prev = m_values[t-1]
        B_prev_prev = B_values[t-2] if t >= 2 else 0
        P_t = P_values[t-1]
        C_gen = 6 * P_t

        data.append({
            't': t,
            'κ': kappa,
            'm_{t-1}': m_prev,
            'B_{t-2}': B_prev_prev,
            'P_t': P_t,
            'C_gen': C_gen,
        })

        print(f"Day {t}:")
        print(f"  m_{t-1} = {m_prev:,}, B_{t-2} = {B_prev_prev:,}, P_t = {P_t:,}")
        print(f"  κ_{t} = {kappa:.10f}")
        print()

    # Try various functional forms
    print("Testing functional forms:")
    print()

    # Form 1: κ_t = a / P_t^b
    log_P = np.array([np.log(d['P_t']) for d in data[1:]])  # Skip day 1
    log_kappa = np.array([np.log(d['κ']) for d in data[1:]])

    coeffs = np.polyfit(log_P, log_kappa, 1)
    b, log_a = coeffs
    a = np.exp(log_a)

    print(f"Form 1: κ_t = {a:.6f} / P_t^({-b:.6f})")
    for d in data:
        t = d['t']
        actual = d['κ']
        predicted = a / (d['P_t'] ** (-b)) if t > 1 else 1.0
        error = abs(actual - predicted) / actual if actual > 0 else 0
        print(f"  Day {t}: error = {100*error:.2f}%")
    print()

    # Form 2: κ_t = a / (m_{t-1}^b * B_{t-2}^c)
    # This requires more data points, skip for now

    # Form 3: κ_t = a * t^b / P_t^c
    # Multi-variate fit...


def search_recurrence_relation():
    """Try to find recurrence: κ_t = f(κ_{t-1}, κ_{t-2}, ...)"""
    print("="*80)
    print("RECURRENCE RELATION SEARCH")
    print("="*80)
    print()

    kappas = [kappa_empirical[t] for t in range(1, 6)]

    print("Testing linear recurrence: κ_t = a*κ_{t-1} + b*κ_{t-2} + c")
    print()

    # Try 2nd-order linear recurrence for days 3-5
    # κ_t = a*κ_{t-1} + b*κ_{t-2} + c

    # Set up system of equations
    # Day 3: κ_3 = a*κ_2 + b*κ_1 + c
    # Day 4: κ_4 = a*κ_3 + b*κ_2 + c
    # Day 5: κ_5 = a*κ_4 + b*κ_3 + c

    A = np.array([
        [kappas[1], kappas[0], 1],  # Day 3
        [kappas[2], kappas[1], 1],  # Day 4
        [kappas[3], kappas[2], 1],  # Day 5
    ])
    b_vec = np.array([kappas[2], kappas[3], kappas[4]])

    try:
        coeffs = np.linalg.solve(A, b_vec)
        a, b, c = coeffs

        print(f"Found coefficients: a={a:.6f}, b={b:.6f}, c={c:.6f}")
        print()

        # Test fit
        print("Testing recurrence on training data:")
        for t in range(3, 5):  # Days 4-5 (indices 3-4)
            actual = kappas[t]
            predicted = a * kappas[t-1] + b * kappas[t-2] + c
            error = abs(actual - predicted) / actual if actual > 0 else 0
            print(f"  κ_{t+1} predicted = {predicted:.8f}, actual = {actual:.8f}, error = {100*error:.2f}%")
        print()

        # Predict days 6-16 using recurrence
        print("Predicting κ_6 through κ_16:")
        kappa_predicted = list(kappas)  # Start with known values

        for t in range(6, 17):
            kappa_t = a * kappa_predicted[t-2] + b * kappa_predicted[t-3] + c
            kappa_predicted.append(kappa_t)
            print(f"  κ_{t} = {kappa_t:.12e}")
        print()

    except np.linalg.LinAlgError:
        print("Could not solve - matrix is singular")
        print()


def main():
    """Run all pattern searches."""
    print("="*80)
    print("DEEP PATTERN ANALYSIS OF κ_t SEQUENCE")
    print("="*80)
    print()

    analyze_log_log_relationship()
    analyze_exponential_relationship()
    analyze_combinatorial_relationship()
    search_recurrence_relation()

    print("="*80)
    print("CONCLUSION")
    print("="*80)
    print()
    print("If a clear pattern emerges, we can:")
    print("1. Extend it symbolically to predict κ_6, κ_7, ... κ_16")
    print("2. Compute m_6, m_7, ... m_16 using the recurrence")
    print("3. Compute g(16) = sum of all m values")
    print()


if __name__ == '__main__':
    main()
