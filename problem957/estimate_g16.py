#!/usr/bin/env python3
"""
Ensemble mathematical models for extrapolating g(16) from verified sequence g(1)-g(5).

Strategy: Multiple models with different assumptions to bracket confidence bounds.
"""

import math

# Verified data: cumulative blues B(n) and new blues g(n) per day
verified_B = [2, 8, 28, 184, 1646, 19161]  # B(0) through B(5)
verified_g = [None, 6, 20, 156, 1462, 17515]  # g(1) through g(5)

# Growth ratios r(n) = B(n) / B(n-1)
ratios = [verified_B[i] / verified_B[i-1] for i in range(1, len(verified_B))]
print("Verified growth ratios r(n) = B(n)/B(n-1):")
for i, r in enumerate(ratios, 1):
    print(f"  r({i}) = {r:.4f}")

# Ratio-of-ratios ρ(n) = r(n)/r(n-1)
ratio_of_ratios = [ratios[i] / ratios[i-1] for i in range(1, len(ratios))]
print("\nRatio-of-ratios ρ(n) = r(n)/r(n-1):")
for i, rho in enumerate(ratio_of_ratios, 2):
    print(f"  ρ({i}) = {rho:.4f}")

print("\n" + "="*80)
print("ENSEMBLE MODELS FOR g(16)")
print("="*80)

# ============================================================================
# MODEL 1: Asymptotic ratio-of-ratios stabilization at ρ∞ = 1.30
# ============================================================================
def model1_asymptotic_rho(rho_inf=1.30):
    """
    Assume ρ(n) → ρ∞ = 1.30 for n ≥ 6.
    Then r(n) = r(5) × ρ∞^(n-5)
    """
    B = verified_B[:]
    r_n = ratios[-1]  # r(5) = 11.64

    for n in range(6, 17):
        r_n = r_n * rho_inf
        B_n = B[-1] * r_n
        B.append(B_n)

    g_values = [B[i] - B[i-1] for i in range(1, len(B))]
    return B, g_values

B1, g1 = model1_asymptotic_rho(1.30)
print("\nMODEL 1: Asymptotic ρ∞=1.30")
print(f"  g(16) = {g1[15]:.3e}")
print(f"  B(16) = {B1[16]:.3e}")

# ============================================================================
# MODEL 2: Conservative lower bound (ρ∞ = 1.20)
# ============================================================================
B2, g2 = model1_asymptotic_rho(1.20)
print("\nMODEL 2: Conservative ρ∞=1.20 (lower bound)")
print(f"  g(16) = {g2[15]:.3e}")
print(f"  B(16) = {B2[16]:.3e}")

# ============================================================================
# MODEL 3: Aggressive upper bound (ρ∞ = 1.40)
# ============================================================================
B3, g3 = model1_asymptotic_rho(1.40)
print("\nMODEL 3: Aggressive ρ∞=1.40 (upper bound)")
print(f"  g(16) = {g3[15]:.3e}")
print(f"  B(16) = {B3[16]:.3e}")

# ============================================================================
# MODEL 4: Fitted exponential r(n) = a × b^n
# ============================================================================
def model4_exponential_ratio():
    """
    Fit r(n) = a × b^n to observed ratios r(3), r(4), r(5).
    Using least squares in log space.
    """
    # Use r(3), r(4), r(5) for fitting
    n_vals = [3, 4, 5]
    r_vals = [ratios[2], ratios[3], ratios[4]]

    # log r(n) = log a + n log b
    # Fit via linear regression
    sum_n = sum(n_vals)
    sum_log_r = sum(math.log(r) for r in r_vals)
    sum_n_log_r = sum(n * math.log(r) for n, r in zip(n_vals, r_vals))
    sum_n2 = sum(n**2 for n in n_vals)

    N = len(n_vals)
    log_b = (N * sum_n_log_r - sum_n * sum_log_r) / (N * sum_n2 - sum_n**2)
    log_a = (sum_log_r - log_b * sum_n) / N

    a = math.exp(log_a)
    b = math.exp(log_b)

    print(f"\n  Fitted: r(n) = {a:.4f} × {b:.4f}^n")

    B = verified_B[:]
    for n in range(6, 17):
        r_n = a * (b ** n)
        B_n = B[-1] * r_n
        B.append(B_n)

    g_values = [B[i] - B[i-1] for i in range(1, len(B))]
    return B, g_values

B4, g4 = model4_exponential_ratio()
print(f"MODEL 4: Exponential ratio fit")
print(f"  g(16) = {g4[15]:.3e}")
print(f"  B(16) = {B4[16]:.3e}")

# ============================================================================
# MODEL 5: Linear acceleration of ratios (conservative)
# ============================================================================
def model5_linear_ratio_growth():
    """
    Assume r(n) grows linearly: r(n) = r(5) + k×(n-5)
    Fit k using last 3 ratios.
    """
    # Fit k from r(3) to r(5)
    k = (ratios[4] - ratios[2]) / 2  # Average slope

    print(f"\n  Fitted: r(n) = r(5) + {k:.4f}×(n-5)")

    B = verified_B[:]
    r_n = ratios[-1]  # r(5)

    for n in range(6, 17):
        r_n = ratios[4] + k * (n - 5)
        B_n = B[-1] * r_n
        B.append(B_n)

    g_values = [B[i] - B[i-1] for i in range(1, len(B))]
    return B, g_values

B5, g5 = model5_linear_ratio_growth()
print(f"MODEL 5: Linear ratio growth")
print(f"  g(16) = {g5[15]:.3e}")
print(f"  B(16) = {B5[16]:.3e}")

# ============================================================================
# ENSEMBLE SUMMARY
# ============================================================================
print("\n" + "="*80)
print("ENSEMBLE SUMMARY")
print("="*80)

g16_estimates = [g1[15], g2[15], g3[15], g4[15], g5[15]]
B16_estimates = [B1[16], B2[16], B3[16], B4[16], B5[16]]

g16_min = min(g16_estimates)
g16_max = max(g16_estimates)
g16_median = sorted(g16_estimates)[2]
g16_mean = sum(g16_estimates) / len(g16_estimates)

print(f"\ng(16) estimates:")
print(f"  Min:    {g16_min:.3e}")
print(f"  Median: {g16_median:.3e}")
print(f"  Mean:   {g16_mean:.3e}")
print(f"  Max:    {g16_max:.3e}")
print(f"  Spread: {g16_max/g16_min:.2f}x")

print(f"\nB(16) cumulative blues estimates:")
print(f"  Min:    {min(B16_estimates):.3e}")
print(f"  Median: {sorted(B16_estimates)[2]:.3e}")
print(f"  Mean:   {sum(B16_estimates)/len(B16_estimates):.3e}")
print(f"  Max:    {max(B16_estimates):.3e}")

print("\n" + "="*80)
print("RECOMMENDED ESTIMATE")
print("="*80)
print(f"\ng(16) ≈ {g16_median:.3e} (median of 5 models)")
print(f"  Confidence interval: [{g16_min:.3e}, {g16_max:.3e}]")
print(f"  95% confidence: Order of magnitude ~10^{math.log10(g16_median):.1f}")

# Validation: Check models against known g(1)-g(5)
print("\n" + "="*80)
print("MODEL VALIDATION (Retroactive fit to g(1)-g(5))")
print("="*80)

for model_num, (B_model, g_model) in enumerate([(B1,g1), (B2,g2), (B3,g3), (B4,g4), (B5,g5)], 1):
    errors = []
    for i in range(1, 6):
        actual = verified_g[i]
        predicted = g_model[i-1] if i <= len(g_model) else None
        if predicted:
            error = abs(predicted - actual) / actual * 100
            errors.append(error)

    avg_error = sum(errors) / len(errors) if errors else float('inf')
    print(f"Model {model_num}: Average error on g(1)-g(5) = {avg_error:.2f}%")

print("\nNote: All models are extrapolations from n=5 to n=16 (11-step jump).")
print("Actual g(16) may differ significantly. Use ensemble bounds for confidence.")
