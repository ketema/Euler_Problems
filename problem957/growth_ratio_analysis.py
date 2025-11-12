#!/usr/bin/env python3

"""
Analysis of INCREASING growth ratios identified by AI Panel.

Growth ratios: 4×, 3.5×, 6.57×, 8.94×, 11.6×
AI Panel insight: INCREASING multipliers suggest factorial/exponential, not polynomial.
"""

# Known sequence
g = [2, 8, 28, 184, 1644, 19068]

print("="*70)
print("GROWTH RATIO ANALYSIS")
print("="*70)
print()

# First-order ratios
print("First-order ratios (g(n)/g(n-1)):")
ratios1 = []
for i in range(1, len(g)):
    ratio = g[i] / g[i-1]
    ratios1.append(ratio)
    print(f"  g({i})/g({i-1}) = {g[i]}/{g[i-1]} = {ratio:.6f}")

print()

# Second-order ratios (ratio of ratios)
print("Second-order ratios (growth acceleration):")
ratios2 = []
for i in range(1, len(ratios1)):
    ratio2 = ratios1[i] / ratios1[i-1]
    ratios2.append(ratio2)
    print(f"  r({i})/r({i-1}) = {ratios1[i]:.6f}/{ratios1[i-1]:.6f} = {ratio2:.6f}")

print()

# Check if second-order ratios are stabilizing
if ratios2:
    avg_accel = sum(ratios2) / len(ratios2)
    print(f"Average acceleration: {avg_accel:.6f}")
    print(f"Std deviation: {(sum((r-avg_accel)**2 for r in ratios2)/len(ratios2))**0.5:.6f}")

    if max(ratios2) / min(ratios2) < 1.5:
        print(f"\n✓ Second-order ratios relatively stable!")
        print(f"  This suggests: g(n) ~ g(n-1) × r(n-1) × {avg_accel:.3f}")
        print(f"  Which means: g(n) grows as product of increasing terms")
        print()

print()
print("="*70)
print("TESTING: Could g(n+1)/g(n) follow simple pattern?")
print("="*70)
print()

# Test if ratios follow linear pattern
print("Testing: r(n) = a*n + b")
# Use linear regression on ratio indices
n_vals = list(range(1, len(ratios1)+1))
r_vals = ratios1

# Simple linear regression
n_sum = sum(n_vals)
r_sum = sum(r_vals)
n_count = len(n_vals)
n_sq_sum = sum(n*n for n in n_vals)
nr_sum = sum(n*r for n, r in zip(n_vals, r_vals))

a = (n_count * nr_sum - n_sum * r_sum) / (n_count * n_sq_sum - n_sum**2)
b = (r_sum - a * n_sum) / n_count

print(f"  Best fit: r(n) = {a:.6f}*n + {b:.6f}")
print()
print("  Verification:")
for i, (n, r_actual) in enumerate(zip(n_vals, r_vals)):
    r_predicted = a * n + b
    error = abs(r_predicted - r_actual)
    print(f"    n={n}: predicted={r_predicted:.6f}, actual={r_actual:.6f}, error={error:.6f}")

print()
print("Using this formula to extrapolate:")
current_g = 19068  # g(5)
for n in range(6, 17):
    ratio_pred = a * n + b
    next_g = current_g * ratio_pred
    print(f"  g({n}) = g({n-1}) × {ratio_pred:.6f} = {int(next_g):,}")
    current_g = next_g

print()
print("="*70)
print(f"EXTRAPOLATED: g(16) = {int(current_g):,}")
print("="*70)
print()

# Sanity check
if a > 0:
    print("✓ Positive slope: growth ratios INCREASING (matches AI Panel observation)")
else:
    print("✗ Negative slope: growth ratios DECREASING (contradicts data)")

if current_g > 19068:
    print(f"✓ Result > g(5): {current_g} > 19068")
else:
    print(f"✗ Result ≤ g(5): Invalid")

growth_rate = (current_g / 19068) ** (1/11)
print(f"✓ Average growth rate g(5)→g(16): {growth_rate:.3f}× per day")
