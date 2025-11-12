#!/usr/bin/env sage

"""
CRITICAL PATTERN ANALYSIS - 7 Data Points

After computing g(7), we now have:
g(0)=2, g(1)=8, g(2)=28, g(3)=184, g(4)=1644, g(5)=19068, g(6)=256388, g(7)=3748844

Growth function:
new(2)=6, new(8)=20, new(28)=156, new(184)=1460, new(1644)=17424, new(19068)=237320, new(256388)=3492456

QUESTIONS:
1. Ratios: 3.5, 6.6, 8.9, 11.6, 13.4, 14.6 - is there a pattern?
2. Are we approaching saturation or is growth unbounded?
3. Can we fit a better model with 7 points?
4. Should we implement EGGlog approach instead?
"""

from sage.all import *
import math

print("="*70)
print("PATTERN ANALYSIS WITH 7 DATA POINTS")
print("="*70)
print()

# Growth sequence
g_values = [2, 8, 28, 184, 1644, 19068, 256388, 3748844]

# Growth function data
growth_data = [
    (2, 6),
    (8, 20),
    (28, 156),
    (184, 1460),
    (1644, 17424),
    (19068, 237320),
    (256388, 3492456),
]

print("SEQUENCE DATA:")
print()
for i, g in enumerate(g_values):
    print(f"  g({i}) = {g:,}")
print()

print("="*70)
print("RATIO ANALYSIS")
print("="*70)
print()

ratios = []
for i in range(len(g_values)-1):
    ratio = float(g_values[i+1]) / float(g_values[i])
    ratios.append(ratio)
    print(f"  g({i+1})/g({i}) = {ratio:.6f}")

print()
print("Ratio differences (checking for arithmetic pattern):")
for i in range(len(ratios)-1):
    diff = ratios[i+1] - ratios[i]
    print(f"  Δr({i}) = {diff:.6f}")

print()
print("ANALYSIS:")
if ratios[-1] < ratios[-2]:
    print("  ✓ Ratios are DECREASING - potential saturation")
    print(f"    Latest: {ratios[-1]:.4f} < {ratios[-2]:.4f}")
else:
    print(f"  ✗ Ratios still INCREASING - no saturation")
    print(f"    Latest: {ratios[-1]:.4f} > {ratios[-2]:.4f}")

print()

print("="*70)
print("GROWTH FUNCTION ANALYSIS")
print("="*70)
print()

print("Growth function data: new(m)")
print()
for m, new in growth_data:
    print(f"  new({m:,}) = {new:,}")
print()

# Analyze new(m)/m² ratio
print("Checking new(m)/m² convergence:")
print()
ratios_m2 = []
for m, new in growth_data:
    ratio = float(new) / float(m**2)
    ratios_m2.append(ratio)
    print(f"  new({m:,})/m² = {ratio:.8f}")

print()
print("Is new(m)/m² converging?")
for i in range(len(ratios_m2)-1):
    diff = ratios_m2[i+1] - ratios_m2[i]
    print(f"  Δ({i}) = {diff:.8f}")

print()
if abs(ratios_m2[-1] - ratios_m2[-2]) < 0.001:
    print("  ✓ CONVERGENCE DETECTED - new(m)/m² ≈ constant")
    print(f"    Limiting value ≈ {ratios_m2[-1]:.6f}")
else:
    print(f"  ⚠ Still varying significantly")
    print(f"    Latest change: {abs(ratios_m2[-1] - ratios_m2[-2]):.6f}")

print()

# Check new(m)/m^k for different k
print("="*70)
print("TESTING POWER LAW: new(m) = c·m^k")
print("="*70)
print()

for k in [1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2]:
    ratios_k = [float(new) / float(m**k) for m, new in growth_data]
    variance = sum((ratios_k[i+1] - ratios_k[i])**2 for i in range(len(ratios_k)-1))
    print(f"k = {k:.1f}: variance = {variance:.6f}, latest ratio = {ratios_k[-1]:.6f}")

print()

# Check exponential growth
print("="*70)
print("TESTING EXPONENTIAL: new(m) = c·exp(d·m)")
print("="*70)
print()

log_new = [math.log(new) for _, new in growth_data]
log_m = [math.log(m) for m, _ in growth_data]

# Linear fit to log-log
A = matrix(QQ, [[log_m[i], 1] for i in range(len(log_m))])
b_vec = vector(RR, log_new)
ATA = A.transpose() * A
ATb = A.transpose() * b_vec
solution = ATA.solve_right(ATb)
k_fit, log_c = solution

print(f"Log-log fit: log(new) = {k_fit:.6f}·log(m) + {log_c:.6f}")
print(f"Power law: new(m) ≈ {math.exp(float(log_c)):.6f}·m^{k_fit:.6f}")
print()

# Verify fit quality
print("Fit quality:")
for i, (m, new_actual) in enumerate(growth_data):
    new_predicted = math.exp(float(log_c)) * (m ** float(k_fit))
    error = abs(new_predicted - new_actual) / new_actual * 100
    print(f"  m={m:7,}: predicted={new_predicted:10,.0f}, actual={new_actual:7,}, error={error:5.2f}%")

print()

print("="*70)
print("EXTRAPOLATION TO g(16)")
print("="*70)
print()

c_fit = math.exp(float(log_c))
print(f"Using power law: new(m) ≈ {c_fit:.6f}·m^{k_fit:.6f}")
print()

m_current = Integer(2)
g_sequence_extrapolated = [m_current]

print(f"g(0) = {m_current}")

for day in range(1, 17):
    # Use power law
    new_points = c_fit * (float(m_current) ** float(k_fit))
    new_points_int = Integer(round(new_points))

    m_current = m_current + new_points_int
    g_sequence_extrapolated.append(m_current)

    if day <= 7:
        expected = g_values[day]
        error = abs(m_current - expected)
        pct_error = 100 * float(error) / float(expected)
        match = "✓" if pct_error < 10 else "✗"
        print(f"g({day:2d}) = {m_current:20,} (expected {expected:10,}, error {pct_error:5.2f}%) {match}")
    else:
        print(f"g({day:2d}) = {m_current:20,}")

print()
print("="*70)
print(f"EXTRAPOLATED g(16) = {g_sequence_extrapolated[16]:,}")
print("="*70)
print()

# Sanity checks
print("Sanity checks:")
if g_sequence_extrapolated[16] > 0:
    print(f"  ✓ Result is positive")
if all(g_sequence_extrapolated[i+1] > g_sequence_extrapolated[i] for i in range(16)):
    print(f"  ✓ Sequence is monotonically increasing")
if g_sequence_extrapolated[16] < 10**15:
    print(f"  ✓ Result is reasonable PE size (< 10^15)")
elif g_sequence_extrapolated[16] < 10**100:
    print(f"  ⚠ Result is large but not absurd (< 10^100)")
else:
    print(f"  ✗ Result is absurdly large: LIKELY INVALID")

print()
print("="*70)
print("TIMING ANALYSIS")
print("="*70)
print()

timings = [0.00, 0.00, 0.00, 0.01, 0.20, 9.56, 806.88]
print("Computation times:")
for i, t in enumerate(timings):
    print(f"  g({i+1}) took {t:.2f}s")

print()
print("Timing ratios:")
for i in range(1, len(timings)):
    if timings[i-1] > 0:
        ratio = timings[i] / timings[i-1]
        print(f"  t({i+1})/t({i}) = {ratio:.2f}x")

print()
print("Estimated g(8) time:")
if len(timings) >= 2:
    # Use last two ratios
    ratio_6_7 = timings[-1] / timings[-2] if timings[-2] > 0 else 84
    estimated_g8_time = timings[-1] * ratio_6_7
    print(f"  Based on g(6)->g(7) ratio ({ratio_6_7:.1f}x): {estimated_g8_time:.0f}s = {estimated_g8_time/3600:.1f} hours")
    print()
    if estimated_g8_time > 3600:
        print("  ✗ g(8) is COMPUTATIONALLY INTRACTABLE with current approach")
        print("    Need alternative: EGGlog symbolic rewriting or better algorithm")

print()
print("="*70)
