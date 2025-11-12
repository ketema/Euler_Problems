#!/usr/bin/env sage

"""
Find EXACT formula for new(m) = am² + bm + c

Known data:
- m=2: new=6
- m=8: new=20
- m=28: new=156
- m=184: new=1460
- m=1644: new=17424

User's prediction: a ≈ 1.5 (=3/2), b ≈ -1.5 (=-3/2), c ≈ 0
"""

from sage.all import *

# Known data
data = [
    (2, 6),
    (8, 20),
    (28, 156),
    (184, 1460),
    (1644, 17424),
]

print("="*70)
print("EXACT FORMULA SOLUTION: new(m) = am² + bm + c")
print("="*70)
print()

# System of equations using first 3 points:
# 4a + 2b + c = 6     (m=2)
# 64a + 8b + c = 20   (m=8)
# 784a + 28b + c = 156 (m=28)

print("Setting up system of equations using first 3 data points:")
print("  4a + 2b + c = 6")
print("  64a + 8b + c = 20")
print("  784a + 28b + c = 156")
print()

# Solve using Sage's linear algebra
A = matrix(QQ, [
    [4, 2, 1],
    [64, 8, 1],
    [784, 28, 1]
])

b_vec = vector(QQ, [6, 20, 156])
solution = A.solve_right(b_vec)
a, b, c = solution

print("EXACT SOLUTION:")
print("  a = %s = %.10f" % (a, float(a)))
print("  b = %s = %.10f" % (b, float(b)))
print("  c = %s = %.10f" % (c, float(c)))
print()
print("Formula: new(m) = (%s)m² + (%s)m + (%s)" % (a, b, c))
print()

# Check if this matches 3/2, -3/2, 0
expected_a = QQ(3)/QQ(2)
expected_b = QQ(-3)/QQ(2)
expected_c = QQ(0)

if a == expected_a and b == expected_b and c == expected_c:
    print("✓✓✓ MATCHES USER'S PREDICTION: new(m) = (3/2)m² - (3/2)m")
    print("    Simplifies to: new(m) = (3/2)m(m-1) = 3·C(m,2)")
else:
    print("✗ Does NOT match user's prediction (3/2, -3/2, 0)")
    print("  Got: (%s, %s, %s)" % (a, b, c))

print()
print("="*70)
print("VERIFICATION ON ALL DATA POINTS")
print("="*70)
print()
print("m     | Predicted | Actual  | Error")
print("------|-----------|---------|--------")

max_error = 0
for m_val, new_actual in data:
    new_predicted = a * m_val**2 + b * m_val + c
    error = abs(float(new_predicted - new_actual))
    max_error = max(max_error, error)
    status = "✓" if error < 0.0001 else "✗"
    print("%5d | %9.2f | %7d | %6.4f %s" % (m_val, float(new_predicted), new_actual, error, status))

print()
if max_error < 0.0001:
    print("✓✓✓ PERFECT FIT on all 5 data points!")
else:
    print("✗ Formula does NOT fit perfectly (max error: %.4f)" % max_error)
    print("  This suggests new(m) is NOT a simple quadratic")

print()
print("="*70)
print("TRYING ALTERNATIVE APPROACH: Use ALL 5 points for overdetermined system")
print("="*70)
print()

# Try least-squares fit using all 5 points
A_full = matrix(QQ, [[m**2, m, 1] for m, _ in data])
b_full = vector(QQ, [new for _, new in data])

# Least squares: (A^T A)x = A^T b
ATA = A_full.transpose() * A_full
ATb = A_full.transpose() * b_full
solution_ls = ATA.solve_right(ATb)
a_ls, b_ls, c_ls = solution_ls

print("Least-squares solution using ALL 5 points:")
print("  a = %s = %.10f" % (a_ls, float(a_ls)))
print("  b = %s = %.10f" % (b_ls, float(b_ls)))
print("  c = %s = %.10f" % (c_ls, float(c_ls)))
print()

# Check for 3/2, -3/2, 0
if abs(float(a_ls) - 1.5) < 0.001 and abs(float(b_ls) + 1.5) < 0.001 and abs(float(c_ls)) < 0.001:
    print("✓✓✓ CLOSE TO (3/2, -3/2, 0)!")
    print("    Testing exact formula: new(m) = (3/2)m² - (3/2)m")
    a_test = QQ(3)/QQ(2)
    b_test = QQ(-3)/QQ(2)
    c_test = QQ(0)

    print()
    print("Testing new(m) = (3/2)m(m-1):")
    print("m     | Predicted | Actual  | Error")
    print("------|-----------|---------|--------")

    for m_val, new_actual in data:
        new_predicted = a_test * m_val**2 + b_test * m_val + c_test
        error = abs(float(new_predicted - new_actual))
        status = "✓" if error < 0.0001 else "✗"
        print("%5d | %9.2f | %7d | %6.4f %s" % (m_val, float(new_predicted), new_actual, error, status))

print()
print("="*70)
print("EXTRAPOLATING TO g(16) using least-squares coefficients")
print("="*70)
print()

# Use the least-squares solution
a_final = a_ls
b_final = b_ls
c_final = c_ls

g_current = 2  # g(0) = 2
g_sequence = [g_current]

print("g(0) = %d" % g_current)

for day in range(1, 17):
    m_start = g_current
    new_points = a_final * m_start**2 + b_final * m_start + c_final
    g_current = g_current + new_points
    g_sequence.append(g_current)

    if day <= 5:
        expected = [2, 8, 28, 184, 1644, 19068][day]
        error = abs(float(g_current - expected))
        match = "✓" if error < 0.5 else "✗"
        print("g(%2d) = %20s (expected %8d, error %.2f) %s" % (day, str(int(g_current)), expected, error, match))
    else:
        print("g(%2d) = %20s" % (day, str(int(g_current))))

print()
print("="*70)
print("ANSWER: g(16) = %s" % str(int(g_sequence[16])))
print("="*70)
print()

# Sanity checks
print("Sanity checks:")
if g_sequence[16] > 0:
    print("  ✓ Result is positive: %s" % str(int(g_sequence[16])))
else:
    print("  ✗ Result is negative: INVALID")

if g_sequence[16] > g_sequence[5]:
    print("  ✓ g(16) > g(5)")
else:
    print("  ✗ g(16) ≤ g(5): INVALID")

growth_rate = (g_sequence[16] / g_sequence[5]) ** (QQ(1)/QQ(11))
print("  Average growth rate g(5)→g(16): %.3f× per day" % float(growth_rate))

monotonic = all(g_sequence[i+1] > g_sequence[i] for i in range(16))
if monotonic:
    print("  ✓ Sequence is monotonically increasing")
else:
    print("  ✗ Sequence is NOT monotonic: INVALID")

print()
print("="*70)
