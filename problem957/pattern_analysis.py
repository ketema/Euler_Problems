"""
Analyze patterns in the sequence g(0)=2, g(1)=17, g(2)=1100
"""

from fractions import Fraction
import math

# Known values
g = {0: 2, 1: 17, 2: 1100}

# Added points per day
m = {
    0: g[1] - g[0],  # 15
    1: g[2] - g[1],  # 1083
}

# Upper bounds (from formula)
def U(b):
    """Upper bound U(b) = (3+b)(2+b)·b(b+1)/8"""
    return (3 + b) * (2 + b) * b * (b + 1) // 8

# Collapse factors
kappa = {}
for t in range(2):
    b_t = g[t]
    u_t = U(b_t)
    kappa[t] = Fraction(m[t], u_t)
    print(f"Day {t}: m_{t} = {m[t]}, U(g({t})) = U({b_t}) = {u_t}")
    print(f"  κ_{t} = {m[t]}/{u_t} = {kappa[t]} ≈ {float(kappa[t]):.6f}")
    print(f"  Collapse = {100*(1-float(kappa[t])):.2f}%")
    print()

print("="*70)
print("Looking for patterns...")
print("="*70)

# Check if g values have special properties
print("\nModular arithmetic:")
for mod in [2, 3, 5, 7, 11, 13]:
    print(f"mod {mod:2d}: g(0)≡{g[0]%mod}, g(1)≡{g[1]%mod}, g(2)≡{g[2]%mod}")

# Check factorizations
print("\nFactorizations:")
for t, val in g.items():
    factors = []
    n = val
    for p in [2, 3, 5, 7, 11, 13, 17]:
        count = 0
        while n % p == 0:
            count += 1
            n //= p
        if count > 0:
            factors.append(f"{p}^{count}")
    if n > 1:
        factors.append(str(n))
    print(f"g({t}) = {val} = {' × '.join(factors) if factors else '1'}")

# Look at ratios
print("\nRatios:")
print(f"g(1)/g(0) = {g[1]}/{g[0]} = {g[1]/g[0]}")
print(f"g(2)/g(1) = {g[2]}/{g[1]} = {g[2]/g[1]:.6f}")

# Look at m ratios
print(f"\nm(1)/m(0) = {m[1]}/{m[0]} = {m[1]/m[0]} = {Fraction(m[1], m[0])}")

# Connection to hyperbola?
print("\n" + "="*70)
print("Connection to hyperbola x(x-1) = 3y(y-1)?")
print("="*70)

# The hyperbola has coefficient 3
# The sequence values:
print(f"g(1) = 17 = 17 × 1")
print(f"g(2) = 1100 = 4 × 275 = 4 × 11 × 25 = 100 × 11")
print(f"  Note: 1100 = 100 × 11")
print(f"  Note: 11 = 2×3 + 5")
print(f"  Note: 17 = 2×8 + 1")

# Check if there's a pattern in successive differences
print("\nSuccessive differences:")
print(f"Δg(0) = g(1) - g(0) = {g[1] - g[0]} = 15 = 3×5")
print(f"Δg(1) = g(2) - g(1) = {g[2] - g[1]} = 1083 = 3×361 = 3×19²")

# Look at second differences
print(f"\nΔ²g(0) = Δg(1) - Δg(0) = {m[1] - m[0]} = 1068 = 4×267 = 12×89")

print("\n" + "="*70)
print("Observation: Both first differences are divisible by 3")
print("(matching the coefficient in the hyperbola equation!)")
print("="*70)

# Try to predict g(3) if there's a pattern
print("\nIf there's a polynomial growth pattern:")
# Fit to g(t) = at³ + bt² + ct + d
# g(0) = 2: d = 2
# g(1) = 17: a + b + c + 2 = 17 → a + b + c = 15
# g(2) = 1100: 8a + 4b + 2c + 2 = 1100 → 8a + 4b + 2c = 1098 → 4a + 2b + c = 549

# From a + b + c = 15 and 4a + 2b + c = 549:
# Subtract: 3a + b = 534
# Need another equation... try quadratic
print("Trying quadratic fit g(t) = at² + bt + c:")
# g(0) = 2: c = 2
# g(1) = 17: a + b + 2 = 17 → a + b = 15
# g(2) = 1100: 4a + 2b + 2 = 1100 → 4a + 2b = 1098 → 2a + b = 549

# From a + b = 15 and 2a + b = 549:
a = 549 - 15
b = 15 - a
c = 2
print(f"  a = {a}, b = {b}, c = {c}")
print(f"  g(t) = {a}t² + {b}t + {c}")
print(f"  Check: g(0) = {c} ✓" if c == 2 else f"  Check: g(0) = {c} ✗")
print(f"  Check: g(1) = {a + b + c} ✓" if a + b + c == 17 else f"  Check: g(1) = {a + b + c} ✗")
print(f"  Check: g(2) = {4*a + 2*b + c} ✓" if 4*a + 2*b + c == 1100 else f"  Check: g(2) = {4*a + 2*b + c} ✗")

if 4*a + 2*b + c == 1100:
    print(f"\n  Quadratic fit works! Prediction:")
    for t in range(3, 17):
        g_pred = a*t*t + b*t + c
        print(f"    g({t}) = {g_pred}")
else:
    print(f"\n  Quadratic doesn't fit exactly (off by {4*a + 2*b + c - 1100})")
