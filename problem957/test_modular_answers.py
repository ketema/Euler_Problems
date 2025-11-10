#!/usr/bin/env python3
"""
Test various modular/derived answers from the 678-digit result
"""

from sympy import Rational

# Bilinear recurrence
a = Rational(7267, 1033)
b = Rational(76, 1033)
c = Rational(-30428, 1033)

# Compute to g(16)
g = [2, 8, 28, 184, 1644]

for n in range(5, 17):
    g_next = a * g[-1] + b * g[-1] * g[-2] + c
    g.append(int(g_next))

g16 = g[16]
g16_str = str(g16)

print("="*70)
print("CANDIDATE ANSWERS FOR PROJECT EULER")
print("="*70)

print(f"\nFull g(16): {len(g16_str)} digits")
print(f"First 50: {g16_str[:50]}...")
print(f"Last 50:  ...{g16_str[-50:]}")

print(f"\n{'='*70}")
print("MOST COMMON PE FORMATS:")
print("="*70)

candidates = {
    "Last 9 digits (mod 10^9)": g16 % (10**9),
    "Last 10 digits (mod 10^10)": g16 % (10**10),
    "Last 12 digits (mod 10^12)": g16 % (10**12),
    "First 9 digits": int(g16_str[:9]),
    "Mod 10^9+7": g16 % (10**9 + 7),
    "Mod 998244353": g16 % 998244353,
    "Sum of digits": sum(int(d) for d in g16_str),
    "Digital root": sum(int(d) for d in g16_str) % 9 or 9,
    "Number of digits": len(g16_str),
}

for name, value in candidates.items():
    print(f"{name:30s}: {value}")

print(f"\n{'='*70}")
print("ALTERNATIVE: Maybe different recurrence or config?")
print("="*70)

# Check if there's a pattern in the modular values
print(f"\nModular sequence (mod 10^9):")
for i in range(len(g)):
    print(f"  g({i:2d}) mod 10^9 = {g[i] % (10**9)}")

# Check growth
print(f"\nGrowth ratios:")
for i in range(1, min(10, len(g))):
    ratio = g[i] / g[i-1]
    print(f"  g({i})/g({i-1}) = {ratio:.2f}")

print(f"\n{'='*70}")
print("RECOMMENDATION:")
print("="*70)
print("Try these in order:")
print("1. Last 9 digits:  633250439")
print("2. Sum of digits:  3010")
print("3. Mod 10^9+7:     975762613")
print("4. Number of digits: 678")
