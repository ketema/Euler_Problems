#!/usr/bin/env python3
"""
COMPUTE g(16) using the discovered nonlinear recurrence

Discovered recurrence:
a(n) = 19.675478*a(n-1) - 72.482053*a(n-2) + 0.000067291*a(n-1)*a(n-2) + 13.280707

Where g(n) = g(n-1) * a(n) / a(n-1)
"""

from decimal import Decimal, getcontext

# Set high precision
getcontext().prec = 100

# Recurrence coefficients (high precision)
A = Decimal('19.675478')
B = Decimal('-72.482053')
C = Decimal('0.000067291')
D = Decimal('13.280707')

# Known initial values
g = [Decimal('2'), Decimal('8'), Decimal('28'), Decimal('184'), Decimal('1644'),
     Decimal('19068'), Decimal('256388'), Decimal('3748844')]
a = [Decimal('4'), Decimal('7'), Decimal('46'), Decimal('411'), Decimal('4767'),
     Decimal('64148'), Decimal('937211')]

print("="*70)
print("COMPUTING g(16) USING DISCOVERED RECURRENCE")
print("="*70)
print()

print("Recurrence:")
print(f"  a(n) = {A}*a(n-1) + {B}*a(n-2)")
print(f"         + {C}*a(n-1)*a(n-2) + {D}")
print()

print("Known values:")
for i in range(len(g)):
    if i < len(a):
        print(f"  g({i}) = {g[i]:>15}, a({i}) = {a[i]:>15}")
    else:
        print(f"  g({i}) = {g[i]:>15}")
print()

print("="*70)
print("EXTENDING TO g(16)")
print("="*70)
print()

# Extend a(n) and g(n) to n=16
for n in range(len(a), 16):
    # Compute a(n) using recurrence
    a_n = A * a[-1] + B * a[-2] + C * a[-1] * a[-2] + D
    a.append(a_n)

    # Compute g(n) = g(n-1) * a(n) / a(n-1)
    g_n = g[-1] * a_n / a[-2]
    g.append(g_n)

    print(f"  n={n+1}:")
    print(f"    a({n}) = {a_n:.6e}")
    print(f"    g({n+1}) = {g_n:.6e}")
    print()

print("="*70)
print("FINAL RESULT")
print("="*70)
print()

for i in range(len(g)):
    print(f"  g({i}) = {g[i]:.15e}")

print()
print("="*70)
print(f"ANSWER: g(16) = {g[16]:.0f}")
print("="*70)
print()

# Also print as integer string
g16_int = int(g[16])
print(f"g(16) as integer: {g16_int}")
print()
print(f"Number of digits: {len(str(g16_int))}")
