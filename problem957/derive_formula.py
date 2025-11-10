#!/usr/bin/env python3
"""
Derive formula for g(n) from first principles - no OEIS, no simulation past day 3.

Facts from projective plane:
- 3 red points (fixed)
- Start with 2 blue points
- Each day: draw 3B lines (one per red-blue pair)
- In projective plane: every 2 distinct lines intersect at 1 point
- New blues = intersections that land on WHITE points

Goal: Find mathematical formula relating g(n+1) to g(n)
"""

def analyze_line_intersections(num_reds, num_blues):
    """
    Analyze intersections when we have num_reds red and num_blues blue.

    Lines created: R × B
    Potential intersections: C(R×B, 2) = R×B×(R×B-1)/2

    But some intersections are at existing colored points:
    - Each red has B lines through it → C(B,2) coincidences per red
    - Each blue has R lines through it → C(R,2) coincidences per blue
    """
    R = num_reds
    B = num_blues

    num_lines = R * B
    max_intersections = num_lines * (num_lines - 1) // 2

    # Coincidences at red points
    red_coincidences = R * (B * (B - 1) // 2)

    # Coincidences at blue points
    blue_coincidences = B * (R * (R - 1) // 2)

    # Account for lines being counted in both reds and blues
    # (Each line passes through 1 red and 1 blue, so it's counted once total)

    # Minimum new points (if everything else coincides)
    min_new = max_intersections - red_coincidences - blue_coincidences

    return {
        'lines': num_lines,
        'max_intersections': max_intersections,
        'red_coincidences': red_coincidences,
        'blue_coincidences': blue_coincidences,
        'min_new': min_new
    }


print("="*70)
print("DERIVING FORMULA FROM FIRST PRINCIPLES")
print("="*70)
print()

# Known values (verified from projective plane implementation)
g = [2, 8, 28, 184]
R = 3  # Always 3 reds

print("Known values:")
for i, val in enumerate(g):
    print(f"g({i}) = {val}")
print()

print("="*70)
print("ANALYZING LINE INTERSECTION STRUCTURE")
print("="*70)
print()

for i in range(len(g) - 1):
    B_current = g[i]
    B_next = g[i + 1]
    new_blues = B_next - B_current

    analysis = analyze_line_intersections(R, B_current)

    print(f"Day {i} → {i+1}:")
    print(f"  Current blues: {B_current}")
    print(f"  Lines created: {analysis['lines']}")
    print(f"  Max intersections: {analysis['max_intersections']}")
    print(f"  Red coincidences: {analysis['red_coincidences']}")
    print(f"  Blue coincidences: {analysis['blue_coincidences']}")
    print(f"  Min possible new: {analysis['min_new']}")
    print(f"  Actual new blues: {new_blues}")
    print(f"  Extra coincidences: {analysis['min_new'] - new_blues}")
    print()

print("="*70)
print("SEARCHING FOR FORMULA")
print("="*70)
print()

# Try to find pattern in new blues
new_blues_seq = [g[i+1] - g[i] for i in range(len(g)-1)]
print(f"New blues sequence: {new_blues_seq}")

# Check if it's polynomial in B
print()
print("Trying polynomial fit: new_blues = a·B² + b·B + c")
print()

# For B=2: new=6
# For B=8: new=20
# For B=28: new=156

# Let's solve the system
import numpy as np

B_vals = np.array([2, 8, 28])
new_vals = np.array([6, 20, 156])

# Set up matrix equation [B², B, 1] * [a, b, c]ᵀ = new_vals
A = np.column_stack([B_vals**2, B_vals, np.ones_like(B_vals)])
coeffs = np.linalg.solve(A, new_vals)

a, b, c = coeffs
print(f"Coefficients: a={a:.6f}, b={b:.6f}, c={c:.6f}")
print()

# Verify
print("Verification:")
for B, expected in zip(B_vals, new_vals):
    predicted = a * B**2 + b * B + c
    print(f"  B={B}: predicted={predicted:.2f}, actual={expected}, diff={abs(predicted-expected):.2f}")

print()
print(f"Formula: new_blues(B) = {a:.6f}·B² + {b:.6f}·B + {c:.6f}")
print()

# Use formula to compute g(4)
if abs(a * B_vals[0]**2 + b * B_vals[0] + c - new_vals[0]) < 0.01:
    print("Formula fits! Using it to predict further...")
    print()

    g_computed = [2]  # Start with g(0)
    for day in range(1, 17):
        B_current = g_computed[-1]
        new_blues = a * B_current**2 + b * B_current + c
        g_computed.append(int(round(B_current + new_blues)))

    print("Computed sequence using formula:")
    for i in range(min(6, len(g_computed))):
        print(f"g({i}) = {g_computed[i]}")

    print()
    print(f"g(16) = {g_computed[16]}")
else:
    print("Formula doesn't fit perfectly. Need different approach.")
