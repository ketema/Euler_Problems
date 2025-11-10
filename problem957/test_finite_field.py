#!/usr/bin/env python3
"""
Test if problem is in FINITE projective plane PG(2,q)

PG(2,q) has q² + q + 1 points total
- PG(2,2): 7 points
- PG(2,3): 13 points
- PG(2,4): 21 points
- PG(2,5): 31 points
- PG(2,7): 57 points
- PG(2,8): 73 points
- PG(2,9): 91 points

If problem saturates at q² + q + 1 points, growth stops!
"""

print("="*70)
print("FINITE FIELD HYPOTHESIS")
print("="*70)

# Known sequence
seq = [2, 8, 28, 184, 1644]

print(f"\nKnown: {seq}")
print(f"\nIf problem is in PG(2,q), growth must stop at q²+q+1 points")

# Check which finite fields could work
for q in [2, 3, 4, 5, 7, 8, 9, 11, 13, 16, 17, 19]:
    max_points = q**2 + q + 1
    print(f"\nPG(2,{q:2d}): {max_points:4d} points total")

    # Check if our sequence exceeds this
    exceeded_at = None
    for i, g in enumerate(seq):
        if g > max_points:
            exceeded_at = i
            break

    if exceeded_at is not None:
        print(f"  ✗ Exceeded at g({exceeded_at}) = {seq[exceeded_at]} > {max_points}")
    else:
        print(f"  ✓ Compatible through g(4)={seq[-1]}")
        print(f"    → g(16) ≤ {max_points}")

print("\n" + "="*70)
print("ANALYSIS")
print("="*70)

# Our sequence grows: 2 → 8 → 28 → 184 → 1644
# PG(2,q) bounds:
# q=7: 57 points (exceeded by g(2)=28? no, 28<57)
# q=9: 91 points (exceeded by g(3)=184? yes!)
# q=11: 133 points (exceeded by g(3)=184? yes!)
# q=13: 183 points (exceeded by g(3)=184? yes!)
# q=16: 273 points (exceeded by g(3)=184? no, 184<273)
# q=17: 307 points (exceeded by g(4)=1644? yes!)
# q=19: 381 points (exceeded by g(4)=1644? yes!)

# Need q² + q + 1 > 1644
# q²+q+1 > 1644
# q² + q - 1643 > 0
# q ≈ (-1 + √(1+4*1643))/2 ≈ 40

import math
min_q = math.ceil((-1 + math.sqrt(1 + 4*1643)) / 2)
print(f"\nTo accommodate g(4)=1644, need q ≥ {min_q}")
print(f"PG(2,{min_q}): {min_q**2 + min_q + 1} points")

# Check if there's a prime power near this
print(f"\nPrime powers near {min_q}:")
candidates = [32, 37, 41, 43, 47, 49, 53]
for q in candidates:
    max_pts = q**2 + q + 1
    print(f"  PG(2,{q}): {max_pts:,} points")

# Special case: what if blues saturate but total doesn't?
print("\n" + "="*70)
print("ALTERNATIVE: What if ONLY BLUES saturate?")
print("="*70)
print("Maybe g(n) stops growing at some point?")
print("Check if there's a limit in our sequence...")

# Look for convergence
if len(seq) >= 5:
    ratios = [seq[i]/seq[i-1] for i in range(1, len(seq))]
    print(f"\nGrowth ratios: {[f'{r:.2f}' for r in ratios]}")
    print(f"Ratios are INCREASING → no saturation yet")

print("\n" + "="*70)
print("CONCLUSION")
print("="*70)
print("IF problem is in finite field:")
print("  • Need PG(2,q) with q ≥ 40")
print("  • Likely q = 41, 43, 47, or 49")
print("  • g(16) ≤ q²+q+1")
print("\nFor q=41: g(16) ≤ 1,723")
print("For q=43: g(16) ≤ 1,893")
print("For q=47: g(16) ≤ 2,257")
print("For q=49: g(16) ≤ 2,451")
print("\nBUT: These are much smaller than polynomial predictions!")
print("     This would resolve the paradox!")
