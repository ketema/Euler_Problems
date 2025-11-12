#!/usr/bin/env python3

"""
EXACT PROJECTIVE SOLVER - Problem 957

Uses exact rational arithmetic with canonical projective coordinates.
O(3m²) intersections per day (between-pencil only).
No same-day cascades, strict white-only filtering.
"""

from fractions import Fraction
from math import gcd
from itertools import product
import time

# ---------- Helpers: integer canonicalization in P^2 ----------
def igcd3(a, b, c):
    """GCD of three integers (by absolute value)."""
    return gcd(gcd(abs(a), abs(b)), abs(c))

def canon_triplet(X, Y, Z):
    """
    Make integer triple primitive with fixed sign convention.
    Returns canonical form for deduplication.
    """
    if X == 0 and Y == 0 and Z == 0:
        raise ValueError("zero vector")
    g = igcd3(X, Y, Z)
    X //= g
    Y //= g
    Z //= g
    # Sign convention: first nonzero coordinate positive
    if X < 0 or (X == 0 and Y < 0) or (X == 0 and Y == 0 and Z < 0):
        X, Y, Z = -X, -Y, -Z
    return (X, Y, Z)

def frac_point_to_int_triplet(px, py, pz=Fraction(1, 1)):
    """
    Lift (Fraction, Fraction, Fraction) to canonical integer triple.
    Clears denominators by LCM.
    """
    denx = px.denominator
    deny = py.denominator
    denz = pz.denominator
    # Compute LCM of three denominators
    lcm_xy = denx * deny // gcd(denx, deny)
    L = lcm_xy * denz // gcd(lcm_xy, denz)
    X = int(px * L)
    Y = int(py * L)
    Z = int(pz * L)
    return canon_triplet(X, Y, Z)

def cross(u, v):
    """3D cross product for projective geometry."""
    ux, uy, uz = u
    vx, vy, vz = v
    return (uy*vz - uz*vy, uz*vx - ux*vz, ux*vy - uy*vx)

# ---------- Configuration ----------
# Reds in canonical position (integer triples)
R1 = (0, 0, 1)
R2 = (1, 0, 1)
R3 = (0, 1, 1)
REDS = {R1, R2, R3}

def affine_point(x, y):
    """Convert affine (x, y) to canonical projective triplet."""
    return frac_point_to_int_triplet(Fraction(x), Fraction(y), Fraction(1))

# Initial blues on parabola (generic position)
B0 = {
    affine_point(1, 1),
    affine_point(2, 4),
}

def day_step(Bt):
    """
    Compute Bt+1 given Bt (sets of canonical integer triples).
    Returns (Bt_plus_1, new_points_added).
    """
    # Lines for each pencil: l = R_i × b
    lines_R1 = {cross(R1, b) for b in Bt}
    lines_R2 = {cross(R2, b) for b in Bt}
    lines_R3 = {cross(R3, b) for b in Bt}

    # Between-pencil intersections (3m²)
    candidates = set()

    def add_intersections(LA, LB):
        for la in LA:
            for lb in LB:
                # Line intersection: P = la × lb
                P = cross(la, lb)
                if P == (0, 0, 0):
                    continue  # Parallel/identical lines
                P = canon_triplet(*P)
                # White-only: skip if already colored
                if P in REDS or P in Bt:
                    continue
                candidates.add(P)

    add_intersections(lines_R1, lines_R2)
    add_intersections(lines_R1, lines_R3)
    add_intersections(lines_R2, lines_R3)

    # No same-day cascade: computed using only Bt
    return Bt | candidates, candidates

def run(n_days, verbose=True):
    """Run simulation for n_days. Returns g(0) through g(n_days)."""
    Bt = set(B0)
    history = [len(Bt)]  # g(0) = 2

    if verbose:
        print("="*70)
        print("EXACT PROJECTIVE SOLVER")
        print("="*70)
        print()
        print(f"g(0) = {len(Bt)}")

    for day in range(1, n_days + 1):
        start_time = time.time()
        Bt, new_pts = day_step(Bt)
        elapsed = time.time() - start_time
        history.append(len(Bt))

        if verbose:
            print(f"g({day:2d}) = {len(Bt):8,} ({len(new_pts):8,} new, {elapsed:6.2f}s)")

    if verbose:
        print()
        print("="*70)
        print("VERIFICATION")
        print("="*70)
        print()
        expected = [2, 8, 28, 184, 1644, 19068]
        for i in range(min(len(history), len(expected))):
            match = "✓" if history[i] == expected[i] else "✗"
            print(f"  g({i}) = {history[i]:8,} (expected {expected[i]:8,}) {match}")

    return history

if __name__ == "__main__":
    print("Testing exact projective solver...")
    print()

    # First verify g(1), g(2)
    hist_short = run(2, verbose=True)

    print()
    print("="*70)
    print("EXTENDING TO g(16)")
    print("="*70)
    print()

    # Now extend to g(16)
    hist_full = run(16, verbose=True)

    print()
    print("="*70)
    print(f"ANSWER: g(16) = {hist_full[16]:,}")
    print("="*70)
