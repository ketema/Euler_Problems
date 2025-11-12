#!/usr/bin/env python3

"""
EGGLOG HYBRID SOLVER - Problem 957 Phase 1

Strategy: Python handles geometry (cross products, canonicalization),
EGGlog handles symbolic deduplication via e-graph merging.

This avoids encoding complex projective arithmetic in EGGlog while
leveraging its strength: automatic term merging and redundancy elimination.
"""

from fractions import Fraction
from math import gcd
import subprocess
import time
import tempfile
import os

# ---------- Geometry (Python) ----------

def igcd3(a, b, c):
    """GCD of three integers."""
    return gcd(gcd(abs(a), abs(b)), abs(c))

def canon_triplet(X, Y, Z):
    """Canonical projective coordinates."""
    if X == 0 and Y == 0 and Z == 0:
        raise ValueError("zero vector")
    g = igcd3(X, Y, Z)
    X //= g
    Y //= g
    Z //= g
    # Sign convention: first nonzero positive
    if X < 0 or (X == 0 and Y < 0) or (X == 0 and Y == 0 and Z < 0):
        X, Y, Z = -X, -Y, -Z
    return (X, Y, Z)

def cross(u, v):
    """3D cross product for projective geometry."""
    ux, uy, uz = u
    vx, vy, vz = v
    return (uy*vz - uz*vy, uz*vx - ux*vz, ux*vy - uy*vx)

# ---------- Configuration ----------

R1 = (0, 0, 1)
R2 = (1, 0, 1)
R3 = (0, 1, 1)
REDS = {R1, R2, R3}

def affine_point(x, y):
    """Convert affine to projective."""
    from fractions import Fraction
    px = Fraction(x)
    py = Fraction(y)
    pz = Fraction(1)

    # Clear denominators
    denx = px.denominator
    deny = py.denominator
    denz = pz.denominator
    lcm_xy = denx * deny // gcd(denx, deny)
    L = lcm_xy * denz // gcd(lcm_xy, denz)

    X = int(px * L)
    Y = int(py * L)
    Z = int(pz * L)

    return canon_triplet(X, Y, Z)

B0 = {
    affine_point(1, 1),
    affine_point(2, 4),
}

# ---------- EGGlog Interface ----------

def point_to_egglog(p):
    """Convert point tuple to EGGlog term."""
    return f"(pt {p[0]} {p[1]} {p[2]})"

def generate_egglog_program(reds, blues, day):
    """Generate EGGlog program for one day step."""

    lines = []
    lines.append(";; Auto-generated for day " + str(day))
    lines.append("")

    # Datatype declarations
    lines.append("(datatype Point)")
    lines.append("(function pt (i64 i64 i64) Point)")
    lines.append("")

    # Declare reds
    for i, r in enumerate(reds):
        lines.append(f"(let R{i+1} {point_to_egglog(r)})")
    lines.append("")

    # Declare current blues
    blue_list = list(blues)
    for i, b in enumerate(blue_list):
        lines.append(f"(let B{i} {point_to_egglog(b)})")
    lines.append("")

    # Generate all candidate intersections symbolically
    lines.append(";; Between-pencil intersections")

    candidate_terms = []

    # Each pencil: lines through red and each blue
    for r_idx, r in enumerate(reds):
        for b_idx in range(len(blue_list)):
            # This is a line: R_i through B_j
            # We'll compute intersection with other pencils
            for r2_idx, r2 in enumerate(reds):
                if r2_idx <= r_idx:
                    continue  # Only between different pencils

                for b2_idx in range(len(blue_list)):
                    # Intersection of (R_r_idx, B_b_idx) with (R_r2_idx, B_b2_idx)
                    term = f"(isect R{r_idx+1} B{b_idx} R{r2_idx+1} B{b2_idx})"
                    candidate_terms.append(term)

    # Declare intersection function
    lines.append("(function isect (Point Point Point Point) Point)")
    lines.append("")

    # Create let bindings for each candidate
    for i, term in enumerate(candidate_terms):
        lines.append(f"(let C{i} {term})")

    lines.append("")
    lines.append(";; Saturation")
    lines.append("(run 10)")
    lines.append("")

    # Extract all candidates
    lines.append(";; Extract candidates")
    for i in range(len(candidate_terms)):
        lines.append(f"(extract C{i})")

    return "\n".join(lines)

def run_egglog(program):
    """Run EGGlog program and parse output."""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.egg', delete=False) as f:
        f.write(program)
        temp_path = f.name

    try:
        result = subprocess.run(
            ['egglog', temp_path],
            capture_output=True,
            text=True,
            timeout=30
        )

        return result.stdout, result.stderr
    finally:
        os.unlink(temp_path)

def parse_egglog_output(stdout):
    """Extract canonical points from EGGlog output."""
    # Parse lines like: (pt 1 2 3)
    points = set()

    for line in stdout.split('\n'):
        line = line.strip()
        if line.startswith('(pt ') and line.endswith(')'):
            # Extract numbers
            content = line[4:-1]  # Remove "(pt " and ")"
            parts = content.split()
            if len(parts) == 3:
                try:
                    X = int(parts[0])
                    Y = int(parts[1])
                    Z = int(parts[2])
                    points.add((X, Y, Z))
                except ValueError:
                    continue

    return points

# ---------- Hybrid Solver ----------

def day_step_hybrid(Bt):
    """
    Compute Bt+1 using Python geometry + EGGlog deduplication.

    Strategy:
    1. Python computes all intersections with exact arithmetic
    2. Python canonicalizes to integer triples
    3. Python deduplicates (EGGlog would be overkill for this phase)

    For now, we'll skip EGGlog and just use Python - Phase 1 is to verify
    the geometry works correctly first.
    """
    # Lines for each pencil
    lines_R1 = {cross(R1, b) for b in Bt}
    lines_R2 = {cross(R2, b) for b in Bt}
    lines_R3 = {cross(R3, b) for b in Bt}

    candidates = set()

    def add_intersections(LA, LB):
        for la in LA:
            for lb in LB:
                P = cross(la, lb)
                if P == (0, 0, 0):
                    continue
                P = canon_triplet(*P)
                # White-only filtering
                if P in REDS or P in Bt:
                    continue
                candidates.add(P)

    add_intersections(lines_R1, lines_R2)
    add_intersections(lines_R1, lines_R3)
    add_intersections(lines_R2, lines_R3)

    return Bt | candidates, candidates

def run_hybrid(n_days, verbose=True):
    """Run hybrid solver for n_days."""
    Bt = set(B0)
    history = [len(Bt)]

    if verbose:
        print("="*70)
        print("EGGLOG HYBRID SOLVER (Phase 1 - Python geometry)")
        print("="*70)
        print()
        print(f"g(0) = {len(Bt)}")

    for day in range(1, n_days + 1):
        start_time = time.time()
        Bt, new_pts = day_step_hybrid(Bt)
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
        expected = [2, 8, 28, 184, 1644, 19068, 256388, 3748844]
        for i in range(min(len(history), len(expected))):
            match = "✓" if history[i] == expected[i] else "✗"
            print(f"  g({i}) = {history[i]:8,} (expected {expected[i]:8,}) {match}")

    return history

if __name__ == "__main__":
    print("Testing EGGlog hybrid solver (Phase 1)...")
    print()

    # Verify g(1), g(2)
    hist_short = run_hybrid(2, verbose=True)

    print()
    print("="*70)
    print("BENCHMARK: Compare with exact projective solver")
    print("="*70)
    print()

    # Now test g(8) - should this be faster?
    # (Spoiler: No, because we're not using EGGlog yet)
    print("Computing g(3)...")
    hist_3 = run_hybrid(3, verbose=True)

    print()
    print("Phase 1 complete. This is still pure Python.")
    print("Phase 2 would integrate EGGlog for symbolic deduplication.")
    print()
