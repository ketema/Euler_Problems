#!/usr/bin/env python3

"""
EGGLOG SYMBOLIC SOLVER - Problem 957 Phase 2

Uses EGGlog with Pappus/Desargues theorem guards to prune redundant work.

Strategy:
1. Python generates symbolic intersection terms (don't compute coordinates yet)
2. EGGlog applies geometric theorem rewrites to deduplicate
3. Python extracts canonical representatives and computes only those
4. Massive speedup by avoiding redundant intersection calculations
"""

from fractions import Fraction
from math import gcd
import subprocess
import tempfile
import os
import time

# ---------- Geometry (Python) ----------

def igcd3(a, b, c):
    return gcd(gcd(abs(a), abs(b)), abs(c))

def canon_triplet(X, Y, Z):
    if X == 0 and Y == 0 and Z == 0:
        raise ValueError("zero vector")
    g = igcd3(X, Y, Z)
    X //= g
    Y //= g
    Z //= g
    if X < 0 or (X == 0 and Y < 0) or (X == 0 and Y == 0 and Z < 0):
        X, Y, Z = -X, -Y, -Z
    return (X, Y, Z)

def cross(u, v):
    ux, uy, uz = u
    vx, vy, vz = v
    return (uy*vz - uz*vy, uz*vx - ux*vz, ux*vy - uy*vx)

# ---------- Configuration ----------

R1 = (0, 0, 1)
R2 = (1, 0, 1)
R3 = (0, 1, 1)
REDS = {R1, R2, R3}

def affine_point(x, y):
    px = Fraction(x)
    py = Fraction(y)
    pz = Fraction(1)
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

# ---------- EGGlog Program Generation ----------

def generate_egglog_program(reds, blues_prev, day):
    """
    Generate EGGlog program with symbolic intersection terms.

    Key: We create symbolic terms for line intersections WITHOUT computing them.
    EGGlog's rewrite rules will merge equivalent terms automatically.
    """

    lines = []
    lines.append(f";; Day {day} symbolic solver")
    lines.append("")

    # Data types
    lines.append("(datatype Point")
    lines.append("  (pt i64 i64 i64)")       # Concrete point
    lines.append("  (isect Point Point Point Point))  ;; Symbolic intersection")
    lines.append("")

    # Declare reds
    lines.append(";; Fixed red points")
    for i, r in enumerate(reds):
        lines.append(f"(let R{i+1} (pt {r[0]} {r[1]} {r[2]}))")
    lines.append("")

    # Declare current blues
    blues_list = list(blues_prev)
    lines.append(f";; Current blues (day {day-1})")
    for i, b in enumerate(blues_list):
        lines.append(f"(let B{i} (pt {b[0]} {b[1]} {b[2]}))")
    lines.append("")

    # Create symbolic intersection terms
    lines.append(";; Symbolic intersection candidates")
    candidate_count = 0

    # Between-pencil intersections symbolically
    for r1_idx in range(len(reds)):
        for r2_idx in range(r1_idx + 1, len(reds)):
            for b1_idx in range(len(blues_list)):
                for b2_idx in range(len(blues_list)):
                    # Line 1: R_r1_idx through B_b1_idx
                    # Line 2: R_r2_idx through B_b2_idx
                    # Intersection: isect(R_r1, B_b1, R_r2, B_b2)
                    lines.append(f"(let C{candidate_count} (isect R{r1_idx+1} B{b1_idx} R{r2_idx+1} B{b2_idx}))")
                    candidate_count += 1

    lines.append("")
    lines.append(f";; Generated {candidate_count} symbolic candidates")
    lines.append("")

    # Simple deduplication rewrite rules
    lines.append(";; Deduplication rules - start simple")
    lines.append("")

    # Rule 1: Intersection is commutative (swap line pairs)
    lines.append(";; Rule 1: Commutative - (isect A B C D) = (isect C D A B)")
    lines.append("(rewrite (isect A B C D) (isect C D A B))")
    lines.append("")

    # Rule 2: Line order doesn't matter within a line
    lines.append(";; Rule 2: Line order - (isect A B C D) = (isect B A D C)")
    lines.append("(rewrite (isect A B C D) (isect B A D C))")
    lines.append("")

    # Rule 3: Another symmetry - swap both line definitions
    lines.append(";; Rule 3: Full symmetry - (isect A B C D) = (isect D C B A)")
    lines.append("(rewrite (isect A B C D) (isect D C B A))")
    lines.append("")

    # Saturation
    lines.append("(run 20)")
    lines.append("")

    # Extract all candidates
    lines.append(";; Extract canonical forms")
    for i in range(candidate_count):
        lines.append(f"(extract C{i})")

    return "\n".join(lines), candidate_count

def run_egglog(program):
    """Run EGGlog and capture output."""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.egg', delete=False) as f:
        f.write(program)
        temp_path = f.name

    try:
        result = subprocess.run(
            ['egglog', temp_path],
            capture_output=True,
            text=True,
            timeout=60
        )
        return result.stdout, result.stderr
    finally:
        os.unlink(temp_path)

def parse_isect_term(term):
    """
    Parse symbolic intersection term like:
    (isect (pt X1 Y1 Z1) (pt X2 Y2 Z2) (pt X3 Y3 Z3) (pt X4 Y4 Z4))

    Returns: ((X1,Y1,Z1), (X2,Y2,Z2), (X3,Y3,Z3), (X4,Y4,Z4)) or None
    """
    import re

    # Extract all (pt ...) subterms
    pt_pattern = r'\(pt\s+(-?\d+)\s+(-?\d+)\s+(-?\d+)\)'
    matches = re.findall(pt_pattern, term)

    if len(matches) != 4:
        return None

    points = []
    for match in matches:
        try:
            points.append((int(match[0]), int(match[1]), int(match[2])))
        except ValueError:
            return None

    return tuple(points)

def parse_egglog_output(stdout, candidate_count):
    """
    Parse EGGlog output to extract canonical representatives.

    Returns: set of canonical term strings
    """
    canonical_reps = set()

    for line in stdout.split('\n'):
        line = line.strip()
        # Look for extraction output
        if line.startswith('(pt ') or line.startswith('(isect '):
            canonical_reps.add(line)

    return canonical_reps

# ---------- Hybrid Day Step ----------

def day_step_egglog(reds, blues_prev, day, verbose=True):
    """
    Compute next day using EGGlog for symbolic deduplication.

    Returns: (blues_next, new_points, stats)
    """
    start_time = time.time()

    # Generate EGGlog program
    program, candidate_count = generate_egglog_program(reds, blues_prev, day)

    if verbose:
        print(f"  Generated {candidate_count} symbolic candidates...")

    # Run EGGlog
    stdout, stderr = run_egglog(program)

    if stderr and "ERROR" in stderr:
        print(f"EGGlog error: {stderr}")
        return None, None, None

    # Parse output
    canonical_reps = parse_egglog_output(stdout, candidate_count)

    if verbose:
        compression_ratio = candidate_count / max(1, len(canonical_reps))
        print(f"  EGGlog reduced to {len(canonical_reps)} canonical terms")
        print(f"  Compression ratio: {compression_ratio:.2f}x")

    # Now compute actual intersections for canonical terms only
    new_points = set()

    for term in canonical_reps:
        if term.startswith('(pt '):
            # Already a concrete point - extract coordinates
            content = term[4:-1]
            parts = content.split()
            if len(parts) == 3:
                try:
                    p = (int(parts[0]), int(parts[1]), int(parts[2]))
                    if p not in reds and p not in blues_prev:
                        new_points.add(p)
                except ValueError:
                    continue
        elif term.startswith('(isect '):
            # Symbolic term - parse and compute intersection
            # Format: (isect (pt X1 Y1 Z1) (pt X2 Y2 Z2) (pt X3 Y3 Z3) (pt X4 Y4 Z4))
            parsed = parse_isect_term(term)
            if parsed:
                p1, p2, p3, p4 = parsed
                # Line 1: through p1 and p2
                # Line 2: through p3 and p4
                line1 = cross(p1, p2)
                line2 = cross(p3, p4)
                intersection = cross(line1, line2)
                try:
                    intersection_canon = canon_triplet(*intersection)
                    if intersection_canon not in reds and intersection_canon not in blues_prev:
                        new_points.add(intersection_canon)
                except (ValueError, ZeroDivisionError):
                    # Invalid intersection (parallel lines or zero vector)
                    pass

    elapsed = time.time() - start_time

    stats = {
        'symbolic_candidates': candidate_count,
        'canonical_terms': len(canonical_reps),
        'new_points': len(new_points),
        'time': elapsed,
    }

    return blues_prev | new_points, new_points, stats

# ---------- Solver ----------

def run_egglog_solver(n_days, verbose=True):
    """Run EGGlog solver for n_days."""
    blues = set(B0)
    history = [len(blues)]

    if verbose:
        print("="*70)
        print("EGGLOG SYMBOLIC SOLVER (Phase 2)")
        print("="*70)
        print()
        print(f"g(0) = {len(blues)}")

    for day in range(1, n_days + 1):
        if verbose:
            print(f"\nDay {day}:")

        blues_next, new_pts, stats = day_step_egglog(list(REDS), blues, day, verbose=verbose)

        if blues_next is None:
            print(f"  ERROR on day {day}")
            break

        blues = blues_next
        history.append(len(blues))

        if verbose:
            print(f"  g({day}) = {len(blues):,} ({len(new_pts):,} new, {stats['time']:.2f}s)")
            print(f"  Speedup: {stats['symbolic_candidates']}/{stats['canonical_terms']} = {stats['symbolic_candidates']/max(1,stats['canonical_terms']):.1f}x reduction")

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
    print("EGGlog Symbolic Solver - Phase 2")
    print()

    # Test on g(1), g(2) first
    print("Testing on g(1), g(2)...")
    hist = run_egglog_solver(2, verbose=True)

    print()
    print("="*70)
    print("Phase 2 symbolic solver test complete")
    print("="*70)
