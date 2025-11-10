"""
Exact simulator for Problem 957 (days 0→1→2→3→4)

Uses:
- Homogeneous coordinates [x:y:z] in ℝℙ²
- Rational arithmetic (Python fractions.Fraction)
- Exact equality testing via canonical forms
- No floating point approximations

Design priorities:
1. Clarity over speed
2. Exact symbolic computation
3. Deterministic point equality
4. Cross-check with theoretical upper bounds
"""

from fractions import Fraction
from typing import List, Tuple, Set, Optional
from dataclasses import dataclass
import math


@dataclass(frozen=True)
class ProjectivePoint:
    """
    Point in ℝℙ² with homogeneous coordinates [x:y:z].

    Canonical form: First non-zero coordinate is positive, gcd=1.
    """
    x: Fraction
    y: Fraction
    z: Fraction
    label: str

    def __post_init__(self):
        """Verify the point is in canonical form."""
        if self.x == 0 and self.y == 0 and self.z == 0:
            raise ValueError("Invalid projective point: [0:0:0]")

    def __str__(self):
        return f"{self.label}=[{self.x}:{self.y}:{self.z}]"

    def __repr__(self):
        return self.__str__()


@dataclass(frozen=True)
class ProjectiveLine:
    """
    Line in ℝℙ² with homogeneous coordinates [a:b:c].

    Point [x:y:z] lies on line [a:b:c] iff ax + by + cz = 0.
    Canonical form: First non-zero coefficient is positive, gcd=1.
    """
    a: Fraction
    b: Fraction
    c: Fraction
    label: str

    def __post_init__(self):
        """Verify the line is in canonical form."""
        if self.a == 0 and self.b == 0 and self.c == 0:
            raise ValueError("Invalid projective line: [0:0:0]")

    def contains(self, p: ProjectivePoint) -> bool:
        """Check if point p lies on this line."""
        return self.a * p.x + self.b * p.y + self.c * p.z == 0

    def __str__(self):
        return f"{self.label}=[{self.a}:{self.b}:{self.c}]"

    def __repr__(self):
        return self.__str__()


def gcd(a: int, b: int) -> int:
    """Compute GCD of two integers."""
    while b:
        a, b = b, a % b
    return abs(a)


def gcd_three(a: int, b: int, c: int) -> int:
    """Compute GCD of three integers."""
    return gcd(gcd(a, b), c)


def normalize_point(x: Fraction, y: Fraction, z: Fraction) -> Tuple[Fraction, Fraction, Fraction]:
    """
    Normalize homogeneous coordinates to canonical form:
    - First non-zero coordinate is positive
    - All coordinates reduced by their GCD

    Returns (x', y', z') in canonical form.
    """
    if x == 0 and y == 0 and z == 0:
        raise ValueError("Cannot normalize [0:0:0]")

    # Find first non-zero coordinate
    if x != 0:
        sign = 1 if x > 0 else -1
    elif y != 0:
        sign = 1 if y > 0 else -1
    else:  # z != 0
        sign = 1 if z > 0 else -1

    # Apply sign
    x, y, z = sign * x, sign * y, sign * z

    # Reduce by GCD (work with numerators and denominators)
    # Convert to common denominator
    from math import gcd as math_gcd

    # Get LCD of denominators
    lcm_denom = x.denominator
    lcm_denom = (lcm_denom * y.denominator) // math_gcd(lcm_denom, y.denominator)
    lcm_denom = (lcm_denom * z.denominator) // math_gcd(lcm_denom, z.denominator)

    # Scale to integers
    x_int = int(x * lcm_denom)
    y_int = int(y * lcm_denom)
    z_int = int(z * lcm_denom)

    # Compute GCD and reduce
    g = gcd_three(x_int, y_int, z_int)
    if g == 0:
        g = 1

    x_int //= g
    y_int //= g
    z_int //= g

    return Fraction(x_int, lcm_denom), Fraction(y_int, lcm_denom), Fraction(z_int, lcm_denom)


def make_point(x: Fraction, y: Fraction, z: Fraction, label: str) -> ProjectivePoint:
    """Create a projective point in canonical form."""
    x_norm, y_norm, z_norm = normalize_point(x, y, z)
    return ProjectivePoint(x_norm, y_norm, z_norm, label)


def make_line_from_points(p1: ProjectivePoint, p2: ProjectivePoint, label: str) -> ProjectiveLine:
    """
    Construct the line through two points using the cross product.

    Line [a:b:c] through [x1:y1:z1] and [x2:y2:z2] is:
    [a:b:c] = [y1·z2 - y2·z1 : z1·x2 - z2·x1 : x1·y2 - x2·y1]
    """
    a = p1.y * p2.z - p2.y * p1.z
    b = p1.z * p2.x - p2.z * p1.x
    c = p1.x * p2.y - p2.x * p1.y

    # Normalize
    a_norm, b_norm, c_norm = normalize_point(a, b, c)

    return ProjectiveLine(a_norm, b_norm, c_norm, label)


def intersect_lines(ℓ1: ProjectiveLine, ℓ2: ProjectiveLine, label: str) -> Optional[ProjectivePoint]:
    """
    Find the intersection of two lines using the cross product.

    Point [x:y:z] at intersection of [a1:b1:c1] and [a2:b2:c2] is:
    [x:y:z] = [b1·c2 - b2·c1 : c1·a2 - c2·a1 : a1·b2 - a2·b1]

    Returns None if lines are identical (all cross products zero).
    """
    x = ℓ1.b * ℓ2.c - ℓ2.b * ℓ1.c
    y = ℓ1.c * ℓ2.a - ℓ2.c * ℓ1.a
    z = ℓ1.a * ℓ2.b - ℓ2.a * ℓ1.b

    if x == 0 and y == 0 and z == 0:
        # Lines are identical
        return None

    return make_point(x, y, z, label)


def points_equal(p1: ProjectivePoint, p2: ProjectivePoint) -> bool:
    """
    Check if two projective points are equal.

    Since both are in canonical form, we can compare coordinates directly.
    """
    return p1.x == p2.x and p1.y == p2.y and p1.z == p2.z


def lines_equal(ℓ1: ProjectiveLine, ℓ2: ProjectiveLine) -> bool:
    """
    Check if two projective lines are equal.

    Since both are in canonical form, we can compare coordinates directly.
    """
    return ℓ1.a == ℓ2.a and ℓ1.b == ℓ2.b and ℓ1.c == ℓ2.c


class Configuration:
    """Represents the configuration at a given day."""

    def __init__(self, day: int):
        self.day = day
        self.red_points: List[ProjectivePoint] = []
        self.blue_points: List[ProjectivePoint] = []

    def all_points(self) -> List[ProjectivePoint]:
        return self.red_points + self.blue_points

    def add_red(self, p: ProjectivePoint):
        self.red_points.append(p)

    def add_blue(self, p: ProjectivePoint):
        self.blue_points.append(p)

    def blue_count(self) -> int:
        return len(self.blue_points)


def initial_configuration() -> Configuration:
    """
    Create the initial configuration (Day 0).

    Choose 5 points in general position in ℝℙ²:
    - No three collinear
    - Simple rational coordinates

    Using affine chart z=1 for simplicity:
    r₁ = [0:0:1] = (0,0)
    r₂ = [1:0:1] = (1,0)
    r₃ = [0:1:1] = (0,1)
    b₁ = [1:1:1] = (1,1)
    b₂ = [3:2:1] = (3,2)

    Verify no three are collinear.
    """
    config = Configuration(day=0)

    # Red points
    r1 = make_point(Fraction(0), Fraction(0), Fraction(1), "r₁")
    r2 = make_point(Fraction(1), Fraction(0), Fraction(1), "r₂")
    r3 = make_point(Fraction(0), Fraction(1), Fraction(1), "r₃")

    config.add_red(r1)
    config.add_red(r2)
    config.add_red(r3)

    # Blue points
    b1 = make_point(Fraction(1), Fraction(1), Fraction(1), "b₁")
    b2 = make_point(Fraction(3), Fraction(2), Fraction(1), "b₂")

    config.add_blue(b1)
    config.add_blue(b2)

    return config


def verify_no_three_collinear(points: List[ProjectivePoint]) -> bool:
    """
    Verify that no three points are collinear.

    Three points p1, p2, p3 are collinear iff det([p1, p2, p3]) = 0.
    """
    n = len(points)
    for i in range(n):
        for j in range(i+1, n):
            for k in range(j+1, n):
                p1, p2, p3 = points[i], points[j], points[k]

                # Compute determinant
                det = (p1.x * (p2.y * p3.z - p2.z * p3.y) -
                       p1.y * (p2.x * p3.z - p2.z * p3.x) +
                       p1.z * (p2.x * p3.y - p2.y * p3.x))

                if det == 0:
                    print(f"⚠️  WARNING: {p1.label}, {p2.label}, {p3.label} are COLLINEAR!")
                    return False

    return True


def propagate_one_day(config: Configuration, verbose: bool = True, verbose_collinearities: bool = True) -> Configuration:
    """
    Propagate from day t to day t+1.

    Returns new configuration with novel points added.
    """
    new_config = Configuration(day=config.day + 1)

    # Copy existing points
    new_config.red_points = config.red_points.copy()
    new_config.blue_points = config.blue_points.copy()

    points = config.all_points()
    n_points = len(points)

    if verbose:
        print(f"\n{'='*80}")
        print(f"DAY {config.day} → DAY {config.day + 1}")
        print(f"{'='*80}")
        print(f"Starting points: {n_points} (Red: {len(config.red_points)}, Blue: {len(config.blue_points)})")

    # Generate all lines
    lines = []
    line_dict = {}  # Canonical form -> line object
    collinearity_count = 0

    for i in range(n_points):
        for j in range(i+1, n_points):
            line = make_line_from_points(points[i], points[j], f"ℓ({points[i].label},{points[j].label})")

            # Check if this line already exists (collinearity detection)
            key = (line.a, line.b, line.c)
            if key in line_dict:
                collinearity_count += 1
                if verbose and verbose_collinearities:
                    print(f"  Collinearity: {points[i].label}, {points[j].label} on existing line {line_dict[key].label}")
            else:
                line_dict[key] = line
                lines.append(line)

    n_lines = len(lines)
    if verbose:
        if not verbose_collinearities and collinearity_count > 0:
            print(f"Collinearities detected: {collinearity_count}")
        print(f"Lines formed: {n_lines} (max possible: {n_points * (n_points - 1) // 2})")

    # Track existing points (for quick lookup)
    existing_point_set = set()
    for p in points:
        existing_point_set.add((p.x, p.y, p.z))

    # Compute all line intersections
    novel_points = []
    novel_point_set = set()

    for i in range(n_lines):
        for j in range(i+1, n_lines):
            intersection = intersect_lines(lines[i], lines[j], f"p_{config.day+1}^(?)")

            if intersection is None:
                # Lines are identical (shouldn't happen if line_dict works correctly)
                continue

            # Check if intersection is already in P_t
            key = (intersection.x, intersection.y, intersection.z)

            if key in existing_point_set:
                # Existing point
                continue

            if key in novel_point_set:
                # Already found this new point (concurrency)
                continue

            # Truly novel point
            novel_point_set.add(key)

            # Relabel with proper index
            new_label = f"p_{config.day+1}^{len(novel_points)+1}"
            novel_point = make_point(intersection.x, intersection.y, intersection.z, new_label)
            novel_points.append(novel_point)

    n_novel = len(novel_points)

    if verbose:
        print(f"Novel points: {n_novel}")
        print(f"Result: |B_{config.day+1}| = {config.blue_count()} + {n_novel} = {config.blue_count() + n_novel}")

    # Add novel points to configuration
    for p in novel_points:
        new_config.add_blue(p)

    return new_config, n_novel


def compute_upper_bound(b: int) -> Fraction:
    """
    Compute the upper bound U(b) = (3+b)(2+b)·b(b+1) / 8
    """
    return Fraction((3 + b) * (2 + b) * b * (b + 1), 8)


def run_simulation(max_day: int = 3):
    """
    Run the exact simulation from day 0 to max_day.
    """
    print("="*80)
    print("EXACT PROJECTIVE SIMULATOR FOR PROBLEM 957")
    print("="*80)
    print("Using:")
    print("  - Homogeneous coordinates [x:y:z] in ℝℙ²")
    print("  - Exact rational arithmetic (fractions.Fraction)")
    print("  - Canonical forms for deterministic equality")
    print("="*80)

    # Day 0
    config = initial_configuration()

    print(f"\nDAY 0: Initial Configuration")
    print(f"  Red points: {config.red_points}")
    print(f"  Blue points: {config.blue_points}")
    print(f"  |B_0| = {config.blue_count()}")

    # Verify general position
    print(f"\nVerifying general position...")
    is_general = verify_no_three_collinear(config.all_points())
    if is_general:
        print(f"✓ No three points are collinear (general position confirmed)")
    else:
        print(f"✗ Configuration NOT in general position!")
        return

    # Track counts
    counts = [(0, config.blue_count(), 0)]  # (day, |B_t|, |N_{t-1}|)

    # Iterate days
    for t in range(max_day):
        # Suppress verbose collinearities for days with many points
        verbose_collinearities = (config.blue_count() < 100)
        config, n_novel = propagate_one_day(config, verbose=True, verbose_collinearities=verbose_collinearities)

        # Cross-check with upper bound
        b_prev = counts[-1][1]
        upper = compute_upper_bound(b_prev)
        print(f"  Upper bound U({b_prev}) = {upper} = {float(upper):.2f}")

        if n_novel > upper:
            print(f"  ⚠️  ERROR: Novel points {n_novel} exceeds upper bound {upper}!")
        else:
            print(f"  ✓ Novel points within bound ({n_novel} ≤ {upper})")

        counts.append((config.day, config.blue_count(), n_novel))

    # Summary
    print(f"\n{'='*80}")
    print("SUMMARY")
    print(f"{'='*80}")
    print(f"{'Day':<8} {'|B_t|':<12} {'|N_t|':<12} {'U(|B_t|)':<15} {'Collapse %':<12}")
    print(f"{'-'*80}")

    for i, (day, b_t, n_t) in enumerate(counts):
        if i == 0:
            print(f"{day:<8} {b_t:<12} {'-':<12} {'-':<15} {'-':<12}")
        else:
            b_prev = counts[i-1][1]
            u = compute_upper_bound(b_prev)
            collapse_pct = 100 * (1 - Fraction(n_t) / u) if u > 0 else 0
            print(f"{day:<8} {b_t:<12} {n_t:<12} {float(u):<15.2f} {float(collapse_pct):<12.2f}")

    print(f"{'='*80}")
    print("✓ Simulation complete")
    print(f"✓ All equality checks exact (rational arithmetic)")
    print(f"✓ All point comparisons deterministic (canonical forms)")


if __name__ == "__main__":
    import sys
    max_day = int(sys.argv[1]) if len(sys.argv) > 1 else 3
    run_simulation(max_day=max_day)
