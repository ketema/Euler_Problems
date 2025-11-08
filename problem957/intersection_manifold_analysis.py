#!/usr/bin/env python3
"""
Deeper analysis: Study the INTERSECTION POINTS as a manifold in higher dimensions.

Key insight: Maybe κ_t relates to the dimensional collapse when we embed
the sequence of intersection points (day 1, day 2, ...) in higher-dimensional space.
"""

import sys
import numpy as np
from sklearn.decomposition import PCA
from sklearn.manifold import MDS, TSNE
import matplotlib.pyplot as plt

sys.path.insert(0, '.')
from src.geometry import Point, Line, intersect

# Original configuration
RED = [
    Point(-1.1420985748, -3.1278529420, 'red'),
    Point(1.7213348846, -0.8343651343, 'red'),
    Point(4.3760906863, 2.3859745813, 'red')
]

BLUE_INIT = [
    Point(-1.8437265624, 1.4483260402, 'blue'),
    Point(-1.0486909239, 2.1320688328, 'blue')
]


def compute_day1_intersections():
    """Compute all Day 1 intersection points."""
    lines = []
    for r in RED:
        for b in BLUE_INIT:
            lines.append(Line(r, b))

    intersections = []
    for i in range(len(lines)):
        for j in range(i+1, len(lines)):
            pt = intersect(lines[i], lines[j])
            if pt is not None:
                # Check if new (not red, not blue)
                is_new = True
                for r in RED:
                    if r == pt:
                        is_new = False
                for b in BLUE_INIT:
                    if b == pt:
                        is_new = False
                if is_new:
                    intersections.append([pt.x, pt.y])

    return np.array(intersections)


def analyze_intersection_manifold():
    """Analyze the manifold formed by intersection points."""
    print("="*80)
    print("INTERSECTION MANIFOLD ANALYSIS")
    print("="*80)
    print()

    # Compute Day 1 intersections
    day1_pts = compute_day1_intersections()
    print(f"Day 1: {len(day1_pts)} intersection points")
    print()

    # Add original blues for context
    blues_2d = np.array([[b.x, b.y] for b in BLUE_INIT])
    all_blues = np.vstack([blues_2d, day1_pts])

    print(f"Total blues after day 1: {len(all_blues)}")
    print()

    # PCA Analysis - what's the intrinsic dimensionality?
    print("="*60)
    print("PCA ANALYSIS - Intrinsic Dimensionality")
    print("="*60)
    print()

    pca = PCA()
    pca.fit(day1_pts)

    print("Explained variance ratio by principal component:")
    for i, var in enumerate(pca.explained_variance_ratio_):
        print(f"  PC{i+1}: {100*var:.4f}% of variance")
    print()

    cumsum = np.cumsum(pca.explained_variance_ratio_)
    print("Cumulative explained variance:")
    for i, cum in enumerate(cumsum):
        print(f"  First {i+1} PCs: {100*cum:.4f}%")
    print()

    # Check if points lie on a 1D curve
    if pca.explained_variance_ratio_[0] > 0.95:
        print("→ Points approximately lie on a 1D curve!")
        print(f"  Dominant PC explains {100*pca.explained_variance_ratio_[0]:.2f}%")
    else:
        print("→ Points do not lie on a simple 1D curve")

    print()

    # Project onto first principal component
    day1_1d = pca.transform(day1_pts)[:, 0]
    print("Day 1 points projected to 1D (first PC):")
    sorted_1d = sorted(enumerate(day1_1d), key=lambda x: x[1])
    for i, (orig_idx, pc1) in enumerate(sorted_1d[:5]):
        print(f"  Point {orig_idx}: PC1 = {pc1:.6f}")
    print(f"  ...")
    print()

    return pca, day1_pts


def analyze_radial_structure():
    """Check if intersections have radial structure from origin."""
    print("="*80)
    print("RADIAL STRUCTURE ANALYSIS")
    print("="*80)
    print()

    day1_pts = compute_day1_intersections()

    # Compute distances from origin
    distances = np.sqrt(day1_pts[:, 0]**2 + day1_pts[:, 1]**2)

    # Compute angles
    angles = np.arctan2(day1_pts[:, 1], day1_pts[:, 0])

    print(f"Distance statistics:")
    print(f"  Min: {distances.min():.6f}")
    print(f"  Max: {distances.max():.6f}")
    print(f"  Mean: {distances.mean():.6f}")
    print(f"  Std: {distances.std():.6f}")
    print()

    # Check if distances cluster
    unique_dists = len(set(np.round(distances, 3)))
    print(f"Unique distance classes (rounded to 3 decimals): {unique_dists}/{len(distances)}")

    if unique_dists < len(distances) / 2:
        print("→ Distances cluster! Possible radial symmetry")
    else:
        print("→ Distances are fairly uniform - no strong radial structure")

    print()

    # Check angular distribution
    angles_deg = np.degrees(angles)
    print("Angular distribution:")
    angle_bins = np.histogram(angles_deg, bins=12)[0]  # 12 bins = 30° each
    for i, count in enumerate(angle_bins):
        angle_start = i * 30 - 180
        print(f"  {angle_start:4d}° to {angle_start+30:4d}°: {count} points")

    print()


def analyze_algebraic_structure():
    """Check if points satisfy an algebraic equation."""
    print("="*80)
    print("ALGEBRAIC VARIETY ANALYSIS")
    print("="*80)
    print()

    day1_pts = compute_day1_intersections()

    print("Testing if points lie on algebraic curves:")
    print()

    # Test various algebraic forms
    x = day1_pts[:, 0]
    y = day1_pts[:, 1]

    # Test: Circle (x-h)² + (y-k)² = r²
    # Fit circle
    A = np.column_stack([x, y, np.ones(len(x))])
    b = x**2 + y**2
    try:
        coeffs = np.linalg.lstsq(A, b, rcond=None)[0]
        h, k = coeffs[0]/2, coeffs[1]/2
        r_squared = coeffs[2] + h**2 + k**2

        # Check fit quality
        residuals = (x - h)**2 + (y - k)**2 - r_squared
        rms_error = np.sqrt(np.mean(residuals**2))

        print(f"Circle fit: (x-{h:.4f})² + (y-{k:.4f})² = {r_squared:.4f}")
        print(f"  RMS error: {rms_error:.6f}")
        if rms_error < 0.01:
            print(f"  → Points lie on circle!")
        print()
    except:
        print("Circle fit failed")
        print()

    # Test: Ellipse ax² + by² + cxy + dx + ey + f = 0
    A = np.column_stack([x**2, y**2, x*y, x, y, np.ones(len(x))])
    b = np.zeros(len(x))
    try:
        # This is homogeneous, use SVD
        U, s, Vt = np.linalg.svd(A)
        coeffs = Vt[-1]  # Last right singular vector

        # Check fit quality
        residuals = A @ coeffs
        rms_error = np.sqrt(np.mean(residuals**2))

        print(f"Conic section fit:")
        print(f"  {coeffs[0]:.4f}x² + {coeffs[1]:.4f}y² + {coeffs[2]:.4f}xy")
        print(f"  + {coeffs[3]:.4f}x + {coeffs[4]:.4f}y + {coeffs[5]:.4f} = 0")
        print(f"  RMS error: {rms_error:.6f}")
        if rms_error < 0.01:
            print(f"  → Points lie on conic section!")
        print()
    except:
        print("Conic fit failed")
        print()

    # Test higher degree: cubic, quartic
    for degree in [3, 4]:
        # Generate all monomial terms up to degree
        terms = []
        for i in range(degree + 1):
            for j in range(degree + 1 - i):
                terms.append(x**i * y**j)

        A = np.column_stack(terms)
        try:
            U, s, Vt = np.linalg.svd(A)
            coeffs = Vt[-1]
            residuals = A @ coeffs
            rms_error = np.sqrt(np.mean(residuals**2))

            print(f"Degree {degree} polynomial fit:")
            print(f"  {len(terms)} terms")
            print(f"  RMS error: {rms_error:.6f}")
            if rms_error < 0.01:
                print(f"  → Points lie on degree-{degree} curve!")
            print()
        except:
            print(f"Degree {degree} fit failed")
            print()


def analyze_kappa_via_dimension():
    """The key question: Does κ_t relate to dimensional collapse?"""
    print("="*80)
    print("κ_t AND DIMENSIONAL COLLAPSE")
    print("="*80)
    print()

    print("Hypothesis: κ_t might relate to how much the intersection")
    print("points 'collapse' from their candidate space.")
    print()

    # Day 1: 6 candidate lines, 15 line pairs, but only 6 unique intersections
    # This is a collapse from 15D candidate space to 6D actual space

    print("Day 1:")
    print("  Candidate line-pairs: 15 (= C(6,2))")
    print("  Unique intersections: 6")
    print("  Dimensional collapse: 15 → 6 (factor of 2.5)")
    print("  κ_1 = 6/6 = 1.0 (no coincidence in final count)")
    print()

    print("Day 2:")
    print("  Candidate line-pairs: 234")
    print("  Unique intersections: 20")
    print("  Dimensional collapse: 234 → 20 (factor of 11.7)")
    print("  κ_2 = 20/162 ≈ 0.123")
    print()

    print("Observation:")
    print("  The 'dimensional collapse factor' (234/20 = 11.7) is close to")
    print("  the reciprocal of κ_2 × (C_gen/candidate_pairs)")
    print()
    print("  Maybe κ_t encodes information about the manifold dimension")
    print("  of the intersection point cloud in higher-dimensional space?")
    print()


def main():
    """Run full manifold analysis."""
    print("="*80)
    print("INTERSECTION MANIFOLD AND DIMENSIONAL ANALYSIS")
    print("="*80)
    print()

    # Analyze manifold
    pca, day1_pts = analyze_intersection_manifold()

    # Radial structure
    analyze_radial_structure()

    # Algebraic structure
    analyze_algebraic_structure()

    # κ_t and dimension
    analyze_kappa_via_dimension()

    print("="*80)
    print("CONCLUSION")
    print("="*80)
    print()
    print("Key findings:")
    print("1. Are Day 1 intersections on a low-dimensional manifold?")
    print("2. Do they satisfy an algebraic equation?")
    print("3. Does κ_t relate to dimensional collapse?")
    print()
    print("If any manifold structure found → κ_t might be derivable!")
    print()


if __name__ == '__main__':
    main()
