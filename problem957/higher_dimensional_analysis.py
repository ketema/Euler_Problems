#!/usr/bin/env python3
"""
Higher-dimensional analysis of Problem 957.

Explore whether the 2D configuration has simpler structure when embedded
in higher dimensions or mapped through space-filling curves.

Key hypothesis: κ_t might have a closed form in a different dimensional
representation.
"""

import sys
import numpy as np
from itertools import combinations
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

sys.path.insert(0, '.')

# Original 2D configuration
RED_2D = np.array([
    [-1.1420985748, -3.1278529420],
    [1.7213348846, -0.8343651343],
    [4.3760906863, 2.3859745813]
])

BLUE_2D = np.array([
    [-1.8437265624, 1.4483260402],
    [-1.0486909239, 2.1320688328]
])


def explore_3d_embeddings():
    """Try various 3D embeddings and analyze structure."""
    print("="*80)
    print("3D EMBEDDING EXPLORATION")
    print("="*80)
    print()

    embeddings = {
        'planar': 'z = 0 (trivial embedding)',
        'parabolic': 'z = x² + y² (paraboloid)',
        'distance': 'z = distance from origin',
        'product': 'z = x * y (hyperbolic paraboloid)',
        'sphere': 'project onto unit sphere',
    }

    results = {}

    for name, description in embeddings.items():
        print(f"Embedding: {name}")
        print(f"  Description: {description}")

        # Create 3D points based on embedding
        if name == 'planar':
            red_3d = np.column_stack([RED_2D, np.zeros(len(RED_2D))])
            blue_3d = np.column_stack([BLUE_2D, np.zeros(len(BLUE_2D))])

        elif name == 'parabolic':
            red_z = RED_2D[:, 0]**2 + RED_2D[:, 1]**2
            blue_z = BLUE_2D[:, 0]**2 + BLUE_2D[:, 1]**2
            red_3d = np.column_stack([RED_2D, red_z])
            blue_3d = np.column_stack([BLUE_2D, blue_z])

        elif name == 'distance':
            red_z = np.sqrt(RED_2D[:, 0]**2 + RED_2D[:, 1]**2)
            blue_z = np.sqrt(BLUE_2D[:, 0]**2 + BLUE_2D[:, 1]**2)
            red_3d = np.column_stack([RED_2D, red_z])
            blue_3d = np.column_stack([BLUE_2D, blue_z])

        elif name == 'product':
            red_z = RED_2D[:, 0] * RED_2D[:, 1]
            blue_z = BLUE_2D[:, 0] * BLUE_2D[:, 1]
            red_3d = np.column_stack([RED_2D, red_z])
            blue_3d = np.column_stack([BLUE_2D, blue_z])

        elif name == 'sphere':
            # Stereographic projection onto unit sphere
            def to_sphere(pts_2d):
                x, y = pts_2d[:, 0], pts_2d[:, 1]
                denom = 1 + x**2 + y**2
                return np.column_stack([
                    2*x / denom,
                    2*y / denom,
                    (x**2 + y**2 - 1) / denom
                ])
            red_3d = to_sphere(RED_2D)
            blue_3d = to_sphere(BLUE_2D)

        # Analyze 3D structure
        all_3d = np.vstack([red_3d, blue_3d])

        # Check for planar subsets
        is_planar = check_coplanarity(all_3d)
        print(f"  All 5 points coplanar: {is_planar}")

        # Check if points lie on a simple surface
        surface_type = detect_surface(all_3d)
        print(f"  Surface type: {surface_type}")

        # Compute 3D distances
        dists = []
        for i in range(len(all_3d)):
            for j in range(i+1, len(all_3d)):
                dist = np.linalg.norm(all_3d[i] - all_3d[j])
                dists.append(dist)

        # Check for regular polytope structure
        unique_dists = len(set(np.round(dists, 6)))
        print(f"  Unique distance classes: {unique_dists}")

        results[name] = {
            'coplanar': is_planar,
            'surface': surface_type,
            'unique_distances': unique_dists,
            'points_3d': all_3d
        }

        print()

    return results


def check_coplanarity(points):
    """Check if all points lie in a plane."""
    if len(points) < 4:
        return True

    # Take first 3 points to define a plane
    p1, p2, p3 = points[:3]

    # Normal vector to plane
    v1 = p2 - p1
    v2 = p3 - p1
    normal = np.cross(v1, v2)
    normal = normal / np.linalg.norm(normal)

    # Check if all other points lie on this plane
    for p in points[3:]:
        # Vector from p1 to p
        v = p - p1
        # Dot product with normal should be ~0
        dist = abs(np.dot(v, normal))
        if dist > 1e-6:
            return False

    return True


def detect_surface(points):
    """Try to detect if points lie on a known surface."""
    # Check for sphere
    center = np.mean(points, axis=0)
    radii = [np.linalg.norm(p - center) for p in points]
    if np.std(radii) < 1e-6:
        return f"Sphere (radius {np.mean(radii):.6f})"

    # Check for plane
    if check_coplanarity(points):
        return "Plane"

    # Check for paraboloid z = ax² + by² + c
    try:
        X = np.column_stack([points[:, 0]**2, points[:, 1]**2, np.ones(len(points))])
        z = points[:, 2]
        coeffs, residuals, _, _ = np.linalg.lstsq(X, z, rcond=None)
        if residuals.size > 0 and residuals[0] < 1e-6:
            return f"Paraboloid z={coeffs[0]:.3f}x²+{coeffs[1]:.3f}y²+{coeffs[2]:.3f}"
    except:
        pass

    return "General position"


def analyze_projective_coordinates():
    """Analyze configuration in projective coordinates [x:y:z]."""
    print("="*80)
    print("PROJECTIVE COORDINATE ANALYSIS")
    print("="*80)
    print()

    # Convert to homogeneous coordinates [x:y:1]
    red_proj = np.column_stack([RED_2D, np.ones(len(RED_2D))])
    blue_proj = np.column_stack([BLUE_2D, np.ones(len(BLUE_2D))])

    print("Red points in projective coordinates [x:y:z]:")
    for i, p in enumerate(red_proj):
        print(f"  R{i} = [{p[0]:.6f} : {p[1]:.6f} : {p[2]:.6f}]")
    print()

    print("Blue points in projective coordinates:")
    for i, p in enumerate(blue_proj):
        print(f"  B{i} = [{p[0]:.6f} : {p[1]:.6f} : {p[2]:.6f}]")
    print()

    # Try to find a projective transformation that simplifies structure
    # For example, can we transform so some points go to infinity?

    # Check cross-ratios in projective coordinates
    print("Checking projective invariants...")
    # For lines through pairs of points
    for i, r1 in enumerate(red_proj):
        for j, r2 in enumerate(red_proj):
            if i >= j:
                continue
            # Line through r1 and r2
            line_coefs = np.cross(r1, r2)
            print(f"  Line through R{i} and R{j}: {line_coefs}")

    print()


def hilbert_curve_analysis():
    """Analyze if Hilbert curve mapping reveals structure."""
    print("="*80)
    print("HILBERT SPACE-FILLING CURVE ANALYSIS")
    print("="*80)
    print()

    # Try to map 2D points to 1D via Hilbert curve
    # Then see if 1D positions have simple pattern

    print("Hypothesis: If we map points via Hilbert curve to 1D,")
    print("maybe the 1D ordering reveals a pattern in κ_t")
    print()

    # For a proper Hilbert curve we need to discretize
    # Let's use a simple approach: map (x,y) to t via Hilbert indexing

    try:
        from hilbertcurve.hilbertcurve import HilbertCurve

        # Normalize coordinates to [0, 2^p - 1]
        all_points = np.vstack([RED_2D, BLUE_2D])
        min_x, max_x = all_points[:, 0].min(), all_points[:, 0].max()
        min_y, max_y = all_points[:, 1].min(), all_points[:, 1].max()

        # Scale to [0, 255] for 8-bit Hilbert curve
        p = 8  # 8 bits per dimension
        N = 2**p

        scaled = np.zeros_like(all_points, dtype=int)
        scaled[:, 0] = ((all_points[:, 0] - min_x) / (max_x - min_x) * (N - 1)).astype(int)
        scaled[:, 1] = ((all_points[:, 1] - min_y) / (max_y - min_y) * (N - 1)).astype(int)

        # Map to Hilbert curve indices
        hilbert = HilbertCurve(p, 2)
        indices = [hilbert.distance_from_point([x, y]) for x, y in scaled]

        print(f"Hilbert curve order: {p} bits per dimension")
        print(f"Point -> 1D index mapping:")
        for i, (pt, idx) in enumerate(zip(all_points, indices)):
            label = f"R{i}" if i < len(RED_2D) else f"B{i - len(RED_2D)}"
            print(f"  {label} ({pt[0]:.4f}, {pt[1]:.4f}) -> index {idx}")

        print()
        print("Sorted by Hilbert index:")
        sorted_indices = sorted(enumerate(indices), key=lambda x: x[1])
        for orig_idx, hilbert_idx in sorted_indices:
            label = f"R{orig_idx}" if orig_idx < len(RED_2D) else f"B{orig_idx - len(RED_2D)}"
            print(f"  {hilbert_idx:6d}: {label}")

        print()
        print("Analysis: Does the Hilbert ordering reveal any pattern?")
        # Check if reds and blues cluster in Hilbert space
        red_indices = indices[:len(RED_2D)]
        blue_indices = indices[len(RED_2D):]

        print(f"  Red indices: {red_indices}")
        print(f"  Blue indices: {blue_indices}")
        print(f"  Are reds contiguous? {max(red_indices) - min(red_indices) == 2}")
        print(f"  Are blues contiguous? {max(blue_indices) - min(blue_indices) == 1}")

    except ImportError:
        print("Note: hilbertcurve library not installed")
        print("Install with: pip install hilbertcurve")
        print()
        print("Alternative: Manual Z-order curve (Morton code)")
        print()

        # Implement simple Z-order curve (interleave bits)
        def morton_encode(x, y, bits=16):
            """Encode (x,y) as Morton code (Z-order curve)."""
            result = 0
            for i in range(bits):
                result |= ((x & (1 << i)) << i) | ((y & (1 << i)) << (i + 1))
            return result

        # Scale to integer coordinates
        all_points = np.vstack([RED_2D, BLUE_2D])
        min_x, max_x = all_points[:, 0].min(), all_points[:, 0].max()
        min_y, max_y = all_points[:, 1].min(), all_points[:, 1].max()

        N = 2**16
        scaled_x = ((all_points[:, 0] - min_x) / (max_x - min_x) * (N - 1)).astype(int)
        scaled_y = ((all_points[:, 1] - min_y) / (max_y - min_y) * (N - 1)).astype(int)

        morton_codes = [morton_encode(x, y) for x, y in zip(scaled_x, scaled_y)]

        print("Morton code (Z-order curve) mapping:")
        for i, (pt, code) in enumerate(zip(all_points, morton_codes)):
            label = f"R{i}" if i < len(RED_2D) else f"B{i - len(RED_2D)}"
            print(f"  {label} ({pt[0]:.4f}, {pt[1]:.4f}) -> {code}")

        print()

    print()


def explore_higher_dimensions():
    """Try 4D, 5D embeddings."""
    print("="*80)
    print("HIGHER DIMENSIONAL EMBEDDINGS (4D+)")
    print("="*80)
    print()

    # Hypothesis: Maybe in 4D or 5D, the 5 points become vertices
    # of a regular polytope, and κ_t follows from polytope symmetry

    print("Testing 4D embedding strategies:")
    print()

    # Strategy 1: Add two more coordinates based on distances
    all_2d = np.vstack([RED_2D, BLUE_2D])

    # Compute pairwise distances in 2D
    n = len(all_2d)
    dists = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            dists[i, j] = np.linalg.norm(all_2d[i] - all_2d[j])

    # Use MDS to find 4D embedding that preserves distances
    from sklearn.manifold import MDS

    for dim in [3, 4, 5]:
        mds = MDS(n_components=dim, dissimilarity='precomputed', random_state=42)
        embedding = mds.fit_transform(dists)

        print(f"{dim}D MDS embedding (preserves pairwise distances):")
        print(f"  Stress: {mds.stress_:.6f} (lower is better fit)")

        # Check if this creates a regular polytope
        # For regular polytope, all edges should have same length
        embedded_dists = []
        for i in range(n):
            for j in range(i+1, n):
                d = np.linalg.norm(embedding[i] - embedding[j])
                embedded_dists.append(d)

        unique_dists = len(set(np.round(embedded_dists, 6)))
        print(f"  Unique distance classes: {unique_dists}")

        if unique_dists <= 3:
            print(f"  → Possible regular or semi-regular polytope structure!")

        print()


def main():
    """Run all higher-dimensional analyses."""

    print("="*80)
    print("HIGHER-DIMENSIONAL ANALYSIS OF PROBLEM 957")
    print("="*80)
    print()
    print("Hypothesis: The 2D configuration may have simpler structure")
    print("when viewed in higher dimensions or through space-filling curves.")
    print()
    print("If we find a space where κ_t has a pattern, we can derive it!")
    print()

    # 3D embeddings
    embedding_results = explore_3d_embeddings()

    # Projective coordinates
    analyze_projective_coordinates()

    # Hilbert curve
    hilbert_curve_analysis()

    # Higher dimensions
    explore_higher_dimensions()

    print("="*80)
    print("CONCLUSION")
    print("="*80)
    print()
    print("Analysis complete. Key questions:")
    print("1. Did any embedding reveal regular polytope structure?")
    print("2. Did Hilbert curve ordering show patterns?")
    print("3. Did higher dimensions simplify the incidence structure?")
    print()
    print("If YES to any: That space might reveal κ_t formula!")
    print()


if __name__ == '__main__':
    main()
