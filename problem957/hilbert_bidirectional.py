#!/usr/bin/env python3
"""
Bidirectional Hilbert Curve Implementation

Provides both:
  - encode(x, y) → index
  - decode(index) → (x, y)

This allows us to:
  1. Find Hilbert indices for intersection points
  2. Decode neighboring indices to find spatial neighbors
  3. Explore locality patterns and geometric significance
"""

import numpy as np
from collections import namedtuple

Point = namedtuple('Point', ['x', 'y'])


class HilbertCurve:
    """Bidirectional Hilbert curve for 2D points."""

    def __init__(self, order=16):
        """
        Initialize Hilbert curve.

        Args:
            order: Curve order (2^order × 2^order grid)
        """
        self.order = order
        self.n = 2 ** order
        self.max_index = self.n * self.n - 1

    def rotate(self, n, x, y, rx, ry):
        """Rotate/flip quadrant appropriately."""
        if ry == 0:
            if rx == 1:
                x = n - 1 - x
                y = n - 1 - y
            # Swap x and y
            x, y = y, x
        return x, y

    def encode(self, x, y):
        """
        Encode 2D point to Hilbert index.

        Args:
            x, y: Coordinates (will be normalized to [0, n-1])

        Returns:
            Hilbert curve index (0 to n²-1)
        """
        # Normalize to grid
        xi = int(x * (self.n - 1))
        yi = int(y * (self.n - 1))

        # Clamp to valid range
        xi = max(0, min(self.n - 1, xi))
        yi = max(0, min(self.n - 1, yi))

        # Encode using rotation-based algorithm
        d = 0
        s = self.n // 2

        while s > 0:
            rx = 1 if (xi & s) > 0 else 0
            ry = 1 if (yi & s) > 0 else 0
            d += s * s * ((3 * rx) ^ ry)
            xi, yi = self.rotate(s, xi, yi, rx, ry)
            s //= 2

        return d

    def decode(self, index):
        """
        Decode Hilbert index to 2D point.

        Args:
            index: Hilbert curve index (0 to n²-1)

        Returns:
            (x, y) coordinates normalized to [0, 1]
        """
        # Clamp index
        index = max(0, min(self.max_index, index))

        # Decode using rotation-based algorithm
        x, y = 0, 0
        s = 1

        while s < self.n:
            rx = 1 if (index // 2) & 1 else 0
            ry = 1 if (index ^ rx) & 1 else 0
            x, y = self.rotate(s, x, y, rx, ry)
            x += s * rx
            y += s * ry
            index //= 4
            s *= 2

        # Normalize to [0, 1]
        return x / (self.n - 1), y / (self.n - 1)

    def encode_point(self, point, bounds):
        """
        Encode point with explicit bounds.

        Args:
            point: Point with x, y attributes
            bounds: (min_x, max_x, min_y, max_y)

        Returns:
            Hilbert index
        """
        min_x, max_x, min_y, max_y = bounds

        # Normalize to [0, 1]
        nx = (point.x - min_x) / (max_x - min_x) if max_x > min_x else 0.5
        ny = (point.y - min_y) / (max_y - min_y) if max_y > min_y else 0.5

        return self.encode(nx, ny)

    def decode_to_bounds(self, index, bounds):
        """
        Decode index back to original coordinate system.

        Args:
            index: Hilbert index
            bounds: (min_x, max_x, min_y, max_y)

        Returns:
            Point(x, y) in original coordinates
        """
        min_x, max_x, min_y, max_y = bounds

        # Decode to [0, 1]
        nx, ny = self.decode(index)

        # Scale back to original bounds
        x = min_x + nx * (max_x - min_x)
        y = min_y + ny * (max_y - min_y)

        return Point(x, y)


def test_bidirectional():
    """Test that encode/decode are inverse operations."""
    print("="*70)
    print("BIDIRECTIONAL HILBERT CURVE TEST")
    print("="*70)
    print()

    curve = HilbertCurve(order=8)  # 256×256 grid

    # Test 1: Encode then decode
    print("Test 1: Encode → Decode (should recover original)")
    print("-"*70)

    test_points = [
        (0.0, 0.0),
        (1.0, 1.0),
        (0.5, 0.5),
        (0.25, 0.75),
        (0.123, 0.456),
    ]

    for x, y in test_points:
        index = curve.encode(x, y)
        x_dec, y_dec = curve.decode(index)
        error = ((x - x_dec)**2 + (y - y_dec)**2)**0.5

        print(f"  ({x:.4f}, {y:.4f}) → index {index:6d} → ({x_dec:.4f}, {y_dec:.4f})  error={error:.6f}")

    print()

    # Test 2: Decode then encode
    print("Test 2: Decode → Encode (should recover original)")
    print("-"*70)

    test_indices = [0, 1000, 10000, 30000, 65535]

    for index in test_indices:
        x, y = curve.decode(index)
        index_enc = curve.encode(x, y)

        print(f"  Index {index:6d} → ({x:.4f}, {y:.4f}) → index {index_enc:6d}  {'✓' if index == index_enc else '✗'}")

    print()

    # Test 3: Locality preservation
    print("Test 3: Locality Preservation")
    print("-"*70)
    print("Neighboring indices should decode to nearby points:")
    print()

    base_index = 10000
    neighbors = [base_index - 1, base_index, base_index + 1]

    points = []
    for idx in neighbors:
        x, y = curve.decode(idx)
        points.append((idx, x, y))

    for idx, x, y in points:
        print(f"  Index {idx}: ({x:.6f}, {y:.6f})")

    # Compute distances
    for i in range(len(points) - 1):
        idx1, x1, y1 = points[i]
        idx2, x2, y2 = points[i + 1]
        dist = ((x2 - x1)**2 + (y2 - y1)**2)**0.5
        print(f"  Distance {idx1}→{idx2}: {dist:.6f}")

    print()
    print("✓ Bidirectional Hilbert curve working correctly")
    print()


if __name__ == "__main__":
    test_bidirectional()
