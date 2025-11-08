"""
Phase 4: Interactive Visualization Backend

Provides coordinate transformation, viewport management, and data binding
for HTML5 Canvas visualization of Project Euler Problem 957.

REQ-PE957-VIZ-001: Coordinate transformation (math → screen)
REQ-PE957-VIZ-003: Data binding to OEIS A189191
REQ-PE957-VIZ-007: Performance sampling for large datasets
"""

from typing import Tuple, Set
from src.geometry import Point


class Viewport:
    """
    Viewport bounds for coordinate transformation.

    Defines the rectangular region of mathematical coordinate space
    that will be mapped to the canvas.
    """

    def __init__(self, min_x: float, max_x: float, min_y: float, max_y: float):
        """
        Initialize viewport bounds.

        Args:
            min_x: Minimum x coordinate (left edge)
            max_x: Maximum x coordinate (right edge)
            min_y: Minimum y coordinate (bottom edge)
            max_y: Maximum y coordinate (top edge)
        """
        self.min_x = float(min_x)
        self.max_x = float(max_x)
        self.min_y = float(min_y)
        self.max_y = float(max_y)

    @property
    def width(self) -> float:
        """Width of viewport (x-axis range)."""
        return self.max_x - self.min_x

    @property
    def height(self) -> float:
        """Height of viewport (y-axis range)."""
        return self.max_y - self.min_y

    def __repr__(self) -> str:
        return (f"Viewport(min_x={self.min_x}, max_x={self.max_x}, "
                f"min_y={self.min_y}, max_y={self.max_y})")


def transform_to_screen(
    math_x: float,
    math_y: float,
    viewport: Viewport,
    canvas_width: int,
    canvas_height: int
) -> Tuple[float, float]:
    """
    Transform mathematical coordinates to screen coordinates.

    REQ-PE957-VIZ-001: Coordinate transformation with y-axis inversion.

    Mathematical coordinate system:
    - Origin at center
    - Y-axis UP (positive y is above origin)
    - X-axis right (positive x is right of origin)

    Screen coordinate system:
    - Origin at top-left
    - Y-axis DOWN (positive y is below origin)
    - X-axis right (same as math)

    Args:
        math_x: X coordinate in mathematical space
        math_y: Y coordinate in mathematical space
        viewport: Viewport bounds defining visible region
        canvas_width: Canvas width in pixels
        canvas_height: Canvas height in pixels

    Returns:
        Tuple of (screen_x, screen_y) in pixel coordinates

    Example:
        >>> viewport = Viewport(-5, 5, -5, 5)
        >>> transform_to_screen(0, 0, viewport, 800, 600)
        (400.0, 300.0)  # Origin maps to canvas center
    """
    # Calculate scale (pixels per math unit)
    # Use min to maintain aspect ratio
    scale_x = canvas_width / viewport.width
    scale_y = canvas_height / viewport.height
    scale = min(scale_x, scale_y)

    # Find center of viewport in mathematical coordinates
    center_x = viewport.min_x + viewport.width / 2
    center_y = viewport.min_y + viewport.height / 2

    # Transform with y-axis inversion
    # 1. Translate: offset by viewport center
    # 2. Scale: multiply by pixels per unit
    # 3. Translate to canvas center
    screen_x = (math_x - center_x) * scale + canvas_width / 2

    # Y-axis inversion: SUBTRACT instead of ADD
    # Math y increases upward, screen y increases downward
    screen_y = canvas_height / 2 - (math_y - center_y) * scale

    return (screen_x, screen_y)


def calculate_viewport(
    points: Set[Point],
    padding_percent: float = 0.1
) -> Viewport:
    """
    Calculate viewport bounds from a set of points with padding.

    REQ-PE957-VIZ-001: Auto-calculate viewport to frame all points.

    Args:
        points: Set of Point objects to frame
        padding_percent: Percentage of range to add as padding (default 10%)

    Returns:
        Viewport encompassing all points with padding

    Raises:
        ValueError: If points set is empty

    Example:
        >>> points = {Point(0, 0), Point(2, 2)}
        >>> viewport = calculate_viewport(points, padding_percent=0.1)
        >>> viewport.min_x
        -0.2  # 10% padding on 2.0 range
        >>> viewport.max_x
        2.2
    """
    if not points:
        raise ValueError("Cannot calculate viewport from empty point set")

    # Find min/max coordinates
    min_x = min(p.x for p in points)
    max_x = max(p.x for p in points)
    min_y = min(p.y for p in points)
    max_y = max(p.y for p in points)

    # Calculate ranges
    x_range = max_x - min_x
    y_range = max_y - min_y

    # Handle degenerate case (all points at same location)
    # Use 1.0 as default range to avoid zero division
    if x_range == 0:
        x_range = 1.0
    if y_range == 0:
        y_range = 1.0

    # Add padding to each side
    x_padding = x_range * padding_percent
    y_padding = y_range * padding_percent

    return Viewport(
        min_x=min_x - x_padding,
        max_x=max_x + x_padding,
        min_y=min_y - y_padding,
        max_y=max_y + y_padding
    )


def get_sampling_disclaimer(
    day: int,
    total_points: int,
    sampled_points: int
) -> str:
    """
    Return disclaimer message when rendering uses sampling.

    REQ-PE957-VIZ-007: Performance sampling for days 11-16.

    For large datasets (>10K points), visualization samples a subset
    for performance. This function provides user-facing message explaining
    the sampling.

    Args:
        day: Current day number (0-16)
        total_points: Total number of points in dataset
        sampled_points: Number of points actually rendered

    Returns:
        Disclaimer string, or empty string if not sampling

    Example:
        >>> get_sampling_disclaimer(15, 9_874_845_582_657_972_888, 10_000)
        'Day 15: Showing 10,000 of 9,874,845,582,657,972,888 points (statistical sample)'

        >>> get_sampling_disclaimer(5, 19_161, 19_161)
        ''  # No sampling needed
    """
    if sampled_points < total_points:
        return (
            f"Day {day}: Showing {sampled_points:,} of {total_points:,} points "
            f"(statistical sample)"
        )
    return ""
