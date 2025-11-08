"""
Geometry primitives for Project Euler Problem 957.

Implements Point and Line classes with epsilon-based equality and intersection algorithms.
"""

from typing import Optional

# Epsilon tolerance for floating-point comparisons (REQ-PE957-001)
EPSILON = 1e-9


def _float_equal(a: float, b: float) -> bool:
    """Check if two floats are equal within epsilon tolerance."""
    return abs(a - b) <= EPSILON


class Point:
    """
    Represents a 2D point with color attribute.

    REQ-PE957-001: Point representation with epsilon equality and hashability.

    Attributes:
        x: X-coordinate (float)
        y: Y-coordinate (float)
        color: Point color ('red', 'blue', or 'white')
    """

    def __init__(self, x: float, y: float, color: str = 'white'):
        """
        Create a point with coordinates and color.

        Args:
            x: X-coordinate
            y: Y-coordinate
            color: Point color ('red', 'blue', or 'white'), defaults to 'white'

        Raises:
            TypeError: If x or y are not numeric
            ValueError: If color is not 'red', 'blue', or 'white'
        """
        # Validate coordinate types
        if not isinstance(x, (int, float)):
            raise TypeError(f"x coordinate must be numeric, got {type(x).__name__}")
        if not isinstance(y, (int, float)):
            raise TypeError(f"y coordinate must be numeric, got {type(y).__name__}")

        # Validate color
        valid_colors = {'red', 'blue', 'white'}
        if color not in valid_colors:
            raise ValueError(f"color must be one of {valid_colors}, got '{color}'")

        self.x = float(x)
        self.y = float(y)
        self.color = color

    def __eq__(self, other) -> bool:
        """
        Check equality using epsilon tolerance.

        Points are equal if both coordinates differ by less than EPSILON (1e-9).
        """
        if not isinstance(other, Point):
            return False
        return _float_equal(self.x, other.x) and _float_equal(self.y, other.y)

    def __hash__(self) -> int:
        """
        Make Point hashable for use in sets.

        Hash based on rounded coordinates to ensure equal points have same hash.
        """
        # Round to 9 decimal places (epsilon = 1e-9)
        x_rounded = round(self.x / EPSILON) * EPSILON
        y_rounded = round(self.y / EPSILON) * EPSILON
        return hash((x_rounded, y_rounded))

    def __str__(self) -> str:
        """String representation for debugging."""
        return f"Point({self.x}, {self.y}, '{self.color}')"

    def __repr__(self) -> str:
        """Detailed string representation."""
        return self.__str__()


class Line:
    """
    Represents an infinite line passing through two points.

    REQ-PE957-002: Line representation with vertical line support and parallel/identical detection.

    A line can be:
    - Non-vertical: y = mx + b (stored as slope m, intercept b)
    - Vertical: x = c (stored as vertical=True, x_intercept=c)
    """

    def __init__(self, p1: Point, p2: Point):
        """
        Create a line passing through two distinct points.

        Args:
            p1: First point
            p2: Second point

        Raises:
            ValueError: If points are identical or within epsilon tolerance
        """
        # Validate points are distinct (using epsilon equality)
        if p1 == p2:
            raise ValueError(
                f"Line requires two distinct points. "
                f"Points ({p1.x}, {p1.y}) and ({p2.x}, {p2.y}) "
                f"are within epsilon tolerance."
            )

        self.p1 = p1
        self.p2 = p2

        # Check if line is vertical
        if _float_equal(p1.x, p2.x):
            self.vertical = True
            self.x_intercept = p1.x
            self.slope = None
            self.intercept = None
        else:
            self.vertical = False
            # Calculate slope and intercept
            self.slope = (p2.y - p1.y) / (p2.x - p1.x)
            self.intercept = p1.y - self.slope * p1.x
            self.x_intercept = None

    def is_parallel(self, other: 'Line') -> bool:
        """
        Check if this line is parallel to another line.

        Lines are parallel if:
        - Both are vertical, OR
        - Both have the same slope
        """
        if not isinstance(other, Line):
            return False

        # Both vertical
        if self.vertical and other.vertical:
            return True

        # One vertical, one not
        if self.vertical or other.vertical:
            return False

        # Both non-vertical: compare slopes
        return _float_equal(self.slope, other.slope)

    def is_identical(self, other: 'Line') -> bool:
        """
        Check if this line is identical to another line.

        Lines are identical if:
        - Both are vertical with same x-intercept, OR
        - Both have same slope and intercept
        """
        if not isinstance(other, Line):
            return False

        # Must be parallel first
        if not self.is_parallel(other):
            return False

        # Both vertical: check x-intercept
        if self.vertical:
            return _float_equal(self.x_intercept, other.x_intercept)

        # Both non-vertical: check intercept
        return _float_equal(self.intercept, other.intercept)

    def intersect(self, other: 'Line') -> Optional[Point]:
        """
        Calculate intersection point with another line.

        Args:
            other: Another line

        Returns:
            Point at intersection, or None if lines are parallel/identical
        """
        return intersect(self, other)

    def __str__(self) -> str:
        """String representation for debugging."""
        if self.vertical:
            return f"Line(x={self.x_intercept})"
        else:
            return f"Line(y={self.slope}x+{self.intercept})"

    def __repr__(self) -> str:
        """Detailed string representation."""
        return self.__str__()


def intersect(line1: Line, line2: Line) -> Optional[Point]:
    """
    Calculate intersection point of two lines.

    REQ-PE957-003: Line intersection with vertical line support and epsilon precision.

    Args:
        line1: First line
        line2: Second line

    Returns:
        Point at intersection, or None if lines are parallel/identical
    """
    # Parallel or identical lines don't intersect
    if line1.is_parallel(line2):
        return None

    # Case 1: Both non-vertical
    if not line1.vertical and not line2.vertical:
        # Solve: m1*x + b1 = m2*x + b2
        # x = (b2 - b1) / (m1 - m2)
        x = (line2.intercept - line1.intercept) / (line1.slope - line2.slope)
        y = line1.slope * x + line1.intercept
        return Point(x, y)

    # Case 2: line1 is vertical, line2 is not
    if line1.vertical and not line2.vertical:
        x = line1.x_intercept
        y = line2.slope * x + line2.intercept
        return Point(x, y)

    # Case 3: line2 is vertical, line1 is not
    if line2.vertical and not line1.vertical:
        x = line2.x_intercept
        y = line1.slope * x + line1.intercept
        return Point(x, y)

    # Case 4: Both vertical (handled by parallel check above)
    return None
