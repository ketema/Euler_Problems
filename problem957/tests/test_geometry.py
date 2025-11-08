"""
Test suite for geometry primitives (Point, Line, intersection)

Phase 1 of Project Euler Problem 957 - Point Genesis
Tests written BLIND from requirements and TSR only.

Requirements: REQ-PE957-001, REQ-PE957-002, REQ-PE957-003
Coverage target: >85% line coverage, >80% branch coverage
Data isolation: QS5 compliant (ephemeral in-memory fixtures only)

All tests should FAIL initially (RED phase) until implementation provided.
"""

import pytest
import math


# ============================================================================
# FIXTURES (Ephemeral, in-memory only - QS5 compliance)
# ============================================================================

@pytest.fixture
def epsilon():
    """Epsilon tolerance for float equality per REQ-PE957-001"""
    return 1e-9


@pytest.fixture
def sample_points():
    """In-memory sample point coordinates for testing (ephemeral)"""
    return {
        'origin': (0.0, 0.0),
        'unit_x': (1.0, 0.0),
        'unit_y': (0.0, 1.0),
        'diagonal': (1.0, 1.0),
        'neg_diagonal': (-1.0, -1.0),
    }


@pytest.fixture
def sample_colored_points():
    """Sample point tuples with colors (ephemeral)"""
    return {
        'red1': (1.0, 0.0, 'red'),
        'red2': (-0.5, 0.866, 'red'),
        'blue1': (0.0, 0.0, 'blue'),
        'blue2': (2.0, 0.0, 'blue'),
        'white1': (0.5, 0.5, 'white'),
    }


@pytest.fixture
def sample_lines():
    """Sample line endpoints for testing (ephemeral)"""
    return {
        'horizontal': ((0.0, 0.0), (1.0, 0.0)),
        'vertical': ((0.0, 0.0), (0.0, 1.0)),
        'diagonal': ((0.0, 0.0), (1.0, 1.0)),
        'parallel_diagonal': ((0.0, 1.0), (1.0, 2.0)),
    }


# ============================================================================
# POINT CONSTRUCTION AND PROPERTIES (8 tests)
# ============================================================================

def test_point_construction_with_coordinates():
    """
    REQ-PE957-001: Point can be constructed with x, y coordinates

    Test: Create point with numeric coordinates
    """
    from src.geometry import Point

    p = Point(1.0, 2.0)

    assert hasattr(p, 'x'), (
        f"Test: test_point_construction_with_coordinates\n"
        f"Requirement: REQ-PE957-001 (Point representation)\n\n"
        f"Expected: Point object has 'x' attribute\n"
        f"Got: Point object missing 'x' attribute\n\n"
        f"Fix guidance: Implement Point.__init__ to accept x coordinate:\n"
        f"  def __init__(self, x: float, y: float, color: str = 'white'):\n"
        f"      self.x = x\n"
        f"      self.y = y\n"
        f"      self.color = color\n"
    )

    assert hasattr(p, 'y'), (
        f"Test: test_point_construction_with_coordinates\n"
        f"Requirement: REQ-PE957-001 (Point representation)\n\n"
        f"Expected: Point object has 'y' attribute\n"
        f"Got: Point object missing 'y' attribute\n\n"
        f"Fix guidance: See x attribute guidance above.\n"
    )

    assert p.x == 1.0, (
        f"Test: test_point_construction_with_coordinates\n"
        f"Requirement: REQ-PE957-001 (Point representation)\n\n"
        f"Expected: Point(1.0, 2.0).x → 1.0\n"
        f"Got: {p.x}\n\n"
        f"Fix guidance: Store x coordinate as-is: self.x = x\n"
    )

    assert p.y == 2.0, (
        f"Test: test_point_construction_with_coordinates\n"
        f"Requirement: REQ-PE957-001 (Point representation)\n\n"
        f"Expected: Point(1.0, 2.0).y → 2.0\n"
        f"Got: {p.y}\n\n"
        f"Fix guidance: Store y coordinate as-is: self.y = y\n"
    )


def test_point_construction_with_color():
    """
    REQ-PE957-001: Point has color attribute ('red', 'blue', or 'white')

    Test: Create point with explicit color
    """
    from src.geometry import Point

    p_red = Point(1.0, 2.0, 'red')
    p_blue = Point(3.0, 4.0, 'blue')
    p_white = Point(5.0, 6.0, 'white')

    assert p_red.color == 'red', (
        f"Test: test_point_construction_with_color\n"
        f"Requirement: REQ-PE957-001 (Point color attribute)\n\n"
        f"Expected: Point(1.0, 2.0, 'red').color → 'red'\n"
        f"Got: {p_red.color if hasattr(p_red, 'color') else 'missing color attribute'}\n\n"
        f"Fix guidance: Store color in __init__:\n"
        f"  self.color = color\n"
    )

    assert p_blue.color == 'blue', (
        f"Test: test_point_construction_with_color\n"
        f"Requirement: REQ-PE957-001 (Point color attribute)\n\n"
        f"Expected: Point(3.0, 4.0, 'blue').color → 'blue'\n"
        f"Got: {p_blue.color if hasattr(p_blue, 'color') else 'missing color attribute'}\n\n"
        f"Fix guidance: Ensure color parameter is accepted and stored.\n"
    )


def test_point_default_color_is_white():
    """
    REQ-PE957-001: Point defaults to 'white' color if not specified

    Test: Create point without specifying color
    """
    from src.geometry import Point

    p = Point(1.0, 2.0)

    assert p.color == 'white', (
        f"Test: test_point_default_color_is_white\n"
        f"Requirement: REQ-PE957-001 (Point default color)\n\n"
        f"Expected: Point(1.0, 2.0).color → 'white' (default)\n"
        f"Got: {p.color if hasattr(p, 'color') else 'missing color attribute'}\n\n"
        f"Fix guidance: Set default parameter in __init__:\n"
        f"  def __init__(self, x: float, y: float, color: str = 'white'):\n"
        f"      self.color = color\n"
    )


def test_point_supports_negative_coordinates():
    """
    REQ-PE957-001: Point supports negative coordinates

    Test: Points can exist in all quadrants
    """
    from src.geometry import Point

    p1 = Point(-1.0, -2.0)
    p2 = Point(-3.0, 4.0)
    p3 = Point(5.0, -6.0)

    assert p1.x == -1.0 and p1.y == -2.0, (
        f"Test: test_point_supports_negative_coordinates\n"
        f"Requirement: REQ-PE957-001 (Point coordinates)\n\n"
        f"Expected: Point(-1.0, -2.0) stores negative coordinates correctly\n"
        f"Got: x={p1.x}, y={p1.y}\n\n"
        f"Fix guidance: No special handling needed for negative values.\n"
        f"Store coordinates as-is: self.x = x, self.y = y\n"
    )


def test_point_supports_zero_coordinates():
    """
    REQ-PE957-001: Point supports zero coordinates (origin)

    Test: Point at origin (0, 0)
    """
    from src.geometry import Point

    p = Point(0.0, 0.0)

    assert p.x == 0.0 and p.y == 0.0, (
        f"Test: test_point_supports_zero_coordinates\n"
        f"Requirement: REQ-PE957-001 (Point coordinates)\n\n"
        f"Expected: Point(0.0, 0.0) represents origin\n"
        f"Got: x={p.x}, y={p.y}\n\n"
        f"Fix guidance: Handle zero values same as any other float.\n"
    )


def test_point_supports_large_coordinates():
    """
    REQ-PE957-001: Point supports large coordinate values

    Test: Points far from origin
    """
    from src.geometry import Point

    p = Point(1e10, -1e10)

    assert p.x == 1e10 and p.y == -1e10, (
        f"Test: test_point_supports_large_coordinates\n"
        f"Requirement: REQ-PE957-001 (Point coordinates)\n\n"
        f"Expected: Point can store large float values\n"
        f"Got: x={p.x}, y={p.y}\n\n"
        f"Fix guidance: Use float type for coordinates (no range limits).\n"
    )


def test_point_supports_fractional_coordinates():
    """
    REQ-PE957-001: Point supports fractional coordinates

    Test: Points with decimal values
    """
    from src.geometry import Point

    p = Point(0.123456789, 0.987654321)

    assert abs(p.x - 0.123456789) < 1e-15, (
        f"Test: test_point_supports_fractional_coordinates\n"
        f"Requirement: REQ-PE957-001 (Point coordinates)\n\n"
        f"Expected: Point(0.123456789, 0.987654321).x → 0.123456789\n"
        f"Got: {p.x}\n\n"
        f"Fix guidance: Use float type to preserve decimal precision.\n"
    )


def test_point_string_representation():
    """
    REQ-PE957-001: Point has useful string representation for debugging

    Test: str(Point) returns readable format
    """
    from src.geometry import Point

    p = Point(1.0, 2.0, 'red')
    s = str(p)

    assert '1.0' in s and '2.0' in s, (
        f"Test: test_point_string_representation\n"
        f"Requirement: REQ-PE957-001 (Point debugging)\n\n"
        f"Expected: str(Point(1.0, 2.0, 'red')) contains coordinates\n"
        f"Got: '{s}'\n\n"
        f"Fix guidance: Implement __str__ or __repr__:\n"
        f"  def __repr__(self):\n"
        f"      return f'Point({{self.x}}, {{self.y}}, {{self.color!r}})'\n"
    )


# ============================================================================
# POINT EQUALITY AND HASHING (6 tests)
# ============================================================================

def test_point_equality_within_epsilon(epsilon):
    """
    REQ-PE957-001: Point equality must use epsilon tolerance (1e-9)

    Test: Points with coordinates differing by less than epsilon are equal
    """
    from src.geometry import Point

    p1 = Point(0.0, 0.0)
    p2 = Point(1e-10, 0.0)  # Difference less than epsilon

    assert p1 == p2, (
        f"Test: test_point_equality_within_epsilon\n"
        f"Requirement: REQ-PE957-001 (Point epsilon equality)\n\n"
        f"Expected: Point(0.0, 0.0) == Point(1e-10, 0.0) → True\n"
        f"         (difference 1e-10 < epsilon {epsilon})\n"
        f"Got: False (points not equal)\n\n"
        f"Fix guidance: Implement __eq__ method for Point class.\n"
        f"Compare x and y coordinates using epsilon tolerance:\n"
        f"  EPSILON = 1e-9\n"
        f"  def __eq__(self, other):\n"
        f"      if not isinstance(other, Point):\n"
        f"          return False\n"
        f"      return (abs(self.x - other.x) < EPSILON and\n"
        f"              abs(self.y - other.y) < EPSILON)\n"
    )


def test_point_equality_at_epsilon_boundary(epsilon):
    """
    REQ-PE957-001: Point equality at exact epsilon threshold

    Test: Points exactly epsilon apart are equal (boundary case)
    """
    from src.geometry import Point

    p1 = Point(0.0, 0.0)
    p2 = Point(1e-9, 0.0)  # Exactly at epsilon

    assert p1 == p2, (
        f"Test: test_point_equality_at_epsilon_boundary\n"
        f"Requirement: REQ-PE957-001 (Point epsilon equality boundary)\n\n"
        f"Expected: Point(0.0, 0.0) == Point(1e-9, 0.0) → True\n"
        f"         (difference 1e-9 == epsilon, use < not <=)\n"
        f"Got: False\n\n"
        f"Fix guidance: Use strict inequality (abs(diff) < EPSILON).\n"
        f"For boundary: 1e-9 should be considered equal (just barely).\n"
    )


def test_point_inequality_beyond_epsilon(epsilon):
    """
    REQ-PE957-001: Point inequality when difference exceeds epsilon

    Test: Points differing by more than epsilon are not equal
    """
    from src.geometry import Point

    p1 = Point(0.0, 0.0)
    p2 = Point(1.1e-9, 0.0)  # Exceeds epsilon

    assert p1 != p2, (
        f"Test: test_point_inequality_beyond_epsilon\n"
        f"Requirement: REQ-PE957-001 (Point epsilon inequality)\n\n"
        f"Expected: Point(0.0, 0.0) != Point(1.1e-9, 0.0) → True\n"
        f"         (difference 1.1e-9 > epsilon {epsilon})\n"
        f"Got: Points are equal (should not be)\n\n"
        f"Fix guidance: Ensure epsilon comparison is strict:\n"
        f"  if abs(self.x - other.x) >= EPSILON: return False\n"
        f"  if abs(self.y - other.y) >= EPSILON: return False\n"
    )


def test_point_equality_both_coordinates_within_epsilon(epsilon):
    """
    REQ-PE957-001: Point equality requires BOTH coordinates within epsilon

    Test: Both x and y must satisfy epsilon tolerance
    """
    from src.geometry import Point

    p1 = Point(0.0, 0.0)
    p2 = Point(1e-10, 1e-10)  # Both within epsilon
    p3 = Point(1e-10, 1e-8)   # x within, y exceeds

    assert p1 == p2, (
        f"Test: test_point_equality_both_coordinates_within_epsilon\n"
        f"Requirement: REQ-PE957-001 (Point epsilon equality)\n\n"
        f"Expected: Point(0.0, 0.0) == Point(1e-10, 1e-10) → True\n"
        f"         (both x and y differences < epsilon)\n"
        f"Got: False\n\n"
        f"Fix guidance: Check both coordinates:\n"
        f"  return (abs(self.x - other.x) < EPSILON and\n"
        f"          abs(self.y - other.y) < EPSILON)\n"
    )

    assert p1 != p3, (
        f"Test: test_point_equality_both_coordinates_within_epsilon\n"
        f"Requirement: REQ-PE957-001 (Point epsilon equality)\n\n"
        f"Expected: Point(0.0, 0.0) != Point(1e-10, 1e-8) → True\n"
        f"         (y difference 1e-8 > epsilon)\n"
        f"Got: Points are equal (should not be)\n\n"
        f"Fix guidance: BOTH coordinates must be within epsilon (AND logic).\n"
    )


def test_point_hashable_for_sets():
    """
    REQ-PE957-001: Points must be hashable for set operations

    Test: Points can be added to sets (enables deduplication)
    """
    from src.geometry import Point

    p1 = Point(1.0, 2.0)
    p2 = Point(3.0, 4.0)
    p3 = Point(1.0, 2.0)  # Same as p1

    point_set = {p1, p2, p3}

    assert len(point_set) == 2, (
        f"Test: test_point_hashable_for_sets\n"
        f"Requirement: REQ-PE957-001 (Point hashability)\n\n"
        f"Expected: Set deduplicates equal points\n"
        f"         {{Point(1,2), Point(3,4), Point(1,2)}} → 2 unique points\n"
        f"Got: {len(point_set)} points in set\n\n"
        f"Fix guidance: Implement __hash__ method for Point class.\n"
        f"Points equal by __eq__ must have same hash:\n"
        f"  def __hash__(self):\n"
        f"      # Round to epsilon precision for consistent hashing\n"
        f"      return hash((round(self.x / EPSILON) * EPSILON,\n"
        f"                   round(self.y / EPSILON) * EPSILON))\n"
    )


def test_point_hash_consistency_with_equality():
    """
    REQ-PE957-001: Points equal by epsilon must hash to same value

    Test: Hash consistency with __eq__ (Python requirement)
    """
    from src.geometry import Point

    p1 = Point(0.0, 0.0)
    p2 = Point(1e-10, 0.0)  # Equal by epsilon

    assert hash(p1) == hash(p2), (
        f"Test: test_point_hash_consistency_with_equality\n"
        f"Requirement: REQ-PE957-001 (Point hash consistency)\n\n"
        f"Expected: If p1 == p2, then hash(p1) == hash(p2)\n"
        f"         Point(0.0, 0.0) == Point(1e-10, 0.0) → True\n"
        f"         So hash(p1) must equal hash(p2)\n"
        f"Got: hash({p1.x}, {p1.y}) != hash({p2.x}, {p2.y})\n\n"
        f"Fix guidance: Round coordinates to epsilon precision before hashing.\n"
        f"This ensures points within epsilon have same hash:\n"
        f"  def __hash__(self):\n"
        f"      x_rounded = round(self.x / EPSILON) * EPSILON\n"
        f"      y_rounded = round(self.y / EPSILON) * EPSILON\n"
        f"      return hash((x_rounded, y_rounded))\n"
    )


# ============================================================================
# LINE CONSTRUCTION AND PROPERTIES (7 tests)
# ============================================================================

def test_line_construction_from_two_points():
    """
    REQ-PE957-002: Line constructed from two distinct points

    Test: Create line from two Point objects
    """
    from src.geometry import Point, Line

    p1 = Point(0.0, 0.0)
    p2 = Point(1.0, 1.0)
    line = Line(p1, p2)

    assert hasattr(line, 'p1') and hasattr(line, 'p2'), (
        f"Test: test_line_construction_from_two_points\n"
        f"Requirement: REQ-PE957-002 (Line representation)\n\n"
        f"Expected: Line object stores two points (p1, p2)\n"
        f"Got: Line missing point attributes\n\n"
        f"Fix guidance: Implement Line.__init__ to store points:\n"
        f"  def __init__(self, p1: Point, p2: Point):\n"
        f"      if p1 == p2:\n"
        f"          raise ValueError(...)\n"
        f"      self.p1 = p1\n"
        f"      self.p2 = p2\n"
    )


def test_line_handles_vertical_line():
    """
    REQ-PE957-002: Line handles vertical lines (undefined slope)

    Test: Create vertical line (x=constant)
    """
    from src.geometry import Point, Line

    p1 = Point(0.0, 0.0)
    p2 = Point(0.0, 1.0)
    line = Line(p1, p2)

    # Line should exist and handle vertical case
    assert line is not None, (
        f"Test: test_line_handles_vertical_line\n"
        f"Requirement: REQ-PE957-002 (Vertical line handling)\n\n"
        f"Expected: Line(Point(0,0), Point(0,1)) creates vertical line (x=0)\n"
        f"Got: Line construction failed\n\n"
        f"Fix guidance: Detect vertical lines in __init__:\n"
        f"  if abs(p2.x - p1.x) < EPSILON:\n"
        f"      self.is_vertical = True\n"
        f"      self.x = p1.x  # Constant x value\n"
        f"  else:\n"
        f"      self.is_vertical = False\n"
        f"      self.slope = (p2.y - p1.y) / (p2.x - p1.x)\n"
        f"      self.intercept = p1.y - self.slope * p1.x\n"
    )


def test_line_handles_horizontal_line():
    """
    REQ-PE957-002: Line handles horizontal lines (slope = 0)

    Test: Create horizontal line (y=constant)
    """
    from src.geometry import Point, Line

    p1 = Point(0.0, 0.0)
    p2 = Point(1.0, 0.0)
    line = Line(p1, p2)

    assert line is not None, (
        f"Test: test_line_handles_horizontal_line\n"
        f"Requirement: REQ-PE957-002 (Horizontal line handling)\n\n"
        f"Expected: Line(Point(0,0), Point(1,0)) creates horizontal line (y=0)\n"
        f"         Slope = 0\n"
        f"Got: Line construction failed\n\n"
        f"Fix guidance: Handle zero slope case:\n"
        f"  slope = (p2.y - p1.y) / (p2.x - p1.x) = 0.0 / 1.0 = 0.0\n"
        f"No special handling needed (slope=0 is valid).\n"
    )


def test_line_parallel_detection_same_slope():
    """
    REQ-PE957-002: Lines with same slope are parallel

    Test: Detect parallel lines (y=x and y=x+1)
    """
    from src.geometry import Point, Line

    line1 = Line(Point(0.0, 0.0), Point(1.0, 1.0))  # y = x
    line2 = Line(Point(0.0, 1.0), Point(1.0, 2.0))  # y = x + 1

    assert line1.is_parallel(line2), (
        f"Test: test_line_parallel_detection_same_slope\n"
        f"Requirement: REQ-PE957-002 (Parallel line detection)\n\n"
        f"Expected: Line(y=x).is_parallel(Line(y=x+1)) → True\n"
        f"         (both have slope = 1)\n"
        f"Got: False\n\n"
        f"Fix guidance: Implement is_parallel method:\n"
        f"  def is_parallel(self, other: Line) -> bool:\n"
        f"      # Both vertical → parallel\n"
        f"      if self.is_vertical and other.is_vertical:\n"
        f"          return True\n"
        f"      # One vertical, one not → not parallel\n"
        f"      if self.is_vertical or other.is_vertical:\n"
        f"          return False\n"
        f"      # Compare slopes with epsilon\n"
        f"      return abs(self.slope - other.slope) < EPSILON\n"
    )


def test_line_parallel_detection_vertical_lines():
    """
    REQ-PE957-002: Vertical lines are parallel to each other

    Test: Two vertical lines are parallel
    """
    from src.geometry import Point, Line

    line1 = Line(Point(0.0, 0.0), Point(0.0, 1.0))  # x = 0
    line2 = Line(Point(1.0, 0.0), Point(1.0, 1.0))  # x = 1

    assert line1.is_parallel(line2), (
        f"Test: test_line_parallel_detection_vertical_lines\n"
        f"Requirement: REQ-PE957-002 (Vertical line parallel detection)\n\n"
        f"Expected: Line(x=0).is_parallel(Line(x=1)) → True\n"
        f"         (both vertical)\n"
        f"Got: False\n\n"
        f"Fix guidance: Check both lines for is_vertical flag:\n"
        f"  if self.is_vertical and other.is_vertical:\n"
        f"      return True\n"
    )


def test_line_identical_detection():
    """
    REQ-PE957-002: Lines passing through same points are identical

    Test: Detect identical lines (y=x defined by different point pairs)
    """
    from src.geometry import Point, Line

    line1 = Line(Point(0.0, 0.0), Point(1.0, 1.0))  # y = x
    line2 = Line(Point(2.0, 2.0), Point(3.0, 3.0))  # y = x (different points)

    assert line1.is_identical(line2), (
        f"Test: test_line_identical_detection\n"
        f"Requirement: REQ-PE957-002 (Identical line detection)\n\n"
        f"Expected: Line((0,0), (1,1)).is_identical(Line((2,2), (3,3))) → True\n"
        f"         (both are y=x, same line)\n"
        f"Got: False\n\n"
        f"Fix guidance: Implement is_identical method:\n"
        f"  def is_identical(self, other: Line) -> bool:\n"
        f"      # Must be parallel first\n"
        f"      if not self.is_parallel(other):\n"
        f"          return False\n"
        f"      # Check if other's point lies on self's line\n"
        f"      # For vertical: same x coordinate\n"
        f"      if self.is_vertical:\n"
        f"          return abs(self.x - other.x) < EPSILON\n"
        f"      # For non-vertical: same intercept\n"
        f"      return abs(self.intercept - other.intercept) < EPSILON\n"
    )


def test_line_not_identical_if_parallel_different_intercept():
    """
    REQ-PE957-002: Parallel lines with different intercepts are not identical

    Test: Parallel but distinct lines
    """
    from src.geometry import Point, Line

    line1 = Line(Point(0.0, 0.0), Point(1.0, 1.0))  # y = x
    line2 = Line(Point(0.0, 1.0), Point(1.0, 2.0))  # y = x + 1

    assert not line1.is_identical(line2), (
        f"Test: test_line_not_identical_if_parallel_different_intercept\n"
        f"Requirement: REQ-PE957-002 (Identical line detection)\n\n"
        f"Expected: Line(y=x).is_identical(Line(y=x+1)) → False\n"
        f"         (parallel but different intercepts)\n"
        f"Got: True (should be False)\n\n"
        f"Fix guidance: Check intercept after confirming parallel:\n"
        f"  if not self.is_parallel(other):\n"
        f"      return False\n"
        f"  return abs(self.intercept - other.intercept) < EPSILON\n"
    )


# ============================================================================
# LINE INTERSECTION ALGORITHM (7 tests)
# ============================================================================

def test_line_intersection_perpendicular_lines():
    """
    REQ-PE957-003: Line intersection for perpendicular lines

    Test: Intersection of x-axis and y-axis at origin
    """
    from src.geometry import Point, Line

    line1 = Line(Point(0.0, 0.0), Point(1.0, 0.0))  # y = 0 (horizontal)
    line2 = Line(Point(0.0, 0.0), Point(0.0, 1.0))  # x = 0 (vertical)

    intersection = line1.intersect(line2)

    assert intersection is not None, (
        f"Test: test_line_intersection_perpendicular_lines\n"
        f"Requirement: REQ-PE957-003 (Line intersection)\n\n"
        f"Expected: Line(y=0).intersect(Line(x=0)) → Point(0, 0)\n"
        f"Got: None\n\n"
        f"Fix guidance: Implement intersect method:\n"
        f"  def intersect(self, other: Line) -> Point | None:\n"
        f"      # Parallel or identical → no single intersection\n"
        f"      if self.is_parallel(other):\n"
        f"          return None\n"
        f"      # One vertical, one not\n"
        f"      if self.is_vertical:\n"
        f"          x = self.x\n"
        f"          y = other.slope * x + other.intercept\n"
        f"          return Point(x, y)\n"
        f"      if other.is_vertical:\n"
        f"          x = other.x\n"
        f"          y = self.slope * x + self.intercept\n"
        f"          return Point(x, y)\n"
        f"      # Both non-vertical: solve y = m1*x + b1 = m2*x + b2\n"
        f"      x = (other.intercept - self.intercept) / (self.slope - other.slope)\n"
        f"      y = self.slope * x + self.intercept\n"
        f"      return Point(x, y)\n"
    )

    assert intersection == Point(0.0, 0.0), (
        f"Test: test_line_intersection_perpendicular_lines\n"
        f"Requirement: REQ-PE957-003 (Line intersection)\n\n"
        f"Expected: Intersection at Point(0, 0)\n"
        f"Got: {intersection}\n\n"
        f"Fix guidance: See algorithm above. Verify calculation.\n"
    )


def test_line_intersection_diagonal_lines():
    """
    REQ-PE957-003: Line intersection for diagonal lines

    Test: Intersection of y=x and y=-x+1 at (0.5, 0.5)
    """
    from src.geometry import Point, Line

    line1 = Line(Point(0.0, 0.0), Point(1.0, 1.0))  # y = x
    line2 = Line(Point(0.0, 1.0), Point(1.0, 0.0))  # y = -x + 1

    intersection = line1.intersect(line2)

    assert intersection is not None, (
        f"Test: test_line_intersection_diagonal_lines\n"
        f"Requirement: REQ-PE957-003 (Line intersection)\n\n"
        f"Expected: Line(y=x).intersect(Line(y=-x+1)) → Point(0.5, 0.5)\n"
        f"Got: None\n\n"
        f"Fix guidance: Use slope-intercept algebra:\n"
        f"  line1: y = x (slope=1, intercept=0)\n"
        f"  line2: y = -x + 1 (slope=-1, intercept=1)\n"
        f"  Set equal: x = -x + 1 → 2x = 1 → x = 0.5\n"
        f"  y = 0.5\n"
    )

    expected = Point(0.5, 0.5)
    assert intersection == expected, (
        f"Test: test_line_intersection_diagonal_lines\n"
        f"Requirement: REQ-PE957-003 (Line intersection)\n\n"
        f"Expected: Intersection at Point(0.5, 0.5)\n"
        f"Got: {intersection}\n\n"
        f"Fix guidance: Check algebra for non-vertical lines:\n"
        f"  x = (b2 - b1) / (m1 - m2)\n"
        f"  y = m1 * x + b1\n"
    )


def test_line_intersection_parallel_returns_none():
    """
    REQ-PE957-003: Parallel lines have no intersection

    Test: Return None for parallel lines
    """
    from src.geometry import Point, Line

    line1 = Line(Point(0.0, 0.0), Point(1.0, 1.0))  # y = x
    line2 = Line(Point(0.0, 1.0), Point(1.0, 2.0))  # y = x + 1

    intersection = line1.intersect(line2)

    assert intersection is None, (
        f"Test: test_line_intersection_parallel_returns_none\n"
        f"Requirement: REQ-PE957-003 (Parallel line intersection)\n\n"
        f"Expected: Line(y=x).intersect(Line(y=x+1)) → None\n"
        f"         (parallel lines never intersect)\n"
        f"Got: {intersection}\n\n"
        f"Fix guidance: Check for parallel before calculating intersection:\n"
        f"  if self.is_parallel(other):\n"
        f"      return None\n"
    )


def test_line_intersection_identical_returns_none():
    """
    REQ-PE957-003: Identical lines have infinite intersections (return None)

    Test: Return None for identical lines
    """
    from src.geometry import Point, Line

    line1 = Line(Point(0.0, 0.0), Point(1.0, 1.0))  # y = x
    line2 = Line(Point(2.0, 2.0), Point(3.0, 3.0))  # y = x (same line)

    intersection = line1.intersect(line2)

    assert intersection is None, (
        f"Test: test_line_intersection_identical_returns_none\n"
        f"Requirement: REQ-PE957-003 (Identical line intersection)\n\n"
        f"Expected: Line(y=x).intersect(Line(y=x)) → None\n"
        f"         (identical lines have infinite intersections, no single point)\n"
        f"Got: {intersection}\n\n"
        f"Fix guidance: Identical lines are parallel, so is_parallel check handles this:\n"
        f"  if self.is_parallel(other):\n"
        f"      return None  # Covers both parallel AND identical\n"
    )


def test_line_intersection_vertical_line_with_nonvertical():
    """
    REQ-PE957-003: Intersection of vertical and non-vertical line

    Test: Vertical line x=2 intersects y=x at (2, 2)
    """
    from src.geometry import Point, Line

    line1 = Line(Point(2.0, 0.0), Point(2.0, 1.0))  # x = 2 (vertical)
    line2 = Line(Point(0.0, 0.0), Point(1.0, 1.0))  # y = x

    intersection = line1.intersect(line2)

    assert intersection is not None, (
        f"Test: test_line_intersection_vertical_line_with_nonvertical\n"
        f"Requirement: REQ-PE957-003 (Vertical line intersection)\n\n"
        f"Expected: Line(x=2).intersect(Line(y=x)) → Point(2, 2)\n"
        f"Got: None\n\n"
        f"Fix guidance: Handle vertical line case:\n"
        f"  if self.is_vertical:\n"
        f"      x = self.x\n"
        f"      y = other.slope * x + other.intercept\n"
        f"      return Point(x, y)\n"
    )

    expected = Point(2.0, 2.0)
    assert intersection == expected, (
        f"Test: test_line_intersection_vertical_line_with_nonvertical\n"
        f"Requirement: REQ-PE957-003 (Vertical line intersection)\n\n"
        f"Expected: Intersection at Point(2, 2)\n"
        f"Got: {intersection}\n\n"
        f"Fix guidance: For vertical line x=2 and y=x:\n"
        f"  x = 2, y = 1*2 + 0 = 2\n"
    )


def test_line_intersection_two_vertical_lines_parallel():
    """
    REQ-PE957-003: Two vertical lines are parallel (no intersection)

    Test: Vertical lines x=0 and x=1 never intersect
    """
    from src.geometry import Point, Line

    line1 = Line(Point(0.0, 0.0), Point(0.0, 1.0))  # x = 0
    line2 = Line(Point(1.0, 0.0), Point(1.0, 1.0))  # x = 1

    intersection = line1.intersect(line2)

    assert intersection is None, (
        f"Test: test_line_intersection_two_vertical_lines_parallel\n"
        f"Requirement: REQ-PE957-003 (Vertical line intersection)\n\n"
        f"Expected: Line(x=0).intersect(Line(x=1)) → None\n"
        f"         (both vertical → parallel → no intersection)\n"
        f"Got: {intersection}\n\n"
        f"Fix guidance: is_parallel should catch this:\n"
        f"  if self.is_vertical and other.is_vertical:\n"
        f"      return True  # Parallel\n"
        f"Then intersect returns None for parallel lines.\n"
    )


def test_line_intersection_with_epsilon_precision():
    """
    REQ-PE957-003: Intersection handles floating-point precision

    Test: Intersection coordinates respect epsilon tolerance
    """
    from src.geometry import Point, Line

    # Create lines that intersect at a point requiring float precision
    line1 = Line(Point(0.0, 0.0), Point(1.0, 1.0/3.0))  # y = (1/3)x
    line2 = Line(Point(0.0, 1.0), Point(1.0, 0.0))      # y = -x + 1

    intersection = line1.intersect(line2)

    assert intersection is not None, (
        f"Test: test_line_intersection_with_epsilon_precision\n"
        f"Requirement: REQ-PE957-003 (Float precision in intersection)\n\n"
        f"Expected: Intersection point exists\n"
        f"Got: None\n\n"
        f"Fix guidance: Handle floating-point arithmetic correctly.\n"
        f"Use Point constructor which handles epsilon internally.\n"
    )

    # Verify intersection is reasonable (don't hard-code exact value due to float precision)
    assert 0.0 <= intersection.x <= 1.0, (
        f"Test: test_line_intersection_with_epsilon_precision\n"
        f"Requirement: REQ-PE957-003 (Float precision in intersection)\n\n"
        f"Expected: Intersection x-coordinate in range [0, 1]\n"
        f"Got: x = {intersection.x}\n\n"
        f"Fix guidance: Verify intersection calculation algebra.\n"
    )


# ============================================================================
# ERROR HANDLING (4 tests)
# ============================================================================

def test_point_raises_typeerror_invalid_coordinates():
    """
    REQ-PE957-001: Point validates coordinate types

    Test: Raise TypeError for non-numeric coordinates
    """
    from src.geometry import Point

    with pytest.raises(TypeError) as exc_info:
        Point("a", 2.0)

    error_msg = str(exc_info.value)
    assert "numeric" in error_msg.lower() or "float" in error_msg.lower(), (
        f"Test: test_point_raises_typeerror_invalid_coordinates\n"
        f"Requirement: REQ-PE957-001 (Type safety)\n\n"
        f"Expected: TypeError with message mentioning 'numeric' or 'float'\n"
        f"Got: '{error_msg}'\n\n"
        f"Fix guidance: Validate coordinate types in __init__:\n"
        f"  def __init__(self, x: float, y: float, color: str = 'white'):\n"
        f"      if not isinstance(x, (int, float)):\n"
        f"          raise TypeError(\n"
        f"              f'Point coordinates must be numeric (float/int). '\n"
        f"              f'Got type {{type(x)}} for x coordinate.'\n"
        f"          )\n"
        f"      if not isinstance(y, (int, float)):\n"
        f"          raise TypeError(\n"
        f"              f'Point coordinates must be numeric (float/int). '\n"
        f"              f'Got type {{type(y)}} for y coordinate.'\n"
        f"          )\n"
    )


def test_point_raises_valueerror_invalid_color():
    """
    REQ-PE957-001: Point validates color attribute

    Test: Raise ValueError for invalid color string
    """
    from src.geometry import Point

    with pytest.raises(ValueError) as exc_info:
        Point(1.0, 2.0, 'green')

    error_msg = str(exc_info.value)
    assert "red" in error_msg and "blue" in error_msg and "white" in error_msg, (
        f"Test: test_point_raises_valueerror_invalid_color\n"
        f"Requirement: REQ-PE957-001 (Color validation)\n\n"
        f"Expected: ValueError listing valid colors ('red', 'blue', 'white')\n"
        f"Got: '{error_msg}'\n\n"
        f"Fix guidance: Validate color in __init__:\n"
        f"  VALID_COLORS = {{'red', 'blue', 'white'}}\n"
        f"  if color not in VALID_COLORS:\n"
        f"      raise ValueError(\n"
        f"          f\"Point color must be 'red', 'blue', or 'white'. \"\n"
        f"          f\"Got '{{color}}'.\"\n"
        f"      )\n"
    )


def test_line_raises_valueerror_identical_points():
    """
    REQ-PE957-002: Line validates two distinct points

    Test: Raise ValueError when constructing line from identical points
    """
    from src.geometry import Point, Line

    p = Point(0.0, 0.0)

    with pytest.raises(ValueError) as exc_info:
        Line(p, p)

    error_msg = str(exc_info.value)
    assert "distinct" in error_msg.lower() or "identical" in error_msg.lower(), (
        f"Test: test_line_raises_valueerror_identical_points\n"
        f"Requirement: REQ-PE957-002 (Line validation)\n\n"
        f"Expected: ValueError mentioning 'distinct' or 'identical' points\n"
        f"Got: '{error_msg}'\n\n"
        f"Fix guidance: Validate points in Line.__init__:\n"
        f"  def __init__(self, p1: Point, p2: Point):\n"
        f"      if p1 == p2:\n"
        f"          raise ValueError(\n"
        f"              f'Line requires two distinct points. '\n"
        f"              f'Got identical points at ({{p1.x}}, {{p1.y}}).'\n"
        f"          )\n"
    )


def test_line_raises_valueerror_points_within_epsilon():
    """
    REQ-PE957-002: Line validates points are epsilon-distinct

    Test: Raise ValueError when points are within epsilon (too close)
    """
    from src.geometry import Point, Line

    p1 = Point(0.0, 0.0)
    p2 = Point(1e-10, 0.0)  # Within epsilon, considered equal

    with pytest.raises(ValueError) as exc_info:
        Line(p1, p2)

    error_msg = str(exc_info.value)
    assert "epsilon" in error_msg.lower() or "distinct" in error_msg.lower(), (
        f"Test: test_line_raises_valueerror_points_within_epsilon\n"
        f"Requirement: REQ-PE957-002 (Line validation with epsilon)\n\n"
        f"Expected: ValueError mentioning 'epsilon' or points not 'distinct'\n"
        f"Got: '{error_msg}'\n\n"
        f"Fix guidance: Points within epsilon are considered equal (p1 == p2).\n"
        f"The validation if p1 == p2 should catch this case:\n"
        f"  if p1 == p2:  # Uses epsilon equality\n"
        f"      raise ValueError(\n"
        f"          f'Line requires two distinct points. '\n"
        f"          f'Points ({{p1.x}}, {{p1.y}}) and ({{p2.x}}, {{p2.y}}) '\n"
        f"          f'are within epsilon tolerance.'\n"
        f"      )\n"
    )
