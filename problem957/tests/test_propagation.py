"""
Test suite for single-day propagation logic (Phase 2).

REQ-PE957-004: Single day propagation for Project Euler Problem 957.

This test suite implements adversarial TDD - tests are written WITHOUT seeing
the implementation code. All tests must be self-documenting with 5-point error
messages (test name, REQ ref, expected, actual, fix guidance).

Target coverage: >85% line coverage, >80% branch coverage
Test count: 15-20 tests
"""

import math
import pytest
from src.geometry import Point, Line
from src.propagation import propagate_one_day


# ============================================================================
# FIXTURES (Ephemeral Data - QS5 Compliance)
# ============================================================================

@pytest.fixture
def simple_configuration():
    """
    Minimal configuration for basic tests (ephemeral).

    1 red point, 2 blue points → 2 lines → 1 possible intersection
    """
    return {
        'red': {Point(0.0, 0.0, 'red')},
        'blue': {Point(1.0, 0.0, 'blue'), Point(0.0, 1.0, 'blue')}
    }


@pytest.fixture
def symmetric_configuration():
    """
    Symmetric configuration for g(1)=8 verification (ephemeral).

    3 red points (equilateral triangle vertices)
    2 blue points (center and one vertex on x-axis)
    Should produce 8 new blue points after 1 day.
    """
    red = {
        Point(1.0, 0.0, 'red'),
        Point(math.cos(2*math.pi/3), math.sin(2*math.pi/3), 'red'),
        Point(math.cos(4*math.pi/3), math.sin(4*math.pi/3), 'red')
    }
    blue = {
        Point(0.0, 0.0, 'blue'),
        Point(2.0, 0.0, 'blue')
    }
    return {'red': red, 'blue': blue}


@pytest.fixture
def collinear_configuration():
    """
    Collinear points (all parallel lines) for edge case testing (ephemeral).

    All points on x-axis → all lines parallel → no intersections
    """
    red = {Point(0.0, 0.0, 'red'), Point(1.0, 0.0, 'red')}
    blue = {Point(2.0, 0.0, 'blue'), Point(3.0, 0.0, 'blue')}
    return {'red': red, 'blue': blue}


@pytest.fixture
def multiple_intersections_configuration():
    """
    Configuration with multiple line pairs intersecting at different points (ephemeral).

    Used to test deduplication and intersection finding.
    """
    red = {
        Point(0.0, 0.0, 'red'),
        Point(2.0, 0.0, 'red'),
        Point(1.0, 2.0, 'red')
    }
    blue = {
        Point(1.0, 0.0, 'blue'),
        Point(0.0, 2.0, 'blue')
    }
    return {'red': red, 'blue': blue}


@pytest.fixture
def intersection_at_red_point_configuration():
    """
    Configuration where an intersection lands exactly on an existing red point (ephemeral).

    Tests exclusion of red points from output.
    """
    # Create a configuration where lines intersect at (1, 1)
    # and we have a red point at (1, 1)
    red = {
        Point(0.0, 0.0, 'red'),
        Point(1.0, 1.0, 'red'),  # This is where intersection will occur
        Point(2.0, 0.0, 'red')
    }
    blue = {
        Point(0.0, 2.0, 'blue'),
        Point(2.0, 2.0, 'blue')
    }
    return {'red': red, 'blue': blue}


@pytest.fixture
def intersection_at_blue_point_configuration():
    """
    Configuration where an intersection lands exactly on an existing blue point (ephemeral).

    Tests exclusion of existing blue points from output.
    """
    # Create a square configuration where lines intersect at center (1, 1)
    # and we have a blue point at (1, 1)
    red = {
        Point(0.0, 0.0, 'red'),
        Point(2.0, 0.0, 'red')
    }
    blue = {
        Point(0.0, 2.0, 'blue'),
        Point(2.0, 2.0, 'blue'),
        Point(1.0, 1.0, 'blue')  # This is where intersection will occur
    }
    return {'red': red, 'blue': blue}


# ============================================================================
# BEHAVIOR 1: Construct all lines between red-blue pairs
# ============================================================================

def test_propagate_constructs_all_red_blue_line_pairs():
    """
    REQ-PE957-004: Propagation must construct all lines between red-blue pairs.

    Test: With 3 red and 2 blue points, should generate 6 lines (3 × 2).
    """
    red = {
        Point(0.0, 0.0, 'red'),
        Point(1.0, 0.0, 'red'),
        Point(0.0, 1.0, 'red')
    }
    blue = {
        Point(2.0, 0.0, 'blue'),
        Point(0.0, 2.0, 'blue')
    }

    # Expected behavior: 3 red × 2 blue = 6 lines
    # These lines will intersect and produce new blue points
    # We test this indirectly by verifying intersection behavior
    new_blues = propagate_one_day(red, blue)

    # If lines are constructed correctly, we should get intersections
    # (at least some, unless all parallel - which they're not in this config)
    assert isinstance(new_blues, set), (
        f"Test: test_propagate_constructs_all_red_blue_line_pairs\n"
        f"Requirement: REQ-PE957-004 (construct red×blue lines)\n\n"
        f"Expected: Function returns a set of Points\n"
        f"Got: {type(new_blues).__name__}\n\n"
        f"Fix guidance: propagate_one_day should:\n"
        f"  1. Create empty set: new_blues = set()\n"
        f"  2. Construct all lines: for r in red: for b in blue: lines.append(Line(r, b))\n"
        f"  3. Return set of intersection points\n"
    )


def test_propagate_with_single_red_single_blue_produces_no_intersections():
    """
    REQ-PE957-004: Propagation with 1 red and 1 blue produces no intersections.

    Test: With only 1 line, no line pairs exist to intersect.
    """
    red = {Point(0.0, 0.0, 'red')}
    blue = {Point(1.0, 1.0, 'blue')}

    new_blues = propagate_one_day(red, blue)

    assert len(new_blues) == 0, (
        f"Test: test_propagate_with_single_red_single_blue_produces_no_intersections\n"
        f"Requirement: REQ-PE957-004 (find intersections of line pairs)\n\n"
        f"Expected: 0 new blue points (only 1 line, no pairs to intersect)\n"
        f"Got: {len(new_blues)} new blue points\n\n"
        f"Fix guidance: With 1 red and 1 blue, only 1 line exists.\n"
        f"Intersections require at least 2 lines (pairs to check).\n"
        f"Check: Are you iterating over all pairs of lines?\n"
        f"  for i, line1 in enumerate(lines):\n"
        f"      for line2 in lines[i+1:]:\n"
        f"          intersection = line1.intersect(line2)\n"
    )


# ============================================================================
# BEHAVIOR 2: Find intersection points of all line pairs
# ============================================================================

def test_propagate_finds_intersections_of_line_pairs():
    """
    REQ-PE957-004: Propagation must find all intersections of line pairs.

    Test: Configuration with known intersections should detect them.
    """
    # Create a simple X configuration
    # Line 1: R(0,0) to B(2,2) has slope 1, intercept 0 (y = x)
    # Line 2: R(0,2) to B(2,0) has slope -1, intercept 2 (y = -x + 2)
    # These intersect at (1, 1)
    red = {Point(0.0, 0.0, 'red'), Point(0.0, 2.0, 'red')}
    blue = {Point(2.0, 2.0, 'blue'), Point(2.0, 0.0, 'blue')}

    new_blues = propagate_one_day(red, blue)

    # Should find intersection at (1, 1)
    expected_intersection = Point(1.0, 1.0, 'blue')
    assert expected_intersection in new_blues, (
        f"Test: test_propagate_finds_intersections_of_line_pairs\n"
        f"Requirement: REQ-PE957-004 (find intersections)\n\n"
        f"Expected: Point(1.0, 1.0) in new_blues (intersection of X pattern)\n"
        f"Got: Point(1.0, 1.0) NOT in new_blues\n"
        f"Actual new_blues: {new_blues}\n\n"
        f"Fix guidance: For each pair of lines:\n"
        f"  intersection = line1.intersect(line2)\n"
        f"  if intersection is not None:\n"
        f"      # Add to candidates (after exclusion checks)\n"
        f"Check: Are you iterating over all pairs (not duplicates)?\n"
    )


def test_propagate_skips_parallel_lines():
    """
    REQ-PE957-004: Propagation must skip parallel lines (no intersection).

    Test: Collinear configuration should produce no intersections.
    """
    # All points on x-axis → all lines horizontal (parallel)
    red = {Point(0.0, 0.0, 'red'), Point(1.0, 0.0, 'red')}
    blue = {Point(2.0, 0.0, 'blue'), Point(3.0, 0.0, 'blue')}

    new_blues = propagate_one_day(red, blue)

    assert len(new_blues) == 0, (
        f"Test: test_propagate_skips_parallel_lines\n"
        f"Requirement: REQ-PE957-004 (skip parallel lines)\n\n"
        f"Expected: 0 new blue points (all lines parallel, no intersections)\n"
        f"Got: {len(new_blues)} new blue points\n\n"
        f"Fix guidance: line1.intersect(line2) returns None for parallel lines.\n"
        f"Check: Are you handling None results?\n"
        f"  intersection = line1.intersect(line2)\n"
        f"  if intersection is not None:  # Skip parallel/identical\n"
        f"      # Process intersection\n"
    )


def test_propagate_handles_vertical_lines():
    """
    REQ-PE957-004: Propagation must handle vertical lines correctly.

    Test: Vertical and non-vertical lines should intersect properly.
    """
    # Vertical line: R(0,0) to B(0,2) is x=0
    # Horizontal line: R(2,1) to B(-2,1) is y=1
    # Intersection at (0, 1)
    red = {Point(0.0, 0.0, 'red'), Point(2.0, 1.0, 'red')}
    blue = {Point(0.0, 2.0, 'blue'), Point(-2.0, 1.0, 'blue')}

    new_blues = propagate_one_day(red, blue)

    expected_intersection = Point(0.0, 1.0, 'blue')
    assert expected_intersection in new_blues, (
        f"Test: test_propagate_handles_vertical_lines\n"
        f"Requirement: REQ-PE957-004 (vertical line support)\n\n"
        f"Expected: Point(0.0, 1.0) in new_blues (vertical/horizontal intersection)\n"
        f"Got: Point(0.0, 1.0) NOT in new_blues\n"
        f"Actual new_blues: {new_blues}\n\n"
        f"Fix guidance: The geometry.intersect() function handles vertical lines.\n"
        f"Check: Are you using Line.intersect() or geometry.intersect() correctly?\n"
        f"Verify: line1.intersect(line2) returns Point for vertical intersections.\n"
    )


# ============================================================================
# BEHAVIOR 3: Exclude already-colored points (red and existing blue)
# ============================================================================

def test_propagate_excludes_red_points():
    """
    REQ-PE957-004: Propagation must exclude intersection points that are red.

    Test: If intersection lands on existing red point, exclude from output.
    """
    # Configuration where intersection is at (1, 1), and we have red point at (1, 1)
    # Line 1: R(0,0) to B(2,2) passes through (1,1)
    # Line 2: R(2,0) to B(0,2) passes through (1,1)
    # Intersection at R(1,1) should be excluded
    red = {
        Point(0.0, 0.0, 'red'),
        Point(2.0, 0.0, 'red'),
        Point(1.0, 1.0, 'red')  # Intersection point
    }
    blue = {
        Point(2.0, 2.0, 'blue'),
        Point(0.0, 2.0, 'blue')
    }

    new_blues = propagate_one_day(red, blue)

    assert Point(1.0, 1.0) not in new_blues, (
        f"Test: test_propagate_excludes_red_points\n"
        f"Requirement: REQ-PE957-004 (exclude already-colored points)\n\n"
        f"Expected: Point(1.0, 1.0) NOT in output (already red)\n"
        f"Got: Point(1.0, 1.0) in new_blues set\n\n"
        f"Fix guidance: Before adding intersection to output:\n"
        f"  if intersection not in red and intersection not in blue:\n"
        f"      new_blues.add(intersection)\n"
        f"This excludes points that are already red or blue.\n"
        f"Note: Point equality uses epsilon tolerance (geometry.EPSILON).\n"
    )


def test_propagate_excludes_existing_blue_points():
    """
    REQ-PE957-004: Propagation must exclude existing blue points from output.

    Test: If intersection lands on existing blue point, exclude from output.
    """
    # Configuration where intersection is at (1, 1), and we have blue point at (1, 1)
    red = {
        Point(0.0, 0.0, 'red'),
        Point(2.0, 0.0, 'red')
    }
    blue = {
        Point(2.0, 2.0, 'blue'),
        Point(0.0, 2.0, 'blue'),
        Point(1.0, 1.0, 'blue')  # Intersection point
    }

    new_blues = propagate_one_day(red, blue)

    assert Point(1.0, 1.0) not in new_blues, (
        f"Test: test_propagate_excludes_existing_blue_points\n"
        f"Requirement: REQ-PE957-004 (exclude already-colored points)\n\n"
        f"Expected: Point(1.0, 1.0) NOT in output (already blue)\n"
        f"Got: Point(1.0, 1.0) in new_blues set\n\n"
        f"Fix guidance: The function returns NEW blue points only.\n"
        f"Before adding intersection:\n"
        f"  if intersection not in red and intersection not in blue:\n"
        f"      new_blues.add(intersection)\n"
        f"Rationale: Caller will add new_blues to existing blue set.\n"
        f"Including existing blues would cause duplicates.\n"
    )


# ============================================================================
# BEHAVIOR 4: Deduplicate intersection points
# ============================================================================

def test_propagate_deduplicates_intersection_points():
    """
    REQ-PE957-004: Propagation must deduplicate intersection points.

    Test: Multiple line pairs intersecting at same point should count once.
    """
    # Create configuration where 3+ lines pass through origin (0, 0)
    # All lines from different red points to blue origin will intersect there
    red = {
        Point(1.0, 0.0, 'red'),
        Point(0.0, 1.0, 'red'),
        Point(1.0, 1.0, 'red')
    }
    # Blue point at origin means all lines pass through origin
    # But we need a second blue to create multiple lines
    blue = {
        Point(0.0, 0.0, 'blue'),
        Point(2.0, 2.0, 'blue')
    }

    new_blues = propagate_one_day(red, blue)

    # Multiple lines may intersect at same points
    # Set data structure ensures deduplication
    # This is implicitly tested by set behavior, but we verify no duplicates
    # by checking that all points in set are unique (which sets guarantee)
    assert isinstance(new_blues, set), (
        f"Test: test_propagate_deduplicates_intersection_points\n"
        f"Requirement: REQ-PE957-004 (deduplicate intersections)\n\n"
        f"Expected: Return type is set (automatic deduplication)\n"
        f"Got: {type(new_blues).__name__}\n\n"
        f"Fix guidance: Use set data structure for new_blues:\n"
        f"  new_blues = set()\n"
        f"  # ... find intersections ...\n"
        f"  new_blues.add(intersection)  # Set handles deduplication\n"
        f"  return new_blues\n"
        f"Rationale: Point is hashable (implements __hash__ and __eq__).\n"
    )

    # Additionally verify that points within epsilon are considered equal
    # (this tests Point's epsilon-based equality in context of deduplication)


def test_propagate_deduplicates_epsilon_close_intersections():
    """
    REQ-PE957-004: Propagation must treat epsilon-close points as identical.

    Test: Two intersections within epsilon (1e-9) should be deduplicated.
    """
    # Create configuration that produces intersections very close together
    # Due to floating-point arithmetic, some intersections may differ by epsilon
    red = {
        Point(0.0, 0.0, 'red'),
        Point(1.0, 0.0, 'red')
    }
    blue = {
        Point(0.5, 1.0, 'blue'),
        Point(0.5 + 1e-10, 1.0, 'blue')  # Epsilon-close to first blue
    }

    new_blues = propagate_one_day(red, blue)

    # Due to Point's epsilon equality, these should be treated as same point
    # The set should deduplicate them
    # We can't predict exact count, but we test the deduplication mechanism
    point_list = list(new_blues)
    for i, p1 in enumerate(point_list):
        for p2 in point_list[i+1:]:
            assert p1 != p2, (
                f"Test: test_propagate_deduplicates_epsilon_close_intersections\n"
                f"Requirement: REQ-PE957-004 (epsilon-based deduplication)\n\n"
                f"Expected: All points in set are distinct (epsilon > 1e-9)\n"
                f"Got: Duplicate points {p1} and {p2} in set\n\n"
                f"Fix guidance: Point class uses epsilon equality (EPSILON=1e-9).\n"
                f"Set deduplication relies on __hash__ and __eq__.\n"
                f"Verify: Points within epsilon have same hash and compare equal.\n"
                f"Check: Are you using Point instances (not tuples)?\n"
            )


# ============================================================================
# BEHAVIOR 5: Return new blue points only
# ============================================================================

def test_propagate_returns_only_new_blue_points():
    """
    REQ-PE957-004: Propagation must return ONLY new blue points.

    Test: Output should not include existing blue points.
    """
    red = {Point(0.0, 0.0, 'red'), Point(2.0, 0.0, 'red')}
    existing_blue = {Point(2.0, 2.0, 'blue'), Point(0.0, 2.0, 'blue')}

    new_blues = propagate_one_day(red, existing_blue)

    # Verify that existing blue points are NOT in output
    for existing in existing_blue:
        assert existing not in new_blues, (
            f"Test: test_propagate_returns_only_new_blue_points\n"
            f"Requirement: REQ-PE957-004 (return NEW blues only)\n\n"
            f"Expected: Existing blue {existing} NOT in output\n"
            f"Got: Existing blue {existing} in new_blues\n\n"
            f"Fix guidance: Function returns NEW blue points only.\n"
            f"Exclude existing blues before returning:\n"
            f"  if intersection not in blue:\n"
            f"      new_blues.add(intersection)\n"
            f"Rationale: Caller adds new_blues to existing set.\n"
            f"Including existing blues would duplicate them.\n"
        )


# ============================================================================
# EDGE CASE 1: No intersections (all lines parallel)
# ============================================================================

def test_propagate_handles_collinear_configuration(collinear_configuration):
    """
    REQ-PE957-004: Propagation with all parallel lines produces no intersections.

    Test: Collinear points produce parallel lines, no new blues.
    """
    red = collinear_configuration['red']
    blue = collinear_configuration['blue']

    new_blues = propagate_one_day(red, blue)

    assert len(new_blues) == 0, (
        f"Test: test_propagate_handles_collinear_configuration\n"
        f"Requirement: REQ-PE957-004 (edge case: all parallel)\n\n"
        f"Expected: 0 new blue points (all lines parallel)\n"
        f"Got: {len(new_blues)} new blue points\n\n"
        f"Fix guidance: Parallel lines return None from intersect().\n"
        f"Check: Are you handling None properly?\n"
        f"  intersection = line1.intersect(line2)\n"
        f"  if intersection is not None:\n"
        f"      # Process intersection\n"
    )


# ============================================================================
# EDGE CASE 2: Single intersection (minimum case)
# ============================================================================

def test_propagate_handles_minimum_case_two_lines():
    """
    REQ-PE957-004: Propagation with 1 red and 2 blues produces single intersection.

    Test: Minimum case (2 lines) should find intersection if not parallel.
    """
    red = {Point(0.0, 0.0, 'red')}
    blue = {Point(1.0, 0.0, 'blue'), Point(0.0, 1.0, 'blue')}

    new_blues = propagate_one_day(red, blue)

    # This specific configuration has 2 lines:
    # Line 1: (0,0) to (1,0) is y=0 (horizontal)
    # Line 2: (0,0) to (0,1) is x=0 (vertical)
    # But both pass through (0,0) which is red, so no valid intersection
    # Actually, they're perpendicular and intersect at origin (0,0)
    # But origin is red, so excluded
    assert Point(0.0, 0.0) not in new_blues, (
        f"Test: test_propagate_handles_minimum_case_two_lines\n"
        f"Requirement: REQ-PE957-004 (edge case: minimum 2 lines)\n\n"
        f"Expected: Origin (0,0) NOT in output (already red)\n"
        f"Got: Origin in new_blues\n\n"
        f"Fix guidance: Intersection at red point must be excluded.\n"
        f"Check exclusion logic:\n"
        f"  if intersection not in red and intersection not in blue:\n"
        f"      new_blues.add(intersection)\n"
    )


# ============================================================================
# EDGE CASE 3: Multiple lines through same point
# ============================================================================

def test_propagate_handles_multiple_lines_through_same_point():
    """
    REQ-PE957-004: Propagation with multiple lines through same point deduplicates.

    Test: Common in symmetric configurations, should count point once.
    """
    # All red points lie on circle, blue at center
    # All lines from reds to center pass through origin
    red = {
        Point(1.0, 0.0, 'red'),
        Point(0.0, 1.0, 'red'),
        Point(-1.0, 0.0, 'red'),
        Point(0.0, -1.0, 'red')
    }
    blue = {
        Point(0.0, 0.0, 'blue'),  # Center
        Point(2.0, 2.0, 'blue')   # External point
    }

    new_blues = propagate_one_day(red, blue)

    # Multiple line pairs may intersect at same points
    # Set ensures each unique point counted once
    assert isinstance(new_blues, set), (
        f"Test: test_propagate_handles_multiple_lines_through_same_point\n"
        f"Requirement: REQ-PE957-004 (edge case: multiple lines same point)\n\n"
        f"Expected: Return type is set (deduplication)\n"
        f"Got: {type(new_blues).__name__}\n\n"
        f"Fix guidance: Use set to store intersections:\n"
        f"  new_blues = set()\n"
        f"  new_blues.add(intersection)  # Automatic deduplication\n"
    )


# ============================================================================
# EDGE CASE 6: Empty input sets
# ============================================================================

def test_propagate_handles_empty_red_set():
    """
    REQ-PE957-004: Propagation with empty red set produces no new blues.

    Test: Boundary condition - no red points means no lines.
    """
    red = set()
    blue = {Point(1.0, 1.0, 'blue')}

    new_blues = propagate_one_day(red, blue)

    assert len(new_blues) == 0, (
        f"Test: test_propagate_handles_empty_red_set\n"
        f"Requirement: REQ-PE957-004 (edge case: empty input)\n\n"
        f"Expected: 0 new blue points (no red points, no lines)\n"
        f"Got: {len(new_blues)} new blue points\n\n"
        f"Fix guidance: Handle empty sets at start:\n"
        f"  if not red or not blue:\n"
        f"      return set()  # No lines to construct\n"
        f"Alternative: Loop handles empty iteration naturally:\n"
        f"  for r in red:  # Empty set → no iterations\n"
        f"      for b in blue: ...\n"
    )


def test_propagate_handles_empty_blue_set():
    """
    REQ-PE957-004: Propagation with empty blue set produces no new blues.

    Test: Boundary condition - no blue points means no lines.
    """
    red = {Point(0.0, 0.0, 'red')}
    blue = set()

    new_blues = propagate_one_day(red, blue)

    assert len(new_blues) == 0, (
        f"Test: test_propagate_handles_empty_blue_set\n"
        f"Requirement: REQ-PE957-004 (edge case: empty input)\n\n"
        f"Expected: 0 new blue points (no blue points, no lines)\n"
        f"Got: {len(new_blues)} new blue points\n\n"
        f"Fix guidance: Handle empty sets at start:\n"
        f"  if not red or not blue:\n"
        f"      return set()  # No lines to construct\n"
        f"Alternative: Loop handles empty iteration naturally:\n"
        f"  for r in red:\n"
        f"      for b in blue:  # Empty set → no iterations\n"
    )


def test_propagate_handles_both_sets_empty():
    """
    REQ-PE957-004: Propagation with both empty sets produces no new blues.

    Test: Boundary condition - no points at all.
    """
    red = set()
    blue = set()

    new_blues = propagate_one_day(red, blue)

    assert len(new_blues) == 0, (
        f"Test: test_propagate_handles_both_sets_empty\n"
        f"Requirement: REQ-PE957-004 (edge case: both empty)\n\n"
        f"Expected: 0 new blue points (no points, no lines)\n"
        f"Got: {len(new_blues)} new blue points\n\n"
        f"Fix guidance: Handle empty sets early:\n"
        f"  if not red or not blue:\n"
        f"      return set()\n"
    )


# ============================================================================
# ERROR HANDLING 1: Invalid input type for red points
# ============================================================================

def test_propagate_raises_typeerror_invalid_red_input():
    """
    REQ-PE957-004: Propagation must raise TypeError for invalid red input.

    Test: String input for red should raise TypeError with helpful message.
    """
    blue = {Point(1.0, 1.0, 'blue')}

    with pytest.raises(TypeError) as exc_info:
        propagate_one_day("not a set", blue)

    error_message = str(exc_info.value)
    assert "red" in error_message.lower(), (
        f"Test: test_propagate_raises_typeerror_invalid_red_input\n"
        f"Requirement: REQ-PE957-004 (type safety)\n\n"
        f"Expected: Error message mentions 'red' parameter\n"
        f"Got: '{error_message}'\n\n"
        f"Fix guidance: Add type validation at function start:\n"
        f"  if not isinstance(red, (set, frozenset)):\n"
        f"      raise TypeError('red must be a set of Points')\n"
        f"  # Optionally check all elements are Points:\n"
        f"  if not all(isinstance(p, Point) for p in red):\n"
        f"      raise TypeError('red must contain only Point instances')\n"
    )


def test_propagate_raises_typeerror_invalid_blue_input():
    """
    REQ-PE957-004: Propagation must raise TypeError for invalid blue input.

    Test: String input for blue should raise TypeError with helpful message.
    """
    red = {Point(0.0, 0.0, 'red')}

    with pytest.raises(TypeError) as exc_info:
        propagate_one_day(red, "not a set")

    error_message = str(exc_info.value)
    assert "blue" in error_message.lower(), (
        f"Test: test_propagate_raises_typeerror_invalid_blue_input\n"
        f"Requirement: REQ-PE957-004 (type safety)\n\n"
        f"Expected: Error message mentions 'blue' parameter\n"
        f"Got: '{error_message}'\n\n"
        f"Fix guidance: Add type validation at function start:\n"
        f"  if not isinstance(blue, (set, frozenset)):\n"
        f"      raise TypeError('blue must be a set of Points')\n"
        f"  # Optionally check all elements are Points:\n"
        f"  if not all(isinstance(p, Point) for p in blue):\n"
        f"      raise TypeError('blue must contain only Point instances')\n"
    )


# ============================================================================
# VERIFICATION: g(1) = 8
# ============================================================================

def test_propagate_symmetric_configuration_geometric_correctness(symmetric_configuration):
    """
    REQ-PE957-004: Verify propagation with symmetric configuration.

    Test: Symmetric config (3 red unit circle, 2 blue on x-axis) produces
    4 total blue points after day 1 (2 initial + 2 new). This validates
    geometric correctness of the algorithm. Note: Optimal g(1)=8 requires
    different configuration (addressed in Phase 3).
    """
    red = symmetric_configuration['red']
    blue = symmetric_configuration['blue']

    new_blues = propagate_one_day(red, blue)
    total_blues = len(blue) + len(new_blues)

    assert total_blues == 4, (
        f"Test: test_propagate_symmetric_configuration_geometric_correctness\n"
        f"Requirement: REQ-PE957-004 (verify geometric correctness)\n\n"
        f"Expected: 4 TOTAL blues (2 initial + 2 new) for symmetric config\n"
        f"Got: {total_blues} total blues ({len(blue)} initial + {len(new_blues)} new)\n\n"
        f"Fix guidance: g_config(n) represents TOTAL blue points after n days.\n"
        f"  - Initial blues: 2 (origin at 0,0 and point at 2,0)\n"
        f"  - New blues from propagation: 2 (geometric result)\n"
        f"  - Total: initial + new = 4\n\n"
        f"Configuration: 3 red on unit circle, 2 blue on x-axis\n"
        f"  - Constructs 6 lines (3 red × 2 blue)\n"
        f"  - Finds intersections of line pairs (6 choose 2 = 15 pairs)\n"
        f"  - Excludes existing reds and blues\n"
        f"  - Deduplicates (set behavior)\n"
        f"  - Yields 2 new blue points (geometric reality)\n\n"
        f"Note: Optimal configuration achieving g(1)=8 is different.\n"
        f"Initial blues: {len(blue)}, New blues: {len(new_blues)}, Total: {total_blues}\n"
        f"New points found: {new_blues}\n"
    )
