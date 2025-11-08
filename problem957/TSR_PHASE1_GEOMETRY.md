# Test Specification Review (TSR) - Phase 1: Geometry Primitives

**Feature**: Computational geometry primitives for Project Euler Problem 957
**Requirements Reference**: REQ-PE957-001, REQ-PE957-002, REQ-PE957-003
**Target Coverage**: >85% line coverage, >80% branch coverage
**Date**: 2025-11-06
**Coordinator**: Claude Code (TDD Subagent Architecture POC)

---

## Feature Information

This TSR specifies tests for foundational geometry classes (Point, Line) and line intersection algorithm. These primitives are critical for all subsequent phases of the point propagation simulation.

---

## What Will Be Tested

### Core Functionality

**Behavior 1**: Point class with coordinates and color
- Requirement: REQ-PE957-001
- Point has x, y coordinates (float)
- Point has color attribute ('red', 'blue', or 'white')
- Point can be constructed: `Point(1.0, 2.0, 'red')`

**Behavior 2**: Point equality with epsilon tolerance
- Requirement: REQ-PE957-001
- Points equal if coordinates within epsilon (1e-9)
- Example: `Point(1.0, 2.0) == Point(1.0 + 1e-10, 2.0)` → True
- Example: `Point(1.0, 2.0) == Point(1.0 + 1e-8, 2.0)` → False

**Behavior 3**: Point hashability for set operations
- Requirement: REQ-PE957-001
- Points can be added to sets
- Points with same coordinates (within epsilon) should hash to same value
- Enables deduplication of intersection points

**Behavior 4**: Line construction from two points
- Requirement: REQ-PE957-002
- Line created from two distinct points
- Example: `Line(Point(0,0), Point(1,1))` creates line y=x
- Line should handle vertical lines: `Line(Point(0,0), Point(0,1))` (x=0)

**Behavior 5**: Line parallel detection
- Requirement: REQ-PE957-002
- Lines with same slope are parallel
- Example: `Line((0,0), (1,1)).is_parallel(Line((0,1), (1,2)))` → True
- Vertical lines parallel to vertical lines

**Behavior 6**: Line identical detection
- Requirement: REQ-PE957-002
- Lines passing through same points are identical
- Example: `Line((0,0), (1,1)).is_identical(Line((2,2), (3,3)))` → True

**Behavior 7**: Line intersection calculation
- Requirement: REQ-PE957-003
- Return intersection point if lines cross
- Example: `Line((0,0), (1,1)).intersect(Line((0,1), (1,0)))` → Point(0.5, 0.5)
- Return None if lines parallel or identical

---

### Edge Cases

**Edge case 1**: Point equality at epsilon boundary
- Why tested: Floating-point precision critical for correctness
- Input: `Point(0, 0) == Point(1e-9, 0)`
- Expected: True (exactly at epsilon threshold)
- Requirement: REQ-PE957-001

**Edge case 2**: Point equality beyond epsilon
- Why tested: Verify epsilon boundary is enforced
- Input: `Point(0, 0) == Point(1.1e-9, 0)`
- Expected: False (exceeds epsilon)
- Requirement: REQ-PE957-001

**Edge case 3**: Vertical line handling
- Why tested: Vertical lines have undefined slope (division by zero)
- Input: `Line(Point(0,0), Point(0,1))` (x=0)
- Expected: Line stores vertical flag, handles correctly
- Requirement: REQ-PE957-002

**Edge case 4**: Horizontal line handling
- Why tested: Slope is exactly zero
- Input: `Line(Point(0,0), Point(1,0))` (y=0)
- Expected: Line handles zero slope correctly
- Requirement: REQ-PE957-002

**Edge case 5**: Intersection of perpendicular lines
- Why tested: Special case (slopes multiply to -1)
- Input: `Line((0,0), (1,0)).intersect(Line((0,0), (0,1)))`
- Expected: Point(0, 0) (intersection at origin)
- Requirement: REQ-PE957-003

**Edge case 6**: Parallel lines (no intersection)
- Why tested: Common case in propagation algorithm
- Input: `Line((0,0), (1,1)).intersect(Line((0,1), (1,2)))`
- Expected: None (parallel, no intersection)
- Requirement: REQ-PE957-003

**Edge case 7**: Identical lines (infinite intersections)
- Why tested: Degenerates intersection algorithm
- Input: `Line((0,0), (1,1)).intersect(Line((2,2), (3,3)))`
- Expected: None (identical, treat as no single intersection)
- Requirement: REQ-PE957-003

**Edge case 8**: Vertical line intersection
- Why tested: Cannot use slope-intercept form
- Input: `Line((0,0), (0,1)).intersect(Line((0,0), (1,0)))`
- Expected: Point(0, 0)
- Requirement: REQ-PE957-003

**Edge case 9**: Two vertical lines
- Why tested: Both have undefined slopes
- Input: `Line((0,0), (0,1)).intersect(Line((1,0), (1,1)))`
- Expected: None (parallel vertical lines)
- Requirement: REQ-PE957-003

---

### Error Handling

**Error condition 1**: Invalid point construction
- Expected behavior: Raise TypeError for non-numeric coordinates
- Requirement: REQ-PE957-001 (type safety)
- Input: `Point("a", 2.0)`
- Error message must: "Point coordinates must be numeric (float/int). Got type <class 'str'> for x coordinate."

**Error condition 2**: Invalid color for point
- Expected behavior: Raise ValueError for invalid color string
- Requirement: REQ-PE957-001 (valid colors: 'red', 'blue', 'white')
- Input: `Point(1.0, 2.0, 'green')`
- Error message must: "Point color must be 'red', 'blue', or 'white'. Got 'green'."

**Error condition 3**: Line constructed from identical points
- Expected behavior: Raise ValueError (cannot define line from single point)
- Requirement: REQ-PE957-002 (two distinct points required)
- Input: `Line(Point(0,0), Point(0,0))`
- Error message must: "Line requires two distinct points. Got identical points at (0.0, 0.0)."

**Error condition 4**: Line constructed from nearly-identical points (within epsilon)
- Expected behavior: Raise ValueError (points too close to define meaningful line)
- Requirement: REQ-PE957-002 (epsilon tolerance applies)
- Input: `Line(Point(0,0), Point(1e-10, 0))`
- Error message must: "Line requires two distinct points. Points (0.0, 0.0) and (1e-10, 0.0) are within epsilon tolerance."

---

## Test Strategy

### Fixtures and Data Isolation (QS5 Compliance)

```python
import pytest
import math

@pytest.fixture
def sample_points():
    """In-memory sample points for testing (ephemeral)"""
    return {
        'origin': (0.0, 0.0),
        'unit_x': (1.0, 0.0),
        'unit_y': (0.0, 1.0),
        'diagonal': (1.0, 1.0),
        'neg_diagonal': (-1.0, -1.0),
    }

@pytest.fixture
def sample_colored_points():
    """Sample points with colors (ephemeral)"""
    # Will use Point class once implemented
    return {
        'red1': (1.0, 0.0, 'red'),
        'red2': (-0.5, 0.866, 'red'),
        'blue1': (0.0, 0.0, 'blue'),
        'blue2': (2.0, 0.0, 'blue'),
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

@pytest.fixture
def epsilon():
    """Epsilon tolerance for float equality (matching requirement)"""
    return 1e-9
```

**DATA ISOLATION RULE**: All fixtures use in-memory tuples and dicts. NO database connections, NO file I/O, NO persistent storage. Pure ephemeral data per QS5.

### Test Organization

**File**: `tests/test_geometry.py`
**Test count estimate**: 25-30 tests
**Naming convention**: `test_[class]_[behavior]_[scenario]`

Examples:
- `test_point_construction_with_coordinates`
- `test_point_equality_within_epsilon`
- `test_point_equality_exceeds_epsilon`
- `test_point_hashable_for_sets`
- `test_point_raises_typeerror_for_invalid_coordinates`
- `test_line_construction_from_two_points`
- `test_line_parallel_detection_same_slope`
- `test_line_intersection_perpendicular_lines`
- `test_line_intersection_parallel_returns_none`
- `test_line_raises_valueerror_for_identical_points`

**Test grouping**:
- Point construction and properties: 8-10 tests
- Point equality and hashing: 5-7 tests
- Line construction and properties: 6-8 tests
- Line intersection algorithm: 10-12 tests
- Error handling: 4-5 tests

---

## Coverage Targets

**Line coverage**: >85% (requirement: QS1)
**Branch coverage**: >80%
**Edge cases**: 100% of 9 identified edge cases above

**Rationale**:
- All requirements (REQ-PE957-001, 002, 003) will be validated
- Edge cases cover epsilon boundary, vertical/horizontal lines, parallel/identical lines
- Error handling covers type safety and domain validation

**Excluded from coverage**: None (all code paths testable)

---

## Error Message Standard Reminder

**CRITICAL**: All test failure messages MUST be self-documenting per 5-point checklist.

**Required 5 elements**:
1. Test name (what scenario failed)
2. Requirement reference (REQ-PE957-XXX)
3. Expected behavior (spec)
4. Actual behavior (result)
5. Fix guidance (HOW to fix, not solution)

**Example excellent error message**:

```python
def test_point_equality_within_epsilon():
    """
    REQ-PE957-001: Point equality must use epsilon tolerance (1e-9)

    Test: Points with coordinates differing by less than epsilon are equal
    """
    p1 = Point(0.0, 0.0)
    p2 = Point(1e-10, 0.0)

    assert p1 == p2, (
        f"Test: test_point_equality_within_epsilon\n"
        f"Requirement: REQ-PE957-001 (Point epsilon equality)\n\n"
        f"Expected: Point(0.0, 0.0) == Point(1e-10, 0.0) → True\n"
        f"         (difference 1e-10 is less than epsilon 1e-9)\n"
        f"Got: False (points not equal)\n\n"
        f"Fix guidance: Implement __eq__ method for Point class.\n"
        f"Compare x and y coordinates using epsilon tolerance:\n"
        f"  return abs(self.x - other.x) < EPSILON and abs(self.y - other.y) < EPSILON\n"
        f"where EPSILON = 1e-9 per REQ-PE957-001.\n"
    )
```

This error message enables blind implementation:
- Coder knows WHAT to implement (__eq__ method)
- Coder knows HOW to compare (abs difference < epsilon)
- Coder knows the epsilon value (1e-9)
- Coder understands WHY (floating-point precision)

---

## Coordinator Review Checklist

Before spawning test-writer subagent, verify:

- [x] **Requirements mapped to tests**: REQ-PE957-001, 002, 003 all covered
- [x] **Edge cases comprehensive**: 9 edge cases identified (epsilon boundary, vertical lines, parallel/identical, intersections)
- [x] **Error messages will be self-documenting**: 5-point standard explained with example
- [x] **Data isolation confirmed**: All fixtures ephemeral (in-memory tuples/dicts, no persistence)
- [x] **Scope clear**: Geometry primitives only (Point, Line, intersection). No propagation logic (Phase 2).
- [x] **Success criteria defined**: 25-30 tests, >85% coverage, all tests RED initially
- [x] **Token budget reasonable**: 15-20K for test-writer, complexity matches scope

---

## Scope Boundaries (Prevent Scope Creep)

**IN SCOPE**:
- Point class: x, y, color, epsilon equality, hashability
- Line class: construction, parallel detection, identical detection
- Line intersection: algorithm handles all cases (vertical, parallel, perpendicular)
- Error handling: type safety, domain validation

**OUT OF SCOPE** (explicitly excluded):
- Point propagation logic (that's Phase 2)
- Multi-day simulation (that's Phase 3)
- Initial configuration (that's Phase 3)
- Web visualization (that's Phase 4)
- Performance optimization (spatial indexing, etc.)
- 3D points or higher dimensions
- Curved lines (arcs, bezier, etc.)
- Non-Euclidean geometry

**If test-writer includes out-of-scope tests** → Coordinator will reject and respawn with clarified TSR

---

## AI Board Usage Guidance for Test-Writer

**Test-writer can consult AI Board ONESHOT** (~1.5K tokens per consultation).

**Likely consultation topics**:
- "Should I test negative coordinates for Point class?"
- "How to handle intersection of two vertical lines in tests?"
- "Is this error message self-documenting per 5-point standard?"
- "What edge cases for line intersection am I missing?"

**Test-writer should NOT consult for**:
- Trivial decisions (test names, fixture organization)
- Questions answered in this TSR
- Scope questions (this TSR defines scope clearly)
- Implementation details (test-writer is blind to implementation)

**Budget**: Maximum 2-3 AI Board ONESHOT consultations (~4.5K tokens total)

---

## Success Criteria for Test-Writer

Test-writer succeeds when:

1. ✓ All scenarios in "What Will Be Tested" have corresponding tests
2. ✓ All 9 edge cases have corresponding tests
3. ✓ All 4 error conditions have corresponding tests
4. ✓ Total test count: 25-30 tests
5. ✓ All tests failing (RED phase confirmed via pytest)
6. ✓ Error messages pass 5-point checklist (validated via AI Board ONESHOT)
7. ✓ Coverage target >85% achievable (per test suite design)
8. ✓ Data isolation enforced (all fixtures ephemeral, no persistence)
9. ✓ 3-5 example error messages provided for coordinator spot-check
10. ✓ Within token budget (15-20K)

**Deliverable format**: Complete test file `tests/test_geometry.py` with passing pytest RED phase (all tests fail before implementation)

---

## Template Usage Notes

1. ✓ All sections filled out completely
2. ✓ Edge cases specified with rationale
3. ✓ Scope boundaries clear (geometry only, no propagation)
4. ✓ Requirements referenced (REQ-PE957-001, 002, 003)
5. ✓ Token budget estimated (15-20K test-writer)
6. ✓ Review checklist completed (all boxes checked)

---

## Version History

- **2025-11-06**: Initial TSR for Phase 1 (Geometry Primitives)
- **AI Board Conversation ID**: eb46641f-42f3-44d8-abc0-23a05f5dcc2d
- **Constitutional Reference**: CL11 (TDD Subagent Architecture)
- **Template Source**: `.claude/helpers/tsr-template.md`
