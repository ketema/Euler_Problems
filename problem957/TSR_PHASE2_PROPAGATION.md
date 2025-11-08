# Test Specification Review (TSR) - Phase 2: Propagation Logic

**Feature**: Single day propagation for Project Euler Problem 957
**Requirements Reference**: REQ-PE957-004
**Target Coverage**: >85% line coverage, >80% branch coverage
**Date**: 2025-11-06
**Coordinator**: Claude Code (TDD Subagent Architecture POC)

---

## Feature Information

This TSR specifies tests for the single-day propagation logic that uses the geometry primitives from Phase 1. This implements the core simulation mechanic: "every line passing through a red point and a blue point is constructed, then every white point where two different such lines meet turns blue."

**Dependencies**: Phase 1 complete (Point, Line, intersect from src/geometry.py)

---

## What Will Be Tested

### Core Functionality

**Behavior 1**: Construct all lines between red-blue pairs
- Requirement: REQ-PE957-004
- Given 3 red points and 2 blue points
- Should generate 6 lines (3 × 2 combinations)
- Example: `red = {R1, R2, R3}, blue = {B1, B2}` → 6 lines

**Behavior 2**: Find intersection points of all line pairs
- Requirement: REQ-PE957-004
- For n lines, check all pairs (n choose 2)
- Skip parallel lines (no intersection)
- Skip identical lines (infinite intersections)
- Return set of intersection points

**Behavior 3**: Exclude already-colored points
- Requirement: REQ-PE957-004
- Intersection points that are red → exclude
- Intersection points that are already blue → exclude
- Only NEW blue points count

**Behavior 4**: Deduplicate intersection points
- Requirement: REQ-PE957-004
- Use set data structure (Point is hashable)
- Multiple line pairs may intersect at same point
- Count each unique point only once

**Behavior 5**: Return new blue points
- Requirement: REQ-PE957-004
- Output: set of NEW blue points only
- Do not include existing blue points in output
- Rationale: Caller adds these to existing blue set

---

### Edge Cases

**Edge case 1**: No intersections (all lines parallel)
- Why tested: Degenerate configuration
- Input: Collinear red points, collinear blue points
- Expected: Empty set (no new blue points)
- Requirement: REQ-PE957-004

**Edge case 2**: Single intersection (two lines only)
- Why tested: Minimum case (2 lines minimum)
- Input: 1 red, 2 blue → 2 lines
- Expected: 1 intersection point (if not parallel)
- Requirement: REQ-PE957-004

**Edge case 3**: Multiple lines through same point
- Why tested: Common in symmetric configurations
- Input: Configuration where 3+ lines intersect at origin
- Expected: Single point in output (deduplication)
- Requirement: REQ-PE957-004

**Edge case 4**: New blue point coincides with red point
- Why tested: Intersection at existing red point
- Input: Intersection lands exactly on red point
- Expected: Excluded from output (already colored)
- Requirement: REQ-PE957-004

**Edge case 5**: New blue point coincides with existing blue point
- Why tested: Intersection at existing blue point
- Input: Intersection lands exactly on existing blue point
- Expected: Excluded from output (already blue)
- Requirement: REQ-PE957-004

**Edge case 6**: Empty input sets
- Why tested: Boundary condition
- Input: Empty red set or empty blue set
- Expected: Empty output (no lines to construct)
- Requirement: REQ-PE957-004

**Edge case 7**: Epsilon-close intersections
- Why tested: Floating-point precision
- Input: Two intersections within epsilon (1e-9)
- Expected: Treated as same point (deduplication via Point equality)
- Requirement: REQ-PE957-004 (uses REQ-PE957-001 epsilon)

---

### Error Handling

**Error condition 1**: Invalid input type for red points
- Expected behavior: Raise TypeError if not set/iterable of Points
- Requirement: REQ-PE957-004 (type safety)
- Input: `propagate_one_day("not a set", blue)`
- Error message must include: "red must be a set of Points"

**Error condition 2**: Invalid input type for blue points
- Expected behavior: Raise TypeError if not set/iterable of Points
- Requirement: REQ-PE957-004 (type safety)
- Input: `propagate_one_day(red, "not a set")`
- Error message must include: "blue must be a set of Points"

---

## Test Strategy

### Fixtures and Data Isolation (QS5 Compliance)

```python
import pytest
from src.geometry import Point

@pytest.fixture
def simple_configuration():
    """Minimal configuration for basic tests (ephemeral)"""
    return {
        'red': {Point(0.0, 0.0, 'red')},
        'blue': {Point(1.0, 0.0, 'blue'), Point(0.0, 1.0, 'blue')}
    }

@pytest.fixture
def symmetric_configuration():
    """Symmetric configuration for g(1)=8 verification (ephemeral)"""
    import math
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
    """Collinear points (all parallel lines) for edge case (ephemeral)"""
    red = {Point(0.0, 0.0, 'red'), Point(1.0, 0.0, 'red')}
    blue = {Point(2.0, 0.0, 'blue'), Point(3.0, 0.0, 'blue')}
    return {'red': red, 'blue': blue}
```

**DATA ISOLATION RULE**: All fixtures use in-memory Point sets. NO database connections, NO file I/O, NO persistent storage. Pure ephemeral data per QS5.

### Test Organization

**File**: `tests/test_propagation.py`
**Test count estimate**: 15-20 tests
**Naming convention**: `test_propagate_[behavior]_[scenario]`

Examples:
- `test_propagate_generates_correct_line_count`
- `test_propagate_finds_intersections`
- `test_propagate_excludes_red_points`
- `test_propagate_excludes_existing_blue_points`
- `test_propagate_deduplicates_intersection_points`
- `test_propagate_handles_parallel_lines`
- `test_propagate_symmetric_configuration_g1`
- `test_propagate_raises_typeerror_invalid_red_input`

**Test grouping**:
- Basic functionality: 5-7 tests
- Exclusion logic: 3-4 tests
- Edge cases: 5-7 tests
- Error handling: 2-3 tests

---

## Coverage Targets

**Line coverage**: >85% (requirement: QS1)
**Branch coverage**: >80%
**Edge cases**: 100% of 7 identified edge cases above

**Rationale**:
- REQ-PE957-004 fully validated (single day propagation)
- Edge cases cover parallel lines, collinear points, deduplication, exclusions
- Error handling covers type safety

**Excluded from coverage**: None (all code paths testable)

---

## Error Message Standard Reminder

**CRITICAL**: All test failure messages MUST be self-documenting per 5-point checklist.

**Required 5 elements**:
1. Test name (what scenario failed)
2. Requirement reference (REQ-PE957-004)
3. Expected behavior (spec)
4. Actual behavior (result)
5. Fix guidance (HOW to fix, not solution)

**Example excellent error message**:

```python
def test_propagate_excludes_existing_blue_points():
    """
    REQ-PE957-004: Propagation must exclude existing blue points

    Test: Intersection points that are already blue should not be in output
    """
    red = {Point(0.0, 0.0, 'red')}
    blue = {Point(1.0, 0.0, 'blue'), Point(0.0, 1.0, 'blue')}

    # Origin is already blue, line from R(0,0) to B(0,1) is x=0 (vertical)
    # Line from R(0,0) to B(1,0) is y=0 (horizontal)
    # These lines intersect at (0,0) which is already blue

    new_blues = propagate_one_day(red, blue)

    assert Point(0.0, 0.0) not in new_blues, (
        f"Test: test_propagate_excludes_existing_blue_points\\n"
        f"Requirement: REQ-PE957-004 (exclude already-colored points)\\n\\n"
        f"Expected: Point(0.0, 0.0) NOT in output (already blue)\\n"
        f"Got: Point(0.0, 0.0) in new_blues set\\n\\n"
        f"Fix guidance: Before adding intersection to output, check:\\n"
        f"  if intersection not in red and intersection not in blue:\\n"
        f"      new_blues.add(intersection)\\n"
        f"This excludes already-colored points per problem statement.\\n"
    )
```

This error message enables blind implementation:
- Coder knows WHAT to check (intersection not in existing sets)
- Coder knows HOW to implement (conditional before adding to output)
- Coder understands WHY (problem statement excludes already-colored points)

---

## Coordinator Review Checklist

Before spawning test-writer subagent, verify:

- [x] **Requirements mapped to tests**: REQ-PE957-004 covered
- [x] **Edge cases comprehensive**: 7 edge cases identified (parallel, collinear, deduplication, exclusions, empty, epsilon)
- [x] **Error messages will be self-documenting**: 5-point standard explained with example
- [x] **Data isolation confirmed**: All fixtures ephemeral (in-memory Point sets, no persistence)
- [x] **Scope clear**: Single day propagation only. No multi-day simulation (Phase 3).
- [x] **Success criteria defined**: 15-20 tests, >85% coverage, all tests RED initially
- [x] **Token budget reasonable**: 8-12K for test-writer (simpler than Phase 1)
- [x] **Dependencies available**: Phase 1 complete (Point, Line, intersect working)

---

## Scope Boundaries (Prevent Scope Creep)

**IN SCOPE**:
- Single day propagation: red×blue lines → intersections → new blues
- Exclusion logic: skip red points, skip existing blue points
- Deduplication: use set data structure (Point hashable)
- Edge cases: parallel lines, empty sets, epsilon-close points
- Error handling: type safety for inputs

**OUT OF SCOPE** (explicitly excluded):
- Multi-day simulation (that's Phase 3)
- Initial configuration generation (hard-coded in fixtures)
- Performance optimization (spatial indexing, etc.)
- Visualization (that's Phase 4)
- Configuration validation (assume valid inputs)
- Line drawing or rendering

**If test-writer includes out-of-scope tests** → Coordinator will reject and respawn with clarified TSR

---

## AI Board Usage Guidance for Test-Writer

**Test-writer can consult AI Board ONESHOT** (~1.5K tokens per consultation).

**Likely consultation topics**:
- "Should I test the case where all lines pass through the origin?"
- "How to handle intersection at existing blue point in error message?"
- "Is this error message self-documenting per 5-point standard?"
- "What other edge cases for deduplication am I missing?"

**Test-writer should NOT consult for**:
- Trivial decisions (test names, fixture organization)
- Questions answered in this TSR
- Scope questions (this TSR defines scope clearly)
- Implementation details (test-writer is blind to implementation)

**Budget**: Maximum 1-2 AI Board ONESHOT consultations (~3K tokens total)

---

## Success Criteria for Test-Writer

Test-writer succeeds when:

1. ✓ All scenarios in "What Will Be Tested" have corresponding tests
2. ✓ All 7 edge cases have corresponding tests
3. ✓ All 2 error conditions have corresponding tests
4. ✓ Total test count: 15-20 tests
5. ✓ All tests failing (RED phase confirmed via pytest)
6. ✓ Error messages pass 5-point checklist (validated via AI Board ONESHOT if needed)
7. ✓ Coverage target >85% achievable (per test suite design)
8. ✓ Data isolation enforced (all fixtures ephemeral, no persistence)
9. ✓ 3-5 example error messages provided for coordinator spot-check
10. ✓ Within token budget (8-12K)
11. ✓ Uses Phase 1 geometry primitives correctly (imports from src.geometry)

**Deliverable format**: Complete test file `tests/test_propagation.py` with passing pytest RED phase (all tests fail before implementation)

---

## Known Good Test Case

**Verification**: With symmetric configuration, g(1) = 8

```python
def test_propagate_symmetric_configuration_g1():
    """
    REQ-PE957-004: Verify propagation produces g(1)=8

    Test: Symmetric configuration should generate 8 new blue points after 1 day
    """
    import math
    red = {
        Point(1.0, 0.0, 'red'),
        Point(math.cos(2*math.pi/3), math.sin(2*math.pi/3), 'red'),
        Point(math.cos(4*math.pi/3), math.sin(4*math.pi/3), 'red')
    }
    blue = {
        Point(0.0, 0.0, 'blue'),
        Point(2.0, 0.0, 'blue')
    }

    new_blues = propagate_one_day(red, blue)

    assert len(new_blues) == 8, (
        f"Test: test_propagate_symmetric_configuration_g1\\n"
        f"Requirement: REQ-PE957-004 (verify known result)\\n\\n"
        f"Expected: 8 new blue points after day 1 (g(1)=8)\\n"
        f"Got: {len(new_blues)} new blue points\\n\\n"
        f"Fix guidance: Check line construction (should be 6 lines)\\n"
        f"Check intersection finding (should find all unique intersections)\\n"
        f"Check exclusion logic (existing reds/blues excluded)\\n"
        f"Debug: Print intersection points to verify coordinates\\n"
    )
```

This test validates the core algorithm against the known problem result.

---

## Template Usage Notes

1. ✓ All sections filled out completely
2. ✓ Edge cases specified with rationale
3. ✓ Scope boundaries clear (single day only, no multi-day)
4. ✓ Requirements referenced (REQ-PE957-004)
5. ✓ Token budget estimated (8-12K test-writer)
6. ✓ Review checklist completed (all boxes checked)
7. ✓ Dependencies noted (Phase 1 geometry primitives)

---

## Version History

- **2025-11-06**: Initial TSR for Phase 2 (Propagation Logic)
- **Dependencies**: Phase 1 complete (commits c164202 RED, acd12eb GREEN)
- **Constitutional Reference**: CL11 (TDD Subagent Architecture)
- **Template Source**: `.claude/helpers/tsr-template.md`
