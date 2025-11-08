# Requirements: Project Euler Problem 957 - Point Genesis

**Problem Source**: https://projecteuler.net/problem=957
**Date**: 2025-11-06
**Project**: prototypes/problem957

---

## Problem Statement

A plane begins with five colored points: three red and two blue. All other points are initially white.

**Daily Process**:
"On each day, every line passing through a red point and a blue point is constructed. Then every white point, where two different such lines meet, turns blue."

**Goal**: Calculate g(16), where g(n) represents the maximum possible count of blue points achievable after n days.

**Known Values**:
- g(1) = 8
- g(2) = 28

---

## Core Requirements

### REQ-PE957-001: Point Representation
**Priority**: Critical
**Description**: Represent geometric points with color state

**Specifications**:
- Point has x, y coordinates (float)
- Point has color: 'red', 'blue', or 'white'
- Points are equal if coordinates match within epsilon (1e-9)
- Points must be hashable for set operations
- Red points never change color
- Blue and white points can become blue

**Rationale**: Foundation for all geometry operations

**Acceptance Criteria**:
- Point(1.0, 2.0, 'red') creates red point at (1, 2)
- Point(1.0, 2.0) == Point(1.0 + 1e-10, 2.0) (epsilon equality)
- Point(1.0, 2.0) != Point(1.0 + 1e-8, 2.0) (exceeds epsilon)
- Points can be added to sets (hashable)
- Point color can be queried and updated (blue/white → blue only)

---

### REQ-PE957-002: Line Representation
**Priority**: Critical
**Description**: Represent infinite lines passing through two points

**Specifications**:
- Line constructed from two distinct points
- Line can determine if it passes through a given point
- Line can determine if it's parallel to another line
- Line can determine if it's identical to another line
- Handle vertical lines (undefined slope)

**Rationale**: Need to construct lines between red/blue pairs and detect intersections

**Acceptance Criteria**:
- Line(Point(0,0), Point(1,1)) creates line y=x
- Line(Point(0,0), Point(0,1)) handles vertical line (x=0)
- Parallel lines detected: Line((0,0), (1,1)) parallel to Line((0,1), (1,2))
- Identical lines detected: Line((0,0), (1,1)) identical to Line((2,2), (3,3))

---

### REQ-PE957-003: Line Intersection
**Priority**: Critical
**Description**: Calculate intersection point of two lines

**Specifications**:
- Return intersection point if lines cross
- Return None if lines are parallel
- Return None if lines are identical
- Handle floating point precision (epsilon tolerance)
- Handle vertical lines correctly

**Rationale**: Core algorithm for finding new blue points

**Acceptance Criteria**:
- Line((0,0), (1,1)) ∩ Line((0,1), (1,0)) = Point(0.5, 0.5)
- Line((0,0), (1,0)) ∩ Line((0,0), (0,1)) = Point(0, 0)
- Line((0,0), (1,1)) ∩ Line((0,1), (1,2)) = None (parallel)
- Intersection within epsilon tolerance handled correctly

---

### REQ-PE957-004: Single Day Propagation
**Priority**: Critical
**Description**: Execute one day of the point genesis process

**Specifications**:
- Input: set of red points, set of blue points
- Process:
  1. Construct all lines passing through (red_point, blue_point) pairs
  2. Find all intersection points where 2+ different lines meet
  3. Identify white points (not in red set, not in blue set)
  4. These white intersection points turn blue
- Output: set of NEW blue points (not including previously blue)
- Exclude points already colored (red or blue)

**Rationale**: Core simulation logic per problem statement

**Acceptance Criteria**:
- With 3 red, 2 blue: generates 6 lines
- Intersection points calculated for all line pairs
- Only NEW blue points returned (not existing blue)
- Points coinciding with red points excluded
- Points coinciding with existing blue points excluded

---

### REQ-PE957-005: Multi-Day Simulation
**Priority**: Critical
**Description**: Run point genesis process for n days

**Specifications**:
- Input: initial red points, initial blue points, number of days n
- Process:
  1. For each day from 1 to n:
     - Run single day propagation
     - Add new blue points to blue set
  2. Track total blue points accumulated
- Output: count of blue points after n days (g(n))

**Rationale**: Calculate g(16) as required by problem

**Acceptance Criteria**:
- With optimal configuration: g(1) = 8
- With optimal configuration: g(2) = 28
- Can calculate g(16)
- Blue point count increases monotonically (never decreases)

---

### REQ-PE957-006: Initial Configuration
**Priority**: High
**Description**: Provide initial point placement for simulation

**Specifications**:
- 3 red points at specified coordinates
- 2 blue points at specified coordinates
- Configuration should be near-optimal to achieve g(1)=8, g(2)=28
- Use symmetric placement for deterministic results

**Suggested Configuration** (verification):
- Red points: equilateral triangle on unit circle
  - R1 = (1, 0)
  - R2 = (cos(2π/3), sin(2π/3)) ≈ (-0.5, 0.866)
  - R3 = (cos(4π/3), sin(4π/3)) ≈ (-0.5, -0.866)
- Blue points: aligned on x-axis
  - B1 = (0, 0) (origin)
  - B2 = (2, 0)

**Rationale**: Need reproducible starting configuration

**Acceptance Criteria**:
- Configuration specified and documented
- Configuration is used consistently across tests
- Configuration produces g(1) = 8 (verified)
- Configuration produces g(2) = 28 (verified)

---

### REQ-PE957-007: Web Visualization
**Priority**: Medium
**Description**: Provide web-based visual representation

**Specifications**:
- HTML page with canvas element
- Display all points colored appropriately:
  - Red points: red circles
  - Blue points: blue circles
  - White points: light gray (if shown)
- Display lines between red-blue pairs (optional)
- Display final state after n days
- Show g(n) result prominently

**Rationale**: User explicitly requested "web based interface visual representation"

**Acceptance Criteria**:
- HTML page loads in browser
- Canvas shows coordinate system
- Points are visible and colored correctly
- g(16) result displayed
- Can run locally without external dependencies

---

## Edge Cases

### EDGE-PE957-001: Collinear Initial Points
**Description**: What if initial points are collinear?
**Expected**: Fewer intersections, lower g(n)
**Test**: Verify with collinear configuration

### EDGE-PE957-002: Overlapping Initial Points
**Description**: What if red and blue points coincide?
**Expected**: Treat as single point, fewer lines generated
**Test**: Should be prevented by configuration validation

### EDGE-PE957-003: Parallel Lines
**Description**: Lines that never intersect
**Expected**: No intersection point generated
**Test**: Verify parallel line detection works

### EDGE-PE957-004: Multiple Lines Through Same Point
**Description**: 3+ lines intersecting at one point
**Expected**: Count as single new blue point
**Test**: Verify deduplication using set data structure

### EDGE-PE957-005: New Blue Point on Existing Line
**Description**: New blue point lies on existing red-blue line
**Expected**: Still turns blue (no special handling)
**Test**: Verify this doesn't affect simulation

---

## Out of Scope

### OOS-PE957-001: Configuration Optimization
**Description**: Finding optimal initial placement to maximize g(n)
**Rationale**: Computationally expensive, not required for POC
**Alternative**: Use known good configuration from problem hints

### OOS-PE957-002: Animated Visualization
**Description**: Frame-by-frame animation showing daily progression
**Rationale**: Nice-to-have, not essential for validation
**Alternative**: Show final state only

### OOS-PE957-003: Interactive Point Placement
**Description**: User can drag points to test configurations
**Rationale**: Out of scope for TDD validation
**Alternative**: Hard-code configuration

### OOS-PE957-004: Performance Optimization
**Description**: Spatial indexing, GPU acceleration
**Rationale**: Problem scale is small (16 days), optimization premature
**Alternative**: Accept O(n²) intersection algorithm

---

## Quality Standards

**QS1 (TDD/BDD)**: >85% test coverage, all edge cases tested
**QS2 (Design)**: DRY, separation of concerns, functional patterns
**QS4 (File Size)**: ≤500 lines per file
**QS5 (Data Isolation)**: Tests use in-memory data only (no persistence)

---

## Success Criteria

1. ✓ All requirements implemented and tested
2. ✓ g(1) = 8 verified with test configuration
3. ✓ g(2) = 28 verified with test configuration
4. ✓ g(16) calculated successfully
5. ✓ Web visualization displays final state
6. ✓ All tests passing (GREEN)
7. ✓ >85% code coverage
8. ✓ TDD subagent architecture validated

---

## Dependencies

**Language**: Python 3.10+
**Testing**: pytest
**Web**: HTML5, Canvas API, vanilla JavaScript (no frameworks)
**Math**: Standard library only (no NumPy dependency for POC)

---

## References

- Project Euler Problem 957: https://projecteuler.net/problem=957
- TDD Subagent Architecture: /.claude/agents/
- TSR Template: /.claude/helpers/tsr-template.md
