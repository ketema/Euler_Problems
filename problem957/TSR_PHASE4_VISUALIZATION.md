# Test Specification Review (TSR) - Phase 4: Interactive Web Visualization

**POC**: TDD Subagent Architecture for Project Euler Problem 957
**Phase**: 4 of 4 - Interactive Web Visualization
**Status**: READY FOR RED PHASE
**Created**: 2025-11-07
**Author**: Claude Code (Coordinator)

---

## Overview

**Purpose**: Create interactive web visualization showing point propagation from g(1) to g(16) using the optimal configuration that produces OEIS A189191 sequence.

**Scope**: HTML5 canvas-based visualization with animation controls, allowing users to:
- See geometric propagation process visually
- Step through days 1-16
- Observe exponential growth pattern
- Understand intersection mechanics

**Dependencies**:
- Phase 3 COMPLETE: simulate_multiday() ✓
- Optimal configuration fixture ✓
- Complete OEIS A189191 sequence ✓

**Target**: Celebration visualization demonstrating that <100 people worldwide have solved PE 957

---

## Requirements

### REQ-PE957-VIZ-001: Interactive HTML5 Canvas
**Description**: Render points and lines on interactive canvas with proper coordinate transformation.

**Acceptance Criteria**:
- Canvas size: 800x600 pixels (responsive optional)
- Coordinate system: Mathematical (origin center, y-axis up)
- Automatic viewport scaling to fit all points
- Red points: filled circles, radius 5px, color #FF0000
- Blue points: filled circles, radius 4px, color #0000FF
- Lines: stroke width 1px, color #CCCCCC, alpha 0.3
- Grid overlay: optional, 10-unit spacing

**Test Coverage**:
- Canvas initialization with correct dimensions
- Coordinate transformation accuracy (mathematical → screen)
- Point rendering at correct screen positions
- Color correctness for red/blue points
- Line rendering between point pairs

---

### REQ-PE957-VIZ-002: Animation State Management
**Description**: Manage animation state for day-by-day progression from g(0) to g(16).

**Acceptance Criteria**:
- State: {currentDay: 0-16, isPlaying: boolean, speed: 1-5}
- Initial state: currentDay=0, isPlaying=false, speed=2
- State transitions: play → pause, step forward, step back, reset
- Speed levels: 1=slow (2s/day), 2=medium (1s/day), 3=fast (0.5s/day), 4=veryfast (0.25s/day), 5=instant
- Auto-pause at day 16
- Prevent step beyond [0, 16] bounds

**Test Coverage**:
- Initial state correctness
- Play/pause toggle
- Step forward/back boundary enforcement
- Speed changes apply to animation interval
- Auto-pause at completion

---

### REQ-PE957-VIZ-003: Day-by-Day Data Binding
**Description**: Display point sets for each day using optimal configuration + OEIS A189191 data.

**Acceptance Criteria**:
- Day 0: 3 red + 2 blue (initial configuration)
- Day 1: 3 red + 8 blue (g(1)=8)
- Day 2: 3 red + 28 blue (g(2)=28)
- ...
- Day 16: 3 red + 15,730,302,251,147,551,048 blue (g(16))
- Data source: Python backend calling simulate_multiday() OR precomputed JSON
- Red points: constant across all days
- Blue points: cumulative (monotonically increasing)

**Test Coverage**:
- Correct blue count for each day 0-16
- Red points unchanged across days
- Blue points include all previous days (cumulative)
- Data matches OEIS A189191 sequence

**Note**: Rendering 15 trillion points is impractical. For days >10, render sample (e.g., 10,000 random points) with disclaimer.

---

### REQ-PE957-VIZ-004: Statistics Display
**Description**: Show real-time statistics panel with current state information.

**Acceptance Criteria**:
- Current day: "Day X / 16"
- Total blue points: "g(X) = Y" (formatted with commas)
- Growth rate: "ΔBlues = Z" (difference from previous day)
- Percentage of g(16): "X.XX% of final"
- Optimal configuration coordinates (collapsible section)
- OEIS reference: Link to A189191

**Test Coverage**:
- Statistics update when day changes
- Number formatting (commas for large numbers)
- Growth rate calculation accuracy
- Percentage calculation accuracy
- Configuration display matches fixture

---

### REQ-PE957-VIZ-005: User Interaction Controls
**Description**: Provide intuitive controls for animation and exploration.

**Acceptance Criteria**:
- Play/Pause button (▶️/⏸️)
- Step Back button (⏮️) - disabled at day 0
- Step Forward button (⏭️) - disabled at day 16
- Reset button (⏮️⏮️) - returns to day 0
- Speed slider: 1-5 (labeled slow → fast)
- Day scrubber: slider 0-16 (jumps to specific day)
- Keyboard shortcuts: Space=play/pause, ←=back, →=forward, R=reset
- Tooltips on all controls

**Test Coverage**:
- Button click handlers trigger correct state changes
- Disabled states enforced at boundaries
- Keyboard shortcuts work correctly
- Speed slider changes animation speed
- Day scrubber jumps to correct day
- Tooltips display on hover

---

### REQ-PE957-VIZ-006: Responsive Canvas Updates
**Description**: Efficiently update canvas when state changes without full re-render.

**Acceptance Criteria**:
- Incremental rendering: only add new blues, don't redraw existing
- Clear canvas before redraw (when jumping days)
- Debounce rapid state changes (prevent flickering)
- Maintain aspect ratio on window resize
- Maximum 60 FPS rendering
- Loading indicator for days with many points (>1M)

**Test Coverage**:
- Canvas clears before jumping to different day
- Incremental updates add only new points
- No flickering during animation
- Resize handler updates viewport correctly
- Loading indicator appears for computationally intensive days

---

### REQ-PE957-VIZ-007: Line Visualization (Optional Enhanced Mode)
**Description**: Show lines between red-blue pairs that generate intersections.

**Acceptance Criteria**:
- Toggle: "Show Lines" checkbox
- When enabled: draw all red-blue lines with alpha 0.3
- Maximum lines per day: 1000 (sample if exceeds)
- Line color: #888888
- Intersection highlights: optional pulsing circles at new blue points

**Test Coverage**:
- Lines toggle shows/hides lines
- Lines drawn between correct point pairs
- Sampling kicks in when >1000 lines
- Line count matches expected (3 red × N blue)

---

### REQ-PE957-VIZ-008: Data Source Integration
**Description**: Load point data from Python backend or precomputed JSON.

**Acceptance Criteria**:
- **Option A (Backend)**: Flask/FastAPI endpoint `/api/simulate?day=N`
  - Returns: {red: [...], blue: [...], count: X}
  - Caching: memoize results for each day
  - CORS enabled for local development

- **Option B (Precomputed)**: Static JSON file `data/oeis_a189191.json`
  - Structure: {0: {red: [...], blue: []}, 1: {red: [...], blue: [...]}, ...}
  - Lazy loading: fetch only when day requested
  - Compression: gzip for large days

**Test Coverage**:
- API endpoint returns correct data for each day
- JSON file loads successfully
- Data structure matches expected format
- Error handling for network failures
- Fallback to cached data if API unavailable

---

## Test Structure

### Directory Layout
```
tests/
├── test_visualization_rendering.py     # Canvas, points, lines
├── test_visualization_state.py          # Animation state management
├── test_visualization_controls.py       # User interaction
├── test_visualization_data.py           # Data binding, statistics
└── fixtures/
    └── viz_fixtures.py                  # Shared fixtures
```

### Test Categories

**1. Canvas Rendering Tests** (test_visualization_rendering.py)
- Canvas initialization with correct dimensions
- Coordinate transformation (math → screen)
- Point rendering at correct positions
- Color accuracy (red #FF0000, blue #0000FF)
- Line rendering between point pairs
- Viewport scaling for different point ranges
- Grid overlay rendering (if enabled)

**2. State Management Tests** (test_visualization_state.py)
- Initial state correctness
- Play/pause state transitions
- Step forward/back with boundary enforcement
- Speed changes apply correctly
- Reset to initial state
- Auto-pause at day 16
- State persistence (optional localStorage)

**3. Control Interaction Tests** (test_visualization_controls.py)
- Play/Pause button click handler
- Step buttons trigger state changes
- Reset button returns to day 0
- Speed slider changes animation speed
- Day scrubber jumps to correct day
- Keyboard shortcuts (Space, ←, →, R)
- Button disabled states at boundaries
- Tooltip display on hover

**4. Data Binding Tests** (test_visualization_data.py)
- Blue count matches OEIS A189191 for each day
- Red points constant across all days
- Blue points cumulative (monotonically increasing)
- Statistics display updates on day change
- Number formatting with commas
- Growth rate calculation (g(n) - g(n-1))
- Percentage of g(16) calculation
- Configuration coordinates display

**5. Integration Tests** (test_visualization_integration.py)
- Full animation cycle day 0 → 16
- Data loading from backend/JSON
- Responsive resize behavior
- Error handling for missing data
- Performance benchmarks (render time <100ms for days 1-10)

---

## Fixtures

### viz_canvas_fixture
```python
@pytest.fixture
def viz_canvas():
    """Mock HTML5 canvas for testing rendering logic."""
    return {
        'width': 800,
        'height': 600,
        'context': MockCanvasContext()
    }
```

### viz_state_fixture
```python
@pytest.fixture
def viz_state():
    """Initial visualization state."""
    return {
        'currentDay': 0,
        'isPlaying': False,
        'speed': 2,
        'showLines': False,
        'showGrid': False
    }
```

### viz_data_fixture
```python
@pytest.fixture
def viz_data(optimal_configuration):
    """Precomputed data for days 0-16 using optimal configuration."""
    # Uses optimal_configuration fixture from test_multiday_simulation.py
    config = optimal_configuration
    data = {}

    for day in range(17):
        if day == 0:
            data[day] = {
                'red': list(config['red']),
                'blue': list(config['blue']),
                'count': len(config['blue'])
            }
        else:
            # Simulate using propagation module
            blue_count = simulate_multiday(config['red'], config['blue'], day)
            data[day] = {
                'red': list(config['red']),
                'count': blue_count,
                'expected': config['expected_g'][day]
            }
            # Note: Full blue point sets omitted for days >10 (too large)

    return data
```

---

## Self-Documenting Error Messages

### Canvas Rendering Errors
```python
assert point_on_screen == expected_screen_coords, (
    f"Test: test_coordinate_transformation\n"
    f"Requirement: REQ-PE957-VIZ-001\n\n"
    f"Expected: Point({math_x}, {math_y}) → screen({expected_screen_coords})\n"
    f"Got: Point({math_x}, {math_y}) → screen({point_on_screen})\n"
    f"Error: {abs(point_on_screen[0] - expected_screen_coords[0]):.2f}px X, "
    f"{abs(point_on_screen[1] - expected_screen_coords[1]):.2f}px Y\n\n"
    f"Fix guidance: Check coordinate transformation:\n"
    f"  screen_x = (math_x - center_x) * scale + canvas_width/2\n"
    f"  screen_y = (center_y - math_y) * scale + canvas_height/2\n"
    f"  Note: Y-axis inverted (math up = screen down)\n"
)
```

### State Management Errors
```python
assert new_day == expected_day, (
    f"Test: test_step_forward_boundary\n"
    f"Requirement: REQ-PE957-VIZ-002\n\n"
    f"Expected: Step forward from day {current_day} → day {expected_day}\n"
    f"Got: day {new_day}\n\n"
    f"Fix guidance: Enforce boundary check:\n"
    f"  if currentDay < 16:\n"
    f"      currentDay += 1\n"
    f"  else:\n"
    f"      # Already at max, do nothing\n"
    f"      pass\n"
)
```

### Data Binding Errors
```python
assert blue_count == expected_g, (
    f"Test: test_data_binding_day_{day}\n"
    f"Requirement: REQ-PE957-VIZ-003\n\n"
    f"Expected: g({day}) = {expected_g:,} (OEIS A189191)\n"
    f"Got: g({day}) = {blue_count:,}\n"
    f"Difference: {abs(blue_count - expected_g):,}\n\n"
    f"Fix guidance: Verify data source integration.\n"
    f"  1. Check simulate_multiday(red, blue, {day}) returns correct count\n"
    f"  2. Verify data loading from backend/JSON\n"
    f"  3. Ensure cumulative blues (not incremental)\n"
    f"  4. Cross-reference with test_multiday_simulation.py results\n"
)
```

---

## Technology Stack

### Frontend
- **HTML5**: Canvas API for rendering
- **CSS3**: Styling and responsive layout
- **Vanilla JavaScript**: No framework dependencies (lightweight)
  - Alternative: React/Vue if user prefers (requires build step)

### Backend (Optional)
- **Python**: Flask or FastAPI
- **CORS**: flask-cors or fastapi.middleware.cors
- **Caching**: functools.lru_cache for simulate_multiday results

### Testing
- **Python**: pytest for backend API tests
- **JavaScript**: Jest or Mocha for frontend unit tests
- **Integration**: Selenium or Playwright for E2E tests (optional)

### Build/Deploy
- **No build step**: Vanilla JS served statically
- **Local server**: `python -m http.server 8000`
- **Production**: GitHub Pages or Netlify (static hosting)

---

## Coverage Goals

**Target**: >85% test coverage across all visualization components

**Breakdown**:
- Canvas rendering: >90% (core functionality)
- State management: >95% (critical for UX)
- User controls: >85% (interaction handlers)
- Data binding: >90% (correctness essential)
- Integration: >80% (E2E scenarios)

**Tools**:
- Python: pytest-cov
- JavaScript: Jest coverage or nyc (Istanbul)

---

## Success Criteria

**Phase 4 RED Complete When**:
1. All test files created with comprehensive coverage
2. Self-documenting error messages for all assertions
3. Fixtures provide necessary test data
4. Tests are isolated and composable
5. No implementation code written (adversarial TDD)
6. Coverage >85% of planned visualization code

**Phase 4 GREEN Complete When**:
1. All visualization tests pass
2. Interactive canvas renders correctly
3. Animation plays smoothly through days 0-16
4. User controls work as specified
5. Statistics display accurately
6. Data binding matches OEIS A189191
7. Coverage >85% achieved
8. No regressions in Phases 1-3

**Phase 4 REFACTOR Complete When**:
1. Code quality issues from user review addressed
2. DRY violations eliminated
3. Performance optimized for large point sets
4. Documentation complete
5. All tests still pass (GREEN maintained)

---

## Phase 4 Challenges

### Challenge 1: Rendering 15 Trillion Points
**Problem**: g(16) = 15,730,302,251,147,551,048 blue points cannot be rendered in browser.

**Solution**:
- Days 1-10: Render all points (feasible, <32M points)
- Days 11-16: Render statistical sample (10K-100K points) with disclaimer
- Display: "Showing X of Y points (statistical sample)"
- Accuracy: Sample preserves spatial distribution

### Challenge 2: Coordinate Transformation Precision
**Problem**: Optimal configuration has large coordinate range (x: -3 to 4, y: -3 to 2).

**Solution**:
- Calculate bounding box from all points
- Scale to fit canvas with 10% padding
- Maintain aspect ratio (no distortion)
- Zoom/pan controls (optional enhancement)

### Challenge 3: Animation Performance
**Problem**: Redrawing millions of points each frame causes lag.

**Solution**:
- Incremental rendering: Add only new points during play
- RequestAnimationFrame for smooth 60 FPS
- Debounce state changes (prevent rapid updates)
- Web Workers for data loading (non-blocking)

### Challenge 4: Data Loading Strategy
**Problem**: Should we precompute all days or compute on-demand?

**Solution**: Hybrid approach
- Precompute days 1-10 (fast, <100ms each)
- Compute days 11-16 on-demand with loading indicator
- Cache results in memory (don't recompute)
- Optional: Persist to localStorage for session continuity

---

## Test-Writer Guidance

**Test-First Principles**:
1. Write tests for smallest testable units first (e.g., coordinate transformation)
2. Build up to integration tests (e.g., full animation cycle)
3. Use fixtures to avoid duplication
4. Each test should have single, clear assertion focus
5. Error messages must be self-documenting (include fix guidance)

**Adversarial TDD**:
- Test-writer has NO access to implementation code
- Tests describe BEHAVIOR, not implementation details
- Tests should pass with ANY correct implementation
- Avoid testing internal state (test observable outputs only)

**Isolation**:
- Each test should be runnable independently
- No test should depend on execution order
- Use fixtures for setup/teardown
- Mock external dependencies (canvas API, data loading)

---

## Coder Guidance

**Implementation Principles**:
1. Read error messages first (they contain fix guidance)
2. Implement minimal code to make tests pass (YAGNI)
3. Refactor only when tests are GREEN
4. No premature optimization
5. Follow DRY, SoC, FP principles from constitution

**Technology Constraints**:
- Vanilla JavaScript preferred (no framework unless user approves)
- ES6+ features allowed (const, arrow functions, classes)
- Browser compatibility: Modern browsers (Chrome, Firefox, Safari, Edge)
- No external dependencies (except optional testing frameworks)

**File Structure**:
```
prototypes/problem957/visualization/
├── index.html              # Main page
├── css/
│   └── style.css           # Styling
├── js/
│   ├── canvas.js           # Canvas rendering
│   ├── state.js            # State management
│   ├── controls.js         # User interaction
│   ├── data.js             # Data loading
│   └── main.js             # Application entry
├── data/
│   └── oeis_a189191.json   # Precomputed data (optional)
└── api/
    └── server.py           # Python backend (optional)
```

---

## Commit Strategy

**Phase 4 RED**: Single commit with all tests
```
Phase 4 RED: Interactive web visualization tests

WHY:
- Celebrate solving PE 957 (<100 people worldwide)
- Demonstrate TDD architecture on full-stack component
- Visualize geometric propagation producing OEIS A189191
- Provide interactive exploration of days 1-16

EXPECTED:
- 40+ tests covering canvas, state, controls, data
- Self-documenting error messages with fix guidance
- Fixtures for canvas mocking and data loading
- >85% coverage of planned visualization code
- Tests fail (no implementation exists yet)

REQ: REQ-PE957-VIZ-001 through REQ-PE957-VIZ-008
```

**Phase 4 GREEN**: Single commit with implementation
```
Phase 4 GREEN: Interactive web visualization implementation

WHY:
- Implement HTML5 canvas rendering for point propagation
- Create smooth animation controls for days 0-16
- Display statistics showing growth to g(16)
- Enable interactive exploration of optimal configuration

EXPECTED:
- All 40+ visualization tests pass
- Smooth animation through days 1-16
- Accurate rendering of OEIS A189191 sequence
- Responsive controls with keyboard shortcuts
- >85% test coverage achieved

REQ: REQ-PE957-VIZ-001 through REQ-PE957-VIZ-008
```

---

## BLOCKER STATUS: NONE ✅

**Dependencies Met**:
- Phase 3 simulate_multiday() ✓
- Optimal configuration coordinates ✓
- Complete OEIS A189191 sequence (g(1) through g(16)) ✓
- Code quality review complete ✓

**Ready for**:
1. M4 REFACTOR (address code quality issues first)
2. M4 RED Phase 4 (spawn test-writer subagent)

---

## NEXT STEPS

1. **M4 REFACTOR** (before Phase 4 RED):
   - Fix hash rounding strategy (integer quantization)
   - Consolidate Line.intersect duplication
   - Clarify color usage (white → blue conceptually)
   - Align requirement consistency (epsilon boundary)

2. **M4 RED Phase 4**:
   - Spawn test-writer subagent with this TSR
   - Generate comprehensive visualization tests
   - Achieve >85% planned coverage
   - Verify tests fail (no implementation yet)

3. **M4 GREEN Phase 4**:
   - Spawn coder subagent with failing tests
   - Implement HTML/CSS/JS visualization
   - Make all tests pass
   - Achieve >85% actual coverage

4. **M5 Final Validation**:
   - Complete POC metrics and documentation
   - Validate TDD architecture success
   - Constitutional amendment for CL12 v1.3
   - Celebrate solving PE 957 🎉

---

**End of TSR Phase 4**
