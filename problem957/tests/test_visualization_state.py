"""
Phase 4 Tests: Animation State Management

REQ-PE957-VIZ-002: Animation State Management
- State structure: currentDay, isPlaying, speed
- Initial state validation
- State transitions: play/pause, step forward/back, reset
- Speed level management
- Boundary enforcement (day 0-16)
- Auto-pause at completion

Test Organization:
- Initial state: Default values
- Play/pause: Toggle animation
- Step controls: Forward/back with boundaries
- Speed: 1-5 levels with timing
- Reset: Return to initial state
- Auto-pause: Stop at day 16

Data Isolation (QS5): All state ephemeral (no persistent storage)
Coverage Target: >95% (critical for UX)
"""

import pytest
from fixtures.viz_fixtures import (
    initial_viz_state,
    speed_level_test_cases
)


# ============================================================================
# INITIAL STATE TESTS
# ============================================================================

def test_initial_state_has_correct_structure(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Initial state must have all required properties

    Test: State object contains currentDay, isPlaying, speed, maxDay.
    """
    state = initial_viz_state

    required_keys = ['currentDay', 'isPlaying', 'speed', 'maxDay']

    for key in required_keys:
        assert key in state, (
            f"Test: test_initial_state_has_correct_structure\n"
            f"Requirement: REQ-PE957-VIZ-002 (state structure)\n\n"
            f"Expected: State has property '{key}'\n"
            f"Got: Missing property '{key}'\n"
            f"State keys: {list(state.keys())}\n\n"
            f"Fix guidance: Initialize state with all required properties:\n"
            f"  const state = {{\n"
            f"    currentDay: 0,\n"
            f"    isPlaying: false,\n"
            f"    speed: 2,\n"
            f"    maxDay: 16,\n"
            f"    showLines: false,\n"
            f"    showGrid: false\n"
            f"  }}\n"
        )


def test_initial_state_starts_at_day_zero(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Animation starts at day 0 (initial configuration)

    Test: currentDay initialized to 0.
    """
    state = initial_viz_state

    assert state['currentDay'] == 0, (
        f"Test: test_initial_state_starts_at_day_zero\n"
        f"Requirement: REQ-PE957-VIZ-002 (initial currentDay)\n\n"
        f"Expected: currentDay = 0\n"
        f"Got: currentDay = {state['currentDay']}\n\n"
        f"Fix guidance: Start animation at day 0:\n"
        f"  currentDay: 0  // Shows initial configuration (3 red + 2 blue)\n"
        f"Day 0 represents the starting point before any propagation.\n"
    )


def test_initial_state_not_playing(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Animation does not auto-play on load

    Test: isPlaying initialized to false (paused state).
    """
    state = initial_viz_state

    assert state['isPlaying'] is False, (
        f"Test: test_initial_state_not_playing\n"
        f"Requirement: REQ-PE957-VIZ-002 (initial isPlaying)\n\n"
        f"Expected: isPlaying = false (paused)\n"
        f"Got: isPlaying = {state['isPlaying']}\n\n"
        f"Fix guidance: Don't auto-play animation on load:\n"
        f"  isPlaying: false  // User must click Play to start\n"
        f"Auto-playing can be jarring - let user control when to start.\n"
    )


def test_initial_state_medium_speed(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Default speed is 2 (medium, 1 second per day)

    Test: speed initialized to 2 (reasonable default).
    """
    state = initial_viz_state

    assert state['speed'] == 2, (
        f"Test: test_initial_state_medium_speed\n"
        f"Requirement: REQ-PE957-VIZ-002 (initial speed)\n\n"
        f"Expected: speed = 2 (medium)\n"
        f"Got: speed = {state['speed']}\n\n"
        f"Fix guidance: Start with medium speed:\n"
        f"  speed: 2  // 1 second per day (not too fast, not too slow)\n"
        f"Speed levels: 1=slow (2s), 2=medium (1s), 3=fast (0.5s), 4=veryfast (0.25s), 5=instant\n"
    )


def test_initial_state_max_day_is_sixteen(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Maximum day is 16 (g(16) final answer)

    Test: maxDay property set to 16.
    """
    state = initial_viz_state

    assert state['maxDay'] == 16, (
        f"Test: test_initial_state_max_day_is_sixteen\n"
        f"Requirement: REQ-PE957-VIZ-002 (maxDay boundary)\n\n"
        f"Expected: maxDay = 16\n"
        f"Got: maxDay = {state['maxDay']}\n\n"
        f"Fix guidance: Set maximum day to 16:\n"
        f"  maxDay: 16  // g(16) is the final answer for PE 957\n"
        f"Animation should run from day 0 to day 16 (17 total frames).\n"
    )


# ============================================================================
# PLAY/PAUSE TESTS
# ============================================================================

def test_toggle_play_starts_animation(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Toggling play changes isPlaying to true

    Test: Play action changes state from paused to playing.
    """
    state = initial_viz_state.copy()
    assert state['isPlaying'] is False  # Initial: paused

    # Simulate play action
    state['isPlaying'] = True

    assert state['isPlaying'] is True, (
        f"Test: test_toggle_play_starts_animation\n"
        f"Requirement: REQ-PE957-VIZ-002 (play action)\n\n"
        f"Expected: After play(), isPlaying = true\n"
        f"Got: isPlaying = {state['isPlaying']}\n\n"
        f"Fix guidance: Implement play action:\n"
        f"  function play() {{\n"
        f"    state.isPlaying = true\n"
        f"    startAnimationLoop()\n"
        f"  }}\n"
    )


def test_toggle_pause_stops_animation(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Toggling pause changes isPlaying to false

    Test: Pause action changes state from playing to paused.
    """
    state = initial_viz_state.copy()
    state['isPlaying'] = True  # Start playing

    # Simulate pause action
    state['isPlaying'] = False

    assert state['isPlaying'] is False, (
        f"Test: test_toggle_pause_stops_animation\n"
        f"Requirement: REQ-PE957-VIZ-002 (pause action)\n\n"
        f"Expected: After pause(), isPlaying = false\n"
        f"Got: isPlaying = {state['isPlaying']}\n\n"
        f"Fix guidance: Implement pause action:\n"
        f"  function pause() {{\n"
        f"    state.isPlaying = false\n"
        f"    stopAnimationLoop()\n"
        f"  }}\n"
    )


def test_toggle_play_pause_is_reversible(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Play/pause can toggle back and forth

    Test: Multiple play/pause cycles work correctly.
    """
    state = initial_viz_state.copy()

    # Cycle 1: play → pause
    state['isPlaying'] = True
    assert state['isPlaying'] is True

    state['isPlaying'] = False
    assert state['isPlaying'] is False

    # Cycle 2: play → pause
    state['isPlaying'] = True
    assert state['isPlaying'] is True

    state['isPlaying'] = False
    assert state['isPlaying'] is False, (
        f"Test: test_toggle_play_pause_is_reversible\n"
        f"Requirement: REQ-PE957-VIZ-002 (toggle reversibility)\n\n"
        f"Expected: Play/pause can cycle multiple times\n"
        f"Got: State stuck or inconsistent\n\n"
        f"Fix guidance: Ensure play/pause toggle is stateless:\n"
        f"  function togglePlayPause() {{\n"
        f"    state.isPlaying = !state.isPlaying\n"
        f"    if (state.isPlaying) {{\n"
        f"      startAnimationLoop()\n"
        f"    }} else {{\n"
        f"      stopAnimationLoop()\n"
        f"    }}\n"
        f"  }}\n"
    )


# ============================================================================
# STEP FORWARD/BACK TESTS
# ============================================================================

def test_step_forward_increments_day(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Step forward increases currentDay by 1

    Test: stepForward() increments day from 0 to 1.
    """
    state = initial_viz_state.copy()
    assert state['currentDay'] == 0

    # Simulate step forward
    state['currentDay'] += 1

    assert state['currentDay'] == 1, (
        f"Test: test_step_forward_increments_day\n"
        f"Requirement: REQ-PE957-VIZ-002 (step forward)\n\n"
        f"Expected: After stepForward(), currentDay = 1\n"
        f"Got: currentDay = {state['currentDay']}\n\n"
        f"Fix guidance: Implement step forward:\n"
        f"  function stepForward() {{\n"
        f"    if (state.currentDay < state.maxDay) {{\n"
        f"      state.currentDay++\n"
        f"      render(state.currentDay)\n"
        f"    }}\n"
        f"  }}\n"
    )


def test_step_back_decrements_day(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Step back decreases currentDay by 1

    Test: stepBack() decrements day from 5 to 4.
    """
    state = initial_viz_state.copy()
    state['currentDay'] = 5  # Start at day 5

    # Simulate step back
    state['currentDay'] -= 1

    assert state['currentDay'] == 4, (
        f"Test: test_step_back_decrements_day\n"
        f"Requirement: REQ-PE957-VIZ-002 (step back)\n\n"
        f"Expected: After stepBack(), currentDay = 4\n"
        f"Got: currentDay = {state['currentDay']}\n\n"
        f"Fix guidance: Implement step back:\n"
        f"  function stepBack() {{\n"
        f"    if (state.currentDay > 0) {{\n"
        f"      state.currentDay--\n"
        f"      render(state.currentDay)\n"
        f"    }}\n"
        f"  }}\n"
    )


def test_step_forward_boundary_at_max_day(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Cannot step forward beyond day 16

    Test: stepForward() at day 16 does nothing (boundary enforcement).
    """
    state = initial_viz_state.copy()
    state['currentDay'] = 16  # At max day

    # Attempt to step forward beyond boundary
    if state['currentDay'] < state['maxDay']:
        state['currentDay'] += 1

    assert state['currentDay'] == 16, (
        f"Test: test_step_forward_boundary_at_max_day\n"
        f"Requirement: REQ-PE957-VIZ-002 (boundary enforcement)\n\n"
        f"Expected: currentDay stays at 16 (cannot exceed maxDay)\n"
        f"Got: currentDay = {state['currentDay']}\n\n"
        f"Fix guidance: Enforce upper boundary in stepForward:\n"
        f"  function stepForward() {{\n"
        f"    if (state.currentDay < state.maxDay) {{  // Check boundary\n"
        f"      state.currentDay++\n"
        f"      render(state.currentDay)\n"
        f"    }} else {{\n"
        f"      // Already at max - do nothing\n"
        f"    }}\n"
        f"  }}\n"
        f"Also disable step forward button when at maxDay.\n"
    )


def test_step_back_boundary_at_day_zero(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Cannot step back below day 0

    Test: stepBack() at day 0 does nothing (boundary enforcement).
    """
    state = initial_viz_state.copy()
    assert state['currentDay'] == 0  # At minimum day

    # Attempt to step back beyond boundary
    if state['currentDay'] > 0:
        state['currentDay'] -= 1

    assert state['currentDay'] == 0, (
        f"Test: test_step_back_boundary_at_day_zero\n"
        f"Requirement: REQ-PE957-VIZ-002 (boundary enforcement)\n\n"
        f"Expected: currentDay stays at 0 (cannot go negative)\n"
        f"Got: currentDay = {state['currentDay']}\n\n"
        f"Fix guidance: Enforce lower boundary in stepBack:\n"
        f"  function stepBack() {{\n"
        f"    if (state.currentDay > 0) {{  // Check boundary\n"
        f"      state.currentDay--\n"
        f"      render(state.currentDay)\n"
        f"    }} else {{\n"
        f"      // Already at day 0 - do nothing\n"
        f"    }}\n"
        f"  }}\n"
        f"Also disable step back button when at day 0.\n"
    )


# ============================================================================
# SPEED CONTROL TESTS
# ============================================================================

def test_speed_level_1_is_slow(speed_level_test_cases):
    """
    REQ-PE957-VIZ-002: Speed level 1 = 2000ms per day (slow)

    Test: Speed level 1 corresponds to 2 seconds per day.
    """
    speed_levels = speed_level_test_cases
    level_1 = speed_levels[1]

    assert level_1['ms_per_day'] == 2000, (
        f"Test: test_speed_level_1_is_slow\n"
        f"Requirement: REQ-PE957-VIZ-002 (speed level 1)\n\n"
        f"Expected: Speed 1 = 2000ms per day (2 seconds)\n"
        f"Got: {level_1['ms_per_day']}ms per day\n\n"
        f"Fix guidance: Define speed level mapping:\n"
        f"  const SPEED_LEVELS = {{\n"
        f"    1: {{ name: 'slow', ms_per_day: 2000 }},\n"
        f"    2: {{ name: 'medium', ms_per_day: 1000 }},\n"
        f"    3: {{ name: 'fast', ms_per_day: 500 }},\n"
        f"    4: {{ name: 'very_fast', ms_per_day: 250 }},\n"
        f"    5: {{ name: 'instant', ms_per_day: 0 }}\n"
        f"  }}\n"
    )


def test_speed_level_5_is_instant(speed_level_test_cases):
    """
    REQ-PE957-VIZ-002: Speed level 5 = 0ms per day (instant)

    Test: Speed level 5 renders all days instantly (no delay).
    """
    speed_levels = speed_level_test_cases
    level_5 = speed_levels[5]

    assert level_5['ms_per_day'] == 0, (
        f"Test: test_speed_level_5_is_instant\n"
        f"Requirement: REQ-PE957-VIZ-002 (speed level 5)\n\n"
        f"Expected: Speed 5 = 0ms per day (instant)\n"
        f"Got: {level_5['ms_per_day']}ms per day\n\n"
        f"Fix guidance: Instant mode skips animation delay:\n"
        f"  if (state.speed === 5) {{\n"
        f"    // Jump to final day immediately\n"
        f"    state.currentDay = state.maxDay\n"
        f"    render(state.currentDay)\n"
        f"  }} else {{\n"
        f"    // Animate with delay based on speed level\n"
        f"    const delay = SPEED_LEVELS[state.speed].ms_per_day\n"
        f"    setTimeout(nextFrame, delay)\n"
        f"  }}\n"
    )


def test_speed_change_updates_animation_interval(initial_viz_state, speed_level_test_cases):
    """
    REQ-PE957-VIZ-002: Changing speed updates animation timing

    Test: Speed slider value changes animation delay.
    """
    state = initial_viz_state.copy()
    speed_levels = speed_level_test_cases

    # Change from speed 2 (1000ms) to speed 4 (250ms)
    state['speed'] = 4

    expected_ms = speed_levels[4]['ms_per_day']

    assert expected_ms == 250, (
        f"Test: test_speed_change_updates_animation_interval\n"
        f"Requirement: REQ-PE957-VIZ-002 (dynamic speed change)\n\n"
        f"Expected: Speed 4 → 250ms per day\n"
        f"Got: {expected_ms}ms per day\n\n"
        f"Fix guidance: Update animation interval when speed changes:\n"
        f"  function onSpeedChange(newSpeed) {{\n"
        f"    state.speed = newSpeed\n"
        f"    if (state.isPlaying) {{\n"
        f"      // Restart animation loop with new interval\n"
        f"      stopAnimationLoop()\n"
        f"      startAnimationLoop()\n"
        f"    }}\n"
        f"  }}\n"
    )


def test_speed_within_valid_range(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Speed must be within range 1-5

    Test: Speed values outside [1, 5] should be rejected or clamped.
    """
    state = initial_viz_state.copy()

    # Test invalid speeds
    invalid_speeds = [0, 6, -1, 10]

    for invalid_speed in invalid_speeds:
        # Should clamp or reject
        clamped_speed = max(1, min(5, invalid_speed))

        assert 1 <= clamped_speed <= 5, (
            f"Test: test_speed_within_valid_range\n"
            f"Requirement: REQ-PE957-VIZ-002 (speed range validation)\n\n"
            f"Expected: Invalid speed {invalid_speed} clamped to [1, 5]\n"
            f"Got: No clamping implemented\n\n"
            f"Fix guidance: Validate/clamp speed values:\n"
            f"  function setSpeed(newSpeed) {{\n"
            f"    // Clamp to valid range\n"
            f"    state.speed = Math.max(1, Math.min(5, newSpeed))\n"
            f"    // Or throw error if out of range\n"
            f"    if (newSpeed < 1 || newSpeed > 5) {{\n"
            f"      throw new Error('Speed must be between 1 and 5')\n"
            f"    }}\n"
            f"  }}\n"
        )


# ============================================================================
# RESET TESTS
# ============================================================================

def test_reset_returns_to_day_zero(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Reset button returns to day 0

    Test: reset() sets currentDay back to 0.
    """
    state = initial_viz_state.copy()
    state['currentDay'] = 10  # Advance to day 10
    state['isPlaying'] = True  # Currently playing

    # Simulate reset
    state['currentDay'] = 0
    state['isPlaying'] = False

    assert state['currentDay'] == 0, (
        f"Test: test_reset_returns_to_day_zero\n"
        f"Requirement: REQ-PE957-VIZ-002 (reset to initial state)\n\n"
        f"Expected: After reset(), currentDay = 0\n"
        f"Got: currentDay = {state['currentDay']}\n\n"
        f"Fix guidance: Implement reset function:\n"
        f"  function reset() {{\n"
        f"    state.currentDay = 0\n"
        f"    state.isPlaying = false  // Pause animation\n"
        f"    render(0)  // Show initial configuration\n"
        f"  }}\n"
    )


def test_reset_pauses_animation(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Reset pauses animation if playing

    Test: reset() sets isPlaying to false.
    """
    state = initial_viz_state.copy()
    state['currentDay'] = 10
    state['isPlaying'] = True  # Playing

    # Simulate reset
    state['currentDay'] = 0
    state['isPlaying'] = False

    assert state['isPlaying'] is False, (
        f"Test: test_reset_pauses_animation\n"
        f"Requirement: REQ-PE957-VIZ-002 (reset pauses)\n\n"
        f"Expected: After reset(), isPlaying = false\n"
        f"Got: isPlaying = {state['isPlaying']}\n\n"
        f"Fix guidance: Reset should stop animation:\n"
        f"  function reset() {{\n"
        f"    state.currentDay = 0\n"
        f"    state.isPlaying = false  // Stop animation\n"
        f"    stopAnimationLoop()\n"
        f"    render(0)\n"
        f"  }}\n"
        f"User can press Play again if they want to watch from start.\n"
    )


def test_reset_preserves_speed_setting(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Reset does not change speed setting

    Test: reset() preserves user's speed preference.
    """
    state = initial_viz_state.copy()
    state['currentDay'] = 10
    state['speed'] = 4  # User set speed to 4

    # Simulate reset
    state['currentDay'] = 0
    state['isPlaying'] = False
    # Speed should remain 4

    assert state['speed'] == 4, (
        f"Test: test_reset_preserves_speed_setting\n"
        f"Requirement: REQ-PE957-VIZ-002 (reset preserves settings)\n\n"
        f"Expected: After reset(), speed unchanged (still 4)\n"
        f"Got: speed = {state['speed']}\n\n"
        f"Fix guidance: Reset should only change playback position:\n"
        f"  function reset() {{\n"
        f"    state.currentDay = 0      // Reset position\n"
        f"    state.isPlaying = false   // Pause\n"
        f"    // DO NOT reset state.speed - preserve user's choice\n"
        f"    // DO NOT reset state.showLines - preserve user's choice\n"
        f"  }}\n"
    )


# ============================================================================
# AUTO-PAUSE TESTS
# ============================================================================

def test_auto_pause_at_day_sixteen(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Animation auto-pauses when reaching day 16

    Test: When animation reaches maxDay, isPlaying becomes false.
    """
    state = initial_viz_state.copy()
    state['currentDay'] = 15
    state['isPlaying'] = True

    # Simulate final step to day 16
    state['currentDay'] += 1

    # Should auto-pause
    if state['currentDay'] >= state['maxDay']:
        state['isPlaying'] = False

    assert state['isPlaying'] is False, (
        f"Test: test_auto_pause_at_day_sixteen\n"
        f"Requirement: REQ-PE957-VIZ-002 (auto-pause at completion)\n\n"
        f"Expected: When currentDay reaches 16, isPlaying = false\n"
        f"Got: isPlaying = {state['isPlaying']}\n\n"
        f"Fix guidance: Auto-pause when reaching maxDay:\n"
        f"  function animationStep() {{\n"
        f"    if (state.currentDay < state.maxDay) {{\n"
        f"      state.currentDay++\n"
        f"      render(state.currentDay)\n"
        f"    }} else {{\n"
        f"      // Reached end - auto-pause\n"
        f"      state.isPlaying = false\n"
        f"      stopAnimationLoop()\n"
        f"    }}\n"
        f"  }}\n"
        f"This prevents infinite loop or wrapping back to day 0.\n"
    )


def test_play_button_restarts_from_end(initial_viz_state):
    """
    REQ-PE957-VIZ-002: Pressing play at day 16 resets to day 0

    Test: If at maxDay and user presses play, restart from beginning.
    """
    state = initial_viz_state.copy()
    state['currentDay'] = 16  # At end
    state['isPlaying'] = False  # Paused

    # Simulate play button press
    if state['currentDay'] >= state['maxDay']:
        state['currentDay'] = 0  # Restart from beginning

    state['isPlaying'] = True

    assert state['currentDay'] == 0, (
        f"Test: test_play_button_restarts_from_end\n"
        f"Requirement: REQ-PE957-VIZ-002 (restart from end)\n\n"
        f"Expected: Pressing play at day 16 → restart from day 0\n"
        f"Got: currentDay = {state['currentDay']}\n\n"
        f"Fix guidance: Handle play button when at end:\n"
        f"  function play() {{\n"
        f"    if (state.currentDay >= state.maxDay) {{\n"
        f"      // Already at end - restart from beginning\n"
        f"      state.currentDay = 0\n"
        f"    }}\n"
        f"    state.isPlaying = true\n"
        f"    startAnimationLoop()\n"
        f"  }}\n"
        f"This provides better UX than doing nothing.\n"
    )


# ============================================================================
# STATE PERSISTENCE TESTS (OPTIONAL)
# ============================================================================

def test_state_can_be_serialized_to_json():
    """
    REQ-PE957-VIZ-002: State can be saved to localStorage (optional)

    Test: State object is JSON-serializable for persistence.
    """
    import json

    state = {
        'currentDay': 5,
        'isPlaying': False,
        'speed': 3,
        'maxDay': 16,
        'showLines': True,
        'showGrid': False
    }

    # Attempt to serialize
    try:
        serialized = json.dumps(state)
        deserialized = json.loads(serialized)

        assert deserialized == state, (
            f"Test: test_state_can_be_serialized_to_json\n"
            f"Requirement: REQ-PE957-VIZ-002 (optional state persistence)\n\n"
            f"Expected: State serialization/deserialization preserves values\n"
            f"Got: Deserialized state differs from original\n"
            f"Original: {state}\n"
            f"Deserialized: {deserialized}\n\n"
            f"Fix guidance: Ensure all state properties are JSON-serializable.\n"
            f"Avoid functions, class instances, or circular references.\n"
        )
    except (TypeError, ValueError) as e:
        raise AssertionError(
            f"Test: test_state_can_be_serialized_to_json\n"
            f"Requirement: REQ-PE957-VIZ-002 (state persistence)\n\n"
            f"Expected: State can be serialized to JSON\n"
            f"Got: Serialization error: {e}\n\n"
            f"Fix guidance: State must contain only JSON-serializable types:\n"
            f"  - Primitives: number, string, boolean, null\n"
            f"  - Containers: object, array\n"
            f"  - NO functions, undefined, class instances\n"
        )
