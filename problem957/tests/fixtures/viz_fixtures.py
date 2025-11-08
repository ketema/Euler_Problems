"""
Phase 4 Visualization Test Fixtures

Provides mock canvas, state management, and data loading fixtures
for testing visualization components in isolation.

Data Isolation (QS5): All fixtures ephemeral (no DOM, no actual canvas)
"""

import pytest
from typing import Dict, List, Tuple, Any, Set
from src.visualization import Viewport, transform_to_screen, calculate_viewport, get_sampling_disclaimer
from src.geometry import Point


# ============================================================================
# MOCK CANVAS CONTEXT
# ============================================================================

class MockCanvasContext:
    """Mock HTML5 Canvas 2D rendering context for testing.

    Tracks all drawing operations without actual rendering.
    Enables verification of correct canvas API calls.
    """

    def __init__(self):
        self.operations = []  # List of (method, args) tuples
        self.fillStyle = '#000000'
        self.strokeStyle = '#000000'
        self.lineWidth = 1
        self.globalAlpha = 1.0

    def fillRect(self, x, y, width, height):
        """Mock fillRect - records operation."""
        self.operations.append(('fillRect', (x, y, width, height)))

    def clearRect(self, x, y, width, height):
        """Mock clearRect - records operation."""
        self.operations.append(('clearRect', (x, y, width, height)))

    def beginPath(self):
        """Mock beginPath - records operation."""
        self.operations.append(('beginPath', ()))

    def arc(self, x, y, radius, startAngle, endAngle, counterclockwise=False):
        """Mock arc - records operation."""
        self.operations.append(('arc', (x, y, radius, startAngle, endAngle, counterclockwise)))

    def fill(self):
        """Mock fill - records operation."""
        self.operations.append(('fill', ()))

    def stroke(self):
        """Mock stroke - records operation."""
        self.operations.append(('stroke', ()))

    def moveTo(self, x, y):
        """Mock moveTo - records operation."""
        self.operations.append(('moveTo', (x, y)))

    def lineTo(self, x, y):
        """Mock lineTo - records operation."""
        self.operations.append(('lineTo', (x, y)))

    def clear_operations(self):
        """Clear recorded operations for next test."""
        self.operations = []

    def get_operations_by_type(self, operation_type):
        """Get all operations of specific type."""
        return [op for op in self.operations if op[0] == operation_type]

    def count_operations(self, operation_type):
        """Count operations of specific type."""
        return len(self.get_operations_by_type(operation_type))


# ============================================================================
# FIXTURES
# ============================================================================

@pytest.fixture
def mock_canvas():
    """Mock HTML5 canvas with 2D context.

    Returns dict with width, height, and mock context.
    No actual DOM rendering - purely for testing logic.
    """
    return {
        'width': 800,
        'height': 600,
        'context': MockCanvasContext()
    }


@pytest.fixture
def initial_viz_state():
    """Initial visualization state (ephemeral).

    Default state when visualization first loads.
    REQ-PE957-VIZ-002: State management requirements.
    """
    return {
        'currentDay': 0,
        'isPlaying': False,
        'speed': 2,  # Medium speed (1 second per day)
        'showLines': False,
        'showGrid': False,
        'maxDay': 16
    }


@pytest.fixture
def mock_optimal_config_data():
    """Mock optimal configuration point data (ephemeral).

    Subset of actual optimal config for testing rendering logic.
    Uses actual coordinates but limited point count for fast tests.
    """
    red_points = [
        {'x': -1.1420985748, 'y': -3.1278529420, 'color': 'red'},
        {'x': 1.7213348846, 'y': -0.8343651343, 'color': 'red'},
        {'x': 4.3760906863, 'y': 2.3859745813, 'color': 'red'}
    ]

    blue_points = [
        {'x': -1.8437265624, 'y': 1.4483260402, 'color': 'blue'},
        {'x': -1.0486909239, 'y': 2.1320688328, 'color': 'blue'}
    ]

    return {
        'red': red_points,
        'blue': blue_points,
        'bounds': {
            'min_x': -2.0,
            'max_x': 5.0,
            'min_y': -4.0,
            'max_y': 3.0
        }
    }


@pytest.fixture
def mock_oeis_sequence():
    """OEIS A189191 sequence data (ephemeral).

    Expected g(n) values for days 0-16.
    Used for data binding and statistics validation.
    """
    return {
        0: 2,
        1: 8,
        2: 28,
        3: 184,
        4: 1646,
        5: 19161,
        6: 261788,
        7: 4118024,
        8: 73099464,
        9: 1445724584,
        10: 31477742088,
        11: 750198126760,
        12: 19183422035784,
        13: 526224388301160,
        14: 15372370725513256,
        15: 477123999908405064,
        16: 15730302251147551048
    }


@pytest.fixture
def mock_multiday_data(mock_optimal_config_data, mock_oeis_sequence):
    """Mock multi-day point data for all days 0-16 (ephemeral).

    For days 0-10: Full point sets (feasible to render)
    For days 11-16: Sampled points (too many to render)

    Used for testing data loading and day transitions.
    """
    data = {}

    for day in range(17):
        if day == 0:
            # Initial configuration
            data[day] = {
                'day': day,
                'red': mock_optimal_config_data['red'],
                'blue': mock_optimal_config_data['blue'],
                'blue_count': mock_oeis_sequence[day],
                'is_sampled': False
            }
        elif day <= 10:
            # Full point sets for days 1-10
            # Note: In real implementation, would call simulate_multiday
            # For testing, just track counts
            data[day] = {
                'day': day,
                'red': mock_optimal_config_data['red'],
                'blue': [],  # Would contain actual points
                'blue_count': mock_oeis_sequence[day],
                'is_sampled': False
            }
        else:
            # Sampled point sets for days 11-16 (too large)
            data[day] = {
                'day': day,
                'red': mock_optimal_config_data['red'],
                'blue': [],  # Sampled subset
                'blue_count': mock_oeis_sequence[day],
                'is_sampled': True,
                'sample_size': 10000
            }

    return data


@pytest.fixture
def mock_user_controls():
    """Mock user control elements (ephemeral).

    Simulates button/slider state without actual DOM.
    Used for testing control interaction logic.
    """
    return {
        'playPauseButton': {'enabled': True, 'state': 'play'},
        'stepBackButton': {'enabled': False, 'state': 'disabled'},  # At day 0
        'stepForwardButton': {'enabled': True, 'state': 'enabled'},
        'resetButton': {'enabled': True, 'state': 'enabled'},
        'speedSlider': {'value': 2, 'min': 1, 'max': 5},
        'daySlider': {'value': 0, 'min': 0, 'max': 16},
        'showLinesCheckbox': {'checked': False},
        'showGridCheckbox': {'checked': False}
    }


@pytest.fixture
def coordinate_transform_test_cases():
    """Test cases for coordinate transformation (ephemeral).

    Mathematical coordinates → screen coordinates
    Canvas: 800x600, viewport: (-2, -4) to (5, 3)
    Origin at center, y-axis pointing up.
    """
    # Canvas dimensions
    canvas_width = 800
    canvas_height = 600

    # Mathematical viewport bounds
    min_x, max_x = -2.0, 5.0
    min_y, max_y = -4.0, 3.0

    # Calculate scale (pixels per unit)
    x_range = max_x - min_x  # 7.0
    y_range = max_y - min_y  # 7.0
    scale_x = canvas_width / x_range  # 800/7 ≈ 114.29
    scale_y = canvas_height / y_range  # 600/7 ≈ 85.71
    scale = min(scale_x, scale_y)  # Use smaller to fit both axes

    # Center of viewport in math coordinates
    center_x = (min_x + max_x) / 2  # 1.5
    center_y = (min_y + max_y) / 2  # -0.5

    test_cases = [
        {
            'name': 'origin',
            'math': (0.0, 0.0),
            'screen': (
                (0.0 - center_x) * scale + canvas_width / 2,
                (center_y - 0.0) * scale + canvas_height / 2
            )
        },
        {
            'name': 'top_left',
            'math': (min_x, max_y),
            'screen': (
                (min_x - center_x) * scale + canvas_width / 2,
                (center_y - max_y) * scale + canvas_height / 2
            )
        },
        {
            'name': 'bottom_right',
            'math': (max_x, min_y),
            'screen': (
                (max_x - center_x) * scale + canvas_width / 2,
                (center_y - min_y) * scale + canvas_height / 2
            )
        },
        {
            'name': 'center',
            'math': (center_x, center_y),
            'screen': (canvas_width / 2, canvas_height / 2)
        }
    ]

    return {
        'canvas_width': canvas_width,
        'canvas_height': canvas_height,
        'viewport': {'min_x': min_x, 'max_x': max_x, 'min_y': min_y, 'max_y': max_y},
        'scale': scale,
        'center': (center_x, center_y),
        'test_cases': test_cases
    }


@pytest.fixture
def speed_level_test_cases():
    """Speed level definitions for animation (ephemeral).

    REQ-PE957-VIZ-002: Speed levels 1-5
    Maps speed slider value to milliseconds per day.
    """
    return {
        1: {'name': 'slow', 'ms_per_day': 2000},
        2: {'name': 'medium', 'ms_per_day': 1000},
        3: {'name': 'fast', 'ms_per_day': 500},
        4: {'name': 'very_fast', 'ms_per_day': 250},
        5: {'name': 'instant', 'ms_per_day': 0}
    }


@pytest.fixture
def keyboard_shortcut_test_cases():
    """Keyboard shortcut mappings (ephemeral).

    REQ-PE957-VIZ-005: Keyboard shortcuts for controls.
    """
    return {
        'Space': {'action': 'togglePlayPause', 'description': 'Play/Pause animation'},
        'ArrowLeft': {'action': 'stepBackward', 'description': 'Step back one day'},
        'ArrowRight': {'action': 'stepForward', 'description': 'Step forward one day'},
        'KeyR': {'action': 'reset', 'description': 'Reset to day 0'},
        'KeyL': {'action': 'toggleLines', 'description': 'Toggle line visualization'},
        'KeyG': {'action': 'toggleGrid', 'description': 'Toggle grid overlay'}
    }
