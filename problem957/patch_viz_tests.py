#!/usr/bin/env python3
"""
Patch visualization tests to wire up function calls.

The test-writer (adversarial TDD) left `None` placeholders expecting
the implementation phase to wire up actual function calls.
"""

import re

# Add import to test_visualization_rendering.py
rendering_import_old = """import pytest
import math
from fixtures.viz_fixtures import (
    mock_canvas,
    mock_optimal_config_data,
    coordinate_transform_test_cases
)"""

rendering_import_new = """import pytest
import math
from fixtures.viz_fixtures import (
    mock_canvas,
    mock_optimal_config_data,
    coordinate_transform_test_cases
)
from src.visualization import Viewport, transform_to_screen, calculate_viewport, get_sampling_disclaimer
from src.geometry import Point"""

# Patch test_coordinate_transform_origin_to_screen
test_origin_old = """    # This will fail until coordinate transformation is implemented
    actual_screen_x = None
    actual_screen_y = None"""

test_origin_new = """    # Call implementation
    viewport_dict = coordinate_transform_test_cases['viewport']
    viewport = Viewport(**viewport_dict)
    canvas_width = coordinate_transform_test_cases['canvas_width']
    canvas_height = coordinate_transform_test_cases['canvas_height']

    actual_screen_x, actual_screen_y = transform_to_screen(
        math_x, math_y, viewport, canvas_width, canvas_height
    )"""

# Patch test_coordinate_transform_y_axis_inverted
test_yaxis_old = """    # Expected: low y (math) → high y (screen), high y (math) → low y (screen)
    # This will fail until implemented
    screen_low = None
    screen_high = None"""

test_yaxis_new = """    # Expected: low y (math) → high y (screen), high y (math) → low y (screen)
    viewport_dict = coordinate_transform_test_cases['viewport']
    viewport = Viewport(**viewport_dict)
    canvas_width = coordinate_transform_test_cases['canvas_width']
    canvas_height = coordinate_transform_test_cases['canvas_height']

    screen_low = transform_to_screen(point_low[0], point_low[1], viewport, canvas_width, canvas_height)
    screen_high = transform_to_screen(point_high[0], point_high[1], viewport, canvas_width, canvas_height)"""

# Apply patches
def apply_patches():
    # Patch test_visualization_rendering.py
    with open('tests/test_visualization_rendering.py', 'r') as f:
        content = f.read()

    content = content.replace(rendering_import_old, rendering_import_new)
    content = content.replace(test_origin_old, test_origin_new)
    content = content.replace(test_yaxis_old, test_yaxis_new)

    # Patch other coordinate transform tests
    # test_coordinate_transform_maintains_aspect_ratio
    content = re.sub(
        r'# This will fail until aspect ratio is implemented\s+actual_screen_x = None\s+actual_screen_y = None',
        '''viewport_dict = coordinate_transform_test_cases['viewport']
    viewport = Viewport(**viewport_dict)
    canvas_width = coordinate_transform_test_cases['canvas_width']
    canvas_height = coordinate_transform_test_cases['canvas_height']

    actual_screen_x, actual_screen_y = transform_to_screen(
        test_point[0], test_point[1], viewport, canvas_width, canvas_height
    )''',
        content
    )

    # test_coordinate_transform_all_corner_cases
    content = re.sub(
        r'# Test all cases\s+for test_case in test_cases:\s+actual_screen = None',
        '''# Test all cases
    viewport_dict = coordinate_transform_test_cases['viewport']
    viewport = Viewport(**viewport_dict)
    canvas_width = coordinate_transform_test_cases['canvas_width']
    canvas_height = coordinate_transform_test_cases['canvas_height']

    for test_case in test_cases:
        math_x, math_y = test_case['math']
        actual_screen = transform_to_screen(math_x, math_y, viewport, canvas_width, canvas_height)''',
        content
    )

    # test_viewport_calculates_bounds_from_points
    content = re.sub(
        r'# This will fail until viewport calculation is implemented\s+actual_viewport = None',
        '''# Call implementation
    actual_viewport = calculate_viewport(test_points, padding_percent=0.1)''',
        content
    )

    # test_viewport_adds_padding_around_points
    content = re.sub(
        r'# Calculate with different padding\s+viewport_5pct = None\s+viewport_20pct = None',
        '''# Calculate with different padding
    viewport_5pct = calculate_viewport(test_points, padding_percent=0.05)
    viewport_20pct = calculate_viewport(test_points, padding_percent=0.2)''',
        content
    )

    with open('tests/test_visualization_rendering.py', 'w') as f:
        f.write(content)

    # Patch test_visualization_data.py for sampling disclaimer
    with open('tests/test_visualization_data.py', 'r') as f:
        content = f.read()

    # Add import
    content = re.sub(
        r'(import pytest\nimport math)',
        r'\1\nfrom src.visualization import get_sampling_disclaimer',
        content
    )

    # Patch test_sampling_disclaimer_displayed_for_large_days
    content = re.sub(
        r'# This will fail until sampling disclaimer is implemented\s+disclaimer = None',
        '''# Call implementation
    disclaimer = get_sampling_disclaimer(day=15, total_points=total_points, sampled_points=sampled_points)''',
        content
    )

    with open('tests/test_visualization_data.py', 'w') as f:
        f.write(content)

    print("✓ All 7 tests patched successfully!")
    print("  - Added imports to test files")
    print("  - Wired up 7 function calls to replace None placeholders")
    print("\nRun: pytest tests/test_visualization_*.py to verify")

if __name__ == '__main__':
    apply_patches()
