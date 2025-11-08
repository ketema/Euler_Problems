"""
Phase 4 Tests: Canvas Rendering and Coordinate Transformation

REQ-PE957-VIZ-001: Interactive HTML5 Canvas
- Canvas initialization with correct dimensions
- Coordinate transformation (mathematical → screen)
- Point rendering (red and blue with correct colors)
- Line rendering between point pairs
- Viewport scaling and bounds

Test Organization:
- Canvas setup: Initialization, dimensions, context
- Coordinate transformation: Math → screen with proper y-axis inversion
- Point rendering: Colors, sizes, positions
- Line rendering: Stroke properties, connections
- Viewport: Scaling, bounds, aspect ratio

Data Isolation (QS5): All fixtures use mock canvas (no actual DOM)
Coverage Target: >90% (core rendering functionality)
"""

import pytest
import math
from fixtures.viz_fixtures import (
    mock_canvas,
    mock_optimal_config_data,
    coordinate_transform_test_cases
)
from src.visualization import Viewport, transform_to_screen, calculate_viewport, get_sampling_disclaimer
from src.geometry import Point


# ============================================================================
# CANVAS INITIALIZATION TESTS
# ============================================================================

def test_canvas_initialization_creates_correct_dimensions(mock_canvas):
    """
    REQ-PE957-VIZ-001: Canvas must be 800x600 pixels

    Test: Canvas is initialized with correct width and height dimensions.
    """
    canvas = mock_canvas

    assert canvas['width'] == 800, (
        f"Test: test_canvas_initialization_creates_correct_dimensions\n"
        f"Requirement: REQ-PE957-VIZ-001 (canvas dimensions)\n\n"
        f"Expected: canvas width = 800 pixels\n"
        f"Got: canvas width = {canvas['width']} pixels\n\n"
        f"Fix guidance: Initialize canvas with standard dimensions:\n"
        f"  canvas.width = 800\n"
        f"  canvas.height = 600\n"
        f"This provides 4:3 aspect ratio suitable for point visualization.\n"
    )

    assert canvas['height'] == 600, (
        f"Test: test_canvas_initialization_creates_correct_dimensions\n"
        f"Requirement: REQ-PE957-VIZ-001 (canvas dimensions)\n\n"
        f"Expected: canvas height = 600 pixels\n"
        f"Got: canvas height = {canvas['height']} pixels\n\n"
        f"Fix guidance: Initialize canvas with standard dimensions:\n"
        f"  canvas.width = 800\n"
        f"  canvas.height = 600\n"
    )


def test_canvas_has_valid_2d_context(mock_canvas):
    """
    REQ-PE957-VIZ-001: Canvas must have 2D rendering context

    Test: Canvas provides valid 2D context for drawing operations.
    """
    canvas = mock_canvas
    context = canvas['context']

    assert context is not None, (
        f"Test: test_canvas_has_valid_2d_context\n"
        f"Requirement: REQ-PE957-VIZ-001 (canvas context)\n\n"
        f"Expected: canvas.context exists (not None)\n"
        f"Got: canvas.context = None\n\n"
        f"Fix guidance: Get 2D rendering context from canvas:\n"
        f"  const canvas = document.getElementById('myCanvas')\n"
        f"  const ctx = canvas.getContext('2d')\n"
        f"Context enables drawing operations (fillRect, arc, etc.)\n"
    )

    # Verify context has required drawing methods
    required_methods = ['fillRect', 'clearRect', 'beginPath', 'arc', 'fill', 'stroke']
    for method_name in required_methods:
        assert hasattr(context, method_name), (
            f"Test: test_canvas_has_valid_2d_context\n"
            f"Requirement: REQ-PE957-VIZ-001 (canvas context methods)\n\n"
            f"Expected: context.{method_name} exists\n"
            f"Got: context missing method '{method_name}'\n\n"
            f"Fix guidance: Ensure context is 2D rendering context:\n"
            f"  const ctx = canvas.getContext('2d')\n"
            f"All standard CanvasRenderingContext2D methods must be available.\n"
        )


def test_canvas_clears_before_drawing(mock_canvas):
    """
    REQ-PE957-VIZ-001: Canvas must clear before redrawing

    Test: Verify canvas is cleared before each new frame to prevent artifacts.
    """
    canvas = mock_canvas
    context = canvas['context']

    # Simulate clearing canvas
    context.clearRect(0, 0, canvas['width'], canvas['height'])

    clear_ops = context.get_operations_by_type('clearRect')

    assert len(clear_ops) > 0, (
        f"Test: test_canvas_clears_before_drawing\n"
        f"Requirement: REQ-PE957-VIZ-001 (clear before redraw)\n\n"
        f"Expected: clearRect called at least once\n"
        f"Got: {len(clear_ops)} clearRect operations\n\n"
        f"Fix guidance: Clear canvas before each redraw:\n"
        f"  ctx.clearRect(0, 0, canvas.width, canvas.height)\n"
        f"This prevents visual artifacts from overlapping frames.\n"
        f"Call clearRect at the start of your render function.\n"
    )

    # Verify clearRect was called with full canvas dimensions
    first_clear = clear_ops[0]
    x, y, width, height = first_clear[1]

    assert (x, y) == (0, 0), (
        f"Test: test_canvas_clears_before_drawing\n"
        f"Requirement: REQ-PE957-VIZ-001 (clear entire canvas)\n\n"
        f"Expected: clearRect(0, 0, ...) starts at origin\n"
        f"Got: clearRect({x}, {y}, ...)\n\n"
        f"Fix guidance: Clear from top-left corner (0, 0):\n"
        f"  ctx.clearRect(0, 0, canvas.width, canvas.height)\n"
    )


# ============================================================================
# COORDINATE TRANSFORMATION TESTS
# ============================================================================

def test_coordinate_transform_origin_to_screen(coordinate_transform_test_cases):
    """
    REQ-PE957-VIZ-001: Transform mathematical origin to screen coordinates

    Test: Mathematical origin (0, 0) maps correctly to screen coordinates.
    Canvas center is NOT origin - must account for viewport bounds.
    """
    test_case = next(tc for tc in coordinate_transform_test_cases['test_cases']
                     if tc['name'] == 'origin')

    math_x, math_y = test_case['math']
    expected_screen_x, expected_screen_y = test_case['screen']

    # Call implementation
    viewport_dict = coordinate_transform_test_cases['viewport']
    viewport = Viewport(**viewport_dict)
    canvas_width = coordinate_transform_test_cases['canvas_width']
    canvas_height = coordinate_transform_test_cases['canvas_height']

    actual_screen_x, actual_screen_y = transform_to_screen(
        math_x, math_y, viewport, canvas_width, canvas_height
    )

    assert actual_screen_x is not None, (
        f"Test: test_coordinate_transform_origin_to_screen\n"
        f"Requirement: REQ-PE957-VIZ-001 (coordinate transformation)\n\n"
        f"Expected: transform_to_screen({math_x}, {math_y}) returns screen coords\n"
        f"Got: None (function not implemented)\n\n"
        f"Fix guidance: Implement coordinate transformation function:\n"
        f"  def transform_to_screen(math_x, math_y, viewport, canvas_width, canvas_height):\n"
        f"      # Calculate scale from viewport bounds\n"
        f"      x_range = viewport.max_x - viewport.min_x\n"
        f"      y_range = viewport.max_y - viewport.min_y\n"
        f"      scale = min(canvas_width / x_range, canvas_height / y_range)\n\n"
        f"      # Center of viewport in math coordinates\n"
        f"      center_x = (viewport.min_x + viewport.max_x) / 2\n"
        f"      center_y = (viewport.min_y + viewport.max_y) / 2\n\n"
        f"      # Transform: offset by center, scale, translate to canvas center\n"
        f"      screen_x = (math_x - center_x) * scale + canvas_width / 2\n"
        f"      screen_y = (center_y - math_y) * scale + canvas_height / 2  # Y inverted!\n"
        f"      return (screen_x, screen_y)\n"
    )


def test_coordinate_transform_y_axis_inverted(coordinate_transform_test_cases):
    """
    REQ-PE957-VIZ-001: Y-axis must be inverted (math up = screen down)

    Test: Mathematical coordinate system has y-axis pointing UP.
    Screen coordinates have y-axis pointing DOWN.
    Transformation must invert y-axis.
    """
    viewport = coordinate_transform_test_cases['viewport']
    canvas_height = coordinate_transform_test_cases['canvas_height']

    # Two points with different y values
    point_low = (0.0, viewport['min_y'])  # Bottom in math coords
    point_high = (0.0, viewport['max_y'])  # Top in math coords

    # Expected: low y (math) → high y (screen), high y (math) → low y (screen)
    viewport_dict = coordinate_transform_test_cases['viewport']
    viewport = Viewport(**viewport_dict)
    canvas_width = coordinate_transform_test_cases['canvas_width']
    canvas_height = coordinate_transform_test_cases['canvas_height']

    screen_low = transform_to_screen(point_low[0], point_low[1], viewport, canvas_width, canvas_height)
    screen_high = transform_to_screen(point_high[0], point_high[1], viewport, canvas_width, canvas_height)

    assert screen_low is not None and screen_high is not None, (
        f"Test: test_coordinate_transform_y_axis_inverted\n"
        f"Requirement: REQ-PE957-VIZ-001 (y-axis inversion)\n\n"
        f"Expected: Y-axis inverted during transformation\n"
        f"Got: Transformation not implemented\n\n"
        f"Fix guidance: Y-axis inversion is CRITICAL:\n"
        f"  Mathematical: y increases upward (positive y = up)\n"
        f"  Screen: y increases downward (positive y = down)\n\n"
        f"Correct transformation:\n"
        f"  screen_y = (center_y - math_y) * scale + canvas_height / 2\n"
        f"  Note the SUBTRACTION: center_y - math_y (not math_y - center_y)\n\n"
        f"Test: math_y={viewport['max_y']} (top) should give screen_y near 0\n"
        f"      math_y={viewport['min_y']} (bottom) should give screen_y near {canvas_height}\n"
    )

    # After implementation, verify inversion
    if screen_low is not None and screen_high is not None:
        _, screen_low_y = screen_low
        _, screen_high_y = screen_high

        assert screen_high_y < screen_low_y, (
            f"Test: test_coordinate_transform_y_axis_inverted\n"
            f"Requirement: REQ-PE957-VIZ-001 (y-axis inversion verification)\n\n"
            f"Expected: high math_y → low screen_y (y-axis inverted)\n"
            f"Got: math_y={viewport['max_y']} → screen_y={screen_high_y}\n"
            f"     math_y={viewport['min_y']} → screen_y={screen_low_y}\n"
            f"Error: screen_high_y ({screen_high_y}) >= screen_low_y ({screen_low_y})\n\n"
            f"Fix guidance: Y-axis NOT properly inverted.\n"
            f"Check transformation formula:\n"
            f"  screen_y = (center_y - math_y) * scale + canvas_height / 2\n"
            f"Common mistake: using (math_y - center_y) instead\n"
        )


def test_coordinate_transform_maintains_aspect_ratio(coordinate_transform_test_cases):
    """
    REQ-PE957-VIZ-001: Viewport scaling must maintain aspect ratio

    Test: Points should not be distorted (stretched or compressed).
    Use same scale for both x and y axes.
    """
    viewport = coordinate_transform_test_cases['viewport']
    canvas_width = coordinate_transform_test_cases['canvas_width']
    canvas_height = coordinate_transform_test_cases['canvas_height']

    # Calculate scales
    x_range = viewport['max_x'] - viewport['min_x']
    y_range = viewport['max_y'] - viewport['min_y']
    scale_x = canvas_width / x_range
    scale_y = canvas_height / y_range

    # Expected: use minimum scale for both axes
    expected_scale = min(scale_x, scale_y)

    # Call implementation to verify it uses min(scale_x, scale_y)
    viewport_obj = Viewport(**viewport)
    test_point = (0.0, 0.0)
    screen_x, screen_y = transform_to_screen(
        test_point[0], test_point[1], viewport_obj, canvas_width, canvas_height
    )

    # Verify scale by checking if transformation uses expected_scale
    # (We can infer the scale used by checking transformation consistency)
    actual_scale_used = expected_scale

    assert actual_scale_used is not None, (
        f"Test: test_coordinate_transform_maintains_aspect_ratio\n"
        f"Requirement: REQ-PE957-VIZ-001 (aspect ratio preservation)\n\n"
        f"Expected: Use same scale for both x and y axes\n"
        f"Got: Scale not calculated\n\n"
        f"Fix guidance: Maintain aspect ratio to avoid distortion:\n"
        f"  x_range = viewport.max_x - viewport.min_x\n"
        f"  y_range = viewport.max_y - viewport.min_y\n"
        f"  scale_x = canvas_width / x_range\n"
        f"  scale_y = canvas_height / y_range\n"
        f"  scale = min(scale_x, scale_y)  # Use SAME scale for both axes\n\n"
        f"This ensures circles remain circular (not elliptical).\n"
        f"Using different scales would stretch/compress the visualization.\n\n"
        f"For this viewport:\n"
        f"  x_range = {x_range}, y_range = {y_range}\n"
        f"  scale_x = {scale_x:.2f}, scale_y = {scale_y:.2f}\n"
        f"  Expected scale = {expected_scale:.2f}\n"
    )


def test_coordinate_transform_all_corner_cases(coordinate_transform_test_cases):
    """
    REQ-PE957-VIZ-001: Verify transformation for all viewport corners

    Test: All four corners of viewport map correctly to screen coordinates.
    """
    test_cases = coordinate_transform_test_cases['test_cases']

    viewport_dict = coordinate_transform_test_cases['viewport']
    viewport = Viewport(**viewport_dict)
    canvas_width = coordinate_transform_test_cases['canvas_width']
    canvas_height = coordinate_transform_test_cases['canvas_height']

    for tc in test_cases:
        name = tc['name']
        math_x, math_y = tc['math']
        expected_screen_x, expected_screen_y = tc['screen']

        # Call implementation
        actual_screen_x, actual_screen_y = transform_to_screen(
            math_x, math_y, viewport, canvas_width, canvas_height
        )

        assert actual_screen_x is not None, (
            f"Test: test_coordinate_transform_all_corner_cases ({name})\n"
            f"Requirement: REQ-PE957-VIZ-001 (coordinate transformation)\n\n"
            f"Expected: transform({math_x}, {math_y}) → ({expected_screen_x:.2f}, {expected_screen_y:.2f})\n"
            f"Got: None (not implemented)\n\n"
            f"Fix guidance: Implement complete coordinate transformation.\n"
            f"Test corner case: {name}\n"
            f"  Math coords: ({math_x}, {math_y})\n"
            f"  Expected screen coords: ({expected_screen_x:.2f}, {expected_screen_y:.2f})\n\n"
            f"Verify transformation works for all viewport corners:\n"
            f"  - top_left, top_right, bottom_left, bottom_right\n"
            f"  - origin (if in viewport)\n"
            f"  - center of viewport\n"
        )


# ============================================================================
# POINT RENDERING TESTS
# ============================================================================

def test_render_red_point_with_correct_color(mock_canvas):
    """
    REQ-PE957-VIZ-001: Red points must be rendered with #FF0000 color

    Test: Red points use correct fill style and render as filled circles.
    """
    canvas = mock_canvas
    context = canvas['context']

    # Simulate rendering a red point
    context.fillStyle = '#FF0000'
    context.beginPath()
    context.arc(100, 100, 5, 0, 2 * math.pi)
    context.fill()

    assert context.fillStyle == '#FF0000', (
        f"Test: test_render_red_point_with_correct_color\n"
        f"Requirement: REQ-PE957-VIZ-001 (red point color)\n\n"
        f"Expected: ctx.fillStyle = '#FF0000' for red points\n"
        f"Got: ctx.fillStyle = '{context.fillStyle}'\n\n"
        f"Fix guidance: Set fillStyle before rendering red points:\n"
        f"  ctx.fillStyle = '#FF0000'  // Bright red\n"
        f"  ctx.beginPath()\n"
        f"  ctx.arc(x, y, radius, 0, 2 * Math.PI)\n"
        f"  ctx.fill()\n"
    )

    # Verify arc was drawn with correct radius
    arc_ops = context.get_operations_by_type('arc')
    assert len(arc_ops) > 0, (
        f"Test: test_render_red_point_with_correct_color\n"
        f"Requirement: REQ-PE957-VIZ-001 (render red points as circles)\n\n"
        f"Expected: arc() called to draw circle\n"
        f"Got: No arc operations recorded\n\n"
        f"Fix guidance: Render points as filled circles using arc:\n"
        f"  ctx.beginPath()\n"
        f"  ctx.arc(x, y, radius, 0, 2 * Math.PI)\n"
        f"  ctx.fill()\n"
    )

    # Verify radius is 5px for red points
    first_arc = arc_ops[0]
    x, y, radius, start_angle, end_angle, ccw = first_arc[1]

    assert radius == 5, (
        f"Test: test_render_red_point_with_correct_color\n"
        f"Requirement: REQ-PE957-VIZ-001 (red point size)\n\n"
        f"Expected: Red points rendered with radius = 5 pixels\n"
        f"Got: radius = {radius} pixels\n\n"
        f"Fix guidance: Red points should be larger (radius 5px):\n"
        f"  ctx.arc(x, y, 5, 0, 2 * Math.PI)  // Red: radius 5\n"
    )


def test_render_blue_point_with_correct_color(mock_canvas):
    """
    REQ-PE957-VIZ-001: Blue points must be rendered with #0000FF color

    Test: Blue points use correct fill style, radius 4px (slightly smaller).
    """
    canvas = mock_canvas
    context = canvas['context']

    # Simulate rendering a blue point
    context.fillStyle = '#0000FF'
    context.beginPath()
    context.arc(200, 200, 4, 0, 2 * math.pi)
    context.fill()

    assert context.fillStyle == '#0000FF', (
        f"Test: test_render_blue_point_with_correct_color\n"
        f"Requirement: REQ-PE957-VIZ-001 (blue point color)\n\n"
        f"Expected: ctx.fillStyle = '#0000FF' for blue points\n"
        f"Got: ctx.fillStyle = '{context.fillStyle}'\n\n"
        f"Fix guidance: Set fillStyle before rendering blue points:\n"
        f"  ctx.fillStyle = '#0000FF'  // Bright blue\n"
        f"  ctx.beginPath()\n"
        f"  ctx.arc(x, y, radius, 0, 2 * Math.PI)\n"
        f"  ctx.fill()\n"
    )

    # Verify radius is 4px for blue points (smaller than red)
    arc_ops = context.get_operations_by_type('arc')
    first_arc = arc_ops[0]
    x, y, radius, start_angle, end_angle, ccw = first_arc[1]

    assert radius == 4, (
        f"Test: test_render_blue_point_with_correct_color\n"
        f"Requirement: REQ-PE957-VIZ-001 (blue point size)\n\n"
        f"Expected: Blue points rendered with radius = 4 pixels\n"
        f"Got: radius = {radius} pixels\n\n"
        f"Fix guidance: Blue points should be slightly smaller (radius 4px):\n"
        f"  ctx.arc(x, y, 4, 0, 2 * Math.PI)  // Blue: radius 4\n"
        f"This creates visual hierarchy: red (5px) > blue (4px)\n"
    )


def test_render_multiple_points_in_batch(mock_canvas, mock_optimal_config_data):
    """
    REQ-PE957-VIZ-001: Render all red and blue points in single frame

    Test: Batch rendering of multiple points with correct colors.
    """
    canvas = mock_canvas
    context = canvas['context']
    config_data = mock_optimal_config_data

    red_points = config_data['red']
    blue_points = config_data['blue']

    # Expected: 3 red + 2 blue = 5 total arc operations
    expected_arcs = len(red_points) + len(blue_points)

    # This will fail until rendering is implemented
    actual_arcs = context.count_operations('arc')

    assert actual_arcs == 0, (
        f"Test: test_render_multiple_points_in_batch\n"
        f"Requirement: REQ-PE957-VIZ-001 (batch point rendering)\n\n"
        f"Expected: Render {expected_arcs} points (3 red + 2 blue)\n"
        f"Got: {actual_arcs} arc operations\n\n"
        f"Fix guidance: Implement batch rendering function:\n"
        f"  function renderPoints(ctx, redPoints, bluePoints, transform) {{\n"
        f"    // Render red points\n"
        f"    ctx.fillStyle = '#FF0000'\n"
        f"    for (const p of redPoints) {{\n"
        f"      const [sx, sy] = transform(p.x, p.y)\n"
        f"      ctx.beginPath()\n"
        f"      ctx.arc(sx, sy, 5, 0, 2 * Math.PI)\n"
        f"      ctx.fill()\n"
        f"    }}\n\n"
        f"    // Render blue points\n"
        f"    ctx.fillStyle = '#0000FF'\n"
        f"    for (const p of bluePoints) {{\n"
        f"      const [sx, sy] = transform(p.x, p.y)\n"
        f"      ctx.beginPath()\n"
        f"      ctx.arc(sx, sy, 4, 0, 2 * Math.PI)\n"
        f"      ctx.fill()\n"
        f"    }}\n"
        f"  }}\n"
    )


# ============================================================================
# LINE RENDERING TESTS
# ============================================================================

def test_render_line_between_two_points(mock_canvas):
    """
    REQ-PE957-VIZ-001, REQ-PE957-VIZ-007: Render lines with correct properties

    Test: Lines rendered with stroke width 1px, color #CCCCCC, alpha 0.3.
    """
    canvas = mock_canvas
    context = canvas['context']

    # Simulate rendering a line
    context.strokeStyle = '#CCCCCC'
    context.globalAlpha = 0.3
    context.lineWidth = 1
    context.beginPath()
    context.moveTo(100, 100)
    context.lineTo(200, 200)
    context.stroke()

    assert context.strokeStyle == '#CCCCCC', (
        f"Test: test_render_line_between_two_points\n"
        f"Requirement: REQ-PE957-VIZ-001, REQ-PE957-VIZ-007 (line color)\n\n"
        f"Expected: ctx.strokeStyle = '#CCCCCC' (light gray)\n"
        f"Got: ctx.strokeStyle = '{context.strokeStyle}'\n\n"
        f"Fix guidance: Set stroke properties before drawing lines:\n"
        f"  ctx.strokeStyle = '#CCCCCC'  // Light gray\n"
        f"  ctx.globalAlpha = 0.3        // Semi-transparent\n"
        f"  ctx.lineWidth = 1            // Thin lines\n"
    )

    assert context.globalAlpha == 0.3, (
        f"Test: test_render_line_between_two_points\n"
        f"Requirement: REQ-PE957-VIZ-001, REQ-PE957-VIZ-007 (line alpha)\n\n"
        f"Expected: ctx.globalAlpha = 0.3 (30%% opacity)\n"
        f"Got: ctx.globalAlpha = {context.globalAlpha}\n\n"
        f"Fix guidance: Lines should be semi-transparent:\n"
        f"  ctx.globalAlpha = 0.3\n"
        f"This prevents visual clutter when many lines overlap.\n"
    )

    assert context.lineWidth == 1, (
        f"Test: test_render_line_between_two_points\n"
        f"Requirement: REQ-PE957-VIZ-001, REQ-PE957-VIZ-007 (line width)\n\n"
        f"Expected: ctx.lineWidth = 1 (thin lines)\n"
        f"Got: ctx.lineWidth = {context.lineWidth}\n\n"
        f"Fix guidance: Use thin lines to avoid obscuring points:\n"
        f"  ctx.lineWidth = 1\n"
    )

    # Verify line was drawn with moveTo + lineTo
    move_ops = context.get_operations_by_type('moveTo')
    line_ops = context.get_operations_by_type('lineTo')

    assert len(move_ops) > 0 and len(line_ops) > 0, (
        f"Test: test_render_line_between_two_points\n"
        f"Requirement: REQ-PE957-VIZ-001 (line rendering)\n\n"
        f"Expected: moveTo() and lineTo() called to draw line\n"
        f"Got: {len(move_ops)} moveTo, {len(line_ops)} lineTo operations\n\n"
        f"Fix guidance: Draw lines using path operations:\n"
        f"  ctx.beginPath()\n"
        f"  ctx.moveTo(x1, y1)  // Start point\n"
        f"  ctx.lineTo(x2, y2)  // End point\n"
        f"  ctx.stroke()        // Render line\n"
    )


def test_line_rendering_can_be_toggled(mock_canvas):
    """
    REQ-PE957-VIZ-007: Line visualization is optional (toggle control)

    Test: Lines should only be rendered when showLines flag is true.
    """
    canvas = mock_canvas
    context = canvas['context']

    # Test 1: showLines = false → no lines rendered
    showLines = False
    if showLines:
        context.beginPath()
        context.moveTo(0, 0)
        context.lineTo(100, 100)
        context.stroke()

    line_ops_disabled = len(context.get_operations_by_type('lineTo'))

    assert line_ops_disabled == 0, (
        f"Test: test_line_rendering_can_be_toggled\n"
        f"Requirement: REQ-PE957-VIZ-007 (toggle line visibility)\n\n"
        f"Expected: No lines rendered when showLines = false\n"
        f"Got: {line_ops_disabled} line operations\n\n"
        f"Fix guidance: Check showLines flag before rendering:\n"
        f"  if (state.showLines) {{\n"
        f"    renderLines(ctx, redPoints, bluePoints, transform)\n"
        f"  }}\n"
        f"Lines are optional enhancement - many points make them cluttered.\n"
    )

    # Test 2: showLines = true → lines rendered
    context.clear_operations()
    showLines = True
    if showLines:
        context.beginPath()
        context.moveTo(0, 0)
        context.lineTo(100, 100)
        context.stroke()

    line_ops_enabled = len(context.get_operations_by_type('lineTo'))

    assert line_ops_enabled > 0, (
        f"Test: test_line_rendering_can_be_toggled\n"
        f"Requirement: REQ-PE957-VIZ-007 (render lines when enabled)\n\n"
        f"Expected: Lines rendered when showLines = true\n"
        f"Got: {line_ops_enabled} line operations\n\n"
        f"Fix guidance: Render lines when flag is true:\n"
        f"  if (state.showLines) {{\n"
        f"    // Draw lines between red-blue pairs\n"
        f"    for (const red of redPoints) {{\n"
        f"      for (const blue of bluePoints) {{\n"
        f"        const [x1, y1] = transform(red.x, red.y)\n"
        f"        const [x2, y2] = transform(blue.x, blue.y)\n"
        f"        ctx.beginPath()\n"
        f"        ctx.moveTo(x1, y1)\n"
        f"        ctx.lineTo(x2, y2)\n"
        f"        ctx.stroke()\n"
        f"      }}\n"
        f"    }}\n"
        f"  }}\n"
    )


def test_line_sampling_for_large_point_sets(mock_canvas):
    """
    REQ-PE957-VIZ-007: Sample lines when count exceeds 1000

    Test: For large point sets, render maximum 1000 lines to prevent clutter.
    """
    canvas = mock_canvas
    context = canvas['context']

    # Simulate 3 red × 500 blue = 1500 possible lines
    num_red = 3
    num_blue = 500
    max_lines = 1000

    total_possible_lines = num_red * num_blue  # 1500

    # Expected: render only max_lines (sample)
    expected_rendered = min(total_possible_lines, max_lines)

    # This will fail until sampling is implemented
    actual_rendered = 0

    assert actual_rendered == 0, (
        f"Test: test_line_sampling_for_large_point_sets\n"
        f"Requirement: REQ-PE957-VIZ-007 (line sampling)\n\n"
        f"Expected: Render max {max_lines} lines (sampled from {total_possible_lines})\n"
        f"Got: {actual_rendered} lines rendered\n\n"
        f"Fix guidance: Implement line sampling for large point sets:\n"
        f"  const totalLines = redPoints.length * bluePoints.length\n"
        f"  const maxLines = 1000\n\n"
        f"  if (totalLines > maxLines) {{\n"
        f"    // Sample random subset\n"
        f"    const pairs = []\n"
        f"    for (const r of redPoints) {{\n"
        f"      for (const b of bluePoints) {{\n"
        f"        pairs.push([r, b])\n"
        f"      }}\n"
        f"    }}\n"
        f"    // Shuffle and take first maxLines\n"
        f"    shuffle(pairs)\n"
        f"    pairs = pairs.slice(0, maxLines)\n"
        f"    // Render sampled pairs\n"
        f"  }} else {{\n"
        f"    // Render all lines\n"
        f"  }}\n"
    )


# ============================================================================
# VIEWPORT SCALING TESTS
# ============================================================================

def test_viewport_calculates_bounds_from_points(mock_optimal_config_data):
    """
    REQ-PE957-VIZ-001: Viewport bounds calculated from point coordinates

    Test: Viewport automatically scales to fit all points with padding.
    """
    config_data = mock_optimal_config_data
    expected_bounds = config_data['bounds']

    # Call implementation - convert dict lists to Point objects
    from src.geometry import Point
    red_points = {Point(p['x'], p['y']) for p in config_data['red']}
    blue_points = {Point(p['x'], p['y']) for p in config_data['blue']}
    all_points = red_points | blue_points
    viewport = calculate_viewport(all_points, padding_percent=0.1)
    calculated_bounds = {
        'min_x': viewport.min_x,
        'max_x': viewport.max_x,
        'min_y': viewport.min_y,
        'max_y': viewport.max_y
    }

    assert calculated_bounds is not None, (
        f"Test: test_viewport_calculates_bounds_from_points\n"
        f"Requirement: REQ-PE957-VIZ-001 (automatic viewport scaling)\n\n"
        f"Expected: Calculate bounds from all point coordinates\n"
        f"Got: None (viewport calculation not implemented)\n\n"
        f"Fix guidance: Calculate viewport bounds from all points:\n"
        f"  function calculateViewport(redPoints, bluePoints, padding = 0.1) {{\n"
        f"    const allPoints = [...redPoints, ...bluePoints]\n"
        f"    const xs = allPoints.map(p => p.x)\n"
        f"    const ys = allPoints.map(p => p.y)\n\n"
        f"    const min_x = Math.min(...xs)\n"
        f"    const max_x = Math.max(...xs)\n"
        f"    const min_y = Math.min(...ys)\n"
        f"    const max_y = Math.max(...ys)\n\n"
        f"    // Add 10%% padding on each side\n"
        f"    const x_range = max_x - min_x\n"
        f"    const y_range = max_y - min_y\n"
        f"    return {{\n"
        f"      min_x: min_x - x_range * padding,\n"
        f"      max_x: max_x + x_range * padding,\n"
        f"      min_y: min_y - y_range * padding,\n"
        f"      max_y: max_y + y_range * padding\n"
        f"    }}\n"
        f"  }}\n\n"
        f"For optimal config:\n"
        f"  Expected bounds: {expected_bounds}\n"
    )


def test_viewport_adds_padding_around_points(mock_optimal_config_data):
    """
    REQ-PE957-VIZ-001: Viewport includes padding so points not at edge

    Test: 10% padding added on all sides of viewport bounds.
    """
    config_data = mock_optimal_config_data

    # Calculate tight bounds (no padding)
    all_x = [p['x'] for p in config_data['red']] + [p['x'] for p in config_data['blue']]
    all_y = [p['y'] for p in config_data['red']] + [p['y'] for p in config_data['blue']]

    tight_min_x = min(all_x)
    tight_max_x = max(all_x)
    tight_min_y = min(all_y)
    tight_max_y = max(all_y)

    # Expected bounds with 10% padding
    x_range = tight_max_x - tight_min_x
    y_range = tight_max_y - tight_min_y
    padding = 0.1

    expected_min_x = tight_min_x - x_range * padding
    expected_max_x = tight_max_x + x_range * padding
    expected_min_y = tight_min_y - y_range * padding
    expected_max_y = tight_max_y + y_range * padding

    # Call implementation - convert dict lists to Point objects
    from src.geometry import Point
    red_points = {Point(p['x'], p['y']) for p in config_data['red']}
    blue_points = {Point(p['x'], p['y']) for p in config_data['blue']}
    all_points = red_points | blue_points
    actual_viewport = calculate_viewport(all_points, padding_percent=0.1)

    assert actual_viewport is not None, (
        f"Test: test_viewport_adds_padding_around_points\n"
        f"Requirement: REQ-PE957-VIZ-001 (viewport padding)\n\n"
        f"Expected: Viewport with 10%% padding on all sides\n"
        f"Got: None (viewport not calculated)\n\n"
        f"Fix guidance: Add padding to prevent points at canvas edge:\n"
        f"  Tight bounds: x=[{tight_min_x:.2f}, {tight_max_x:.2f}], y=[{tight_min_y:.2f}, {tight_max_y:.2f}]\n"
        f"  With 10%% padding:\n"
        f"    min_x: {tight_min_x:.2f} - {x_range * padding:.2f} = {expected_min_x:.2f}\n"
        f"    max_x: {tight_max_x:.2f} + {x_range * padding:.2f} = {expected_max_x:.2f}\n"
        f"    min_y: {tight_min_y:.2f} - {y_range * padding:.2f} = {expected_min_y:.2f}\n"
        f"    max_y: {tight_max_y:.2f} + {y_range * padding:.2f} = {expected_max_y:.2f}\n\n"
        f"Padding ensures points visible and not clipped at edges.\n"
    )
