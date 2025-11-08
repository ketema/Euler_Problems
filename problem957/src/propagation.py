"""
Propagation logic for Project Euler Problem 957.

Implements single-day propagation simulation where colored points generate new points
through line intersections.
"""

from itertools import combinations
from typing import Set
from src.geometry import Point, Line


def propagate_one_day(red: Set[Point], blue: Set[Point]) -> Set[Point]:
    """
    Simulate one day of propagation.

    REQ-PE957-004: Single day propagation algorithm.
    Process:
    1. Construct all lines from red×blue point pairs
    2. Find intersections of all line pairs
    3. Exclude existing red points and blue points
    4. Deduplicate using set
    5. Return NEW blue points only

    Args:
        red: Set of red points
        blue: Set of existing blue points

    Returns:
        Set of NEW blue points (not including existing blues)

    Raises:
        TypeError: If red or blue are not sets/iterables of Points
    """
    # Validate inputs are iterable
    try:
        iter(red)
        iter(blue)
    except TypeError as e:
        raise TypeError("red and blue must be iterable collections of Points") from e

    # Validate all elements are Points
    for point in red:
        if not isinstance(point, Point):
            raise TypeError(f"All elements in red must be Points, got {type(point).__name__}")

    for point in blue:
        if not isinstance(point, Point):
            raise TypeError(f"All elements in blue must be Points, got {type(point).__name__}")

    # Step 1: Construct all lines between red-blue pairs ONLY
    # REQ-PE957-004: Lines must pass through "a red point and a blue point"
    lines = []
    for r in red:
        for b in blue:
            try:
                line = Line(r, b)
                lines.append(line)
            except ValueError:
                # Points are too close (within epsilon), skip
                pass

    # Step 2: Find all intersections of line pairs
    new_blue = set()
    for line1, line2 in combinations(lines, 2):
        intersection = line1.intersect(line2)
        if intersection is not None:
            new_blue.add(intersection)

    # Step 3: Exclude existing red and blue points
    # Remove all red points
    new_blue = new_blue - red

    # Remove all existing blue points
    new_blue = new_blue - blue

    # Step 4: Deduplication is automatic with set
    # Step 5: Return new blue points
    return new_blue


def simulate_multiday(red: Set[Point], blue: Set[Point], n: int) -> int:
    """
    Simulate n days of propagation and return total blue count.

    REQ-PE957-005: Multi-day propagation simulation.
    REQ-PE957-006: Calculate g(n) for optimal configuration.

    Args:
        red: Set of red points (unchanged throughout)
        blue: Set of initial blue points
        n: Number of days to simulate (n >= 0)

    Returns:
        Total count of blue points after n days

    Raises:
        ValueError: If n < 0
        TypeError: If red/blue are not valid collections
    """
    # Validate n parameter
    if not isinstance(n, int):
        raise TypeError("n must be an integer")
    if n < 0:
        raise ValueError("n must be non-negative")

    # Validate inputs are iterable (propagate_one_day will validate Points)
    try:
        iter(red)
        iter(blue)
    except TypeError as e:
        raise TypeError("red and blue must be iterable collections of Points") from e

    # Edge case: n=0 returns initial blue count
    if n == 0:
        return len(blue)

    # Accumulate all blue points (initial + new from each day)
    all_blues = set(blue)  # Copy initial blues

    # Simulate n days of propagation
    for day in range(n):
        # Get new blues for this day using current accumulated blues
        new_blues = propagate_one_day(red, all_blues)

        # Add new blues to accumulated set
        all_blues = all_blues.union(new_blues)

        # Early termination: if no new blues generated, saturation reached
        if len(new_blues) == 0:
            break

    return len(all_blues)
