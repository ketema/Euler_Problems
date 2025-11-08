#!/usr/bin/env python3
"""
Manual verification of simulate_multiday implementation.
Tests the function directly without pytest overhead.
"""

import sys
sys.path.insert(0, '.')

from src.geometry import Point
from src.propagation import simulate_multiday, propagate_one_day
import math

def create_simple_config():
    """Simple equilateral triangle configuration."""
    red = {
        Point(1.0, 0.0, 'red'),
        Point(math.cos(2*math.pi/3), math.sin(2*math.pi/3), 'red'),
        Point(math.cos(4*math.pi/3), math.sin(4*math.pi/3), 'red'),
    }
    blue = {
        Point(0.0, 0.0, 'blue'),
        Point(2.0, 0.0, 'blue'),
    }
    return red, blue

def test_basic():
    """Test basic functionality."""
    red, blue = create_simple_config()
    result = simulate_multiday(red, blue, n=3)
    assert isinstance(result, int), f"Expected int, got {type(result)}"
    assert result >= len(blue), f"Result {result} should be >= initial blues {len(blue)}"
    print(f"✓ Basic test: n=3 returns {result} (>= {len(blue)})")

def test_n_zero():
    """Test n=0 returns initial count."""
    red, blue = create_simple_config()
    result = simulate_multiday(red, blue, n=0)
    assert result == len(blue), f"n=0 should return {len(blue)}, got {result}"
    print(f"✓ n=0 test: returns {result}")

def test_negative_n():
    """Test negative n raises ValueError."""
    red, blue = create_simple_config()
    try:
        simulate_multiday(red, blue, n=-1)
        assert False, "Should have raised ValueError"
    except ValueError:
        print("✓ Negative n raises ValueError")

def test_invalid_inputs():
    """Test type validation."""
    red, blue = create_simple_config()
    try:
        simulate_multiday(None, blue, n=1)
        assert False, "Should have raised TypeError"
    except TypeError:
        print("✓ Invalid red input raises TypeError")

def test_monotonic():
    """Test that g(n+1) >= g(n)."""
    red, blue = create_simple_config()
    prev = simulate_multiday(red, blue, n=0)
    for n in range(1, 6):
        curr = simulate_multiday(red, blue, n=n)
        assert curr >= prev, f"g({n})={curr} < g({n-1})={prev}"
        prev = curr
    print(f"✓ Monotonic property: g(0)...g(5) non-decreasing")

def test_empty_blues():
    """Test empty initial blues."""
    red, _ = create_simple_config()
    result = simulate_multiday(red, set(), n=5)
    assert result == 0, f"Empty blues should return 0, got {result}"
    print(f"✓ Empty blues test: returns {result}")

def main():
    print("Testing simulate_multiday implementation...\n")

    tests = [
        test_basic,
        test_n_zero,
        test_negative_n,
        test_invalid_inputs,
        test_monotonic,
        test_empty_blues,
    ]

    for test in tests:
        try:
            test()
        except AssertionError as e:
            print(f"✗ {test.__name__}: {e}")
            return 1
        except Exception as e:
            print(f"✗ {test.__name__}: Unexpected error: {e}")
            return 1

    print("\n✓ All manual tests passed!")
    return 0

if __name__ == '__main__':
    sys.exit(main())
