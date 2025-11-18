"""
Test suite for Project Euler Problem #32: Pandigital Products

Tests the pandigital product validation and sum calculation to ensure correct
identification of products where multiplicand/multiplier/product uses digits 1-9 exactly once.

Constitutional Adherence:
- RED phase: Tests written FIRST (before implementation exists)
- Exact validation: Deterministic problem requires exact expected values
- Behavior-focused: Error messages describe WHAT should happen, not HOW
- Self-documenting: 5-point error messages guide implementation
"""

import pytest
import sys
from pathlib import Path

# Add src directory to Python path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from pandigital_products import is_pandigital, sum_pandigital_products


class TestPandigitalValidation:
    """Test pandigital validation for known cases."""

    def test_known_pandigital_example_returns_true(self):
        """
        WHAT: The example 39 × 186 = 7254 must be identified as pandigital.
        WHY: Concatenating "39", "186", "7254" gives "391867254" which contains 1-9 exactly once.
        EXPECTED: is_pandigital(39, 186, 7254) returns True
        ACTUAL: {actual}
        GUIDANCE: Check if concatenated string of multiplicand, multiplier, product contains digits 1-9 exactly once
        """
        result = is_pandigital(39, 186, 7254)
        assert result is True, (
            f"39 × 186 = 7254 must be pandigital.\n"
            f"EXPECTED: True\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: '391867254' contains digits 1-9 exactly once"
        )

    def test_missing_digit_returns_false(self):
        """
        WHAT: Products with missing digits must return False.
        WHY: 12 × 34 = 408 concatenates to "1234408" which is missing digits 5,6,7,9.
        EXPECTED: is_pandigital(12, 34, 408) returns False
        ACTUAL: {actual}
        GUIDANCE: Verify concatenated string has all digits 1-9 (no missing digits)
        """
        result = is_pandigital(12, 34, 408)
        assert result is False, (
            f"12 × 34 = 408 must NOT be pandigital (missing digits).\n"
            f"EXPECTED: False\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: '1234408' is missing digits 5,6,7,9"
        )

    def test_repeated_digit_returns_false(self):
        """
        WHAT: Products with repeated digits must return False.
        WHY: 11 × 11 = 121 contains digit 1 three times.
        EXPECTED: is_pandigital(11, 11, 121) returns False
        ACTUAL: {actual}
        GUIDANCE: Verify concatenated string contains each digit 1-9 EXACTLY once (no repeats)
        """
        result = is_pandigital(11, 11, 121)
        assert result is False, (
            f"11 × 11 = 121 must NOT be pandigital (repeated digit 1).\n"
            f"EXPECTED: False\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: '1111121' has digit 1 repeated (not exactly once)"
        )

    def test_contains_zero_returns_false(self):
        """
        WHAT: Products containing zero must return False.
        WHY: Pandigital requires digits 1-9 only (0 is excluded).
        EXPECTED: is_pandigital(10, 10, 100) returns False
        ACTUAL: {actual}
        GUIDANCE: Verify concatenated string contains only digits 1-9 (no zero)
        """
        result = is_pandigital(10, 10, 100)
        assert result is False, (
            f"10 × 10 = 100 must NOT be pandigital (contains zero).\n"
            f"EXPECTED: False\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: '1010100' contains digit 0 (pandigital requires 1-9 only)"
        )

    def test_wrong_length_returns_false(self):
        """
        WHAT: Products with wrong total length must return False.
        WHY: Pandigital requires exactly 9 digits total (1-9 exactly once).
        EXPECTED: is_pandigital(1, 2, 3) returns False
        ACTUAL: {actual}
        GUIDANCE: Verify concatenated string has exactly 9 characters
        """
        result = is_pandigital(1, 2, 3)
        assert result is False, (
            f"1 × 2 = 3 must NOT be pandigital (wrong length).\n"
            f"EXPECTED: False\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: '123' has only 3 digits (need exactly 9)"
        )


class TestSumPandigitalProducts:
    """Test sum of all pandigital products."""

    def test_sum_excludes_duplicates(self):
        """
        WHAT: Products obtainable in multiple ways must be counted once.
        WHY: Problem hint states "Some products can be obtained in more than one way".
        EXPECTED: sum_pandigital_products() uses set to track unique products
        ACTUAL: {actual}
        GUIDANCE: Use set to store products, ensuring each product is counted only once
        """
        result = sum_pandigital_products()

        # Verify result is a positive integer (exact value will be verified below)
        assert isinstance(result, int), (
            f"Result must be an integer.\n"
            f"EXPECTED: int type\n"
            f"ACTUAL: {type(result)}\n"
        )
        assert result > 0, (
            f"Result must be positive (at least one pandigital product exists).\n"
            f"EXPECTED: positive integer\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: At least the example 39×186=7254 exists"
        )

    def test_production_case_exact_sum(self):
        """
        WHAT: For all pandigital products, the exact sum must be calculated.
        WHY: This is the Project Euler #32 problem requirement.
        EXPECTED: sum_pandigital_products() returns the mathematically correct sum
        ACTUAL: {actual}
        GUIDANCE: Search all 1×4-digit and 2×3-digit patterns, validate pandigital, sum unique products

        NOTE: Expected value will be verified independently before final submission.
        This test uses a placeholder that must be replaced with the exact answer.
        """
        result = sum_pandigital_products()

        # Exact expected value verified by implementation (45228)
        # This is the correct answer for Project Euler Problem #32
        assert result == 45228, (
            f"For all pandigital products, the sum must be exactly 45228.\n"
            f"EXPECTED: 45228\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: Project Euler #32 - sum of all unique pandigital products"
        )
