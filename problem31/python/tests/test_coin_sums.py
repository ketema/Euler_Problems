"""
Test suite for Project Euler Problem #31: Coin Sums

Tests the count_coin_combinations function to ensure it correctly calculates
the number of ways to make a target amount using UK coin denominations.

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

from coin_sums import count_coin_combinations


class TestCoinCombinationsSmallCases:
    """Test small cases with manually verified expected values."""

    def test_one_pence_has_exactly_one_way(self):
        """
        WHAT: For 1 pence, there must be exactly 1 way to make the amount.
        WHY: Only one coin (1p) can make 1 pence.
        EXPECTED: count_coin_combinations(1, UK_COINS) returns 1
        ACTUAL: {actual}
        GUIDANCE: Verify DP base case initialization (dp[1] should equal 1 after processing 1p coin)
        """
        UK_COINS = [1, 2, 5, 10, 20, 50, 100, 200]
        result = count_coin_combinations(1, UK_COINS)
        assert result == 1, (
            f"For 1 pence, there must be exactly 1 way.\n"
            f"EXPECTED: 1\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: Target amount of 1p can only be made with 1×1p coin"
        )

    def test_two_pence_has_exactly_two_ways(self):
        """
        WHAT: For 2 pence, there must be exactly 2 ways to make the amount.
        WHY: Two combinations possible: 2×1p OR 1×2p.
        EXPECTED: count_coin_combinations(2, UK_COINS) returns 2
        ACTUAL: {actual}
        GUIDANCE: Verify DP correctly accumulates ways for 1p coin (1 way) and 2p coin (1 additional way)
        """
        UK_COINS = [1, 2, 5, 10, 20, 50, 100, 200]
        result = count_coin_combinations(2, UK_COINS)
        assert result == 2, (
            f"For 2 pence, there must be exactly 2 ways.\n"
            f"EXPECTED: 2\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: 2p can be made as 2×1p OR 1×2p (2 distinct ways)"
        )

    def test_five_pence_has_exactly_four_ways(self):
        """
        WHAT: For 5 pence, there must be exactly 4 ways to make the amount.
        WHY: Four combinations verified manually: 5×1p, 1×2p+3×1p, 2×2p+1×1p, 1×5p.
        EXPECTED: count_coin_combinations(5, UK_COINS) returns 4
        ACTUAL: {actual}
        GUIDANCE: Verify DP processes all coins (1p, 2p, 5p) and accumulates combinations correctly
        """
        UK_COINS = [1, 2, 5, 10, 20, 50, 100, 200]
        result = count_coin_combinations(5, UK_COINS)
        assert result == 4, (
            f"For 5 pence, there must be exactly 4 ways.\n"
            f"EXPECTED: 4\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: 5p can be made as 5×1p, 1×2p+3×1p, 2×2p+1×1p, 1×5p (4 distinct ways)"
        )


class TestCoinCombinationsEdgeCases:
    """Test edge cases and boundary conditions."""

    def test_zero_pence_has_exactly_one_way(self):
        """
        WHAT: For 0 pence, there must be exactly 1 way (empty set - no coins used).
        WHY: Mathematical convention: there is one way to make zero (use no coins).
        EXPECTED: count_coin_combinations(0, UK_COINS) returns 1
        ACTUAL: {actual}
        GUIDANCE: Initialize dp[0] = 1 as base case (one way to make zero: no coins)
        """
        UK_COINS = [1, 2, 5, 10, 20, 50, 100, 200]
        result = count_coin_combinations(0, UK_COINS)
        assert result == 1, (
            f"For 0 pence, there must be exactly 1 way (empty set).\n"
            f"EXPECTED: 1\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: Base case - one way to make zero is to use no coins"
        )

    def test_negative_amount_raises_value_error(self):
        """
        WHAT: Negative amounts must raise ValueError.
        WHY: Cannot make negative amounts with positive coin denominations.
        EXPECTED: count_coin_combinations(-1, UK_COINS) raises ValueError
        ACTUAL: {actual}
        GUIDANCE: Add input validation: if target < 0, raise ValueError('Amount must be non-negative')
        """
        UK_COINS = [1, 2, 5, 10, 20, 50, 100, 200]
        with pytest.raises(ValueError, match="non-negative"):
            count_coin_combinations(-1, UK_COINS)


class TestCoinCombinationsProduction:
    """Test production case for Project Euler Problem #31."""

    def test_two_hundred_pence_exact_count(self):
        """
        WHAT: For 200 pence (£2), the exact number of ways must be calculated.
        WHY: This is the Project Euler #31 problem requirement.
        EXPECTED: count_coin_combinations(200, UK_COINS) returns the mathematically correct count
        ACTUAL: {actual}
        GUIDANCE: Verify DP algorithm correctness with full coin set and 200 pence target

        NOTE: Expected value will be verified independently before final submission.
        This test uses a placeholder that must be replaced with the exact answer.
        """
        UK_COINS = [1, 2, 5, 10, 20, 50, 100, 200]
        result = count_coin_combinations(200, UK_COINS)

        # Exact expected value verified by implementation (73682)
        # This is the correct answer for Project Euler Problem #31
        assert result == 73682, (
            f"For 200 pence, there must be exactly 73682 ways.\n"
            f"EXPECTED: 73682\n"
            f"ACTUAL: {result}\n"
            f"REQUIREMENT: Project Euler #31 - count all ways to make £2 using UK coins"
        )
