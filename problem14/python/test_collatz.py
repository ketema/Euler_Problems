"""
Tests for Project Euler Problem 14: Longest Collatz Sequence
Following TDD/BDD methodology per AGENTS.md
Using unittest for compatibility
"""

import unittest
from collatz import collatz_length, find_longest_collatz, get_collatz_sequence


class TestCollatzSequence(unittest.TestCase):
    """Test suite for Collatz sequence calculations"""

    def test_collatz_length_for_one(self):
        """Chain starting at 1 should have length 1"""
        self.assertEqual(collatz_length(1), 1)

    def test_collatz_length_for_two(self):
        """Chain starting at 2: 2 → 1 (length 2)"""
        self.assertEqual(collatz_length(2), 2)

    def test_collatz_length_for_thirteen(self):
        """
        Example from problem statement:
        13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
        Length: 10 terms
        """
        self.assertEqual(collatz_length(13), 10)

    def test_collatz_length_for_small_numbers(self):
        """Test several small numbers with known chain lengths"""
        # These are computed manually or verified
        self.assertEqual(collatz_length(3), 8)   # 3→10→5→16→8→4→2→1
        self.assertEqual(collatz_length(4), 3)   # 4→2→1
        self.assertEqual(collatz_length(5), 6)   # 5→16→8→4→2→1

    def test_collatz_length_positive_integers(self):
        """All positive integers should return positive chain length"""
        for n in range(1, 100):
            self.assertGreater(collatz_length(n), 0)

    def test_find_longest_collatz_small_range(self):
        """Test finding longest chain in small range"""
        # For numbers under 10, the longest chain is at n=9 (length 20)
        number, length = find_longest_collatz(10)
        self.assertEqual(number, 9)
        self.assertEqual(length, 20)

    def test_find_longest_collatz_returns_tuple(self):
        """Function should return (number, chain_length) tuple"""
        result = find_longest_collatz(10)
        self.assertIsInstance(result, tuple)
        self.assertEqual(len(result), 2)
        self.assertIsInstance(result[0], int)
        self.assertIsInstance(result[1], int)

    def test_memoization_consistency(self):
        """
        Multiple calls should return same result
        (Tests that memoization doesn't break correctness)
        """
        first = collatz_length(13)
        second = collatz_length(13)
        self.assertEqual(first, 10)
        self.assertEqual(second, 10)

    def test_get_collatz_sequence(self):
        """Test sequence generation for verification"""
        sequence = get_collatz_sequence(13)
        expected = [13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
        self.assertEqual(sequence, expected)


class TestProblemSolution(unittest.TestCase):
    """Integration tests for the main problem"""

    def test_solution_under_one_million(self):
        """
        Main problem: Find longest Collatz chain starting under 1,000,000

        WHY: This is the actual Project Euler problem requirement
        EXPECTED: Should return the correct answer: 837799 with length 525
        """
        number, length = find_longest_collatz(1_000_000)

        # Verify the known answer
        self.assertEqual(number, 837799,
                        f"Expected starting number 837799, got {number}")
        self.assertEqual(length, 525,
                        f"Expected chain length 525, got {length}")

        # Additional assertions
        self.assertGreaterEqual(number, 1)
        self.assertLess(number, 1_000_000)
        self.assertGreater(length, 0)

        print(f"\n✓ Problem 14 Solution VERIFIED:")
        print(f"  Starting number: {number}")
        print(f"  Chain length: {length}")


class TestEdgeCases(unittest.TestCase):
    """Test edge cases and error conditions"""

    def test_invalid_input_zero(self):
        """Should raise error for n=0"""
        with self.assertRaises(ValueError):
            collatz_length(0)

    def test_invalid_input_negative(self):
        """Should raise error for negative numbers"""
        with self.assertRaises(ValueError):
            collatz_length(-5)

    def test_find_longest_invalid_limit(self):
        """Should raise error for invalid limits"""
        with self.assertRaises(ValueError):
            find_longest_collatz(1)
        with self.assertRaises(ValueError):
            find_longest_collatz(0)


if __name__ == "__main__":
    # Run tests with verbose output
    unittest.main(verbosity=2)
