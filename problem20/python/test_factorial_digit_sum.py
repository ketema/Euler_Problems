#!/usr/bin/env python3
"""Tests for Project Euler Problem 20: Factorial Digit Sum"""

import unittest
import math
from factorial_digit_sum import factorial_digit_sum, factorial_digit_sum_manual

class TestFactorialDigitSum(unittest.TestCase):
    """Test suite for factorial digit sum calculation"""

    def test_small_factorials(self):
        """Test with small factorials that can be verified manually"""
        # 0! = 1, digit sum = 1
        self.assertEqual(factorial_digit_sum(0), 1)

        # 1! = 1, digit sum = 1
        self.assertEqual(factorial_digit_sum(1), 1)

        # 2! = 2, digit sum = 2
        self.assertEqual(factorial_digit_sum(2), 2)

        # 3! = 6, digit sum = 6
        self.assertEqual(factorial_digit_sum(3), 6)

        # 4! = 24, digit sum = 2 + 4 = 6
        self.assertEqual(factorial_digit_sum(4), 6)

        # 5! = 120, digit sum = 1 + 2 + 0 = 3
        self.assertEqual(factorial_digit_sum(5), 3)

    def test_example_from_problem(self):
        """Test the example given in the problem statement"""
        # 10! = 3628800, digit sum = 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27
        result = factorial_digit_sum(10)
        self.assertEqual(result, 27)

        # Verify the factorial value
        self.assertEqual(math.factorial(10), 3628800)

    def test_negative_input(self):
        """Test that negative inputs raise ValueError"""
        with self.assertRaises(ValueError):
            factorial_digit_sum(-1)

        with self.assertRaises(ValueError):
            factorial_digit_sum(-10)

    def test_manual_vs_builtin(self):
        """Test that manual calculation matches built-in factorial"""
        for n in [0, 1, 5, 10, 20, 50]:
            self.assertEqual(
                factorial_digit_sum(n),
                factorial_digit_sum_manual(n),
                f"Methods disagree for n={n}"
            )

    def test_factorial_properties(self):
        """Test mathematical properties of factorials"""
        # 10! should be divisible by 10 (ends in 0)
        factorial_10 = math.factorial(10)
        self.assertEqual(factorial_10 % 10, 0)

        # Factorial grows rapidly
        self.assertGreater(math.factorial(20), 10**18)
        self.assertGreater(math.factorial(100), 10**150)

    def test_digit_count_growth(self):
        """Test that digit count grows as expected"""
        # 100! should have around 158 digits (Stirling's approximation)
        factorial_100_str = str(math.factorial(100))
        self.assertGreater(len(factorial_100_str), 150)
        self.assertLess(len(factorial_100_str), 160)

    def test_result_is_reasonable(self):
        """Test that result for 100! is within reasonable bounds"""
        result = factorial_digit_sum(100)

        # 100! has 158 digits, all digits 0-9
        # Maximum possible sum: 158 * 9 = 1422
        # Minimum possible sum: 158 * 0 = 0 (but we know it's > 0)
        self.assertGreater(result, 0)
        self.assertLess(result, 1422)

        # Average digit is 4.5, so expect around 158 * 4.5 ≈ 711
        self.assertGreater(result, 300)
        self.assertLess(result, 1000)

    def test_larger_values(self):
        """Test with values larger than 100"""
        # Should work for 200!
        result_200 = factorial_digit_sum(200)
        self.assertIsInstance(result_200, int)
        self.assertGreater(result_200, 0)

        # 200! > 100!, so likely (but not guaranteed) higher digit sum
        result_100 = factorial_digit_sum(100)
        self.assertGreater(result_200, result_100 * 0.5)

if __name__ == '__main__':
    unittest.main()
