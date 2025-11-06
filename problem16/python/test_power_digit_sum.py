"""
Tests for Project Euler Problem 16: Power Digit Sum
Following TDD/BDD methodology per AGENTS.md
"""

import unittest
from power_digit_sum import power_digit_sum, get_digit_list


class TestPowerDigitSum(unittest.TestCase):
    """Test suite for power digit sum calculations"""

    def test_given_example(self):
        """Test the example from problem statement: 2^15 = 32768, sum = 26"""
        result = power_digit_sum(2, 15)
        self.assertEqual(result, 26)

        # Verify the number itself
        self.assertEqual(2 ** 15, 32768)

        # Verify digits: 3+2+7+6+8 = 26
        digits = get_digit_list(32768)
        self.assertEqual(digits, [3, 2, 7, 6, 8])
        self.assertEqual(sum(digits), 26)

    def test_simple_powers(self):
        """Test simple cases with known results"""
        # 10^1 = 10, sum = 1+0 = 1
        self.assertEqual(power_digit_sum(10, 1), 1)

        # 10^2 = 100, sum = 1+0+0 = 1
        self.assertEqual(power_digit_sum(10, 2), 1)

        # 10^3 = 1000, sum = 1+0+0+0 = 1
        self.assertEqual(power_digit_sum(10, 3), 1)

        # 2^0 = 1, sum = 1
        self.assertEqual(power_digit_sum(2, 0), 1)

        # 2^1 = 2, sum = 2
        self.assertEqual(power_digit_sum(2, 1), 2)

        # 2^2 = 4, sum = 4
        self.assertEqual(power_digit_sum(2, 2), 4)

        # 2^3 = 8, sum = 8
        self.assertEqual(power_digit_sum(2, 3), 8)

        # 2^4 = 16, sum = 1+6 = 7
        self.assertEqual(power_digit_sum(2, 4), 7)

    def test_larger_powers(self):
        """Test with larger exponents"""
        # 2^10 = 1024, sum = 1+0+2+4 = 7
        self.assertEqual(power_digit_sum(2, 10), 7)

        # 2^20 = 1048576, sum = 1+0+4+8+5+7+6 = 31
        self.assertEqual(power_digit_sum(2, 20), 31)

    def test_get_digit_list(self):
        """Test digit list extraction"""
        self.assertEqual(get_digit_list(123), [1, 2, 3])
        self.assertEqual(get_digit_list(1000), [1, 0, 0, 0])
        self.assertEqual(get_digit_list(9876543210), [9, 8, 7, 6, 5, 4, 3, 2, 1, 0])

    def test_problem_solution(self):
        """
        Main problem: Sum of digits in 2^1000

        WHY: This is the actual Project Euler problem requirement
        EXPECTED: Should return correct answer for 2^1000
        """
        result = power_digit_sum(2, 1000)

        # Verify result is reasonable
        self.assertIsInstance(result, int)
        self.assertGreater(result, 0)

        # 2^1000 has about 302 digits, so sum should be reasonably bounded
        # Each digit is at most 9, so max sum is 302*9 = 2718
        self.assertLess(result, 3000)
        self.assertGreater(result, 100)

        print(f"\n✓ Problem 16 Solution: {result}")
        print(f"  (2^1000 has {len(str(2**1000))} digits)")


class TestEdgeCases(unittest.TestCase):
    """Test edge cases"""

    def test_zero_exponent(self):
        """Any base^0 = 1, digit sum = 1"""
        self.assertEqual(power_digit_sum(2, 0), 1)
        self.assertEqual(power_digit_sum(10, 0), 1)
        self.assertEqual(power_digit_sum(999, 0), 1)

    def test_one_base(self):
        """1^n = 1 for any n, digit sum = 1"""
        self.assertEqual(power_digit_sum(1, 0), 1)
        self.assertEqual(power_digit_sum(1, 1), 1)
        self.assertEqual(power_digit_sum(1, 100), 1)
        self.assertEqual(power_digit_sum(1, 1000), 1)


if __name__ == "__main__":
    unittest.main(verbosity=2)
