#!/usr/bin/env python3
"""Tests for Problem 23: Non-Abundant Sums"""
import unittest
from non_abundant_sums import sum_divisors, abundant_numbers, solve

class TestNonAbundantSums(unittest.TestCase):
    def test_sum_divisors_edge_cases(self):
        self.assertEqual(sum_divisors(1), 0)  # No proper divisors
        self.assertEqual(sum_divisors(2), 1)  # Only 1
        self.assertEqual(sum_divisors(3), 1)  # Prime

    def test_sum_divisors_perfect_numbers(self):
        self.assertEqual(sum_divisors(6), 6)   # 1+2+3
        self.assertEqual(sum_divisors(28), 28) # 1+2+4+7+14

    def test_sum_divisors_abundant_numbers(self):
        self.assertEqual(sum_divisors(12), 16) # 1+2+3+4+6
        self.assertEqual(sum_divisors(18), 21) # 1+2+3+6+9
        self.assertEqual(sum_divisors(20), 22) # 1+2+4+5+10

    def test_abundant_numbers_first_is_12(self):
        abundants = abundant_numbers(20)
        self.assertEqual(abundants[0], 12)
        self.assertIn(12, abundants)
        self.assertIn(18, abundants)
        self.assertIn(20, abundants)

    def test_abundant_numbers_count(self):
        abundants = abundant_numbers(28123)
        self.assertGreater(len(abundants), 6000)

    def test_solve_returns_correct_answer(self):
        self.assertEqual(solve(), 4179871)

if __name__ == '__main__':
    unittest.main()
