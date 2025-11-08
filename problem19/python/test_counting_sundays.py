#!/usr/bin/env python3
"""Tests for Project Euler Problem 19: Counting Sundays"""

import unittest
from datetime import date
from counting_sundays import count_sundays_on_first

class TestCountingSundays(unittest.TestCase):
    """Test suite for counting Sundays on first of month"""

    def test_known_dates(self):
        """Test specific known Sunday dates"""
        # Jan 1, 1901 was a Tuesday
        self.assertEqual(date(1901, 1, 1).weekday(), 1)  # Tuesday

        # Jan 6, 1901 was a Sunday
        self.assertEqual(date(1901, 1, 6).weekday(), 6)  # Sunday

    def test_weekday_values(self):
        """Verify that Sunday is represented as 6"""
        # Find a known Sunday and verify
        known_sunday = date(2000, 1, 2)  # Jan 2, 2000 was a Sunday
        self.assertEqual(known_sunday.weekday(), 6)

    def test_leap_year_handling(self):
        """Test that leap years are handled correctly"""
        # 1904, 1908, etc. are leap years in 20th century
        # Feb 29, 1904 should exist
        leap_date = date(1904, 2, 29)
        self.assertEqual(leap_date.day, 29)
        self.assertEqual(leap_date.month, 2)

        # 1900 was NOT a leap year (divisible by 100 but not 400)
        # 2000 WAS a leap year (divisible by 400)
        leap_2000 = date(2000, 2, 29)
        self.assertEqual(leap_2000.day, 29)

    def test_date_range(self):
        """Test that we're checking the correct date range"""
        start = date(1901, 1, 1)
        end = date(2000, 12, 31)

        # Verify range is 100 years
        days_diff = (end - start).days
        # 100 years with 24 leap years (1904-2000, excluding 1900)
        expected_days = 365 * 100 + 24  # 36524 days
        self.assertEqual(days_diff, expected_days)

    def test_small_range(self):
        """Test counting logic with manual verification for small range"""
        # Manually check Jan-Dec 1901
        # We need to count Sundays that fall on the 1st of the month
        # This is hard to verify manually without the algorithm
        # So we'll just verify the function runs without error
        result = count_sundays_on_first()
        self.assertIsInstance(result, int)
        self.assertGreater(result, 0)

    def test_result_is_reasonable(self):
        """Test that result is within reasonable bounds"""
        result = count_sundays_on_first()

        # Over 100 years (1200 months), we'd expect roughly 1200/7 ≈ 171 Sundays
        # on the first of the month (since any day has 1/7 probability)
        self.assertGreater(result, 100)  # At least some Sundays
        self.assertLess(result, 300)     # Not more than 1/4 of all months

    def test_known_answer(self):
        """Test against the known correct answer"""
        result = count_sundays_on_first()
        self.assertEqual(result, 171)

    def test_months_per_year(self):
        """Verify we're checking exactly 12 months per year"""
        # This is more of a logic test - ensure we don't skip months
        start = date(1901, 1, 1)
        end = date(1901, 12, 31)

        months = []
        current = date(1901, 1, 1)
        while current.year == 1901:
            if current.day == 1:
                months.append(current.month)
            if current.month == 12:
                break
            current = date(current.year, current.month + 1, 1)

        self.assertEqual(len(months), 12)
        self.assertEqual(months, list(range(1, 13)))

if __name__ == '__main__':
    unittest.main()
