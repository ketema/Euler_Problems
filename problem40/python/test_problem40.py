"""
Test Suite for Project Euler Problem 40: Champernowne's Constant

Tests the find_digit(n) and solve() functions for calculating the nth digit
of Champernowne's constant: 0.123456789101112131415161718192021...

Requirements Coverage:
- REQ-P40-001: Input validation (positions <= 0)
- REQ-P40-002: Single-digit range (positions 1-9)
- REQ-P40-003: Group boundary transitions (9→10, 99→100, 999→1000)
- REQ-P40-004: Specific required positions (d₁, d₁₀, d₁₀₀, etc.)
- REQ-P40-005: Product calculation for d₁ × d₁₀ × d₁₀₀ × ... × d₁,₀₀₀,₀₀₀
"""

import unittest
from problem40 import find_digit, solve


class TestInputValidation(unittest.TestCase):
    """REQ-P40-001: Input validation tests"""

    def test_zero_position_raises_error(self):
        """
        REQ-P40-001: find_digit(n) must raise ValueError for n <= 0

        Test: Position 0 is invalid (constant starts at position 1)
        """
        with self.assertRaises(ValueError) as context:
            find_digit(0)

        error_message = str(context.exception)
        self.assertIn(
            "positive",
            error_message.lower(),
            "\n" + "="*80 + "\n"
            "ValidationError in test_zero_position_raises_error\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-001 (input validation for position n)\n\n"
            "Test: find_digit(0) must raise ValueError\n\n"
            "Input: find_digit(0)\n"
            "Expected: ValueError with message indicating position must be positive\n"
            f"Got: ValueError raised but message was: '{error_message}'\n\n"
            "Fix guidance: Validate that n > 0 at function entry. Raise ValueError\n"
            "with descriptive message like 'Position must be positive integer'.\n"
            "Champernowne's constant positions start at 1 (not 0).\n\n"
            "Reference: problem40/python/test_problem40.py::test_zero_position_raises_error\n"
            + "="*80
        )

    def test_negative_position_raises_error(self):
        """
        REQ-P40-001: find_digit(n) must raise ValueError for n <= 0

        Test: Negative positions are invalid
        """
        with self.assertRaises(ValueError) as context:
            find_digit(-5)

        error_message = str(context.exception)
        self.assertIn(
            "positive",
            error_message.lower(),
            "\n" + "="*80 + "\n"
            "ValidationError in test_negative_position_raises_error\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-001 (input validation for position n)\n\n"
            "Test: find_digit(-5) must raise ValueError\n\n"
            "Input: find_digit(-5)\n"
            "Expected: ValueError with message indicating position must be positive\n"
            f"Got: ValueError raised but message was: '{error_message}'\n\n"
            "Fix guidance: Validate that n > 0 at function entry. Reject negative\n"
            "values with ValueError. Position numbering starts at 1.\n\n"
            "Reference: problem40/python/test_problem40.py::test_negative_position_raises_error\n"
            + "="*80
        )


class TestSingleDigitRange(unittest.TestCase):
    """REQ-P40-002: Single-digit range (positions 1-9)"""

    def test_position_1(self):
        """
        REQ-P40-002: Position 1 must return digit 1

        Constant starts: 0.123456789101112...
        Position 1 (first digit after decimal) = 1
        """
        result = find_digit(1)
        self.assertEqual(
            result,
            1,
            "\n" + "="*80 + "\n"
            "AssertionError in test_position_1\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-002 (single-digit range positions 1-9)\n\n"
            "Test: find_digit(1) must return 1\n\n"
            "Input: find_digit(1)\n"
            "Expected: 1 (first digit of Champernowne's constant)\n"
            f"Got: {result}\n\n"
            "Fix guidance: Positions 1-9 contain single digits 1-9 respectively.\n"
            "The constant starts: 0.123456789101112131415...\n"
            "Position 1 is the first digit after the decimal point, which is 1.\n"
            "Handle the single-digit range (1-9 digits, positions 1-9) as first case.\n\n"
            "Reference: problem40/python/test_problem40.py::test_position_1\n"
            + "="*80
        )

    def test_position_5(self):
        """
        REQ-P40-002: Position 5 must return digit 5

        Constant: 0.123456789101112...
        Position 5 = 5
        """
        result = find_digit(5)
        self.assertEqual(
            result,
            5,
            "\n" + "="*80 + "\n"
            "AssertionError in test_position_5\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-002 (single-digit range positions 1-9)\n\n"
            "Test: find_digit(5) must return 5\n\n"
            "Input: find_digit(5)\n"
            "Expected: 5 (fifth digit in sequence)\n"
            f"Got: {result}\n\n"
            "Fix guidance: For positions 1-9, return the position itself as the digit.\n"
            "Position n in range [1, 9] corresponds to digit n.\n"
            "Sequence: 1, 2, 3, 4, 5, 6, 7, 8, 9, [then 1,0 from 10]...\n\n"
            "Reference: problem40/python/test_problem40.py::test_position_5\n"
            + "="*80
        )

    def test_position_9(self):
        """
        REQ-P40-002: Position 9 must return digit 9

        Constant: 0.123456789101112...
        Position 9 (last single-digit) = 9
        """
        result = find_digit(9)
        self.assertEqual(
            result,
            9,
            "\n" + "="*80 + "\n"
            "AssertionError in test_position_9\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-002 (single-digit range positions 1-9)\n\n"
            "Test: find_digit(9) must return 9\n\n"
            "Input: find_digit(9)\n"
            "Expected: 9 (last single-digit in sequence)\n"
            f"Got: {result}\n\n"
            "Fix guidance: Position 9 is the last position containing a single-digit\n"
            "number (the digit 9 itself). After position 9, we move to two-digit\n"
            "numbers starting with 10.\n"
            "Verify boundary: positions 1-9 use digits 1-9, position 10 starts '10'.\n\n"
            "Reference: problem40/python/test_problem40.py::test_position_9\n"
            + "="*80
        )


class TestGroupBoundaryTransitions(unittest.TestCase):
    """REQ-P40-003: Boundary transitions between digit groups"""

    def test_transition_single_to_double_digit(self):
        """
        REQ-P40-003: Transition from 9 to 10

        Position 9: last single-digit (9)
        Position 10: first digit of "10" (1)
        """
        result_9 = find_digit(9)
        result_10 = find_digit(10)

        self.assertEqual(
            result_9,
            9,
            "\n" + "="*80 + "\n"
            "AssertionError in test_transition_single_to_double_digit (position 9)\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-003 (boundary transition 9→10)\n\n"
            "Test: find_digit(9) must return 9 (last single-digit)\n\n"
            "Input: find_digit(9)\n"
            "Expected: 9\n"
            f"Got: {result_9}\n\n"
            "Fix guidance: Position 9 contains the digit 9 (last single-digit number).\n"
            "Single-digit numbers (1-9) occupy positions 1-9.\n"
            "This is the boundary before transitioning to two-digit numbers.\n\n"
            "Reference: problem40/python/test_problem40.py::test_transition_single_to_double_digit\n"
            + "="*80
        )

        self.assertEqual(
            result_10,
            1,
            "\n" + "="*80 + "\n"
            "AssertionError in test_transition_single_to_double_digit (position 10)\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-003 (boundary transition 9→10)\n\n"
            "Test: find_digit(10) must return 1 (first digit of '10')\n\n"
            "Input: find_digit(10)\n"
            "Expected: 1 (tens digit of 10)\n"
            f"Got: {result_10}\n\n"
            "Fix guidance: Position 10 starts the two-digit numbers.\n"
            "The number 10 contributes digits '1' and '0' at positions 10 and 11.\n"
            "Calculate: After 9 single-digit numbers (9 positions), position 10 begins\n"
            "two-digit range. Find which two-digit number and which digit within it.\n"
            "Formula: position 10 = first digit of first two-digit number (10).\n\n"
            "Reference: problem40/python/test_problem40.py::test_transition_single_to_double_digit\n"
            + "="*80
        )

    def test_position_11(self):
        """
        REQ-P40-003: Position 11 is second digit of "10"

        Position 11 should be '0' (units digit of 10)
        """
        result = find_digit(11)
        self.assertEqual(
            result,
            0,
            "\n" + "="*80 + "\n"
            "AssertionError in test_position_11\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-003 (two-digit number handling)\n\n"
            "Test: find_digit(11) must return 0 (second digit of '10')\n\n"
            "Input: find_digit(11)\n"
            "Expected: 0 (units digit of 10)\n"
            f"Got: {result}\n\n"
            "Fix guidance: Position 11 is the second digit of the number 10.\n"
            "Number 10 spans positions 10-11 with digits '1' and '0'.\n"
            "After determining which number contains position 11, extract the correct\n"
            "digit from that number. Use modulo to get digit at specific position\n"
            "within multi-digit numbers.\n\n"
            "Reference: problem40/python/test_problem40.py::test_position_11\n"
            + "="*80
        )

    def test_transition_double_to_triple_digit(self):
        """
        REQ-P40-003: Transition from 99 to 100

        Position 189: last digit of "99" (9)
        Position 190: first digit of "100" (1)

        Calculation:
        - Single digits (1-9): 9 positions
        - Two-digit numbers (10-99): 90 numbers × 2 digits = 180 positions
        - Total through 99: 9 + 180 = 189 positions
        """
        result_189 = find_digit(189)
        result_190 = find_digit(190)

        self.assertEqual(
            result_189,
            9,
            "\n" + "="*80 + "\n"
            "AssertionError in test_transition_double_to_triple_digit (position 189)\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-003 (boundary transition 99→100)\n\n"
            "Test: find_digit(189) must return 9 (last digit of '99')\n\n"
            "Input: find_digit(189)\n"
            "Expected: 9 (units digit of 99, last two-digit number)\n"
            f"Got: {result_189}\n\n"
            "Fix guidance: Position 189 is the last position in two-digit range.\n"
            "Calculation:\n"
            "- Single digits (1-9): 9 positions\n"
            "- Two-digit numbers (10-99): 90 numbers × 2 digits/number = 180 positions\n"
            "- Total: 9 + 180 = 189\n"
            "Position 189 contains the last digit of 99 (which is 9).\n"
            "Verify your position-to-number mapping for two-digit range.\n\n"
            "Reference: problem40/python/test_problem40.py::test_transition_double_to_triple_digit\n"
            + "="*80
        )

        self.assertEqual(
            result_190,
            1,
            "\n" + "="*80 + "\n"
            "AssertionError in test_transition_double_to_triple_digit (position 190)\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-003 (boundary transition 99→100)\n\n"
            "Test: find_digit(190) must return 1 (first digit of '100')\n\n"
            "Input: find_digit(190)\n"
            "Expected: 1 (hundreds digit of 100)\n"
            f"Got: {result_190}\n\n"
            "Fix guidance: Position 190 starts the three-digit numbers.\n"
            "After position 189 (end of two-digit range), position 190 begins\n"
            "three-digit range with number 100.\n"
            "Number 100 contributes digits '1', '0', '0' at positions 190, 191, 192.\n"
            "Verify your logic for determining which digit group a position falls into,\n"
            "then which number within that group, then which digit within that number.\n\n"
            "Reference: problem40/python/test_problem40.py::test_transition_double_to_triple_digit\n"
            + "="*80
        )

    def test_transition_triple_to_quadruple_digit(self):
        """
        REQ-P40-003: Transition from 999 to 1000

        Position 2889: last digit of "999" (9)
        Position 2890: first digit of "1000" (1)

        Calculation:
        - Single digits (1-9): 9 positions
        - Two-digit (10-99): 90 × 2 = 180 positions
        - Three-digit (100-999): 900 × 3 = 2700 positions
        - Total through 999: 9 + 180 + 2700 = 2889 positions
        """
        result_2889 = find_digit(2889)
        result_2890 = find_digit(2890)

        self.assertEqual(
            result_2889,
            9,
            "\n" + "="*80 + "\n"
            "AssertionError in test_transition_triple_to_quadruple_digit (position 2889)\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-003 (boundary transition 999→1000)\n\n"
            "Test: find_digit(2889) must return 9 (last digit of '999')\n\n"
            "Input: find_digit(2889)\n"
            "Expected: 9 (units digit of 999, last three-digit number)\n"
            f"Got: {result_2889}\n\n"
            "Fix guidance: Position 2889 is the last position in three-digit range.\n"
            "Calculation:\n"
            "- Single digits (1-9): 9 positions\n"
            "- Two-digit (10-99): 90 × 2 = 180 positions\n"
            "- Three-digit (100-999): 900 × 3 = 2700 positions\n"
            "- Total: 9 + 180 + 2700 = 2889\n"
            "Position 2889 contains the last digit of 999 (which is 9).\n\n"
            "Reference: problem40/python/test_problem40.py::test_transition_triple_to_quadruple_digit\n"
            + "="*80
        )

        self.assertEqual(
            result_2890,
            1,
            "\n" + "="*80 + "\n"
            "AssertionError in test_transition_triple_to_quadruple_digit (position 2890)\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-003 (boundary transition 999→1000)\n\n"
            "Test: find_digit(2890) must return 1 (first digit of '1000')\n\n"
            "Input: find_digit(2890)\n"
            "Expected: 1 (thousands digit of 1000)\n"
            f"Got: {result_2890}\n\n"
            "Fix guidance: Position 2890 starts the four-digit numbers.\n"
            "After position 2889 (end of three-digit range), position 2890 begins\n"
            "four-digit range with number 1000.\n"
            "Number 1000 contributes digits '1', '0', '0', '0' at positions 2890-2893.\n\n"
            "Reference: problem40/python/test_problem40.py::test_transition_triple_to_quadruple_digit\n"
            + "="*80
        )


class TestSpecificRequiredPositions(unittest.TestCase):
    """REQ-P40-004: Specific positions required by problem"""

    def test_d1(self):
        """REQ-P40-004: d₁ = 1"""
        result = find_digit(1)
        self.assertEqual(
            result,
            1,
            "\n" + "="*80 + "\n"
            "AssertionError in test_d1\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-004 (specific required position d₁)\n\n"
            "Test: find_digit(1) must return 1\n\n"
            "Input: find_digit(1)\n"
            "Expected: 1\n"
            f"Got: {result}\n\n"
            "Fix guidance: d₁ is explicitly required to be 1 by the problem.\n"
            "This is the first test position for the final product calculation.\n\n"
            "Reference: problem40/python/test_problem40.py::test_d1\n"
            + "="*80
        )

    def test_d10(self):
        """REQ-P40-004: d₁₀ = 1 (first digit of "10")"""
        result = find_digit(10)
        self.assertEqual(
            result,
            1,
            "\n" + "="*80 + "\n"
            "AssertionError in test_d10\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-004 (specific required position d₁₀)\n\n"
            "Test: find_digit(10) must return 1\n\n"
            "Input: find_digit(10)\n"
            "Expected: 1 (from number 10)\n"
            f"Got: {result}\n\n"
            "Fix guidance: d₁₀ is the 10th digit in the sequence.\n"
            "After single digits 1-9 (positions 1-9), position 10 contains the\n"
            "first digit of the two-digit number 10, which is 1.\n"
            "This is the second test position for the final product calculation.\n\n"
            "Reference: problem40/python/test_problem40.py::test_d10\n"
            + "="*80
        )

    def test_d100(self):
        """
        REQ-P40-004: d₁₀₀ = 5 (from "55")

        Calculation:
        - Positions 1-9: digits 1-9
        - Positions 10-189: two-digit numbers 10-99
        - Position 100 is in two-digit range
        - Position relative to start of two-digit: 100 - 9 = 91
        - Number: 10 + (91-1)//2 = 10 + 45 = 55
        - Digit within number: (91-1) % 2 = 0 (first digit)
        - First digit of 55 = 5
        """
        result = find_digit(100)
        self.assertEqual(
            result,
            5,
            "\n" + "="*80 + "\n"
            "AssertionError in test_d100\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-004 (specific required position d₁₀₀)\n\n"
            "Test: find_digit(100) must return 5\n\n"
            "Input: find_digit(100)\n"
            "Expected: 5 (first digit of number 55)\n"
            f"Got: {result}\n\n"
            "Fix guidance: d₁₀₀ is the 100th digit in the sequence.\n"
            "Position 100 falls in the two-digit range (positions 10-189).\n"
            "Calculation steps:\n"
            "1. Subtract positions before two-digit range: 100 - 9 = 91\n"
            "2. Each two-digit number contributes 2 digits\n"
            "3. Find which two-digit number: 10 + (91-1)//2 = 55\n"
            "4. Find which digit in 55: (91-1) %% 2 = 0 (first digit)\n"
            "5. First digit of 55 is 5\n"
            "This is the third test position for the final product calculation.\n\n"
            "Reference: problem40/python/test_problem40.py::test_d100\n"
            + "="*80
        )

    def test_d1000(self):
        """
        REQ-P40-004: d₁₀₀₀

        Calculate mathematically using the digit group formula
        """
        result = find_digit(1000)
        # Position 1000 is in three-digit range (positions 190-2889)
        # 1000 - 189 = 811 (position within three-digit range)
        # Number: 100 + (811-1)//3 = 100 + 270 = 370
        # Digit: (811-1) % 3 = 0 (first digit)
        # First digit of 370 = 3
        expected = 3

        self.assertEqual(
            result,
            expected,
            "\n" + "="*80 + "\n"
            "AssertionError in test_d1000\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-004 (specific required position d₁₀₀₀)\n\n"
            "Test: find_digit(1000) must return 3\n\n"
            "Input: find_digit(1000)\n"
            "Expected: 3 (first digit of number 370)\n"
            f"Got: {result}\n\n"
            "Fix guidance: d₁₀₀₀ is the 1000th digit in the sequence.\n"
            "Position 1000 falls in the three-digit range (positions 190-2889).\n"
            "Calculation steps:\n"
            "1. Cumulative positions before three-digit: 9 + 180 = 189\n"
            "2. Position within three-digit range: 1000 - 189 = 811\n"
            "3. Each three-digit number contributes 3 digits\n"
            "4. Find which number: 100 + (811-1)//3 = 370\n"
            "5. Find which digit: (811-1) %% 3 = 0 (first digit)\n"
            "6. First digit of 370 is 3\n"
            "This is the fourth test position for the final product calculation.\n\n"
            "Reference: problem40/python/test_problem40.py::test_d1000\n"
            + "="*80
        )

    def test_d10000(self):
        """
        REQ-P40-004: d₁₀,₀₀₀

        Calculate mathematically using the digit group formula
        """
        result = find_digit(10000)
        # Position 10000 is in four-digit range (starts at position 2890)
        # 10000 - 2889 = 7111 (position within four-digit range)
        # Number: 1000 + (7111-1)//4 = 1000 + 1777 = 2777
        # Digit: (7111-1) % 4 = 2 (third digit, 0-indexed)
        # Digits of 2777: 2, 7, 7, 7
        # Third digit (index 2) = 7
        expected = 7

        self.assertEqual(
            result,
            expected,
            "\n" + "="*80 + "\n"
            "AssertionError in test_d10000\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-004 (specific required position d₁₀,₀₀₀)\n\n"
            "Test: find_digit(10000) must return 7\n\n"
            "Input: find_digit(10000)\n"
            "Expected: 7 (third digit of number 2777)\n"
            f"Got: {result}\n\n"
            "Fix guidance: d₁₀,₀₀₀ is the 10000th digit in the sequence.\n"
            "Position 10000 falls in the four-digit range (starts at position 2890).\n"
            "Calculation steps:\n"
            "1. Cumulative positions before four-digit: 9 + 180 + 2700 = 2889\n"
            "2. Position within four-digit range: 10000 - 2889 = 7111\n"
            "3. Each four-digit number contributes 4 digits\n"
            "4. Find which number: 1000 + (7111-1)//4 = 2777\n"
            "5. Find which digit: (7111-1) %% 4 = 2 (third digit, 0-indexed)\n"
            "6. Digits of 2777 are [2, 7, 7, 7], third digit (index 2) is 7\n"
            "This is the fifth test position for the final product calculation.\n\n"
            "Reference: problem40/python/test_problem40.py::test_d10000\n"
            + "="*80
        )

    def test_d100000(self):
        """
        REQ-P40-004: d₁₀₀,₀₀₀

        Calculate mathematically using the digit group formula
        """
        result = find_digit(100000)
        # Position 100000 is in five-digit range (starts at position 38890)
        # Cumulative before five-digit: 9 + 180 + 2700 + 36000 = 38889
        # 100000 - 38889 = 61111 (position within five-digit range)
        # Number: 10000 + (61111-1)//5 = 10000 + 12022 = 22022
        # Digit: (61111-1) % 5 = 0 (first digit)
        # First digit of 22022 = 2
        expected = 2

        self.assertEqual(
            result,
            expected,
            "\n" + "="*80 + "\n"
            "AssertionError in test_d100000\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-004 (specific required position d₁₀₀,₀₀₀)\n\n"
            "Test: find_digit(100000) must return 2\n\n"
            "Input: find_digit(100000)\n"
            "Expected: 2 (first digit of number 22022)\n"
            f"Got: {result}\n\n"
            "Fix guidance: d₁₀₀,₀₀₀ is the 100000th digit in the sequence.\n"
            "Position 100000 falls in the five-digit range (starts at position 38890).\n"
            "Calculation steps:\n"
            "1. Cumulative before five-digit:\n"
            "   - 1-digit: 9 positions\n"
            "   - 2-digit: 90 × 2 = 180 positions\n"
            "   - 3-digit: 900 × 3 = 2700 positions\n"
            "   - 4-digit: 9000 × 4 = 36000 positions\n"
            "   - Total: 38889\n"
            "2. Position within five-digit range: 100000 - 38889 = 61111\n"
            "3. Find which number: 10000 + (61111-1)//5 = 22022\n"
            "4. Find which digit: (61111-1) %% 5 = 0 (first digit)\n"
            "5. First digit of 22022 is 2\n"
            "This is the sixth test position for the final product calculation.\n\n"
            "Reference: problem40/python/test_problem40.py::test_d100000\n"
            + "="*80
        )

    def test_d1000000(self):
        """
        REQ-P40-004: d₁,₀₀₀,₀₀₀

        Calculate mathematically using the digit group formula
        """
        result = find_digit(1000000)
        # Position 1000000 is in six-digit range (starts at position 488890)
        # Cumulative before six-digit: 9 + 180 + 2700 + 36000 + 450000 = 488889
        # 1000000 - 488889 = 511111 (position within six-digit range)
        # Number: 100000 + (511111-1)//6 = 100000 + 85185 = 185185
        # Digit: (511111-1) % 6 = 0 (first digit)
        # First digit of 185185 = 1
        expected = 1

        self.assertEqual(
            result,
            expected,
            "\n" + "="*80 + "\n"
            "AssertionError in test_d1000000\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-004 (specific required position d₁,₀₀₀,₀₀₀)\n\n"
            "Test: find_digit(1000000) must return 1\n\n"
            "Input: find_digit(1000000)\n"
            "Expected: 1 (first digit of number 185185)\n"
            f"Got: {result}\n\n"
            "Fix guidance: d₁,₀₀₀,₀₀₀ is the 1000000th digit in the sequence.\n"
            "Position 1000000 falls in the six-digit range (starts at position 488890).\n"
            "Calculation steps:\n"
            "1. Cumulative before six-digit:\n"
            "   - 1-digit: 9\n"
            "   - 2-digit: 180\n"
            "   - 3-digit: 2700\n"
            "   - 4-digit: 36000\n"
            "   - 5-digit: 90000 × 5 = 450000\n"
            "   - Total: 488889\n"
            "2. Position within six-digit range: 1000000 - 488889 = 511111\n"
            "3. Find which number: 100000 + (511111-1)//6 = 185185\n"
            "4. Find which digit: (511111-1) %% 6 = 0 (first digit)\n"
            "5. First digit of 185185 is 1\n"
            "This is the seventh test position for the final product calculation.\n\n"
            "Reference: problem40/python/test_problem40.py::test_d1000000\n"
            + "="*80
        )


class TestProductCalculation(unittest.TestCase):
    """REQ-P40-005: Product calculation function"""

    def test_solve_returns_product(self):
        """
        REQ-P40-005: solve() must return d₁ × d₁₀ × d₁₀₀ × d₁₀₀₀ × d₁₀₀₀₀ × d₁₀₀₀₀₀ × d₁₀₀₀₀₀₀

        Expected values:
        - d₁ = 1
        - d₁₀ = 1
        - d₁₀₀ = 5
        - d₁₀₀₀ = 3
        - d₁₀₀₀₀ = 7
        - d₁₀₀₀₀₀ = 2
        - d₁₀₀₀₀₀₀ = 1

        Product: 1 × 1 × 5 × 3 × 7 × 2 × 1 = 210
        """
        result = solve()
        expected = 1 * 1 * 5 * 3 * 7 * 2 * 1

        self.assertEqual(
            result,
            expected,
            "\n" + "="*80 + "\n"
            "AssertionError in test_solve_returns_product\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-005 (product calculation)\n\n"
            "Test: solve() must calculate d₁ × d₁₀ × d₁₀₀ × d₁₀₀₀ × d₁₀₀₀₀ × d₁₀₀₀₀₀ × d₁₀₀₀₀₀₀\n\n"
            "Expected product calculation:\n"
            "  d₁ = 1\n"
            "  d₁₀ = 1\n"
            "  d₁₀₀ = 5\n"
            "  d₁₀₀₀ = 3\n"
            "  d₁₀₀₀₀ = 7\n"
            "  d₁₀₀₀₀₀ = 2\n"
            "  d₁₀₀₀₀₀₀ = 1\n"
            f"Expected: {expected} (= 1 × 1 × 5 × 3 × 7 × 2 × 1)\n"
            f"Got: {result}\n\n"
            "Fix guidance: Implement solve() to:\n"
            "1. Call find_digit() for positions 1, 10, 100, 1000, 10000, 100000, 1000000\n"
            "2. Multiply all seven digits together\n"
            "3. Return the product as an integer\n"
            "Verify each find_digit() call returns correct value before multiplying.\n\n"
            "Reference: problem40/python/test_problem40.py::test_solve_returns_product\n"
            + "="*80
        )

    def test_solve_returns_integer(self):
        """
        REQ-P40-005: solve() must return integer type
        """
        result = solve()
        self.assertIsInstance(
            result,
            int,
            "\n" + "="*80 + "\n"
            "TypeError in test_solve_returns_integer\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Requirement: REQ-P40-005 (product calculation return type)\n\n"
            "Test: solve() must return integer type\n\n"
            f"Expected: int\n"
            f"Got: {type(result).__name__}\n\n"
            "Fix guidance: Ensure solve() returns an integer (not float or other type).\n"
            "All digit values are integers (0-9), and their product is an integer.\n"
            "Use int() if needed to ensure integer return type.\n\n"
            "Reference: problem40/python/test_problem40.py::test_solve_returns_integer\n"
            + "="*80
        )


class TestAdditionalEdgeCases(unittest.TestCase):
    """Additional edge cases for robustness"""

    def test_position_12(self):
        """Position 12 should be first digit of "11" (1)"""
        result = find_digit(12)
        self.assertEqual(
            result,
            1,
            "\n" + "="*80 + "\n"
            "AssertionError in test_position_12\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Test: find_digit(12) must return 1 (first digit of '11')\n\n"
            "Input: find_digit(12)\n"
            "Expected: 1 (tens digit of 11)\n"
            f"Got: {result}\n\n"
            "Fix guidance: Sequence at positions 10-13: 1, 0, 1, 1 (from '10', '11')\n"
            "Position 12 is the first digit of the number 11.\n"
            "Verify your position-to-digit mapping within two-digit range.\n\n"
            "Reference: problem40/python/test_problem40.py::test_position_12\n"
            + "="*80
        )

    def test_position_191(self):
        """Position 191 should be second digit of "100" (0)"""
        result = find_digit(191)
        self.assertEqual(
            result,
            0,
            "\n" + "="*80 + "\n"
            "AssertionError in test_position_191\n"
            "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
            "Test: find_digit(191) must return 0 (second digit of '100')\n\n"
            "Input: find_digit(191)\n"
            "Expected: 0 (tens digit of 100)\n"
            f"Got: {result}\n\n"
            "Fix guidance: Number 100 spans positions 190-192 with digits '1', '0', '0'.\n"
            "Position 191 is the second digit of 100, which is 0.\n"
            "After identifying which number (100), extract the correct digit position.\n\n"
            "Reference: problem40/python/test_problem40.py::test_position_191\n"
            + "="*80
        )


if __name__ == '__main__':
    unittest.main()
