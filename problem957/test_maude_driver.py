#!/usr/bin/env python3
"""
Test suite for maude_driver.py - MaudeSolver integration tests
"""

import unittest
from maude_driver import MaudeSolver


class TestMaudeSolverParsing(unittest.TestCase):
    """Test parse_maude_output filtering logic"""

    def setUp(self):
        """Create solver instance for testing"""
        self.solver = MaudeSolver()

    def test_parse_rejects_blue_blue_pairs_at_day_0(self):
        """
        At day 0->1, blue-blue pairs should NOT be counted as new blues.
        Only red-blue pairs generate new intersection points.

        AI Panel Finding: Line 195 accepts B01 * B02 incorrectly
        """
        # Simulate Maude output from day 0->1
        maude_output = """
==========================================
reduce in GENESIS : B01 * B02 .
result Point: B01 * B02
==========================================
reduce in GENESIS : R1 * B01 .
result Point: R1 * B01
==========================================
reduce in GENESIS : R1 * B02 .
result Point: R1 * B02
==========================================
reduce in GENESIS : R2 * B01 .
result Point: R2 * B01
==========================================
reduce in GENESIS : R2 * B02 .
result Point: R2 * B02
==========================================
reduce in GENESIS : R3 * B01 .
result Point: R3 * B01
==========================================
reduce in GENESIS : R3 * B02 .
result Point: R3 * B02
"""

        current_blues = {"B01", "B02"}

        # Parse output
        new_blues = self.solver.parse_maude_output(maude_output, current_blues)

        # Should get exactly 6 new blues (red-blue pairs only)
        self.assertEqual(len(new_blues), 6,
                        f"Expected 6 new blues, got {len(new_blues)}: {list(new_blues.keys())}")

        # All new blues should be red-blue pairs
        for label, (a, b) in new_blues.items():
            has_red = (a in self.solver.reds or b in self.solver.reds)
            has_blue = (a in current_blues or b in current_blues)
            self.assertTrue(has_red and has_blue,
                          f"Blue {label} = ({a}, {b}) is not a red-blue pair")

    def test_parse_accepts_only_red_blue_combinations(self):
        """
        Filtering should accept form IFF it contains one red AND one blue.

        Should accept: R1*B01, R2*B02, etc.
        Should reject: B01*B02 (blue-blue), R1*R2 (red-red)
        """
        maude_output = """
result Point: R1 * B01
result Point: B01 * B02
result Point: R1 * R2
"""
        current_blues = {"B01", "B02"}
        new_blues = self.solver.parse_maude_output(maude_output, current_blues)

        # Should accept only R1*B01 (red-blue pair)
        self.assertEqual(len(new_blues), 1,
                        f"Expected 1 new blue (R1*B01), got {len(new_blues)}")

        # Verify the accepted form
        label = list(new_blues.keys())[0]
        a, b = new_blues[label]
        self.assertTrue(
            (a in self.solver.reds and b in current_blues) or
            (b in self.solver.reds and a in current_blues),
            f"Accepted form ({a}, {b}) is not a red-blue pair"
        )

    def test_day_0_to_1_produces_8_total_blues(self):
        """
        Integration test: g(0)=2, g(1)=8

        Day 0: 2 blues (B01, B02)
        Day 0->1: 6 new blues (red-blue intersections)
        Total: 8 cumulative blues
        """
        sequence = self.solver.solve(target_day=1)

        self.assertEqual(sequence[0], 2, f"g(0) should be 2, got {sequence[0]}")
        self.assertEqual(sequence[1], 8, f"g(1) should be 8, got {sequence[1]}")


if __name__ == '__main__':
    unittest.main()
