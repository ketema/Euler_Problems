#!/usr/bin/env python3
"""
Test suite for provenance FSM - Problem 957

TDD: Write tests FIRST, then implement.

Key insight from AI Panel:
{a,b} ∈ on_same_range IFF both a,b used to construct line L AND L has been accessed
(i.e., third point computed via a⊗b⊗c)
"""

import unittest
from typing import FrozenSet


class TestProvenanceFSM(unittest.TestCase):
    """Tests for provenance finite state machine"""

    def test_initial_state_empty(self):
        """Initially, no pairs are on_same_range"""
        from provenance_fsm import ProvenanceFSM

        fsm = ProvenanceFSM()
        assert not fsm.is_on_same_range('a', 'b')
        assert not fsm.is_on_same_range('b', 'a')  # Commutative

    def test_register_blue_creates_base_pair(self):
        """Registering blue x=a⊗b records base pair {a,b}"""
        from provenance_fsm import ProvenanceFSM

        fsm = ProvenanceFSM()
        fsm.register_blue('x', 'a', 'b')

        # Base pair recorded
        assert fsm.get_base_pairs('x') == {frozenset({'a', 'b'})}

        # But NOT yet on_same_range (line not accessed)
        assert not fsm.is_on_same_range('a', 'b')

    def test_line_access_marks_on_same_range(self):
        """Accessing line L(a,b) marks {a,b} as on_same_range"""
        from provenance_fsm import ProvenanceFSM

        fsm = ProvenanceFSM()
        fsm.register_blue('x', 'a', 'b')

        # Simulate: line L formed by {a,b} is accessed to compute third point
        fsm.access_line('a', 'b')

        # Now {a,b} is on_same_range
        assert fsm.is_on_same_range('a', 'b')
        assert fsm.is_on_same_range('b', 'a')  # Commutative

    def test_triangle_detection(self):
        """Detects complete triangle when all three edges exist"""
        from provenance_fsm import ProvenanceFSM

        fsm = ProvenanceFSM()

        # Create three blues from three pairs
        fsm.register_blue('x1', 'a', 'b')
        fsm.register_blue('x2', 'b', 'c')
        fsm.register_blue('x3', 'a', 'c')

        # Access all three lines
        fsm.access_line('a', 'b')
        fsm.access_line('b', 'c')
        fsm.access_line('a', 'c')

        # Triangle detected
        assert fsm.has_triangle('a', 'b', 'c')

    def test_triangle_not_detected_partial(self):
        """Triangle NOT detected if only 2/3 edges exist"""
        from provenance_fsm import ProvenanceFSM

        fsm = ProvenanceFSM()

        fsm.register_blue('x1', 'a', 'b')
        fsm.register_blue('x2', 'b', 'c')
        # Missing {a,c}

        fsm.access_line('a', 'b')
        fsm.access_line('b', 'c')

        assert not fsm.has_triangle('a', 'b', 'c')

    def test_day_0_to_1_scenario(self):
        """
        Realistic scenario: Day 0→1

        Day 0: 2 blues {b0_1, b0_2}
        Day 0→1: Form 6 intersections from 3 pencil pairs × C(2,2)=1 pair

        At day 0→1, the 6 raw intersections are:
          X(1,2; b0_1, b0_2) - pencil (R1,R2)
          X(1,3; b0_1, b0_2) - pencil (R1,R3)
          X(2,3; b0_1, b0_2) - pencil (R2,R3)

        These are 6 DISTINCT points (g(1)=8 total including 2 day-0 blues).

        But they all share base pair {b0_1, b0_2}.
        """
        from provenance_fsm import ProvenanceFSM

        fsm = ProvenanceFSM()

        # Day 0→1: Generate 6 intersections (only 1 unique pair at day 0)
        fsm.register_blue('r0_0', 'b0_1', 'b0_2')  # Pencil (1,2)
        fsm.register_blue('r0_1', 'b0_1', 'b0_2')  # Pencil (1,2)
        fsm.register_blue('r0_2', 'b0_1', 'b0_2')  # Pencil (1,3)
        fsm.register_blue('r0_3', 'b0_1', 'b0_2')  # Pencil (1,3)
        fsm.register_blue('r0_4', 'b0_1', 'b0_2')  # Pencil (2,3)
        fsm.register_blue('r0_5', 'b0_1', 'b0_2')  # Pencil (2,3)

        # Initially, {b0_1, b0_2} NOT on_same_range
        assert not fsm.is_on_same_range('b0_1', 'b0_2')

        # Day 1→2: Now we use day-1 blues to form new lines
        # Accessing ANY line that includes b0_1 and b0_2 marks them as on_same_range
        fsm.access_line('b0_1', 'b0_2')

        assert fsm.is_on_same_range('b0_1', 'b0_2')

    def test_cumulative_ranges(self):
        """Ranges are cumulative - once on_same_range, stays on_same_range"""
        from provenance_fsm import ProvenanceFSM

        fsm = ProvenanceFSM()
        fsm.register_blue('x', 'a', 'b')
        fsm.access_line('a', 'b')

        assert fsm.is_on_same_range('a', 'b')

        # Register another blue - range status persists
        fsm.register_blue('y', 'c', 'd')
        assert fsm.is_on_same_range('a', 'b')  # Still true


if __name__ == '__main__':
    # Run tests
    import sys

    suite = unittest.TestLoader().loadTestsFromTestCase(TestProvenanceFSM)
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    sys.exit(0 if result.wasSuccessful() else 1)
