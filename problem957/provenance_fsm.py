#!/usr/bin/env python3
"""
Provenance Finite State Machine - Problem 957

Tracks provenance certificates for symbolic rewriting with Pappus/Desargues.

KEY INSIGHT (from AI Panel):
{a,b} ∈ on_same_range IFF both a,b used to construct line L AND L has been accessed
(i.e., third point computed via a⊗b⊗c)
"""

from typing import Set, Dict, FrozenSet
from collections import defaultdict


class ProvenanceFSM:
    """
    Finite state machine for tracking provenance certificates.

    State:
    - base_pairs: {blue_label -> set of frozenset({a,b})} - which pairs created each blue
    - range_pairs: set of frozenset({a,b}) - pairs that are on_same_range (line accessed)
    - accessed_triangles: set of frozenset({a,b,c}) - complete triangles detected
    """

    def __init__(self):
        # base_pairs[blue] = set of frozenset({a,b}) pairs that generated this blue
        self.base_pairs: Dict[str, Set[FrozenSet[str]]] = defaultdict(set)

        # Pairs {a,b} where line L(a,b) has been accessed
        self.range_pairs: Set[FrozenSet[str]] = set()

        # Complete triangles {a,b,c} where all three edges have been accessed
        self.accessed_triangles: Set[FrozenSet[str]] = set()

    def _make_pair(self, a: str, b: str) -> FrozenSet[str]:
        """Create unordered pair (commutative)"""
        return frozenset({a, b})

    def _make_triple(self, a: str, b: str, c: str) -> FrozenSet[str]:
        """Create unordered triple"""
        return frozenset({a, b, c})

    def register_blue(self, blue_label: str, a: str, b: str):
        """
        Register new blue point: blue_label = a ⊗ b

        Records base pair {a,b} for this blue.
        Does NOT mark {a,b} as on_same_range yet (line not accessed).
        """
        pair = self._make_pair(a, b)
        self.base_pairs[blue_label].add(pair)

    def access_line(self, a: str, b: str):
        """
        Mark that line L(a,b) has been accessed.

        This is the critical transition: {a,b} becomes on_same_range.

        Called when: line formed by {a,b} is used to compute a third point.
        """
        pair = self._make_pair(a, b)
        self.range_pairs.add(pair)

        # After marking pair, check for complete triangles
        self._update_triangles()

    def is_on_same_range(self, a: str, b: str) -> bool:
        """Check if {a,b} is on_same_range (line has been accessed)"""
        pair = self._make_pair(a, b)
        return pair in self.range_pairs

    def get_base_pairs(self, blue_label: str) -> Set[FrozenSet[str]]:
        """Get all base pairs {a,b} that generated this blue"""
        return self.base_pairs.get(blue_label, set())

    def has_triangle(self, a: str, b: str, c: str) -> bool:
        """
        Check if triangle {a,b,c} is complete.

        Triangle is complete when ALL three edges have been accessed:
        - {a,b} on_same_range
        - {b,c} on_same_range
        - {a,c} on_same_range
        """
        pair_ab = self._make_pair(a, b)
        pair_bc = self._make_pair(b, c)
        pair_ac = self._make_pair(a, c)

        return (pair_ab in self.range_pairs and
                pair_bc in self.range_pairs and
                pair_ac in self.range_pairs)

    def _update_triangles(self):
        """
        Update complete triangles after new range pair added.

        Scans all combinations of points in range_pairs to detect complete triangles.
        """
        # Get all unique points involved in any range pair
        all_points = set()
        for pair in self.range_pairs:
            all_points.update(pair)

        points_list = sorted(all_points)

        # Check all triples for completeness
        for i, a in enumerate(points_list):
            for j in range(i+1, len(points_list)):
                b = points_list[j]
                for k in range(j+1, len(points_list)):
                    c = points_list[k]

                    if self.has_triangle(a, b, c):
                        triple = self._make_triple(a, b, c)
                        self.accessed_triangles.add(triple)

    def get_triangle_edges(self) -> Set[FrozenSet[str]]:
        """Get all complete triangles"""
        return self.accessed_triangles.copy()


# Example usage demonstrating the FSM
if __name__ == '__main__':
    print("="*80)
    print("PROVENANCE FSM - Example")
    print("="*80)
    print()

    fsm = ProvenanceFSM()

    # Day 0→1: Generate blues
    print("Day 0→1: Generate 6 blues from {b0_1, b0_2}")
    fsm.register_blue('r0_0', 'b0_1', 'b0_2')
    fsm.register_blue('r0_1', 'b0_1', 'b0_2')
    print(f"  {b0_1, b0_2} on_same_range? {fsm.is_on_same_range('b0_1', 'b0_2')}")
    print()

    # Day 1→2: Access line L(b0_1, b0_2)
    print("Day 1→2: Access line L(b0_1, b0_2)")
    fsm.access_line('b0_1', 'b0_2')
    print(f"  {b0_1, b0_2} on_same_range? {fsm.is_on_same_range('b0_1', 'b0_2')}")
    print()

    # Triangle detection
    print("Triangle detection:")
    fsm.register_blue('x1', 'a', 'b')
    fsm.register_blue('x2', 'b', 'c')
    fsm.register_blue('x3', 'a', 'c')

    fsm.access_line('a', 'b')
    fsm.access_line('b', 'c')
    print(f"  {a,b,c} triangle complete (2/3 edges)? {fsm.has_triangle('a', 'b', 'c')}")

    fsm.access_line('a', 'c')
    print(f"  {a,b,c} triangle complete (3/3 edges)? {fsm.has_triangle('a', 'b', 'c')}")
    print()

    print("Complete triangles detected:", len(fsm.get_triangle_edges()))
