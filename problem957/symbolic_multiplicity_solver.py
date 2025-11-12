#!/usr/bin/env python3
"""
Problem 957: Symbolic Solver with Multiplicity Constraint

KEY INSIGHT (from AI Panel analysis):
"color blue all white points where TWO OR MORE lines intersect"
means: multiplicity >= 2, not just pairwise intersection

This solver encodes multiplicity as a SYMBOLIC constraint, avoiding
coordinate simulation entirely.

Approach:
1. Lines encoded as symbolic objects (red_id, blue_id)
2. Intersections encoded as symbolic expressions
3. Multiplicity tracked via line-to-point incidence relation
4. Points with multiplicity >= 2 are colored blue

This avoids O(n^4) coordinate intersection computation.
"""

from dataclasses import dataclass
from typing import Set, Dict, FrozenSet, Tuple, List
from collections import defaultdict
import time

@dataclass(frozen=True, order=True)
class Red:
    """Fixed red point (pencil center)"""
    id: int  # 1, 2, or 3

    def __repr__(self):
        return f"R{self.id}"

@dataclass(frozen=True, order=True)
class Blue:
    """Blue point label"""
    day: int
    index: int

    def __repr__(self):
        return f"b{self.day}_{self.index}"

Point = Red | Blue

@dataclass(frozen=True, order=True)
class Line:
    """Symbolic line through (red, blue)"""
    red: Red
    blue: Blue

    def __repr__(self):
        return f"L({self.red},{self.blue})"

@dataclass(frozen=True, order=True)
class Intersection:
    """Symbolic intersection of two lines"""
    line1: Line
    line2: Line

    def __post_init__(self):
        # Canonical ordering
        if self.line2 < self.line1:
            object.__setattr__(self, 'line1', self.line2)
            object.__setattr__(self, 'line2', self.line1)

    def __repr__(self):
        return f"X({self.line1}∩{self.line2})"

    def is_degenerate(self) -> bool:
        """Check if lines share both endpoints (same line)"""
        return (self.line1.red == self.line2.red and
                self.line1.blue == self.line2.blue)

    def is_existing_point(self, reds: Set[Red], blues: Set[Blue]) -> Red | Blue | None:
        """Check if intersection is at an existing red or blue point"""
        # Two lines through same red meet at that red
        if self.line1.red == self.line2.red:
            return self.line1.red

        # Line(R1, b) ∩ Line(R2, b) meets at b (both pass through b)
        if self.line1.blue == self.line2.blue:
            return self.line1.blue

        return None

class SymbolicMultiplicitySolver:
    def __init__(self):
        self.reds = frozenset({Red(1), Red(2), Red(3)})
        self.blues_by_day: Dict[int, Set[Blue]] = {
            0: {Blue(0, 1), Blue(0, 2)}
        }

        # Provenance: which intersection expressions created each blue
        self.provenance: Dict[Blue, Set[Intersection]] = {}

        # Initialize provenance for day 0 seeds
        for blue in self.blues_by_day[0]:
            self.provenance[blue] = set()

    def get_all_blues(self, day: int) -> Set[Blue]:
        """Get cumulative blues through day"""
        result = set()
        for d in range(day + 1):
            if d in self.blues_by_day:
                result.update(self.blues_by_day[d])
        return result

    def construct_lines(self, blues: Set[Blue]) -> Set[Line]:
        """Construct all lines (red, blue)"""
        lines = set()
        for red in self.reds:
            for blue in blues:
                lines.add(Line(red, blue))
        return lines

    def generate_intersections(self, lines: Set[Line]) -> Set[Intersection]:
        """Generate all pairwise intersections"""
        lines_list = sorted(lines)
        intersections = set()

        for i, line1 in enumerate(lines_list):
            for line2 in lines_list[i+1:]:
                inters = Intersection(line1, line2)
                if not inters.is_degenerate():
                    intersections.add(inters)

        return intersections

    def compute_multiplicity_map(self,
                                  intersections: Set[Intersection],
                                  lines: Set[Line],
                                  existing_blues: Set[Blue]) -> Dict[Intersection, int]:
        """
        Compute symbolic multiplicity: how many lines pass through each intersection.

        Key insight: We can determine multiplicity SYMBOLICALLY without coordinates!

        For intersection X(L1, L2):
        - At minimum, 2 lines pass through it (L1 and L2)
        - Additional lines L3 pass through it if L3 is collinear with X

        In our pencil structure:
        - X( L(R1,b1), L(R2,b2) ) has multiplicity = number of lines from any pencil
          that pass through this point
        """
        multiplicity_map = {}

        # Build incidence relation: which lines pass through which intersections
        # This is SYMBOLIC - no coordinate computation!
        for inters in intersections:
            # At minimum, the two defining lines pass through this intersection
            passing_lines = {inters.line1, inters.line2}

            # Check if any other lines also pass through this intersection
            # In projective geometry with pencils, we can determine this symbolically

            # For our specific configuration (3 pencils), additional coincidences occur when:
            # 1. Three lines form a triangle → their pairwise intersections are distinct
            # 2. Pappus/Desargues configurations → systematic coincidences

            # Simplified: For now, only count the two defining lines
            # (Full implementation would check Pappus/Desargues patterns)

            multiplicity_map[inters] = len(passing_lines)

        return multiplicity_map

    def filter_by_multiplicity(self,
                               intersections: Set[Intersection],
                               multiplicity_map: Dict[Intersection, int],
                               min_multiplicity: int = 2) -> Set[Intersection]:
        """Filter intersections by multiplicity >= threshold"""
        return {inters for inters in intersections
                if multiplicity_map[inters] >= min_multiplicity}

    def deduplicate_intersections(self,
                                   intersections: Set[Intersection],
                                   existing: Set[Point]) -> Dict[Intersection, Blue]:
        """
        Map intersections to new blue labels, deduplicating:
        1. Intersections at existing points (reds or blues)
        2. Multiple intersections that are the same geometric point

        This is where symbolic rewriting (Pappus/Desargues) would be applied.
        """
        new_blues = {}
        blue_counter = 1
        day = max(self.blues_by_day.keys()) + 1

        for inters in sorted(intersections):  # Deterministic ordering
            # Check if intersection is at an existing point
            existing_point = inters.is_existing_point(self.reds,
                                                       self.get_all_blues(day - 1))
            if existing_point is not None:
                continue  # Skip, it's at an existing point

            # Check if we've already created a blue for this intersection
            # (In full implementation, would check if inters is equivalent to
            #  another intersection via Pappus/Desargues rewrite rules)

            # For now, each distinct intersection gets a new blue
            new_label = Blue(day, blue_counter)
            blue_counter += 1
            new_blues[inters] = new_label

            # Record provenance
            self.provenance[new_label] = {inters}

        return new_blues

    def simulate_day(self, from_day: int, verbose: bool = True) -> Set[Blue]:
        """Simulate one day transition symbolically"""
        if verbose:
            print(f"\n{'='*70}")
            print(f"Day {from_day} → Day {from_day + 1}")
            print(f"{'='*70}")

        current_blues = self.get_all_blues(from_day)

        if verbose:
            print(f"Current blues: {len(current_blues)}")

        # Step 1: Construct lines
        lines = self.construct_lines(current_blues)
        if verbose:
            print(f"Lines constructed: {len(lines)}")

        # Step 2: Generate all intersections
        intersections = self.generate_intersections(lines)
        if verbose:
            print(f"Intersection candidates: {len(intersections)}")

        # Step 3: Compute multiplicity symbolically
        multiplicity_map = self.compute_multiplicity_map(
            intersections, lines, current_blues
        )

        # Step 4: Filter by multiplicity >= 2
        valid_intersections = self.filter_by_multiplicity(
            intersections, multiplicity_map, min_multiplicity=2
        )

        if verbose:
            print(f"After multiplicity filter (>=2): {len(valid_intersections)}")

        # Step 5: Deduplicate and assign blue labels
        new_blues_map = self.deduplicate_intersections(
            valid_intersections,
            self.reds | current_blues
        )

        new_blues = set(new_blues_map.values())

        # Update state
        next_blues = current_blues | new_blues
        self.blues_by_day[from_day + 1] = next_blues

        if verbose:
            print(f"New blues created: {len(new_blues)}")
            print(f"g({from_day + 1}) = {len(next_blues)}")

        return next_blues

    def solve_to_day(self, target_day: int) -> List[int]:
        """Solve symbolically to target day"""
        print("="*80)
        print("SYMBOLIC MULTIPLICITY SOLVER")
        print("="*80)
        print()
        print("Encoding multiplicity constraint symbolically (no coordinates)")
        print()

        sequence = [len(self.blues_by_day[0])]

        for day in range(target_day):
            start = time.time()
            self.simulate_day(day, verbose=True)
            elapsed = time.time() - start

            g_next = len(self.get_all_blues(day + 1))
            sequence.append(g_next)

            print(f"  Time: {elapsed:.3f}s")

        return sequence

def main():
    solver = SymbolicMultiplicitySolver()

    print("Testing base cases...")
    sequence = solver.solve_to_day(2)

    print()
    print("="*80)
    print("RESULTS")
    print("="*80)
    print()
    print(f"Sequence: {sequence}")
    print()
    print(f"g(1) = {sequence[1]} (expected: 8)")
    print(f"g(2) = {sequence[2]} (expected: 28)")
    print()

    if sequence[1] == 8 and sequence[2] == 28:
        print("✓ Base cases CORRECT!")
        print()
        print("Extending to g(16)...")
        sequence = solver.solve_to_day(16)
        print()
        print(f"✓ ANSWER: g(16) = {sequence[16]:,}")
    else:
        print("✗ Base cases WRONG - need to fix multiplicity logic")
        print()
        print("DIAGNOSIS:")
        print("  Simplified multiplicity counting (only 2 lines per intersection)")
        print("  Need to implement full Pappus/Desargues coincidence detection")

if __name__ == "__main__":
    main()
