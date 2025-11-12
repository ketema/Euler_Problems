#!/usr/bin/env python3
"""
egglog_driver.py - Python driver for Problem 957 symbolic solver

Orchestrates egglog equality saturation:
1. Declares current blues as consts
2. Adds provenance facts (on_same_range, triangle_edge)
3. Generates pair(a,b) terms for all a≠b
4. Runs saturation
5. Extracts e-class IDs to count distinct blues
6. Filters against colored set
"""

import subprocess
import tempfile
from pathlib import Path
from typing import Set, Dict, List, Tuple

class EgglogSolver:
    def __init__(self):
        self.genesis_template = Path("genesis.egg").read_text()
        self.reds = {"R1", "R2", "R3"}
        self.blues_by_day = {0: {"b0_1", "b0_2"}}
        self.provenance: Dict[str, Set[Tuple[str, str]]] = {}  # blue -> {(a,b) pairs that created it}

    def unordered_pair(self, a: str, b: str) -> Tuple[str, str]:
        return (a, b) if a <= b else (b, a)

    def get_cumulative_blues(self, day: int) -> Set[str]:
        """Get all blues through given day"""
        result = set()
        for d in range(day + 1):
            if d in self.blues_by_day:
                result |= self.blues_by_day[d]
        return result

    def generate_egglog_script(self, day: int) -> str:
        """Generate egglog script for one day transition"""
        script = self.genesis_template + "\n\n"
        script += f"; ===== DAY {day} → {day+1} =====\n\n"

        current_blues = self.get_cumulative_blues(day)
        colored = self.reds | current_blues

        # Declare current blues as consts
        script += "; Current blues as consts\n"
        for b in sorted(current_blues):
            script += f"(const {b} Point)\n"
        script += "\n"

        # Add provenance facts (on_same_range)
        script += "; Provenance facts (on_same_range)\n"
        for blue, pairs in self.provenance.items():
            for a, b in pairs:
                script += f"(on_same_range {a} {b})\n"
        script += "\n"

        # Generate all pair(a,b) terms for a≠b
        script += "; Generate all pair(a,b) terms\n"
        blues_list = sorted(current_blues)
        for i, a in enumerate(blues_list):
            for b in blues_list[i+1:]:
                script += f"(let p_{a}_{b} (pair {a} {b}))\n"
        script += "\n"

        # Run saturation
        script += "; Run saturation\n"
        script += "(run 20)\n\n"

        # Extract e-class IDs for all pairs
        script += "; Extract e-class IDs\n"
        for i, a in enumerate(blues_list):
            for b in blues_list[i+1:]:
                script += f"(query-extract p_{a}_{b})\n"

        return script

    def run_egglog(self, script: str) -> str:
        """Run egglog and return output"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.egg', delete=False) as f:
            f.write(script)
            f.flush()
            temp_path = f.name

        try:
            result = subprocess.run(
                ["/Users/ketema/.local/share/cargo/bin/egglog", temp_path],
                capture_output=True,
                text=True,
                timeout=60
            )
            return result.stdout
        finally:
            Path(temp_path).unlink()

    def parse_egglog_output(self, output: str) -> Dict[str, str]:
        """Parse egglog output to extract e-class IDs for each term

        Output format from query-extract:
        (pair b0_1 b0_2) -> <some eclass representation>

        We need to map each pair to its canonical e-class ID
        """
        eclass_map = {}
        # Simple parsing - extract lines with query results
        for line in output.split('\n'):
            if '(pair' in line and '->' in line:
                # Extract term and e-class
                # Format: "(pair a b) -> <eclass>"
                parts = line.split('->')
                if len(parts) == 2:
                    term = parts[0].strip()
                    eclass = parts[1].strip()
                    eclass_map[term] = eclass
        return eclass_map

    def simulate_day(self, from_day: int) -> int:
        """Simulate one day using egglog"""
        print(f"\nDay {from_day} → {from_day + 1}")

        current_blues = self.get_cumulative_blues(from_day)
        colored = self.reds | current_blues

        print(f"  Current blues: {len(current_blues)}")

        # Generate and run egglog
        script = self.generate_egglog_script(from_day)
        output = self.run_egglog(script)

        print(f"  Egglog output length: {len(output)} chars")

        # Parse e-class IDs
        eclass_map = self.parse_egglog_output(output)

        # Count distinct e-classes (each is a distinct blue)
        distinct_eclasses = set(eclass_map.values())

        print(f"  Generated {len(eclass_map)} pair terms")
        print(f"  Distinct e-classes: {len(distinct_eclasses)}")

        # For now, approximate new blues count
        # In full implementation, need to filter colored and assign labels

        new_blues_count = len(distinct_eclasses)

        return len(current_blues) + new_blues_count

    def solve(self, target_day: int) -> List[int]:
        """Solve to target day"""
        sequence = [len(self.blues_by_day[0])]

        for day in range(target_day):
            g_next = self.simulate_day(day)
            sequence.append(g_next)

            # Update blues_by_day (simplified - need proper labeling in full version)
            # self.blues_by_day[day + 1] = new_blues

        return sequence

if __name__ == "__main__":
    print("="*80)
    print("EGGLOG SYMBOLIC SOLVER - Problem 957")
    print("="*80)
    print()

    solver = EgglogSolver()

    # Test day 0 → 1
    print("Testing day 0 → 1...")
    sequence = solver.solve(1)

    print()
    print("RESULTS:")
    for i, g in enumerate(sequence):
        print(f"  g({i}) = {g}")

    print()
    if len(sequence) > 1:
        print(f"g(1) = {sequence[1]} (expected: 8)")
        if sequence[1] == 8:
            print("✓ CORRECT!")
        else:
            print("✗ Wrong - need to debug egglog output parsing")
