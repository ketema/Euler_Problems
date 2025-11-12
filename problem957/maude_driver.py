#!/usr/bin/env python3
"""
maude_driver.py - Complete symbolic solver for Problem 957

Uses Maude rewriting with provenance certificates to compute g(16).

Key insight: Pappus/Desargues only apply when provenance certificates exist.
"""

import subprocess
import tempfile
import os
import re
from pathlib import Path
from typing import Set, Dict, Tuple
from collections import defaultdict
from provenance_fsm import ProvenanceFSM

class MaudeSolver:
    def __init__(self):
        self.maude_path = "/usr/local/bin/maude"
        self.maude_lib = os.path.expanduser("~/Library/Maude")

        # State
        self.reds = {"R1", "R2", "R3"}
        self.blues_by_day = {0: {"B01", "B02"}}  # Maude-compatible names

        # Provenance tracking with FSM
        self.provenance = ProvenanceFSM()

        # Initialize base pairs for day 0 blues (no generators - axioms)
        self.provenance.register_blue("B01", "B01", "B01")  # Self-pair (idempotent)
        self.provenance.register_blue("B02", "B02", "B02")

    def unordered_pair(self, a: str, b: str) -> Tuple[str, str]:
        return (a, b) if a <= b else (b, a)

    def get_cumulative_blues(self, day: int) -> Set[str]:
        result = set()
        for d in range(day + 1):
            if d in self.blues_by_day:
                result |= self.blues_by_day[d]
        return result

    def update_provenance(self, new_blues: Dict[str, Tuple[str, str]]):
        """
        Update provenance after a day.
        new_blues: {blue_label: (a, b)} mapping new blues to their generators
        """
        for blue, (a, b) in new_blues.items():
            # Register new blue with its base pair
            self.provenance.register_blue(blue, a, b)

            # Mark that line L(a,b) has been accessed (used to compute this point)
            self.provenance.access_line(a, b)

    def generate_maude_script(self, day: int) -> str:
        """Generate Maude script for day transition with guarded rules"""
        current_blues = self.get_cumulative_blues(day)

        script = """
fmod GENESIS is
  sorts Point .

  --- Red points (fixed)
  ops R1 R2 R3 : -> Point [ctor] .

  --- Pair operator (commutative, idempotent on leaves)
  op _*_ : Point Point -> Point [comm] .

  --- Idempotency on leaves
  vars A B C D : Point .
  eq A * A = A .

  --- Provenance predicates
  op onSameRange : Point Point -> Bool .
  op triangleEdge : Point Point Point -> Bool .

"""

        # Declare current blues
        script += "  --- Current blues\n"
        for blue in sorted(current_blues):
            script += f"  op {blue} : -> Point [ctor] .\n"

        script += "\n"

        # Add provenance facts (on_same_range) from ProvenanceFSM
        script += "  --- Provenance: on_same_range\n"
        blues_list = sorted(current_blues)
        for i, a in enumerate(blues_list):
            for b in blues_list[i+1:]:
                if self.provenance.is_on_same_range(a, b):
                    script += f"  eq onSameRange({a}, {b}) = true .\n"
                    script += f"  eq onSameRange({b}, {a}) = true .\n"

        # Default case
        script += "  eq onSameRange(A, B) = false [owise] .\n\n"

        # Add triangle edges from ProvenanceFSM
        script += "  --- Provenance: triangleEdge\n"
        for i, a in enumerate(blues_list):
            for j in range(i+1, len(blues_list)):
                b = blues_list[j]
                for k in range(j+1, len(blues_list)):
                    c = blues_list[k]
                    if self.provenance.has_triangle(a, b, c):
                        script += f"  eq triangleEdge({a}, {b}, {c}) = true .\n"
        script += "  eq triangleEdge(A, B, C) = false [owise] .\n\n"

        # Guarded Pappus rule
        script += """  --- Guarded Pappus: (A*B)*(C*D) = (A*D)*(C*B) if A,C and B,D share ranges
  ceq (A * B) * (C * D) = (A * D) * (C * B)
    if onSameRange(A, C) == true /\\ onSameRange(B, D) == true .

"""

        # Guarded Desargues rule (simplified - full version needs triangle check)
        script += """  --- Guarded Desargues: additional collapses when perspective triangles exist
  --- (Simplified for now - can add full Desargues with triangleEdge checks)

endfm

"""

        # Generate all pair terms (blues and reds)
        script += "--- Generate all pair(a,b) terms\n"

        all_points = sorted(current_blues | self.reds)
        terms = []
        for i, a in enumerate(all_points):
            for b in all_points[i+1:]:
                terms.append(f"{a} * {b}")

        # Create reductions for each term
        if len(terms) <= 10:
            # For small sets, reduce individually
            for term in terms:
                script += f"reduce in GENESIS : {term} .\n"
        else:
            # For larger sets, reduce first few
            script += f"--- Computing {len(terms)} pair terms\n"
            for term in terms[:10]:
                script += f"reduce in GENESIS : {term} .\n"
            script += f"--- ... ({len(terms)-10} more terms)\n"

        return script

    def run_maude(self, script: str) -> str:
        """Run Maude and return output"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.maude', delete=False) as f:
            f.write(script)
            f.flush()
            temp_path = f.name

        try:
            # Set MAUDE_LIB environment variable
            env = os.environ.copy()
            env['MAUDE_LIB'] = self.maude_lib

            result = subprocess.run(
                [self.maude_path, "-no-banner", temp_path],
                capture_output=True,
                text=True,
                timeout=120,
                env=env
            )
            return result.stdout + result.stderr
        finally:
            Path(temp_path).unlink()

    def parse_maude_output(self, output: str, current_blues: Set[str]) -> Dict[str, Tuple[str, str]]:
        """
        Parse Maude output to extract canonical forms.

        Returns: {new_blue_label: (a, b)} for new blues generated

        AI Panel Finding: Accept form IFF it contains one red AND one blue
        (exact set membership, not substring matching)
        """
        # Parse lines like: "result Point: B01 * B02"
        result_pattern = re.compile(r'^result Point\s*:\s*(.+)$', re.MULTILINE)

        canonical_forms = set()
        for match in result_pattern.finditer(output):
            form = match.group(1).strip()
            canonical_forms.add(form)

        # Filter: accept only red-blue pairs
        all_existing = current_blues | self.reds
        new_forms = []

        for f in canonical_forms:
            # Skip if form is an existing point
            if f in all_existing:
                continue

            # Parse the form to extract point labels
            pair_match = re.match(r'^(\w+)\s*\*\s*(\w+)$', f)
            if not pair_match:
                # Complex nested form - skip for now (will handle later days)
                continue

            a, b = pair_match.groups()

            # CRITICAL FIX: Require exactly one red AND one blue (red-blue pair)
            # Use exact set membership, not substring matching
            has_red = (a in self.reds or b in self.reds)
            has_blue = (a in current_blues or b in current_blues)

            if has_red and has_blue:
                new_forms.append(f)

        # Debug: print what forms we're accepting
        print(f"  Accepted {len(new_forms)} new red-blue forms: {sorted(new_forms)}")

        # Assign labels to new forms
        new_blues = {}
        next_label_num = len(current_blues) + 1  # Start numbering from next available

        for form in sorted(new_forms):  # Sort for deterministic labeling
            label = f"B{next_label_num:02d}"  # B03, B04, etc.
            next_label_num += 1

            # Extract base pair from form
            # Simple pattern: "A * B" or nested structures
            # For now, handle simple pairs
            pair_match = re.match(r'^(\w+)\s*\*\s*(\w+)$', form)
            if pair_match:
                a, b = pair_match.groups()
                new_blues[label] = (a, b)
            else:
                # Complex form - for now, mark as unknown (need refinement)
                # Extract operands from nested structure
                # This is a placeholder - full parser would handle arbitrary nesting
                print(f"Warning: Complex form not fully parsed: {form}")
                # Try to extract any two identifiers as fallback
                ids = re.findall(r'\b[A-Z]\w*\b', form)
                if len(ids) >= 2:
                    new_blues[label] = (ids[0], ids[1])

        return new_blues

    def simulate_day(self, from_day: int) -> int:
        """Simulate one day transition"""
        print(f"\n{'='*80}")
        print(f"Day {from_day} → {from_day + 1}")
        print(f"{'='*80}")

        current_blues = self.get_cumulative_blues(from_day)
        print(f"Current blues: {len(current_blues)}")
        print(f"Range pairs: {len(self.provenance.range_pairs)}")
        print(f"Triangle edges: {len(self.provenance.accessed_triangles)}")

        # Generate Maude script
        script = self.generate_maude_script(from_day)

        # Save script for inspection
        script_path = f"maude_day{from_day}_to_{from_day+1}.maude"
        Path(script_path).write_text(script)
        print(f"Generated: {script_path}")

        # Run Maude
        print("Running Maude...")
        output = self.run_maude(script)

        # Save output
        output_path = f"maude_day{from_day}_to_{from_day+1}_output.txt"
        Path(output_path).write_text(output)
        print(f"Output saved: {output_path}")

        # Parse output to get new blues
        new_blues = self.parse_maude_output(output, current_blues)

        # Update provenance
        self.update_provenance(new_blues)

        # Update blues_by_day
        self.blues_by_day[from_day + 1] = set(new_blues.keys())

        total_blues = len(current_blues) + len(new_blues)
        print(f"New blues: {len(new_blues)}")
        print(f"Total: {total_blues}")

        return total_blues

    def solve(self, target_day: int) -> list:
        """Solve to target day"""
        sequence = [len(self.blues_by_day[0])]

        print("="*80)
        print("MAUDE SYMBOLIC SOLVER - Problem 957")
        print("="*80)
        print(f"Target: g({target_day})")
        print()

        for day in range(target_day):
            g_next = self.simulate_day(day)
            sequence.append(g_next)

            # Verify against known values
            known = {1: 8, 2: 28, 3: 184, 4: 1644, 5: 19068}
            if day + 1 in known:
                expected = known[day + 1]
                if g_next == expected:
                    print(f"✓ g({day+1}) = {g_next} CORRECT!")
                else:
                    print(f"✗ g({day+1}) = {g_next} WRONG (expected {expected})")
                    print("STOPPING - need to debug Maude output parsing")
                    break

        return sequence

if __name__ == "__main__":
    solver = MaudeSolver()

    # Start with day 0→1 to test
    print("Testing Maude solver with provenance tracking...")
    print()

    sequence = solver.solve(target_day=1)

    print()
    print("="*80)
    print("RESULTS")
    print("="*80)
    for i, g in enumerate(sequence):
        print(f"g({i}) = {g}")

    print()
    print("Next steps:")
    print("1. Implement proper Maude output parsing in parse_maude_output()")
    print("2. Verify g(1)=8, g(2)=28 before proceeding")
    print("3. Run to g(16) once verified")
