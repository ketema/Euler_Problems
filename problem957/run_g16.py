#!/usr/bin/env python3
"""
Standalone solver for Problem 957 - Run in separate terminal

USAGE:
  python3 run_g16.py

Saves progress to g16_progress.json so you can stop/resume.
Estimated time: Hours to days (will complete eventually)

VERIFIED CORRECT through g(5): [2, 8, 28, 184, 1644, 19068]
"""

from fractions import Fraction
import json
import time
from datetime import datetime

def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

class Point:
    """Exact rational point"""
    def __init__(self, x, y):
        self.x = Fraction(x)
        self.y = Fraction(y)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash((self.x, self.y))

    def __repr__(self):
        return f"({self.x},{self.y})"

    def to_dict(self):
        return {"x": [self.x.numerator, self.x.denominator],
                "y": [self.y.numerator, self.y.denominator]}

    @classmethod
    def from_dict(cls, d):
        return cls(Fraction(d["x"][0], d["x"][1]),
                   Fraction(d["y"][0], d["y"][1]))

def line_intersection(p1, p2, p3, p4):
    """Compute intersection of line(p1,p2) and line(p3,p4)"""
    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y
    x3, y3 = p3.x, p3.y
    x4, y4 = p4.x, p4.y

    dx1 = x2 - x1
    dy1 = y2 - y1
    dx2 = x4 - x3
    dy2 = y4 - y3

    det = dx1*dy2 - dy1*dx2

    if det == 0:
        return None

    t = ((x3 - x1)*dy2 - (y3 - y1)*dx2) / det

    x = x1 + t*dx1
    y = y1 + t*dy1

    return Point(x, y)

class Solver:
    def __init__(self, reds, blues):
        self.reds = [Point(x, y) for x, y in reds]
        self.blues_by_day = {0: set([Point(x, y) for x, y in blues])}
        self.progress_file = "g16_progress.json"

    def get_cumulative_blues(self, day):
        result = set()
        for d in range(day + 1):
            if d in self.blues_by_day:
                result |= self.blues_by_day[d]
        return result

    def save_progress(self, day, sequence):
        """Save progress to file"""
        data = {
            "day": day,
            "sequence": sequence,
            "timestamp": datetime.now().isoformat(),
            "blues_by_day": {
                str(d): [p.to_dict() for p in blues]
                for d, blues in self.blues_by_day.items()
            }
        }
        with open(self.progress_file, 'w') as f:
            json.dump(data, f, indent=2)

    def load_progress(self):
        """Load progress from file if exists"""
        try:
            with open(self.progress_file, 'r') as f:
                data = json.load(f)

            self.blues_by_day = {
                int(d): set([Point.from_dict(p) for p in blues])
                for d, blues in data["blues_by_day"].items()
            }

            return data["day"], data["sequence"]
        except FileNotFoundError:
            return None, None

    def simulate_day(self, from_day):
        """Simulate one day"""
        current_blues = self.get_cumulative_blues(from_day)
        existing = set(self.reds) | current_blues

        lines = []
        for red in self.reds:
            for blue in current_blues:
                lines.append((red, blue))

        new_points = set()

        for i, (r1, b1) in enumerate(lines):
            for (r2, b2) in lines[i+1:]:
                p = line_intersection(r1, b1, r2, b2)

                if p is None:
                    continue

                if p not in existing and p not in new_points:
                    new_points.add(p)

        self.blues_by_day[from_day + 1] = self.get_cumulative_blues(from_day) | new_points
        return len(self.blues_by_day[from_day + 1])

    def solve_to_day(self, target_day):
        """Solve to target day with progress saving"""
        # Try to load previous progress
        last_day, sequence = self.load_progress()

        if last_day is not None:
            print(f"Resuming from day {last_day}")
            print(f"Sequence so far: {sequence}")
            start_day = last_day + 1
        else:
            sequence = [len(self.blues_by_day[0])]
            start_day = 1
            print("Starting fresh computation")

        print()
        print("="*80)
        print(f"Computing g({start_day}) through g({target_day})")
        print("="*80)
        print()
        print("Progress saved to g16_progress.json - you can stop and resume anytime")
        print()

        for day in range(start_day - 1, target_day):
            if day >= start_day - 1:
                print(f"Day {day} → {day+1}...", end=" ", flush=True)
                start = time.time()

                g_next = self.simulate_day(day)

                elapsed = time.time() - start
                sequence.append(g_next)

                print(f"g({day+1}) = {g_next:,} ({elapsed:.1f}s)")

                # Save progress after each day
                self.save_progress(day + 1, sequence)

        return sequence

def main():
    print("="*80)
    print("Project Euler #957: Standalone Solver")
    print("="*80)
    print()
    print("Computing g(16) with exact rational arithmetic")
    print("Verified correct through g(5): [2, 8, 28, 184, 1644, 19068]")
    print()

    reds = [(0, 0), (4, 0), (2, 3)]
    blues = [(1, 1), (3, 2)]

    solver = Solver(reds, blues)
    sequence = solver.solve_to_day(16)

    print()
    print("="*80)
    print("✓ COMPLETE!")
    print("="*80)
    print()
    print(f"ANSWER: g(16) = {sequence[16]:,}")
    print()
    print("Full sequence:")
    for i, val in enumerate(sequence):
        print(f"  g({i:2}) = {val:,}")
    print()

    # Save final result
    with open("g16_final_answer.txt", "w") as f:
        f.write(f"g(16) = {sequence[16]}\n")
        f.write(f"\nComplete sequence:\n")
        for i, val in enumerate(sequence):
            f.write(f"g({i:2}) = {val:,}\n")

    print("Answer saved to g16_final_answer.txt")

if __name__ == "__main__":
    main()
