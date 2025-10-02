#!/usr/bin/env python3
"""
Project Euler Problem 13: Large Sum
Python implementation using built-in arbitrary precision integers.
"""

from pathlib import Path
import sys
from typing import List


class Problem13Error(Exception):
    """Custom exception for Problem 13 errors."""
    pass


def parse_input_numbers(input_text: str) -> List[int]:
    """Parse input text into a list of integers."""
    lines = input_text.strip().split('\n')
    
    if len(lines) != 100:
        raise Problem13Error(f"Expected 100 numbers, got {len(lines)}")
    
    numbers = []
    for i, line in enumerate(lines, 1):
        line = line.strip()
        
        if len(line) != 50:
            raise Problem13Error(f"Line {i} has {len(line)} digits, expected 50")
        
        if not line.isdigit():
            raise Problem13Error(f"Line {i} contains non-digit characters")
        
        numbers.append(int(line))
    
    return numbers


def sum_large_numbers(numbers: List[int]) -> int:
    """Sum a list of large numbers."""
    return sum(numbers)


def extract_first_n_digits(number: int, n: int) -> str:
    """Extract the first n digits from a number."""
    number_str = str(number)
    
    if len(number_str) >= n:
        return number_str[:n]
    else:
        # Pad with zeros on the right to reach n digits
        return number_str.ljust(n, '0')


def solve_problem13(input_text: str) -> str:
    """Solve Project Euler Problem 13."""
    numbers = parse_input_numbers(input_text)
    total = sum_large_numbers(numbers)
    return extract_first_n_digits(total, 10)


def main():
    """Main entry point."""
    numbers_file = Path(__file__).parent.parent / "numbers.txt"
    
    try:
        input_text = numbers_file.read_text()
        result = solve_problem13(input_text)
        print(f"First ten digits of the sum: {result}")
    except FileNotFoundError:
        print(f"Error: Could not find {numbers_file}", file=sys.stderr)
        sys.exit(1)
    except Problem13Error as e:
        print(f"Error solving problem: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
