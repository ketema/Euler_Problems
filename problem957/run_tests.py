#!/usr/bin/env python3
"""Simple test runner to verify all 32 tests pass."""

import subprocess
import sys

def main():
    cmd = [
        './venv/bin/pytest',
        'tests/test_multiday_simulation.py',
        '-v',
        '--tb=no'
    ]

    result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)

    print(result.stdout)
    if result.stderr:
        print("STDERR:", result.stderr, file=sys.stderr)

    sys.exit(result.returncode)

if __name__ == '__main__':
    main()
