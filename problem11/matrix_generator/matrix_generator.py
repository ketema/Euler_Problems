#!/usr/bin/env python3
import sys
import random

def main():
    USAGE = f"Usage: {sys.argv[0]} <size>\n<size> must be an integer between 1 and 100."
    MAX_SIZE = 100  # 100x100 is already 10,000 numbers; larger is unreadable on screen
    if len(sys.argv) != 2:
        print(USAGE, file=sys.stderr)
        sys.exit(1)
    try:
        n = int(sys.argv[1])
    except ValueError:
        print(USAGE, file=sys.stderr)
        sys.exit(1)
    if not (1 <= n <= MAX_SIZE):
        print(USAGE, file=sys.stderr)
        sys.exit(1)
    for _ in range(n):
        print(" ".join(f"{random.randint(1,99):02d}" for _ in range(n)))

if __name__ == "__main__":
    main()
