"""
Project Euler Problem 14: Longest Collatz Sequence

WHY: Find which starting number under 1 million produces longest Collatz chain
EXPECTED: Efficient solution using memoization, all tests pass

The Collatz sequence:
- n → n/2 (if n is even)
- n → 3n + 1 (if n is odd)
- Continue until reaching 1
"""

from typing import Dict, Tuple


# Memoization cache for chain lengths
_cache: Dict[int, int] = {1: 1}


def collatz_length(n: int, use_cache: bool = True) -> int:
    """
    Calculate the length of the Collatz sequence starting at n.

    WHY: Core algorithm for computing chain lengths efficiently
    EXPECTED: Returns accurate chain length, uses memoization

    Args:
        n: Starting positive integer
        use_cache: Whether to use memoization (default True)

    Returns:
        Length of the Collatz chain from n to 1 (inclusive)

    Examples:
        >>> collatz_length(1)
        1
        >>> collatz_length(13)
        10
        >>> collatz_length(2)
        2
    """
    if n <= 0:
        raise ValueError("n must be a positive integer")

    # Check cache first
    if use_cache and n in _cache:
        return _cache[n]

    # Calculate iteratively to avoid stack overflow
    original_n = n
    path = []  # Track path for caching intermediate values

    # Build the path until we hit a cached value or reach 1
    while n not in _cache:
        path.append(n)

        # Apply Collatz rules
        if n % 2 == 0:
            n = n // 2
        else:
            n = 3 * n + 1

    # Now n is either in cache or is 1 (which is in cache)
    # Calculate length for the original number
    chain_length = len(path) + _cache[n]

    # Cache all values in the path (backward caching for efficiency)
    if use_cache:
        for i in range(len(path) - 1, -1, -1):
            chain_length_for_path_element = len(path) - i + _cache[n]
            _cache[path[i]] = chain_length_for_path_element

    return _cache[original_n] if use_cache else chain_length


def find_longest_collatz(limit: int) -> Tuple[int, int]:
    """
    Find the starting number under 'limit' that produces the longest Collatz chain.

    WHY: Solve the main problem - find optimal starting number
    EXPECTED: Returns (starting_number, chain_length) for longest chain

    Args:
        limit: Upper bound (exclusive) for starting numbers

    Returns:
        Tuple of (starting_number, chain_length) with maximum chain length

    Examples:
        >>> find_longest_collatz(10)
        (9, 20)
    """
    if limit <= 1:
        raise ValueError("limit must be greater than 1")

    max_length = 0
    max_number = 0

    for n in range(1, limit):
        length = collatz_length(n)

        if length > max_length:
            max_length = length
            max_number = n

    return (max_number, max_length)


def clear_cache() -> None:
    """
    Clear the memoization cache.

    WHY: Useful for testing and memory management
    EXPECTED: Cache is reset to initial state
    """
    global _cache
    _cache = {1: 1}


def get_collatz_sequence(n: int) -> list:
    """
    Generate the full Collatz sequence for debugging/visualization.

    WHY: Helpful for understanding and verifying chains
    EXPECTED: Returns complete sequence from n to 1

    Args:
        n: Starting positive integer

    Returns:
        List of all numbers in the Collatz sequence

    Examples:
        >>> get_collatz_sequence(13)
        [13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
    """
    if n <= 0:
        raise ValueError("n must be a positive integer")

    sequence = []

    while n != 1:
        sequence.append(n)

        if n % 2 == 0:
            n = n // 2
        else:
            n = 3 * n + 1

    sequence.append(1)
    return sequence


if __name__ == "__main__":
    # Solve the problem
    print("Solving Project Euler Problem 14...")
    print("Finding longest Collatz sequence for starting numbers < 1,000,000")
    print()

    number, length = find_longest_collatz(1_000_000)

    print(f"Answer: {number}")
    print(f"Chain length: {length}")
    print()
    print(f"Sequence preview (first 20 terms):")
    sequence = get_collatz_sequence(number)
    print(" → ".join(map(str, sequence[:20])))
    if len(sequence) > 20:
        print(f"... ({len(sequence) - 20} more terms)")
