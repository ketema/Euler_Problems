#!/usr/bin/env python3
"""
Project Euler Problem 40: Champernowne's Constant

An irrational decimal fraction is created by concatenating the positive integers:
0.123456789101112131415161718192021...

Find specific digits in this sequence and calculate their product.
"""


def _get_first_number(digit_length: int) -> int:
    """Get the first number with specified digit length.

    Args:
        digit_length: Number of digits (1 for 1-9, 2 for 10-99, etc.)

    Returns:
        First number: 1 for digit_length=1, 10^(digit_length-1) otherwise

    Examples:
        - digit_length=1: 1
        - digit_length=2: 10
        - digit_length=3: 100
    """
    return 1 if digit_length == 1 else 10 ** (digit_length - 1)


def _get_group_info(digit_length: int) -> tuple[int, int]:
    """Calculate count and total digits for k-digit numbers.

    Args:
        digit_length: Number of digits (1 for 1-9, 2 for 10-99, etc.)

    Returns:
        Tuple of (count, total_digits_in_group)
        - count: How many numbers have this digit length
        - total_digits: Total digits contributed by this group

    Examples:
        - digit_length=1: 9 numbers (1-9), 9 total digits
        - digit_length=2: 90 numbers (10-99), 180 total digits
        - digit_length=3: 900 numbers (100-999), 2700 total digits
    """
    # For k-digit numbers: 10^(k-1) to 10^k - 1
    # Count = 10^k - 10^(k-1) = 9 * 10^(k-1)
    first_number = _get_first_number(digit_length)
    count = 9 * first_number if digit_length > 1 else 9

    total_digits = count * digit_length
    return count, total_digits


def _calculate_number_from_offset(position_in_group: int, digit_length: int) -> tuple[int, int]:
    """Calculate which number and digit index from offset within a digit group.

    Args:
        position_in_group: Offset position within the group (0-indexed)
        digit_length: Number of digits in this group

    Returns:
        Tuple of (number, digit_index)
        - number: The actual number containing the digit
        - digit_index: Which digit of that number (0-indexed from left)

    Examples:
        - position_in_group=0, digit_length=1: (1, 0) - first digit of 1
        - position_in_group=0, digit_length=2: (10, 0) - first digit of 10
        - position_in_group=2, digit_length=2: (11, 0) - first digit of 11
    """
    # Which number in this group?
    number_index = position_in_group // digit_length

    # Which digit within that number?
    digit_index = position_in_group % digit_length

    # Calculate the actual number
    first_number = _get_first_number(digit_length)
    number = first_number + number_index

    return number, digit_index


def _find_number_and_position(n: int) -> tuple[int, int]:
    """Map position n to (number, digit_index).

    Args:
        n: Position in Champernowne's constant (1-indexed)

    Returns:
        Tuple of (number, digit_index)
        - number: The actual number containing the nth digit
        - digit_index: Which digit of that number (0-indexed from left)

    Examples:
        - n=1: (1, 0) - first digit of number 1
        - n=10: (10, 0) - first digit of number 10
        - n=12: (11, 0) - first digit of number 11

    Raises:
        RuntimeError: If position mapping fails (safety check)
    """
    # Track cumulative position as we move through digit groups
    cumulative_position = 0
    digit_length = 1

    # Find which group (1-digit, 2-digit, etc.) contains position n
    while True:
        # Safety check: prevent infinite loop for unexpected edge cases
        # digit_length > 20 would mean numbers > 10^19, unreasonable for practical use
        if digit_length > 20:
            raise RuntimeError(f"Unexpected: position mapping failed for n={n}")

        count, total_digits = _get_group_info(digit_length)

        # Check if position n falls within this group
        if cumulative_position + total_digits >= n:
            # Position n is in this group
            # Calculate offset within this group (0-indexed)
            position_in_group = n - cumulative_position - 1

            # Delegate to helper function for number calculation
            number, digit_index = _calculate_number_from_offset(position_in_group, digit_length)

            return number, digit_index

        # Move to next group
        cumulative_position += total_digits
        digit_length += 1


def find_digit(n: int) -> int:
    """Find the nth digit of Champernowne's constant.

    Champernowne's constant is formed by concatenating positive integers:
    123456789101112131415...

    Args:
        n: Position of the digit to find (1-indexed)

    Returns:
        The digit at position n

    Raises:
        ValueError: If n <= 0 or n > 10^15 (unreasonably large)

    Examples:
        >>> find_digit(1)
        1
        >>> find_digit(10)
        1
        >>> find_digit(12)
        1
    """
    # Input validation
    if n <= 0:
        raise ValueError("Position n must be positive")

    # Upper bound check: prevent performance degradation for extreme inputs
    # Position > 10^15 would require excessive computation time
    if n > 10**15:
        raise ValueError("Position too large (max 10^15)")

    # Find which number and which digit within that number
    number, digit_index = _find_number_and_position(n)

    # Extract the specific digit from the number
    number_str = str(number)

    # Defensive check: ensure index is valid before accessing
    assert 0 <= digit_index < len(number_str), \
        f"Invalid digit_index {digit_index} for number {number} (length {len(number_str)})"

    digit = int(number_str[digit_index])

    return digit


def solve() -> int:
    """Calculate the product d₁ × d₁₀ × d₁₀₀ × d₁₀₀₀ × d₁₀₀₀₀ × d₁₀₀₀₀₀ × d₁₀₀₀₀₀₀

    Returns:
        Product of digits at positions 1, 10, 100, 1000, 10000, 100000, 1000000
    """
    positions = [1, 10, 100, 1000, 10000, 100000, 1000000]

    product = 1
    for pos in positions:
        digit = find_digit(pos)
        product *= digit

    return product


if __name__ == '__main__':
    print(solve())
