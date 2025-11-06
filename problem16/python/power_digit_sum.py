"""
Project Euler Problem 16: Power Digit Sum

WHY: Calculate sum of digits in 2^1000
EXPECTED: Simple solution using Python's arbitrary precision integers
"""


def power_digit_sum(base: int, exponent: int) -> int:
    """
    Calculate the sum of digits of base^exponent.

    WHY: Core function for solving digit sum problems
    EXPECTED: Returns accurate sum for any base and exponent

    Args:
        base: The base number
        exponent: The power to raise base to

    Returns:
        Sum of all digits in base^exponent

    Examples:
        >>> power_digit_sum(2, 15)
        26
        >>> power_digit_sum(10, 3)
        1
    """
    # Calculate the power
    number = base ** exponent

    # Convert to string and sum digits
    digit_sum = sum(int(digit) for digit in str(number))

    return digit_sum


def get_digit_list(number: int) -> list:
    """
    Get list of individual digits from a number.

    WHY: Useful for analysis and verification
    EXPECTED: Returns digits in order (most significant first)

    Args:
        number: The number to extract digits from

    Returns:
        List of digits

    Examples:
        >>> get_digit_list(32768)
        [3, 2, 7, 6, 8]
    """
    return [int(d) for d in str(number)]


if __name__ == "__main__":
    print("Solving Project Euler Problem 16...")
    print("Calculating sum of digits in 2^1000\n")

    # Verify with given example
    example = power_digit_sum(2, 15)
    print(f"Example: 2^15 = {2**15}, digit sum = {example}")
    assert example == 26, f"Example should be 26, got {example}"

    # Solve the actual problem
    result = power_digit_sum(2, 1000)

    print(f"\nAnswer: {result}")
    print(f"(2^1000 has {len(str(2**1000))} digits)")
