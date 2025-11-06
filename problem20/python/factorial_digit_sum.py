#!/usr/bin/env python3
"""Problem 20: Factorial Digit Sum

Find the sum of the digits in the number n!

n! means n × (n − 1) × ... × 3 × 2 × 1
For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
"""

import math

def factorial_digit_sum(n: int) -> int:
    """Calculate sum of digits in n!

    Args:
        n: Number to calculate factorial of

    Returns:
        Sum of digits in n!

    Examples:
        >>> factorial_digit_sum(10)
        27
        >>> factorial_digit_sum(5)
        3
    """
    if n < 0:
        raise ValueError("Factorial not defined for negative numbers")

    factorial = math.factorial(n)
    digit_sum = sum(int(digit) for digit in str(factorial))
    return digit_sum

def factorial_digit_sum_manual(n: int) -> int:
    """Calculate sum of digits in n! without using math.factorial

    This version manually computes the factorial for demonstration.

    Args:
        n: Number to calculate factorial of

    Returns:
        Sum of digits in n!
    """
    if n < 0:
        raise ValueError("Factorial not defined for negative numbers")

    factorial = 1
    for i in range(2, n + 1):
        factorial *= i

    digit_sum = sum(int(digit) for digit in str(factorial))
    return digit_sum

if __name__ == "__main__":
    # Verify example from problem
    example = factorial_digit_sum(10)
    print(f"10! digit sum: {example}")
    assert example == 27, f"Expected 27, got {example}"

    # Solve the main problem
    result = factorial_digit_sum(100)
    print(f"\nProblem 20 Answer: {result}")

    # Verify both methods agree
    manual_result = factorial_digit_sum_manual(100)
    assert result == manual_result, "Methods should produce same result"

    # Show the actual factorial (first and last digits)
    factorial_100 = math.factorial(100)
    factorial_str = str(factorial_100)
    print(f"\n100! has {len(factorial_str)} digits")
    print(f"First 20 digits: {factorial_str[:20]}")
    print(f"Last 20 digits:  {factorial_str[-20:]}")
