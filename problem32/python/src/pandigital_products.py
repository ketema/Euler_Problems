"""
Project Euler Problem #32: Pandigital Products

Find the sum of all products whose multiplicand/multiplier/product identity
can be written as a 1 through 9 pandigital.

Uses brute force search with pandigital validation.
"""


def is_pandigital(multiplicand: int, multiplier: int, product: int) -> bool:
    """
    Check if the multiplicand, multiplier, and product form a pandigital.

    A pandigital in this context means the concatenation of multiplicand, multiplier,
    and product uses each digit 1-9 exactly once.

    Args:
        multiplicand: First number in multiplication
        multiplier: Second number in multiplication
        product: Result of multiplicand × multiplier

    Returns:
        True if the combination is pandigital, False otherwise
    """
    # Concatenate all three numbers as strings
    concatenated = str(multiplicand) + str(multiplier) + str(product)

    # Check if length is exactly 9 (digits 1-9)
    if len(concatenated) != 9:
        return False

    # Check if it contains each digit 1-9 exactly once
    # Convert to set and compare with set of '1' through '9'
    return set(concatenated) == set('123456789')


def sum_pandigital_products() -> int:
    """
    Find the sum of all products that can be written as pandigital.

    Searches all possible 1×4-digit and 2×3-digit combinations where
    multiplicand × multiplier = product forms a 1-9 pandigital.

    Returns:
        The sum of all unique products that satisfy the pandigital constraint
    """
    # Use set to store unique products (some obtainable multiple ways)
    pandigital_products = set()

    # Pattern 1: 1-digit × 4-digit = 4-digit (1 + 4 + 4 = 9 digits)
    for multiplicand in range(1, 10):  # 1-digit: 1-9
        for multiplier in range(1000, 10000):  # 4-digit: 1000-9999
            product = multiplicand * multiplier
            if is_pandigital(multiplicand, multiplier, product):
                pandigital_products.add(product)

    # Pattern 2: 2-digit × 3-digit = 4-digit (2 + 3 + 4 = 9 digits)
    for multiplicand in range(10, 100):  # 2-digit: 10-99
        for multiplier in range(100, 1000):  # 3-digit: 100-999
            product = multiplicand * multiplier
            if is_pandigital(multiplicand, multiplier, product):
                pandigital_products.add(product)

    # Return sum of all unique products
    return sum(pandigital_products)
