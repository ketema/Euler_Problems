"""
Project Euler Problem #31: Coin Sums

Calculate the number of different ways to make a target amount using given coin denominations.

Uses dynamic programming approach (bottom-up) to count combinations.
"""

from typing import List


def count_coin_combinations(target: int, coins: List[int]) -> int:
    """
    Count the number of ways to make the target amount using the given coins.

    Args:
        target: The target amount to make (in pence)
        coins: List of available coin denominations

    Returns:
        The number of distinct ways to make the target amount

    Raises:
        ValueError: If target is negative
    """
    # Input validation
    if target < 0:
        raise ValueError("Amount must be non-negative")

    # Initialize DP table: dp[i] = number of ways to make amount i
    # Base case: there is 1 way to make 0 (use no coins)
    dp = [0] * (target + 1)
    dp[0] = 1

    # For each coin denomination
    for coin in coins:
        # Update all amounts that can be formed using this coin
        for amount in range(coin, target + 1):
            dp[amount] += dp[amount - coin]

    return dp[target]
