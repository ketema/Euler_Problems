# Project Euler Problem 31: Coin Sums

## Problem Statement

In the United Kingdom the currency is made up of pound (£) and pence (p). There are eight coins in general circulation:

**Coins**: 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p)

**Question**: How many different ways can £2 (200p) be made using any number of coins?

## Example

For smaller amounts:
- **2p** can be made in 2 ways:
  - 1×2p
  - 2×1p

- **5p** can be made in 4 ways:
  - 1×5p
  - 1×2p + 3×1p
  - 2×2p + 1×1p
  - 5×1p

## Analysis

This is the classic **coin change problem** - a canonical example of dynamic programming.

### Problem Classification
- **Type**: Combinatorial counting
- **Technique**: Dynamic programming
- **Variant**: Number of ways (not minimum coins)
- **Constraint**: Unlimited coins of each denomination

### Approaches

#### 1. Recursive (Exponential)
```
count(amount, coins) =
  if amount == 0: return 1
  if amount < 0 or no coins: return 0
  return count(amount, coins[1..]) +           # don't use first coin
         count(amount - coins[0], coins)       # use first coin
```
**Complexity**: O(2^n) - too slow

#### 2. Dynamic Programming (Polynomial)
Build a table: `ways[i]` = number of ways to make amount `i`

```
ways[0] = 1  # base case
for each coin:
  for amount from coin to target:
    ways[amount] += ways[amount - coin]
```

**Complexity**: O(n × m) where n = target amount, m = number of coins

#### 3. Haskell Lazy DP
Use infinite lists with memoization - compute only what's needed!

### Mathematical Insight

This is a **partition function** variant - counting ways to partition a number using specific divisors (coin denominations).

For coins [c₁, c₂, ..., cₙ] and target T:
```
P(T, {c₁, ..., cₙ}) = P(T, {c₂, ..., cₙ}) + P(T - c₁, {c₁, ..., cₙ})
```

Base cases:
- P(0, coins) = 1 (one way to make 0: use no coins)
- P(n, {}) = 0 (no ways with no coins)
- P(n, coins) = 0 if n < 0

## Expected Answer

For **200p** with **8 UK coins**: The answer should be a moderate-sized integer (likely in thousands).

## Implementation Strategy

### Haskell Advantages

1. **Lazy Evaluation**: Build infinite DP table, compute only needed values
2. **Memoization**: Natural with lazy evaluation
3. **Pure Functions**: No mutation, just transformations
4. **Pattern Matching**: Clean handling of base cases
5. **Higher-Order Functions**: `foldr`, `scanl` for DP iterations
6. **Type Safety**: Compile-time guarantees

### Test Cases

1. **Base case**: ways(0, any_coins) = 1
2. **Single coin**: ways(n, [1]) = 1 (only all 1p)
3. **Small amounts**: ways(5, [1,2,5]) = 4
4. **No coins**: ways(n, []) = 0 for n > 0
5. **Amount < smallest coin**: Should return 0
6. **Full problem**: ways(200, [1,2,5,10,20,50,100,200]) = ?

## Complexity Target

- **Time**: O(200 × 8) = O(1600) ≈ constant for this problem
- **Space**: O(200) for DP array
- **Execution**: < 1 millisecond (Haskell compiles to fast native code)

## Why This Problem Suits Haskell

1. **Recursive elegance**: Natural expression of recurrence relation
2. **Lazy DP**: Infinite lists computed on demand
3. **Purity**: No mutable state, just transformations
4. **Composability**: Build complex solution from simple functions
5. **Type safety**: Catch errors at compile time
6. **Performance**: GHC optimizer produces fast code

This will showcase Haskell's strengths beautifully!
