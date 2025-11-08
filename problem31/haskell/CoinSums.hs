{- Project Euler Problem 31: Coin Sums
   Purely functional implementation in Haskell
   Following TDD: RED → GREEN → REFACTOR

   Problem: Count number of ways to make 200p using UK coins
   Technique: Dynamic Programming with immutable arrays

   HASKELL FEATURES DEMONSTRATED:
   - Pure functions (referentially transparent)
   - Immutable data structures (Array)
   - Higher-order functions (foldl, map, replicate)
   - Pattern matching (guards in countWays)
   - Type inference (local functions inferred)
   - Lazy evaluation (arrays evaluated on demand)
   - Function composition (building complex from simple)
-}

module CoinSums (countWays, ukCoins, solve) where

import Data.Array

{- UK coin denominations in pence -}
ukCoins :: [Int]
ukCoins = [1, 2, 5, 10, 20, 50, 100, 200]

{- Count the number of ways to make 'amount' using given 'coins'

   Uses dynamic programming with an immutable array.

   Algorithm:
   - ways[0] = 1 (one way to make 0: use no coins)
   - For each coin c in coins:
       For each amount a from c to target:
         ways[a] += ways[a - c]

   This builds up the number of ways by considering each coin type
   in turn, updating the table to include combinations with that coin.
-}
countWays :: Int -> [Int] -> Int
countWays amount coins
  | amount < 0  = 0  -- negative amount: impossible
  | amount == 0 = 1  -- base case: one way to make 0
  | null coins  = 0  -- no coins: impossible to make positive amount
  | otherwise   = dp ! amount
  where
    -- Initialize DP array: ways[0] = 1, all others = 0
    -- Uses lazy evaluation - only computed elements are evaluated!
    initial :: Array Int Int
    initial = listArray (0, amount) (1 : replicate amount 0)

    -- Fold over each coin, updating the DP array
    -- Pure function - no mutation, just transformations
    dp :: Array Int Int
    dp = foldl updateForCoin initial coins

    -- Update DP array for a single coin
    -- Must process amounts sequentially to see previous updates
    -- Demonstrates: nested folds, immutable updates with (//)
    updateForCoin :: Array Int Int -> Int -> Array Int Int
    updateForCoin arr coin = foldl updateAmount arr [coin .. amount]
      where
        -- Update single amount: new_ways = old_ways + ways(amount - coin)
        updateAmount :: Array Int Int -> Int -> Array Int Int
        updateAmount a i = a // [(i, a ! i + a ! (i - coin))]

{- Solve Problem 31: Count ways to make 200p with UK coins -}
solve :: Int
solve = countWays 200 ukCoins
