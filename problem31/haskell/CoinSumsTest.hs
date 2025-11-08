{- Test Suite for Problem 31: Coin Sums
   Written FIRST following TDD methodology: RED → GREEN → REFACTOR

   These tests will FAIL initially because CoinSums module doesn't exist yet.
   This is CORRECT - we want RED phase first!
-}

module Main (main) where

import Test.HUnit
import CoinSums  -- This will fail compilation - proving tests written first!

-- Test: Base case - one way to make 0 (use no coins)
testZeroAmount :: Test
testZeroAmount = TestCase (assertEqual "ways to make 0" 1 (countWays 0 ukCoins))

-- Test: Single 1p coin - only one way to make any amount (all 1p)
testOnlyOnePence :: Test
testOnlyOnePence = TestList [
    TestCase (assertEqual "1p with only 1p coins" 1 (countWays 1 [1])),
    TestCase (assertEqual "5p with only 1p coins" 1 (countWays 5 [1])),
    TestCase (assertEqual "10p with only 1p coins" 1 (countWays 10 [1]))
  ]

-- Test: No coins available - zero ways for any positive amount
testNoCoins :: Test
testNoCoins = TestList [
    TestCase (assertEqual "1p with no coins" 0 (countWays 1 [])),
    TestCase (assertEqual "5p with no coins" 0 (countWays 5 [])),
    TestCase (assertEqual "100p with no coins" 0 (countWays 100 []))
  ]

-- Test: Amount less than smallest coin
testAmountLessThanSmallest :: Test
testAmountLessThanSmallest = TestCase
  (assertEqual "4p with coins [5,10]" 0 (countWays 4 [5,10]))

-- Test: Small known examples
-- 2p can be made in 2 ways: 1×2p, 2×1p
testTwoPence :: Test
testTwoPence = TestCase (assertEqual "ways to make 2p" 2 (countWays 2 [1,2]))

-- 5p can be made in 4 ways:
-- 1×5p, 1×2p+3×1p, 2×2p+1×1p, 5×1p
testFivePence :: Test
testFivePence = TestCase (assertEqual "ways to make 5p" 4 (countWays 5 [1,2,5]))

-- Test: 10p with coins [1,2,5,10]
testTenPence :: Test
testTenPence = TestCase (assertEqual "ways to make 10p" 11 (countWays 10 [1,2,5,10]))

-- Test: Verify UK coins are defined correctly
testUKCoins :: Test
testUKCoins = TestCase (assertEqual "UK coins" [1,2,5,10,20,50,100,200] ukCoins)

-- Test: Negative amount (should be 0 ways or handled gracefully)
testNegativeAmount :: Test
testNegativeAmount = TestCase (assertEqual "negative amount" 0 (countWays (-5) [1,2,5]))

-- Test: Full problem - 200p with all UK coins
-- We don't know the answer yet, but we can test it exists and is positive
testFullProblem :: Test
testFullProblem = TestCase (assertBool "200p should have positive ways"
                                        (countWays 200 ukCoins > 0))

-- Test: Verify answer is reasonable (not too large)
testReasonableAnswer :: Test
testReasonableAnswer = TestCase (assertBool "200p should have < 100000 ways"
                                              (countWays 200 ukCoins < 100000))

-- Test: DP property - ways(n) >= ways(n-1) for sorted coins
-- More amount shouldn't decrease number of ways
testMonotonicity :: Test
testMonotonicity = TestCase (assertBool "monotonicity"
                                         (countWays 10 [1,2,5] >= countWays 9 [1,2,5]))

-- Test: Symmetry - order of coins shouldn't matter (if sorted)
testCoinOrderIndependence :: Test
testCoinOrderIndependence = TestCase
  (assertEqual "coin order independence"
               (countWays 10 [1,2,5])
               (countWays 10 [5,2,1]))

-- Assemble all tests
tests :: Test
tests = TestList [
    TestLabel "base case - zero amount" testZeroAmount,
    TestLabel "only 1p coins" testOnlyOnePence,
    TestLabel "no coins available" testNoCoins,
    TestLabel "amount less than smallest coin" testAmountLessThanSmallest,
    TestLabel "2p example" testTwoPence,
    TestLabel "5p example" testFivePence,
    TestLabel "10p example" testTenPence,
    TestLabel "UK coins defined" testUKCoins,
    TestLabel "negative amount" testNegativeAmount,
    TestLabel "full problem exists" testFullProblem,
    TestLabel "reasonable answer" testReasonableAnswer,
    TestLabel "monotonicity property" testMonotonicity,
    TestLabel "coin order independence" testCoinOrderIndependence
  ]

-- Run all tests
main :: IO ()
main = do
  putStrLn "Testing Problem 31: Coin Sums"
  putStrLn "=============================="
  counts <- runTestTT tests
  putStrLn ""
  if errors counts + failures counts == 0
    then putStrLn "ALL TESTS PASSED!"
    else putStrLn "SOME TESTS FAILED!"
