{- Main program to solve Problem 31: Coin Sums -}

module Main where

import CoinSums

main :: IO ()
main = do
  putStrLn "PROJECT EULER PROBLEM 31: COIN SUMS"
  putStrLn "===================================="
  putStrLn ""
  putStrLn "UK Coins: 1p, 2p, 5p, 10p, 20p, 50p, 100p, 200p"
  putStrLn ""

  -- Show some examples
  putStrLn "Examples:"
  putStrLn $ "  2p can be made in " ++ show (countWays 2 [1,2]) ++ " ways"
  putStrLn $ "  5p can be made in " ++ show (countWays 5 [1,2,5]) ++ " ways"
  putStrLn $ " 10p can be made in " ++ show (countWays 10 [1,2,5,10]) ++ " ways"
  putStrLn ""

  -- Solve the full problem
  let answer = solve
  putStrLn $ "ANSWER: How many ways can 200p be made?"
  putStrLn $ "        " ++ show answer ++ " ways"
