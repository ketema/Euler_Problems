module Main where

import qualified NaturalSum (someFunc)

main :: IO ()
main = do
  putStrLn "The sum of all the natural numbers < 1000 is: "
  NaturalSum.someFunc