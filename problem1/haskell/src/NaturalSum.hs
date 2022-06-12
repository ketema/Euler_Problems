
module NaturalSum (someFunc) where

someFunc :: IO ()

divisibleBy3Or5 n = (n `mod` 3 == 0) || (n`mod` 5 == 0)

someFunc = print (sum (filter divisibleBy3Or5 [0..999]))