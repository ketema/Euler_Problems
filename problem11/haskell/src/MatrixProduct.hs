module MatrixProduct where

import Data.List (transpose, intercalate, maximumBy)
import System.Console.ANSI

parseMatrix :: String -> [[Int]]
parseMatrix = map (map read . words) . lines

maxProductRight :: [[Int]] -> Int -> Int
maxProductRight matrix n = maximum $
  [product (take n (drop c row)) | row <- matrix, c <- [0..length row - n]]

maxProductDown :: [[Int]] -> Int -> Int
maxProductDown matrix n = maxProductRight (transpose matrix) n

maxProductDiagDownRight :: [[Int]] -> Int -> Int
maxProductDiagDownRight matrix n = maximum $
  [product [matrix !! (r+i) !! (c+i) | i <- [0..n-1]]
    | r <- [0..rows-n], c <- [0..cols-n]]
  where rows = length matrix; cols = length (head matrix)

maxProductDiagDownLeft :: [[Int]] -> Int -> Int
maxProductDiagDownLeft matrix n = maximum $
  [product [matrix !! (r+i) !! (c-i) | i <- [0..n-1]]
    | r <- [0..rows-n], c <- [n-1..cols-1]]
  where rows = length matrix; cols = length (head matrix)

maxProductInMatrix :: [[Int]] -> Int -> Int
maxProductInMatrix matrix n = maximum
  [ maxProductRight matrix n
  , maxProductDown matrix n
  , maxProductDiagDownRight matrix n
  , maxProductDiagDownLeft matrix n
  ]

findMaxProductSequence :: [[Int]] -> Int -> (Int, [(Int, Int)])
findMaxProductSequence matrix n = maximumBy compareF sequences
  where
    rows = length matrix
    cols = length (head matrix)
    sequences = [ (product [matrix !! (r+i*dr) !! (c+i*dc) | i <- [0..n-1]], [(r+i*dr,c+i*dc) | i <- [0..n-1]])
                | r <- [0..rows-1], c <- [0..cols-1], (dr,dc) <- dirs, inBounds r c dr dc]
    dirs = [(0,1),(1,0),(1,1),(1,-1)]
    inBounds r c dr dc = all (\i -> let rr = r+i*dr; cc = c+i*dc in rr >= 0 && rr < rows && cc >= 0 && cc < cols) [0..n-1]
    compareF (a,_) (b,_) = compare a b

colorMatrixSequence :: [[Int]] -> [(Int, Int)] -> String
colorMatrixSequence matrix coords = intercalate "\n" $
  [unwords [colorIf (r, c) (matrix !! r !! c) | c <- [0..cols-1]] | r <- [0..rows-1]]
  where
    coordsSet = coords
    rows = length matrix
    cols = length (head matrix)
    colorIf rc val
      | rc `elem` coordsSet = "\ESC[31m" ++ pad2 val ++ "\ESC[0m"
      | otherwise = pad2 val
    pad2 x = let s = show x in if length s == 1 then '0':s else s