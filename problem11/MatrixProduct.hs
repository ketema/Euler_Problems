module MatrixProduct (
    parseMatrix,
    maxProductRight,
    maxProductDown,
    maxProductDiagDownRight,
    maxProductDiagDownLeft,
    maxProductInMatrix,
    findMaxProductSequence,
    colorMatrixSequence,
    main
) where

import Data.List (transpose, intercalate)
import Data.Char (isSpace)
import System.Environment (getArgs)
import System.IO (readFile)

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
findMaxProductSequence matrix n = maximum sequences
  where
    rows = length matrix
    cols = length (head matrix)
    right = [ (product vals, [(r, c+i) | i <- [0..n-1]])
            | r <- [0..rows-1], c <- [0..cols-n]
            , let vals = [matrix !! r !! (c+i) | i <- [0..n-1]]]
    down = [ (product vals, [(r+i, c) | i <- [0..n-1]])
           | r <- [0..rows-n], c <- [0..cols-1]
           , let vals = [matrix !! (r+i) !! c | i <- [0..n-1]]]
    diagDR = [ (product vals, [(r+i, c+i) | i <- [0..n-1]])
             | r <- [0..rows-n], c <- [0..cols-n]
             , let vals = [matrix !! (r+i) !! (c+i) | i <- [0..n-1]]]
    diagDL = [ (product vals, [(r+i, c-i) | i <- [0..n-1]])
             | r <- [0..rows-n], c <- [n-1..cols-1]
             , let vals = [matrix !! (r+i) !! (c-i) | i <- [0..n-1]]]
    sequences = right ++ down ++ diagDR ++ diagDL

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      let matrix = parseMatrix content
          (product, coords) = findMaxProductSequence matrix 4
      putStrLn $ colorMatrixSequence matrix coords
      putStrLn $ "Greatest product of four adjacent numbers: " ++ show product
    _ -> putStrLn "Usage: MatrixProduct <matrix_file>"
