module Main where

import MatrixProduct (parseMatrix, findMaxProductSequence, colorMatrixSequence)
import System.Environment (getArgs)
import System.IO (readFile)

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
