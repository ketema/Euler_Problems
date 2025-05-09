module Main (main) where

import Test.Hspec
import MatrixProduct

main :: IO ()
main = hspec $ do
  describe "parseMatrix" $ do
    it "parses a matrix from a string" $ do
      let input = "1 2 3\n4 5 6\n7 8 9\n"
      parseMatrix input `shouldBe` [[1,2,3],[4,5,6],[7,8,9]]

  describe "maxProductInMatrix" $ do
    let matrix = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
    it "finds max right (horizontal) product" $ do
      maxProductRight matrix 4 `shouldBe` 43680
    it "finds max down (vertical) product" $ do
      maxProductDown matrix 4 `shouldBe` 6144
    it "finds max diagonal down-right product" $ do
      maxProductDiagDownRight [[1,0,0,4],[0,6,7,0],[0,10,11,0],[13,0,0,16]] 4 `shouldBe` 1056
    it "finds max diagonal down-left product" $ do
      maxProductDiagDownLeft [[0,0,0,4],[0,0,7,0],[0,10,0,0],[13,0,0,0]] 4 `shouldBe` 3640
    it "finds the greatest product in the matrix" $ do
      maxProductInMatrix matrix 4 `shouldBe` 43680

  describe "findMaxProductSequence" $ do
    let matrix = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
    it "returns product and coordinates of max sequence" $ do
      findMaxProductSequence matrix 4 `shouldBe` (43680, [(3,0),(3,1),(3,2),(3,3)])

  describe "colorMatrixSequence" $ do
    let matrix = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        coords = [(3,0),(3,1),(3,2),(3,3)]
        output = colorMatrixSequence matrix coords
    it "colors the sequence in red" $ do
      output `shouldContain` "\ESC[31m13\ESC[0m"
      output `shouldContain` "\ESC[31m14\ESC[0m"
      output `shouldContain` "\ESC[31m15\ESC[0m"
      output `shouldContain` "\ESC[31m16\ESC[0m"
    it "does not color non-sequence values" $ do
      output `shouldContain` "01"
      output `shouldNotContain` "\ESC[31m01\ESC[0m"
