# Haskell Solution for Matrix Product Problem

This folder contains the Haskell implementation for finding the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in a matrix.

## Quick Start

**Simple commands - no cabal knowledge required:**

```sh
make test   # Run tests
make run    # Run solution
```

That's it! The Makefile handles all cabal configuration automatically.

## Project Structure

- `src/MatrixProduct.hs`: Core library module with matrix product functions
- `src/MatrixProductTest.hs`: HSpec test suite
- `app/Main.hs`: Executable entry point
- `MatrixProduct.cabal`: Cabal package configuration
- `Makefile`: Simple build automation
- `matrix.txt`: Input matrix (20x20 grid)

## Requirements

- GHC 9.12.2 or compatible (tested with system GHC via Homebrew)
- Cabal 3.16.0 or later
- Make (standard on macOS/Linux)
- Dependencies: base, hspec, ansi-terminal (auto-installed)

## Installation

If you don't have GHC and Cabal installed:

```sh
# macOS (Homebrew)
brew install ghc cabal-install
```

No need to run `cabal update` - the Makefile handles setup automatically.

## Build & Run

### Using Make (Recommended - Simple!)

```sh
# Run tests
make test

# Run solution
make run

# Build only
make build

# Clean build artifacts
make clean
```

### Using Cabal Directly (Advanced)

```sh
# Run tests
cabal test

Expected output:
```
MatrixProduct
  maxProductRight
    finds max product in right direction [✔]
  maxProductDown
    finds max product in down direction [✔]
  maxProductDiagDownRight
    finds max product in diagonal down-right direction [✔]
  maxProductDiagDownLeft
    finds max product in diagonal down-left direction [✔]
  maxProductInMatrix
    finds overall max product [✔]
  findMaxProductSequence
    returns product and coordinates [✔]
    handles small matrix [✔]
  colorMatrixSequence
    colors the sequence in red [✔]
    does not color non-sequence values [✔]

Finished in 0.0005 seconds
9 examples, 0 failures
```

### Run Solution

```sh
cabal run MatrixProduct matrix.txt
```

Expected output: Matrix with winning sequence highlighted in red, followed by:
```
Greatest product of four adjacent numbers: 70600674
```

### Build Executable

```sh
cabal build
```

## Configuration Notes

This project is configured to use the system GHC (not stack). The cabal file uses:
- Base library version: `>=4.18 && <5` (compatible with GHC 9.12.2)
- Relaxed version constraints to work with system GHC

If you prefer to use stack instead of cabal, you'll need to create a `stack.yaml` file.

## Functions Provided

The `MatrixProduct` module exports:

- `parseMatrix :: String -> [[Int]]` - Parse matrix from text
- `maxProductRight :: [[Int]] -> Int -> Int` - Max product in horizontal direction
- `maxProductDown :: [[Int]] -> Int -> Int` - Max product in vertical direction
- `maxProductDiagDownRight :: [[Int]] -> Int -> Int` - Max product diagonal down-right
- `maxProductDiagDownLeft :: [[Int]] -> Int -> Int` - Max product diagonal down-left
- `maxProductInMatrix :: [[Int]] -> Int -> Int` - Overall maximum product
- `findMaxProductSequence :: [[Int]] -> Int -> (Int, [(Int, Int)])` - Find best sequence with coordinates
- `colorMatrixSequence :: [[Int]] -> [(Int, Int)] -> String` - Generate ANSI colored output

