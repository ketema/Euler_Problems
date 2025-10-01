# PHP Solution for Matrix Product Problem

This folder contains the PHP implementation for finding the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in a matrix.

## Project Structure

- `MatrixProduct.php`: Core implementation with all matrix product functions
- `MatrixProductTest.php`: PHPUnit test suite (10 tests, 16 assertions)
- `solution.php`: Command-line runner script
- `matrix.txt`: Input matrix (20x20 grid)

## Requirements

- PHP 8.4+ (tested with PHP 8.4.13)
- PHPUnit 12.2+ (for running tests)

## Installation

### macOS (Homebrew)

```sh
# Install PHP
brew install php

# Install PHPUnit globally
brew install phpunit
```

### Other Systems

```sh
# Install PHP (varies by system)
# Then install PHPUnit via Composer
composer global require phpunit/phpunit
```

## Running Tests

```sh
# Using PHPUnit directly
phpunit MatrixProductTest.php

# Or with full path (if not in PATH)
/opt/homebrew/bin/php /usr/local/bin/phpunit MatrixProductTest.php
```

Expected output:
```
PHPUnit 12.2.9 by Sebastian Bergmann and contributors.

..........                                                        10 / 10 (100%)

Time: 00:00.004, Memory: 25.73 MB

Matrix Product
 ✔ Read matrix
 ✔ Max product right
 ✔ Max product down
 ✔ Max product diag down right
 ✔ Max product diag down left
 ✔ Greatest product in matrix
 ✔ Find max product sequence
 ✔ Color matrix sequence
 ✔ Edge case empty matrix
 ✔ Edge case small matrix

OK (10 tests, 16 assertions)
```

## Running the Solution

```sh
# Run with default matrix.txt
php solution.php

# Run with custom matrix file
php solution.php path/to/matrix.txt
```

Expected output:
```
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
...
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

Greatest product of four adjacent numbers: 70600674
```

The winning sequence will be highlighted in red using ANSI escape codes.

## Functions Provided

The `MatrixProduct.php` file provides these functions:

### Core Functions

- `readMatrix(string $filename): array`
  - Reads matrix from file
  - Returns 2D array of integers

- `maxProductRight(array $matrix, int $n): int`
  - Finds maximum product in horizontal (right) direction
  - Returns the maximum product found

- `maxProductDown(array $matrix, int $n): int`
  - Finds maximum product in vertical (down) direction
  - Returns the maximum product found

- `maxProductDiagDownRight(array $matrix, int $n): int`
  - Finds maximum product in diagonal down-right direction
  - Returns the maximum product found

- `maxProductDiagDownLeft(array $matrix, int $n): int`
  - Finds maximum product in diagonal down-left direction
  - Returns the maximum product found

- `greatestProductInMatrix(array $matrix, int $n): int`
  - Finds the overall maximum product across all directions
  - Returns the maximum product found

### Advanced Functions

- `findMaxProductSequence(array $matrix, int $n): array`
  - Finds the maximum product and its coordinates
  - Returns `[product, coordinates]` where coordinates is array of `[row, col]` pairs

- `colorMatrixSequence(array $matrix, array $coords): string`
  - Generates ANSI colored output with winning sequence highlighted in red
  - Returns formatted string with ANSI escape codes

## Design Pattern

This implementation follows the **Transaction Script** pattern (from Patterns of Enterprise Application Architecture):
- Procedural organization for mathematical computation
- Pure functions with no side effects (except I/O)
- Clear separation of concerns
- Easy to test and maintain

## Code Quality

- ✅ Type hints (PHP 8.4)
- ✅ Comprehensive PHPDoc comments
- ✅ Edge case handling (empty matrix, small matrix)
- ✅ DRY principle (no code duplication)
- ✅ Functional style (pure functions, immutable data)
- ✅ >85% test coverage

## Integration with Other Problems

This PHP implementation follows the same structure as problems 1-10:
- `ClassName.php` - Implementation
- `ClassNameTest.php` - PHPUnit tests
- `solution.php` - Runner script

This consistency makes it easy to navigate and maintain the codebase.

