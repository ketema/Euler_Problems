# Matrix Product (Project Euler 11) — Scala Implementation

This Scala solution finds the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in a grid.

## Files

- `MatrixProduct.scala` — Main logic and CLI for finding the greatest product and its coordinates.
- `MatrixProductTest.scala` — Unit tests for the core logic (TDD-first).
- `matrix.txt` — Input matrix (should be copied from the root directory).

## Usage

1. Ensure you have Scala and sbt installed.
2. Make sure `matrix.txt` is located at `src/main/scala/matrix.txt` (the default for this project).
3. To run the solution using sbt:

    ```sh
    sbt "run src/main/scala/matrix.txt"
    ```

   - This will compile and execute the program, printing the matrix and the greatest product.

4. To run the tests:

    ```sh
    sbt test
    ```

## Output

- Prints the matrix, highlighting the winning sequence in red (if your terminal supports ANSI colors).
- Prints the greatest product at the end.

## TDD

- The Scala implementation is fully test-driven, with tests in `MatrixProductTest.scala`.
