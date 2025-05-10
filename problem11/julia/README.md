# Matrix Product (Project Euler 11) — Julia Implementation

This Julia solution finds the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in a grid.

## Files

- `matrix_product.jl` — Core logic for finding the greatest product and its coordinates (as a module).
- `matrix_product_test.jl` — Unit tests for the core logic (TDD-first, using Julia's `Test` standard library).
- `matrix_product_cli.jl` — Command-line program to read a matrix from a file, print the matrix with the winning sequence highlighted, and print the greatest product.
- `matrix.txt` — Input matrix (should be copied from the root or typescript version).

## Usage

1. **Ensure you have Julia installed (>= 1.8, Apple Silicon supported).**

2. **Copy or symlink the `matrix.txt` file into this directory.**

3. **Run the tests:**

   ```sh
   julia --project=. matrix_product_test.jl
   ```

4. **Run the CLI:**

   ```sh
   julia --project=. matrix_product_cli.jl matrix.txt
   ```

## Output
- Prints the matrix, highlighting the winning sequence in red (if your terminal supports ANSI colors).
- Prints the greatest product at the end.

---

**TDD:**
- The Julia implementation is fully test-driven, with tests in `matrix_product_test.jl`.
