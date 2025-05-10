# Matrix Product (Project Euler 11) — Ruby Implementation

This Ruby solution finds the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in a grid.

## Files
- `matrix_product.rb` — Core logic for finding the greatest product and its coordinates.
- `matrix_product_test.rb` — Unit tests for the core logic (TDD-first, using Minitest).
- `matrix_product_cli.rb` — Command-line program to read a matrix from a file, print the matrix with the winning sequence highlighted, and print the greatest product.
- `matrix.txt` — Input matrix (should be copied from the root or typescript version).

## Usage

1. **Ensure you have Ruby installed.**
2. **Copy or symlink the `matrix.txt` file into this directory.**
3. **Run the program:**

```sh
ruby matrix_product_cli.rb matrix.txt
```

4. **Run the tests:**

```sh
ruby matrix_product_test.rb
```

## Output
- Prints the matrix, highlighting the winning sequence in red (if your terminal supports ANSI colors).
- Prints the greatest product at the end.

---

**TDD:**
- The Ruby implementation is fully test-driven, with tests in `matrix_product_test.rb`.
