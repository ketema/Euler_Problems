# Matrix Product (Project Euler 11) — Perl Implementation

This Perl solution finds the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in a grid.

## Files
- `matrix_product.pl` — Core logic for finding the greatest product and its coordinates.
- `matrix_product.t` — Unit tests for the core logic (TDD-first, using Test::More).
- `matrix_product_cli.pl` — Command-line program to read a matrix from a file, print the matrix with the winning sequence highlighted, and print the greatest product.
- `matrix.txt` — Input matrix (should be copied from the root or typescript version).

## Usage

1. **Ensure you have Perl installed (and Term::ANSIColor for colored output).**
2. **Copy or symlink the `matrix.txt` file into this directory.**
3. **Run the program:**

```sh
perl matrix_product_cli.pl matrix.txt
```

4. **Run the tests:**

```sh
prove matrix_product.t
```

## Output
- Prints the matrix, highlighting the winning sequence in red (if your terminal supports ANSI colors).
- Prints the greatest product at the end.

---

**TDD:**
- The Perl implementation is fully test-driven, with tests in `matrix_product.t`.
