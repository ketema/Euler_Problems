# Matrix Product (Project Euler 11) — Rust Implementation

This Rust solution finds the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in a grid.

## Files
- `src/main.rs` — Command-line program to read a matrix from a file, print the matrix with the winning sequence highlighted, and print the greatest product.
- `src/lib.rs` — Core logic for finding the greatest product and its coordinates.
- `tests/greatest_product.rs` — Unit tests for the core logic (TDD-first).
- `matrix.txt` — Input matrix (should be copied from the root or typescript version).

## Usage

1. **Ensure you have Rust installed.**
2. **Copy or symlink the `matrix.txt` file into this directory.**
3. **Run the program:**

```sh
# Run the solution
cd rust
cargo run -- matrix.txt
```

4. **Run the tests:**

```sh
cargo test
```

## Output
- Prints the matrix, highlighting the winning sequence in red (if your terminal supports ANSI colors).
- Prints the greatest product at the end.

---

**TDD:**
- The Rust implementation is fully test-driven, with tests in `tests/greatest_product.rs`.
