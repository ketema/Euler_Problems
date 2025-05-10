# Matrix Product (Project Euler 11) — Go Implementation

This Go solution finds the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in a grid.

## Files

- `main.go` — Command-line program to read a matrix from a file, print the matrix with the winning sequence highlighted, and print the greatest product.
- `matrix_product.go` — Core logic for finding the greatest product and its coordinates.
- `matrix_product_test.go` — Unit tests for the core logic (TDD-first).
- `matrix.txt` — Input matrix (should be copied from the root or typescript version).

## Usage

1. Ensure you have Go installed.

2. Copy or symlink the `matrix.txt` file into this directory.

3. Run the program:

```sh
# Run the solution
cd go
# If matrix.txt is in the parent directory, copy it in:
cp ../matrix.txt .
# Run with both files (required if you see 'undefined: GreatestProduct'):
go run main.go matrix_product.go matrix.txt
# Alternatively, if using Go modules (go.mod present), you can simply run:
go run .
```

4. Run the tests:

```sh
go test -v
```

## Troubleshooting

- If you see an error like `undefined: GreatestProduct`, make sure to include both `main.go` and `matrix_product.go` in your go run command, as shown above.

## Output

- Prints the matrix, highlighting the winning sequence in red (if your terminal supports ANSI colors).
- Prints the greatest product at the end.

## TDD

- The Go implementation is fully test-driven, with tests in `matrix_product_test.go`.
