# Matrix Product (Project Euler 11) — Swift Implementation

This Swift solution finds the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in a grid.

## Files
- `MatrixProduct.swift` — Core logic for finding the greatest product and its coordinates.
- `MatrixProductTests.swift` — Unit tests for the core logic (TDD-first, using XCTest).
- `main.swift` — Command-line program to read a matrix from a file, print the matrix with the winning sequence highlighted, and print the greatest product.
- `matrix.txt` — Input matrix (should be copied from the root or typescript version).

## Usage

1. **Ensure you have Swift installed (Swift 5+).**
2. **Copy or symlink the `matrix.txt` file into this directory.**
3. **Compile and run the program:**

```sh
swiftc MatrixProduct.swift main.swift -o matrix_product
./matrix_product matrix.txt
```

4. **Run the tests:**

If using `swift test` (with a SwiftPM package), move the files into the appropriate structure. For standalone testing:

```sh
swiftc -o test MatrixProduct.swift MatrixProductTests.swift -emit-executable && ./test
```

## Output
- Prints the matrix, highlighting the winning sequence in red (if your terminal supports ANSI colors).
- Prints the greatest product at the end.

---

**TDD:**
- The Swift implementation is fully test-driven, with tests in `MatrixProductTests.swift`.
