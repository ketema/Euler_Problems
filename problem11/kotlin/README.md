# Matrix Product (Project Euler 11) — Kotlin Implementation

This Kotlin solution finds the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in a grid.

## Files
- `MatrixProduct.kt` — Core logic for finding the greatest product and its coordinates.
- `MatrixProductTest.kt` — Unit tests for the core logic (TDD-first, using kotlin.test).
- `MatrixProductCLI.kt` — Command-line program to read a matrix from a file, print the matrix with the winning sequence highlighted, and print the greatest product.
- `matrix.txt` — Input matrix (should be copied from the root or typescript version).

## Usage

1. **Ensure you have Kotlin and Gradle installed.**
2. **Copy or symlink the `matrix.txt` file into this directory.**
3. **Build and run the CLI with Gradle:**

```sh
# Run the CLI (prints the matrix and greatest product)
gradle run --args="matrix.txt"
```

4. **Run the tests with Gradle:**

```sh
gradle test
```


```sh
kotlinc MatrixProduct.kt MatrixProductTest.kt -include-runtime -d test.jar
java -jar test.jar
```

## Output
- Prints the matrix, highlighting the winning sequence in red (if your terminal supports ANSI colors).
- Prints the greatest product at the end.

---

**TDD:**
- The Kotlin implementation is fully test-driven, with tests in `MatrixProductTest.kt`.
