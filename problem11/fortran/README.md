# Matrix Product (Project Euler 11) — Fortran Implementation

This Fortran solution finds the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in a grid.

## Files

- `matrix_product.f90` — Main logic and CLI for finding the greatest product.
- `matrix.txt` — Input matrix (should be copied from the root directory).

## Usage

1. Ensure you have a Fortran compiler installed (e.g., `gfortran`).
2. Copy or symlink the `matrix.txt` file into this directory.
3. To compile and run the solution:

    ```sh
    gfortran -o matrix_product matrix_product.f90
    ./matrix_product
    ```

## Output

- Prints the greatest product at the end.

## TDD

- Fortran implementation is simple and does not include automated tests, but the logic is easy to validate by comparing with other implementations.
