# TypeScript Solution for Matrix Product Problem

This folder contains the TypeScript/Node.js implementation for finding the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in a matrix.

- `matrixProduct.ts`: Main solution code.
- `matrixProduct.test.ts`: Unit tests for the core logic.
- `matrix.txt`: Input matrix (copied from the root/problem11 folder).

## Build & Run

To build and run:

```sh
npm install           # Install dependencies
npx tsc               # Compile TypeScript to JavaScript
node dist/matrixProduct.js matrix.txt  # Run the solution
```

To run the test:

```sh
npx ts-node matrixProduct.test.ts
```
