# C# Solution for Matrix Product Problem

This folder contains the C# implementation for finding the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in a matrix.

## Project Structure

- `MatrixProduct.cs`: Core implementation (standalone)
- `MatrixProductTest.cs`: Test implementation (standalone)
- `MatrixProductTestProject/`: .NET project for running tests
- `MatrixProductApp/`: .NET project for running the solution
- `matrix.txt`: Input matrix (20x20 grid)

## Requirements

- .NET 9.0 SDK or later
- macOS, Linux, or Windows

## Build & Run

### Using Make (recommended)

```sh
make test   # Run tests
make run    # Run the solution with matrix.txt
make clean  # Clean build artifacts
```

### Using dotnet directly

```sh
# Run tests
cd MatrixProductTestProject && dotnet run

# Run solution
cd MatrixProductApp && dotnet run matrix.txt
```

## Expected Output

Tests should show:
```
TestGreatestProduct passed!
All tests passed!
```

Solution should show the matrix with the winning sequence highlighted and:
```
Greatest product of four adjacent numbers: 70600674
```
