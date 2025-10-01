# Python Solution for Matrix Product Problem

This folder contains the Python implementation for finding the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in a matrix.

## Project Structure

- `matrix_product.py`: Core implementation with all matrix product functions
- `test_matrix_product.py`: Pytest test suite (8 tests)
- `pyproject.toml`: Poetry configuration for dependency management
- `matrix.txt`: Input matrix (20x20 grid)

## Requirements

- Python 3.8+
- Poetry 2.1+ (for dependency management)
- pytest (managed by Poetry)

## Installation

### Install Poetry (if not already installed)

```sh
# macOS/Linux
curl -sSL https://install.python-poetry.org | python3 -

# Or via Homebrew (macOS)
brew install poetry
```

### Install Dependencies

```sh
# Install project dependencies (including pytest)
poetry install
```

This will:
1. Create a virtual environment
2. Install pytest and other dependencies
3. Set up the project for development

## Running Tests

```sh
# Run all tests
poetry run pytest test_matrix_product.py

# Run with verbose output
poetry run pytest test_matrix_product.py -v

# Run with coverage report
poetry run pytest test_matrix_product.py --cov=matrix_product
```

Expected output:
```
============================= test session starts ==============================
platform darwin -- Python 3.x.x, pytest-x.x.x, pluggy-x.x.x
collected 8 items

test_matrix_product.py ........                                          [100%]

============================== 8 passed in 0.XXs ===============================
```

## Running the Solution

```sh
# Run with default matrix.txt
poetry run python matrix_product.py

# Or activate the virtual environment first
poetry shell
python matrix_product.py
```

Expected output:
```
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
...
Greatest product of four adjacent numbers: 70600674
```

The winning sequence will be highlighted in red using ANSI escape codes.

## Functions Provided

The `matrix_product.py` module provides these functions:

### Core Functions

- `read_matrix(filename: str) -> List[List[int]]`
  - Reads matrix from file
  - Returns 2D list of integers

- `max_product_right(matrix: List[List[int]], n: int) -> int`
  - Finds maximum product in horizontal (right) direction
  - Returns the maximum product found

- `max_product_down(matrix: List[List[int]], n: int) -> int`
  - Finds maximum product in vertical (down) direction
  - Returns the maximum product found

- `max_product_diag_down_right(matrix: List[List[int]], n: int) -> int`
  - Finds maximum product in diagonal down-right direction
  - Returns the maximum product found

- `max_product_diag_down_left(matrix: List[List[int]], n: int) -> int`
  - Finds maximum product in diagonal down-left direction
  - Returns the maximum product found

- `greatest_product_in_matrix(matrix: List[List[int]], n: int) -> int`
  - Finds the overall maximum product across all directions
  - Returns the maximum product found

### Advanced Functions

- `find_max_product_sequence(matrix: List[List[int]], n: int) -> Tuple[int, List[Tuple[int, int]]]`
  - Finds the maximum product and its coordinates
  - Returns `(product, coordinates)` where coordinates is list of `(row, col)` tuples

- `color_matrix_sequence(matrix: List[List[int]], coords: List[Tuple[int, int]]) -> str`
  - Generates ANSI colored output with winning sequence highlighted in red
  - Returns formatted string with ANSI escape codes

## Poetry Commands

```sh
# Install dependencies
poetry install

# Add a new dependency
poetry add package-name

# Add a dev dependency
poetry add --group dev package-name

# Update dependencies
poetry update

# Show installed packages
poetry show

# Activate virtual environment
poetry shell

# Run command in virtual environment
poetry run python script.py

# Exit virtual environment
exit
```

## Why Poetry?

Poetry provides:
- **Dependency management**: Automatic resolution of package versions
- **Virtual environments**: Isolated Python environments per project
- **Lock file**: `poetry.lock` ensures reproducible builds
- **Modern workflow**: Better than pip + requirements.txt

## Testing

The test suite covers:
- ✅ Matrix reading from file
- ✅ All four directions (right, down, diagonal-right, diagonal-left)
- ✅ Overall maximum product
- ✅ Sequence finding with coordinates
- ✅ ANSI color output
- ✅ Edge cases (empty matrix, small matrix)

## Code Quality

- ✅ Type hints (PEP 484)
- ✅ Docstrings (Google style)
- ✅ List comprehensions (Pythonic style)
- ✅ Functional approach (pure functions)
- ✅ Edge case handling
- ✅ >85% test coverage

## Development

```sh
# Format code (if you have black installed)
poetry run black matrix_product.py test_matrix_product.py

# Lint code (if you have pylint installed)
poetry run pylint matrix_product.py

# Type check (if you have mypy installed)
poetry run mypy matrix_product.py
```

## Troubleshooting

### Poetry not found

```sh
# Add Poetry to PATH (add to ~/.bash_profile or ~/.zshrc)
export PATH="$HOME/.local/bin:$PATH"
```

### Virtual environment issues

```sh
# Remove and recreate virtual environment
poetry env remove python
poetry install
```

### pytest not found

```sh
# Make sure you're using poetry run
poetry run pytest test_matrix_product.py

# Or activate the virtual environment first
poetry shell
pytest test_matrix_product.py
```

