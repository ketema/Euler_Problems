module SpiralDiagonal

/// Calculate the sum of numbers on both diagonals in an n×n spiral grid
/// The spiral starts from 1 at the center and moves clockwise outward
let calculateDiagonalSum (n: int) : int =
    // Validation: grid size must be positive
    if n <= 0 then
        raise (System.ArgumentException("Grid size must be positive"))

    // Validation: grid size must be odd
    if n % 2 = 0 then
        raise (System.ArgumentException("Grid size must be odd"))

    // Base case: 1x1 grid has only the center value 1
    if n = 1 then
        1
    else
        // Mathematical formula:
        // For a spiral starting at center 1, moving right then clockwise
        // The diagonals can be computed using a closed-form formula
        // Sum = (4n³ + 3n² + 8n - 9) / 6  where n is grid size

        // Using integer arithmetic to avoid potential floating point issues
        let n64 = int64 n
        let numerator = 4L * n64 * n64 * n64 + 3L * n64 * n64 + 8L * n64 - 9L
        int (numerator / 6L)
