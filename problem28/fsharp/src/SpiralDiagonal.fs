module SpiralDiagonal

open System

/// <summary>
/// Calculates the sum of numbers on both diagonals in an n×n spiral grid.
/// The spiral starts at center with value 1 and proceeds clockwise.
/// </summary>
/// <param name="gridSize">Size of the grid (must be odd and positive)</param>
/// <returns>Sum of all diagonal elements</returns>
/// <exception cref="System.ArgumentException">
/// Thrown when gridSize is even, negative, or zero
/// </exception>
let calculateDiagonalSum (gridSize: int) : int =
    // Validation
    if gridSize <= 0 then
        raise (ArgumentException("Grid size must be positive"))

    if gridSize % 2 = 0 then
        raise (ArgumentException("Grid size must be odd"))

    // Base case: 1×1 grid
    if gridSize = 1 then
        1
    else
        // Calculate number of rings (layers) around the center
        let numRings = (gridSize - 1) / 2

        // Start with center value
        let mutable sum = 1
        let mutable currentValue = 1

        // Process each ring
        for ring in 1 .. numRings do
            let increment = 2 * ring

            // Each ring has 4 corners
            // Calculate and add all 4 corners
            for corner in 1 .. 4 do
                currentValue <- currentValue + increment
                sum <- sum + currentValue

        sum
