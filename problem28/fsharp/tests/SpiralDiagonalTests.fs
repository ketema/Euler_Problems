module SpiralDiagonalTests

open Expecto
open SpiralDiagonal

[<Tests>]
let tests =
    testList "Project Euler #28: Number Spiral Diagonals" [

        // REQ-PE28-001: Basic Functionality - Small Grid Cases
        testList "Basic Functionality - Exact Value Validation" [

            test "1x1 grid diagonal sum must equal 1" {
                let result = calculateDiagonalSum 1
                Expect.equal result 1
                    """REQ-PE28-001: 1x1 grid diagonal sum specification

Expected: calculateDiagonalSum 1 must return exactly 1
Actual: Got different value

Guidance: A 1x1 grid contains only the center value (1). Since this value
appears on both diagonals, the sum of both diagonals is 1. The function
must return exactly this value."""
            }

            test "3x3 grid diagonal sum must equal 25" {
                let result = calculateDiagonalSum 3
                Expect.equal result 25
                    """REQ-PE28-003: 3x3 grid diagonal sum specification

Expected: calculateDiagonalSum 3 must return exactly 25
Actual: Got different value

Guidance: For a 3x3 number spiral grid, the sum of both diagonal numbers
is deterministically 25. This is a mathematically exact answer, not an
approximation. The function must return this exact value."""
            }

            test "5x5 grid diagonal sum must equal 101" {
                let result = calculateDiagonalSum 5
                Expect.equal result 101
                    """REQ-PE28-005: 5x5 grid diagonal sum specification

Expected: calculateDiagonalSum 5 must return exactly 101
Actual: Got different value

Guidance: For a 5x5 number spiral grid, the sum of both diagonal numbers
is deterministically 101. This matches the example given in Project Euler
problem #28. The function must return this exact value."""
            }

            test "7x7 grid diagonal sum must equal 261" {
                let result = calculateDiagonalSum 7
                Expect.equal result 261
                    """REQ-PE28-007: 7x7 grid diagonal sum specification

Expected: calculateDiagonalSum 7 must return exactly 261
Actual: Got different value

Guidance: For a 7x7 number spiral grid, the sum of both diagonal numbers
is deterministically 261. The function must return this exact value."""
            }

            test "9x9 grid diagonal sum must equal 537" {
                let result = calculateDiagonalSum 9
                Expect.equal result 537
                    """REQ-PE28-009: 9x9 grid diagonal sum specification

Expected: calculateDiagonalSum 9 must return exactly 537
Actual: Got different value

Guidance: For a 9x9 number spiral grid, the sum of both diagonal numbers
is deterministically 537. The function must return this exact value."""
            }
        ]

        // REQ-PE28-ERR: Error Handling - Invalid Input Cases
        testList "Error Handling - Input Validation" [

            test "Even grid size must raise ArgumentException with specific message" {
                let testFunc = fun () -> calculateDiagonalSum 4 |> ignore
                Expect.throwsT<System.ArgumentException> testFunc
                    """REQ-PE28-ERR-001: Even grid size rejection specification

Expected: calculateDiagonalSum 4 must raise ArgumentException with message
"Grid size must be odd"
Actual: Did not raise ArgumentException or raised with different message

Guidance: Number spirals require odd grid sizes (1x1, 3x3, 5x5, etc.)
because the spiral starts from a center point. Even grid sizes (2x2, 4x4,
etc.) have no unique center and are invalid. The function must reject
even numbers by raising ArgumentException with the message "Grid size must be odd"."""

                // Verify exact error message
                try
                    calculateDiagonalSum 4 |> ignore
                    failtest "Expected ArgumentException was not raised"
                with
                | :? System.ArgumentException as ex ->
                    Expect.equal ex.Message "Grid size must be odd"
                        "ArgumentException message must be 'Grid size must be odd'"
            }

            test "Negative grid size must raise ArgumentException with specific message" {
                let testFunc = fun () -> calculateDiagonalSum -5 |> ignore
                Expect.throwsT<System.ArgumentException> testFunc
                    """REQ-PE28-ERR-002: Negative grid size rejection specification

Expected: calculateDiagonalSum -5 must raise ArgumentException with message
"Grid size must be positive"
Actual: Did not raise ArgumentException or raised with different message

Guidance: Grid sizes must be positive numbers. Negative values are physically
meaningless for grid dimensions. The function must reject negative numbers
by raising ArgumentException with the message "Grid size must be positive"."""

                // Verify exact error message
                try
                    calculateDiagonalSum -5 |> ignore
                    failtest "Expected ArgumentException was not raised"
                with
                | :? System.ArgumentException as ex ->
                    Expect.equal ex.Message "Grid size must be positive"
                        "ArgumentException message must be 'Grid size must be positive'"
            }

            test "Zero grid size must raise ArgumentException with specific message" {
                let testFunc = fun () -> calculateDiagonalSum 0 |> ignore
                Expect.throwsT<System.ArgumentException> testFunc
                    """REQ-PE28-ERR-003: Zero grid size rejection specification

Expected: calculateDiagonalSum 0 must raise ArgumentException with message
"Grid size must be positive"
Actual: Did not raise ArgumentException or raised with different message

Guidance: A 0x0 grid is meaningless. Grid sizes must be positive odd numbers
starting from 1. The function must reject zero by raising ArgumentException
with the message "Grid size must be positive"."""

                // Verify exact error message
                try
                    calculateDiagonalSum 0 |> ignore
                    failtest "Expected ArgumentException was not raised"
                with
                | :? System.ArgumentException as ex ->
                    Expect.equal ex.Message "Grid size must be positive"
                        "ArgumentException message must be 'Grid size must be positive'"
            }
        ]

        // REQ-PE28-PROD: Production Case - Project Euler Answer
        testList "Production Case - Project Euler #28 Answer" [

            test "1001x1001 grid must return exact Project Euler #28 answer" {
                let result = calculateDiagonalSum 1001
                Expect.equal result 669171001
                    """REQ-PE28-PROD: 1001x1001 grid exact answer specification

Expected: calculateDiagonalSum 1001 must return exactly 669171001
Actual: Got different value

Guidance: This is the exact, deterministic answer to Project Euler problem
#28. For a 1001x1001 number spiral grid, the sum of both diagonals is
669171001. This is NOT a range check or approximation - the function must
return this EXACT value. Any other value indicates incorrect computation
of the diagonal sum."""
            }
        ]

        // REQ-PE28-PERF: Performance - Stress Test
        testList "Performance - Large Grid Handling" [

            test "9999x9999 grid completes in under 500ms" {
                let stopwatch = System.Diagnostics.Stopwatch()
                stopwatch.Start()
                let result = calculateDiagonalSum 9999
                stopwatch.Stop()

                Expect.isLessThan stopwatch.ElapsedMilliseconds 500L
                    """REQ-PE28-PERF-001: Large grid performance specification

Expected: calculateDiagonalSum 9999 must complete in under 500 milliseconds
Actual: Took longer than 500ms

Guidance: The function must compute diagonal sums for large grids efficiently.
For a 9999x9999 grid, the computation must complete in under 500 milliseconds.
This requirement ensures the solution scales to large inputs."""

                // Also verify result is non-zero (basic sanity check)
                Expect.isGreaterThan result 0
                    """REQ-PE28-PERF-002: Large grid result sanity check

Expected: calculateDiagonalSum 9999 must return a positive integer
Actual: Got zero or negative value

Guidance: For any valid grid size, the diagonal sum must be positive because
all numbers in the spiral are positive integers starting from 1."""
            }

            test "9999x9999 grid must not overflow" {
                let result = calculateDiagonalSum 9999

                // Verify result is a valid positive integer (no overflow)
                Expect.isGreaterThan result 0
                    "Result must be positive (no overflow to negative)"

                // The result should be very large but not overflow int range
                Expect.isLessThan result System.Int32.MaxValue
                    """REQ-PE28-PERF-003: Integer overflow prevention specification

Expected: calculateDiagonalSum 9999 must return a value within valid int range
Actual: Result appears to have overflowed

Guidance: For a 9999x9999 grid, the diagonal sum is a very large number but
must fit within the standard integer type used by the function. The function
must handle large computations without integer overflow."""
            }
        ]

        // REQ-PE28-BOUNDARY: Boundary Cases
        testList "Boundary Cases - Edge Values" [

            test "11x11 grid diagonal sum must equal 961" {
                let result = calculateDiagonalSum 11
                Expect.equal result 961
                    """REQ-PE28-BOUNDARY-001: 11x11 grid diagonal sum specification

Expected: calculateDiagonalSum 11 must return exactly 961
Actual: Got different value

Guidance: For an 11x11 number spiral grid, the sum of both diagonal numbers
is deterministically 961. This validates that the function correctly handles
grids beyond the basic small examples."""
            }

            test "Large odd grid (999x999) completes successfully" {
                let result = calculateDiagonalSum 999

                Expect.isGreaterThan result 0
                    """REQ-PE28-BOUNDARY-002: Large odd grid computation specification

Expected: calculateDiagonalSum 999 must return a positive integer
Actual: Got zero or negative value

Guidance: For a 999x999 grid (just below the production case), the function
must compute a valid positive diagonal sum. This validates scaling behavior
near the production input size."""
            }
        ]
    ]

[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] args tests
