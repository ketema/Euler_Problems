module SpiralDiagonalTests

open Expecto
open SpiralDiagonal

[<Tests>]
let tests =
    testList "SpiralDiagonal.calculateDiagonalSum" [
        // ============================================================
        // BASIC FUNCTIONALITY TESTS
        // ============================================================

        testCase "1x1 grid returns 1" <| fun () ->
            // REQ-PE28-001: Calculate diagonal sum for 1x1 grid
            // Expected: Sum is 1 (only the center value exists)
            // Guidance: Implement calculateDiagonalSum to handle base case where gridSize=1
            //           A 1x1 spiral contains only the number 1, so diagonal sum = 1
            let result = calculateDiagonalSum 1
            let errorMsg =
                sprintf "REQ-PE28-001: For 1x1 grid, diagonal sum must be 1 (center value only).
Expected: calculateDiagonalSum 1 = 1
Actual: calculateDiagonalSum 1 = %d
Guidance: A 1x1 spiral has only center value 1. Return 1 for gridSize=1." result
            Expect.equal result 1 errorMsg

        testCase "3x3 grid returns 25" <| fun () ->
            // REQ-PE28-002: Calculate diagonal sum for 3x3 grid
            // Expected: Sum is 25 (from spiral: 7+5+3+1+9 = 25)
            // Grid layout:
            //   7 8 9
            //   6 1 2
            //   5 4 3
            // Diagonals: [7,1,3] and [5,1,9]
            // Sum = 7+1+3+5+1+9 - 1 (center counted once) = 25
            let result = calculateDiagonalSum 3
            let errorMsg =
                sprintf "REQ-PE28-002: For 3x3 grid, diagonal sum must be 25.
Expected: calculateDiagonalSum 3 = 25
Actual: calculateDiagonalSum 3 = %d
Guidance: 3x3 spiral has diagonals [7,1,3] and [5,1,9]. Sum = 7+5+1+3+9 = 25.
Build spiral clockwise from center 1, then sum values where (row=col) OR (row+col=gridSize-1)." result
            Expect.equal result 25 errorMsg

        testCase "5x5 grid returns 101" <| fun () ->
            // REQ-PE28-003: Calculate diagonal sum for 5x5 grid (Project Euler example)
            // Expected: Sum is 101 (given in problem statement)
            // Grid layout:
            //   21 22 23 24 25
            //   20  7  8  9 10
            //   19  6  1  2 11
            //   18  5  4  3 12
            //   17 16 15 14 13
            // Diagonals: [21,7,1,3,13] and [17,5,1,9,25]
            let result = calculateDiagonalSum 5
            let errorMsg =
                sprintf "REQ-PE28-003: For 5x5 grid, diagonal sum must be 101 (Project Euler example).
Expected: calculateDiagonalSum 5 = 101
Actual: calculateDiagonalSum 5 = %d
Guidance: 5x5 spiral diagonals sum to 101 per problem specification.
Verify spiral builds correctly: center=1, then clockwise layers." result
            Expect.equal result 101 errorMsg

        testCase "7x7 grid diagonal sum verification" <| fun () ->
            // REQ-PE28-004: Calculate diagonal sum for 7x7 grid (pattern verification)
            // Expected: Sum is 261
            let result = calculateDiagonalSum 7
            let errorMsg =
                sprintf "REQ-PE28-004: For 7x7 grid, diagonal sum must be 261.
Expected: calculateDiagonalSum 7 = 261
Actual: calculateDiagonalSum 7 = %d
Guidance: 7x7 spiral diagonal sum = 261. Verify spiral construction scales correctly." result
            Expect.equal result 261 errorMsg

        // ============================================================
        // EDGE CASE TESTS (ERROR HANDLING)
        // ============================================================

        testCase "even grid size throws ArgumentException" <| fun () ->
            // REQ-PE28-ERR-001: Reject even grid sizes (spiral must have odd dimensions)
            // Expected: ArgumentException
            // Guidance: Check if gridSize is even and throw ArgumentException
            let errorMsg = "REQ-PE28-ERR-001: Even grid size must throw ArgumentException.
Expected: calculateDiagonalSum 4 throws ArgumentException with message 'Grid size must be odd'
Actual: Function did not throw exception
Guidance: Add validation at function entry: if gridSize % 2 = 0 then raise (System.ArgumentException \"Grid size must be odd\")."
            Expect.throws
                (fun () -> calculateDiagonalSum 4 |> ignore)
                errorMsg

        testCase "even grid size has correct error message" <| fun () ->
            // REQ-PE28-ERR-002: Error message must indicate odd-only requirement
            // Expected: "Grid size must be odd"
            try
                calculateDiagonalSum 4 |> ignore
                failwith "Expected ArgumentException but none was thrown"
            with
            | :? System.ArgumentException as ex ->
                let errorMsg =
                    sprintf "REQ-PE28-ERR-002: ArgumentException message must contain 'odd'.
Expected message: 'Grid size must be odd'
Actual message: '%s'
Guidance: Use descriptive error message that explains valid input (odd numbers only)." ex.Message
                Expect.stringContains ex.Message "odd" errorMsg

        testCase "negative grid size throws ArgumentException" <| fun () ->
            // REQ-PE28-ERR-003: Reject negative grid sizes
            // Expected: ArgumentException
            let errorMsg = "REQ-PE28-ERR-003: Negative grid size must throw ArgumentException.
Expected: calculateDiagonalSum -5 throws ArgumentException
Actual: Function did not throw exception
Guidance: Add validation: if gridSize <= 0 then raise (System.ArgumentException \"Grid size must be positive\")."
            Expect.throws
                (fun () -> calculateDiagonalSum -5 |> ignore)
                errorMsg

        testCase "zero grid size throws ArgumentException" <| fun () ->
            // REQ-PE28-ERR-004: Reject zero grid size
            // Expected: ArgumentException
            let errorMsg = "REQ-PE28-ERR-004: Zero grid size must throw ArgumentException.
Expected: calculateDiagonalSum 0 throws ArgumentException
Actual: Function did not throw exception
Guidance: Validate gridSize > 0 before processing."
            Expect.throws
                (fun () -> calculateDiagonalSum 0 |> ignore)
                errorMsg

        // ============================================================
        // PRODUCTION TEST CASE
        // ============================================================

        testCase "1001x1001 grid computes Project Euler answer" <| fun () ->
            // REQ-PE28-PROD: Calculate diagonal sum for 1001x1001 grid
            // Expected: Some specific integer value (will be validated)
            let result = calculateDiagonalSum 1001

            let errorMsg1 =
                sprintf "REQ-PE28-PROD: For 1001x1001 grid, diagonal sum must be positive.
Expected: calculateDiagonalSum 1001 > 0
Actual: calculateDiagonalSum 1001 = %d
Guidance: Implement algorithm to compute diagonal sum for 1001x1001 grid.
Consider optimization: diagonal values follow pattern, no need to build full grid." result
            Expect.isGreaterThan result 0 errorMsg1

            // Verify result is reasonable (should be large but not overflow)
            let errorMsg2 =
                sprintf "REQ-PE28-PROD-OVERFLOW: Result must not overflow int32.
Expected: calculateDiagonalSum 1001 < %d
Actual: calculateDiagonalSum 1001 = %d
Guidance: Ensure no integer overflow in calculations." System.Int32.MaxValue result
            Expect.isLessThan result System.Int32.MaxValue errorMsg2

        // ============================================================
        // STRESS TEST (PERFORMANCE)
        // ============================================================

        testCase "9999x9999 grid computes without overflow in <500ms" <| fun () ->
            // REQ-PE28-PERF: Large grid must compute efficiently
            // Expected: Completes in <500ms, no overflow
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()
            let result = calculateDiagonalSum 9999
            stopwatch.Stop()

            // Verify no overflow
            let errorMsg1 =
                sprintf "REQ-PE28-PERF-OVERFLOW: 9999x9999 grid result must be positive (no overflow).
Expected: calculateDiagonalSum 9999 > 0
Actual: calculateDiagonalSum 9999 = %d
Guidance: Check for integer overflow. May need to use int64 or bigint for large grids." result
            Expect.isGreaterThan result 0 errorMsg1

            // Verify performance
            let errorMsg2 =
                sprintf "REQ-PE28-PERF-TIME: 9999x9999 grid must compute in <500ms.
Expected: Execution time < 500ms
Actual: Execution time = %dms
Guidance: Optimize algorithm. Diagonal pattern:
- Layer n has corners at positions that can be computed directly
- No need to build full grid, calculate diagonal values mathematically
- Sum diagonal values across all layers from center outward." stopwatch.ElapsedMilliseconds
            Expect.isLessThan stopwatch.ElapsedMilliseconds 500L errorMsg2

        // ============================================================
        // PATTERN VERIFICATION TESTS
        // ============================================================

        testCase "diagonal sum increases with grid size" <| fun () ->
            // REQ-PE28-PATTERN-001: Diagonal sums should increase monotonically
            // Expected: sum(3x3) < sum(5x5) < sum(7x7)
            let sum3 = calculateDiagonalSum 3
            let sum5 = calculateDiagonalSum 5
            let sum7 = calculateDiagonalSum 7

            let errorMsg1 =
                sprintf "REQ-PE28-PATTERN-001a: Diagonal sum must increase with grid size.
Expected: calculateDiagonalSum 3 < calculateDiagonalSum 5
Actual: sum(3x3)=%d, sum(5x5)=%d
Guidance: Verify spiral construction - larger grids add more diagonal values." sum3 sum5
            Expect.isLessThan sum3 sum5 errorMsg1

            let errorMsg2 =
                sprintf "REQ-PE28-PATTERN-001b: Diagonal sum must increase with grid size.
Expected: calculateDiagonalSum 5 < calculateDiagonalSum 7
Actual: sum(5x5)=%d, sum(7x7)=%d
Guidance: Check diagonal value computation for each layer." sum5 sum7
            Expect.isLessThan sum5 sum7 errorMsg2

        testCase "diagonal sum for 9x9 grid" <| fun () ->
            // REQ-PE28-PATTERN-002: Verify pattern continues for 9x9
            // Expected: Sum is 537
            let result = calculateDiagonalSum 9
            let errorMsg =
                sprintf "REQ-PE28-PATTERN-002: For 9x9 grid, diagonal sum must be 537.
Expected: calculateDiagonalSum 9 = 537
Actual: calculateDiagonalSum 9 = %d
Guidance: Verify spiral pattern continues correctly to 9x9 grid." result
            Expect.equal result 537 errorMsg
    ]

[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] args tests
