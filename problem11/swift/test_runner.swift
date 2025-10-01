// Simple test runner without XCTest dependency

func testGreatestProduct4x4() {
    let matrix = [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [9, 10, 11, 12],
        [13, 14, 15, 16]
    ]
    let (result, coords) = greatestProduct(matrix: matrix, adj: 4)
    let expected = 13 * 14 * 15 * 16
    let expectedCoords = [(3,0),(3,1),(3,2),(3,3)]
    
    assert(result == expected, "Expected \(expected), got \(result)")

    // Compare coords manually since tuples don't conform to Equatable
    assert(coords.count == expectedCoords.count, "Coords count mismatch")
    for (i, coord) in coords.enumerated() {
        assert(coord.0 == expectedCoords[i].0 && coord.1 == expectedCoords[i].1,
               "Coord mismatch at index \(i)")
    }

    print("✓ testGreatestProduct4x4 passed")
}

// Main entry point
@main
struct TestRunner {
    static func main() {
        print("Running tests...")
        testGreatestProduct4x4()
        print("All tests passed!")
    }
}

