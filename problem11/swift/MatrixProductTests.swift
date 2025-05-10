import XCTest
@testable import MatrixProduct

final class MatrixProductTests: XCTestCase {
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
        XCTAssertEqual(result, expected)
        XCTAssertEqual(coords, expectedCoords)
    }
}
