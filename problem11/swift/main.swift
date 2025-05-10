import Foundation

func readMatrix(filename: String) throws -> [[Int]] {
    let content = try String(contentsOfFile: filename)
    return content.split(separator: "\n").map {
        $0.split(separator: " ").compactMap { Int($0) }
    }
}

func printMatrixWithHighlight(matrix: [[Int]], coords: [(Int, Int)]) {
    let coordSet = Set(coords.map { "\($0.0),\($0.1)" })
    for (i, row) in matrix.enumerated() {
        for (j, val) in row.enumerated() {
            if coordSet.contains("\(i),\(j)") {
                print("\u{001B}[31m" + String(format: "%02d", val) + "\u{001B}[0m ", terminator: "")
            } else {
                print(String(format: "%02d", val), terminator: " ")
            }
        }
        print()
    }
}

// Entry point
let filename = CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : "matrix.txt"
var matrix: [[Int]] = []
do {
    matrix = try readMatrix(filename: filename)
} catch {
    fputs("Failed to read matrix from \(filename): \(error)\n", stderr)
    exit(1)
}
let (maxProduct, coords) = greatestProduct(matrix: matrix, adj: 4)
printMatrixWithHighlight(matrix: matrix, coords: coords)
print("Greatest product of four adjacent numbers: \(maxProduct)")
