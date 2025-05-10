public func greatestProduct(matrix: [[Int]], adj: Int) -> (Int, [(Int, Int)]) {
    let rows = matrix.count
    let cols = matrix.first?.count ?? 0
    var maxProduct = 0
    var bestCoords: [(Int, Int)] = []
    for i in 0..<rows {
        for j in 0..<cols {
            // right
            if j + adj <= cols {
                var prod = 1
                var coords: [(Int, Int)] = []
                for k in 0..<adj {
                    prod *= matrix[i][j + k]
                    coords.append((i, j + k))
                }
                if prod > maxProduct {
                    maxProduct = prod
                    bestCoords = coords
                }
            }
            // down
            if i + adj <= rows {
                var prod = 1
                var coords: [(Int, Int)] = []
                for k in 0..<adj {
                    prod *= matrix[i + k][j]
                    coords.append((i + k, j))
                }
                if prod > maxProduct {
                    maxProduct = prod
                    bestCoords = coords
                }
            }
            // diag down-right
            if i + adj <= rows && j + adj <= cols {
                var prod = 1
                var coords: [(Int, Int)] = []
                for k in 0..<adj {
                    prod *= matrix[i + k][j + k]
                    coords.append((i + k, j + k))
                }
                if prod > maxProduct {
                    maxProduct = prod
                    bestCoords = coords
                }
            }
            // diag down-left
            if i + adj <= rows && j >= adj - 1 {
                var prod = 1
                var coords: [(Int, Int)] = []
                for k in 0..<adj {
                    prod *= matrix[i + k][j - k]
                    coords.append((i + k, j - k))
                }
                if prod > maxProduct {
                    maxProduct = prod
                    bestCoords = coords
                }
            }
        }
    }
    return (maxProduct, bestCoords)
}
