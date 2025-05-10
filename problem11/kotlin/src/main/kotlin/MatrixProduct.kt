package org.euler.problem11

fun greatestProduct(matrix: List<List<Int>>, adj: Int): Pair<Int, List<Pair<Int, Int>>> {
    val rows = matrix.size
    val cols = if (rows > 0) matrix[0].size else 0
    var max = 0
    var bestCoords = listOf<Pair<Int, Int>>()
    for (i in 0 until rows) {
        for (j in 0 until cols) {
            // right
            if (j + adj <= cols) {
                var prod = 1
                val coords = mutableListOf<Pair<Int, Int>>()
                for (k in 0 until adj) {
                    prod *= matrix[i][j + k]
                    coords.add(Pair(i, j + k))
                }
                if (prod > max) {
                    max = prod
                    bestCoords = coords.toList()
                }
            }
            // down
            if (i + adj <= rows) {
                var prod = 1
                val coords = mutableListOf<Pair<Int, Int>>()
                for (k in 0 until adj) {
                    prod *= matrix[i + k][j]
                    coords.add(Pair(i + k, j))
                }
                if (prod > max) {
                    max = prod
                    bestCoords = coords.toList()
                }
            }
            // diag down-right
            if (i + adj <= rows && j + adj <= cols) {
                var prod = 1
                val coords = mutableListOf<Pair<Int, Int>>()
                for (k in 0 until adj) {
                    prod *= matrix[i + k][j + k]
                    coords.add(Pair(i + k, j + k))
                }
                if (prod > max) {
                    max = prod
                    bestCoords = coords.toList()
                }
            }
            // diag down-left
            if (i + adj <= rows && j - adj + 1 >= 0) {
                var prod = 1
                val coords = mutableListOf<Pair<Int, Int>>()
                for (k in 0 until adj) {
                    prod *= matrix[i + k][j - k]
                    coords.add(Pair(i + k, j - k))
                }
                if (prod > max) {
                    max = prod
                    bestCoords = coords.toList()
                }
            }
        }
    }
    return Pair(max, bestCoords)
}
