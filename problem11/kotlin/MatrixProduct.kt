fun greatestProduct(matrix: List<List<Int>>, adj: Int): Pair<Int, List<Pair<Int, Int>>> {

fun readMatrix(filename: String): List<List<Int>> {
    return java.io.File(filename).readLines()
        .filter { it.isNotBlank() }
        .map { line -> line.trim().split(" ").map { it.toInt() } }
}

fun printMatrix(matrix: List<List<Int>>, coords: List<Pair<Int, Int>>) {
    val coordSet = coords.toSet()
    for (i in matrix.indices) {
        for (j in matrix[i].indices) {
            if (coordSet.contains(Pair(i, j))) {
                print("\u001b[31m%02d\u001b[0m ".format(matrix[i][j]))
            } else {
                print("%02d ".format(matrix[i][j]))
            }
        }
        println()
    }
}

fun main() {
    val matrix = readMatrix("../matrix.txt")
    val (maxProd, coords) = greatestProduct(matrix, 4)
    printMatrix(matrix, coords)
    println("\nGreatest product of four adjacent numbers: $maxProd")
}

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
