import java.io.File

fun readMatrix(filename: String): List<List<Int>> {
    return File(filename).readLines().filter { it.isNotBlank() }.map { line ->
        line.trim().split(Regex("\\s+")).map { it.toInt() }
    }
}

fun printMatrixWithHighlight(matrix: List<List<Int>>, coords: List<Pair<Int, Int>>) {
    val coordSet = coords.toSet()
    for ((i, row) in matrix.withIndex()) {
        for ((j, value) in row.withIndex()) {
            if (coordSet.contains(Pair(i, j))) {
                print("\u001B[31m%02d\u001B[0m ".format(value))
            } else {
                print("%02d ".format(value))
            }
        }
        println()
    }
}

fun main(args: Array<String>) {
    val filename = if (args.isNotEmpty()) args[0] else "matrix.txt"
    val matrix = try {
        readMatrix(filename)
    } catch (e: Exception) {
        System.err.println("Failed to read matrix from $filename: ${e.message}")
        return
    }
    val (max, coords) = greatestProduct(matrix, 4)
    printMatrixWithHighlight(matrix, coords)
    println("Greatest product of four adjacent numbers: $max")
}
