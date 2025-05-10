import scala.io.Source

object MatrixProduct {
  def readMatrix(filename: String): Array[Array[Int]] = {
    val lines = Source.fromFile(filename).getLines().toArray
    lines.map(_.trim.split(" ").map(_.toInt))
  }

  def greatestProduct(matrix: Array[Array[Int]], adj: Int): (Int, List[(Int, Int)]) = {
    val numRows = matrix.length
    val numCols = matrix(0).length
    var maxProd = 0
    var maxCoords: List[(Int, Int)] = Nil

    def updateMax(prod: Int, coords: List[(Int, Int)]): Unit = {
      if (prod > maxProd) {
        maxProd = prod
        maxCoords = coords
      }
    }

    for (r <- 0 until numRows; c <- 0 until numCols) {
      // Right
      if (c + adj <= numCols) {
        val coords = (0 until adj).map(i => (r, c + i)).toList
        val prod = coords.map { case (rr, cc) => matrix(rr)(cc) }.product
        updateMax(prod, coords)
      }
      // Down
      if (r + adj <= numRows) {
        val coords = (0 until adj).map(i => (r + i, c)).toList
        val prod = coords.map { case (rr, cc) => matrix(rr)(cc) }.product
        updateMax(prod, coords)
      }
      // Diagonal right-down
      if (r + adj <= numRows && c + adj <= numCols) {
        val coords = (0 until adj).map(i => (r + i, c + i)).toList
        val prod = coords.map { case (rr, cc) => matrix(rr)(cc) }.product
        updateMax(prod, coords)
      }
      // Diagonal left-down
      if (r + adj <= numRows && c - adj + 1 >= 0) {
        val coords = (0 until adj).map(i => (r + i, c - i)).toList
        val prod = coords.map { case (rr, cc) => matrix(rr)(cc) }.product
        updateMax(prod, coords)
      }
    }
    (maxProd, maxCoords)
  }

  def printMatrix(matrix: Array[Array[Int]], highlight: List[(Int, Int)]): Unit = {
    val highlightSet = highlight.toSet
    for ((row, r) <- matrix.zipWithIndex) {
      for ((value, c) <- row.zipWithIndex) {
        if (highlightSet((r, c))) print(f"\u001b[31m$value%02d\u001b[0m ")
        else print(f"$value%02d ")
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = if (args.length > 0) args(0) else "matrix.txt"
    val matrix = readMatrix(filename)
    val (maxProd, coords) = greatestProduct(matrix, 4)
    println("Matrix:")
    printMatrix(matrix, coords)
    println(s"\nGreatest product of four adjacent numbers: $maxProd")
  }
}
