import org.scalatest.funsuite.AnyFunSuite

class MatrixProductTest extends AnyFunSuite {
  test("greatestProduct finds correct product in sample matrix") {
    val matrix = Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12),
      Array(13, 14, 15, 16)
    )
    val (prod, coords) = MatrixProduct.greatestProduct(matrix, 4)
    assert(prod == 43680)
    assert(coords.contains((3, 0))) // leftmost column, down
  }
}
