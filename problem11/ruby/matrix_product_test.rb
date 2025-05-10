require 'minitest/autorun'
require_relative 'matrix_product'

class TestMatrixProduct < Minitest::Test
  def test_greatest_product_4x4
    matrix = [
      [1, 2, 3, 4],
      [5, 6, 7, 8],
      [9, 10, 11, 12],
      [13, 14, 15, 16]
    ]
    result, coords = greatest_product(matrix, 4)
    expected = 13 * 14 * 15 * 16
    expected_coords = [[3,0],[3,1],[3,2],[3,3]]
    assert_equal expected, result
    assert_equal expected_coords, coords
  end
end
