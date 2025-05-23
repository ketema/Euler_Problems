import unittest
from matrix_product import (
    read_matrix,
    greatest_product_in_matrix,
    max_product_right,
    max_product_down,
    max_product_diag_down_right,
    max_product_diag_down_left,
    find_max_product_sequence,
    color_matrix_sequence,
)

class TestMatrixProduct(unittest.TestCase):
    def setUp(self):
        self.sample_matrix = [
            [8, 2, 22, 97],
            [49, 49, 99, 40],
            [81, 49, 31, 73],
            [52, 70, 95, 23]
        ]

    def test_read_matrix(self):
        # Test reading a matrix from a file
        import tempfile
        with tempfile.NamedTemporaryFile('w+', delete=False) as tmp:
            tmp.write('1 2 3\n4 5 6\n7 8 9\n')
            tmp.flush()
            tmp.seek(0)
            matrix = read_matrix(tmp.name)
            self.assertEqual(matrix, [[1,2,3],[4,5,6],[7,8,9]])

    def test_max_product_right(self):
        # Only right (horizontal) products
        m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        self.assertEqual(max_product_right(m, 4), 13*14*15*16)

    def test_max_product_down(self):
        # Only down (vertical) products
        m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        self.assertEqual(max_product_down(m, 4), 4*8*12*16)

    def test_max_product_diag_down_right(self):
        # Only diagonal down-right product
        m = [[1,0,0,4],[0,6,7,0],[0,10,11,0],[13,0,0,16]]
        self.assertEqual(max_product_diag_down_right(m, 4), 1*6*11*16)

    def test_max_product_diag_down_left(self):
        # Only diagonal down-left product
        m = [[0,0,0,4],[0,0,7,0],[0,10,0,0],[13,0,0,0]]
        self.assertEqual(max_product_diag_down_left(m, 4), 4*7*10*13)

    def test_greatest_product_in_matrix(self):
        # Should return the max of all four directions
        m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        self.assertEqual(greatest_product_in_matrix(m, 4), 13*14*15*16)

    def test_find_max_product_sequence(self):
        m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        product, coords = find_max_product_sequence(m, 4)
        # Should be the last row, right direction
        expected_coords = [(3,0),(3,1),(3,2),(3,3)]
        self.assertEqual(product, 13*14*15*16)
        self.assertEqual(coords, expected_coords)

    def test_color_matrix_sequence(self):
        m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        coords = [(3,0),(3,1),(3,2),(3,3)]
        output = color_matrix_sequence(m, coords)
        # Check that the output contains ANSI red codes for the sequence
        for val in [13,14,15,16]:
            self.assertIn(f'\033[31m{val:02}\033[0m', output)
        # Check that non-sequence values are not colored
        self.assertIn('01', output)
        self.assertNotIn('\033[31m01\033[0m', output)

if __name__ == "__main__":
    unittest.main()
