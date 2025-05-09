import unittest
from matrix_product import read_matrix, greatest_product_in_matrix

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

    def test_greatest_product_in_matrix_horizontal(self):
        # Test horizontal product
        m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        self.assertEqual(greatest_product_in_matrix(m, 4), 13*14*15*16)

    def test_greatest_product_in_matrix_vertical(self):
        # Test vertical product
        m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        self.assertEqual(greatest_product_in_matrix(m, 4), 13*14*15*16)

    def test_greatest_product_in_matrix_diagonal(self):
        # The greatest product in this matrix is from the last row: 13*14*15*16
        m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        self.assertEqual(greatest_product_in_matrix(m, 4), 13*14*15*16)

    def test_greatest_product_in_matrix_antidiagonal(self):
        # The greatest product in this matrix is from the last row: 13*14*15*16
        m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
        self.assertEqual(greatest_product_in_matrix(m, 4), 13*14*15*16)

if __name__ == "__main__":
    unittest.main()
