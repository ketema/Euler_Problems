import unittest
from problem29 import solve

class TestProblem29(unittest.TestCase):
    def test_example(self):
        self.assertEqual(solve(2, 5), 15)

    def test_solution(self):
        self.assertEqual(solve(2, 100), 9183)

if __name__ == '__main__':
    unittest.main()
