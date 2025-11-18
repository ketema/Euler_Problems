import unittest
from problem30 import solve

class TestProblem30(unittest.TestCase):
    def test_example(self):
        self.assertEqual(solve(4), 19316)

    def test_solution(self):
        self.assertEqual(solve(5), 443839)

if __name__ == '__main__':
    unittest.main()
