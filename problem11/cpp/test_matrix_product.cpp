#include <iostream>
#include <vector>
#include <cassert>

const int N = 4;
const int ADJ = 4;
typedef std::vector<std::vector<int> > Matrix;

// Simple 4x4 test matrix for unit tests
Matrix matrix;

void init_matrix() {
    matrix.clear();
    matrix.push_back(std::vector<int>());
    matrix[0].push_back(1); matrix[0].push_back(2); matrix[0].push_back(3); matrix[0].push_back(4);
    matrix.push_back(std::vector<int>());
    matrix[1].push_back(5); matrix[1].push_back(6); matrix[1].push_back(7); matrix[1].push_back(8);
    matrix.push_back(std::vector<int>());
    matrix[2].push_back(9); matrix[2].push_back(10); matrix[2].push_back(11); matrix[2].push_back(12);
    matrix.push_back(std::vector<int>());
    matrix[3].push_back(13); matrix[3].push_back(14); matrix[3].push_back(15); matrix[3].push_back(16);
}

int greatest_product(const Matrix& matrix, std::vector<std::pair<int, int> >& coords) {
    int max = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            // right
            if (j + ADJ <= N) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i][j+k];
                if (prod > max) {
                    max = prod;
                    coords.clear();
                    for (int k = 0; k < ADJ; ++k) coords.emplace_back(i, j+k);
                }
            }
            // down
            if (i + ADJ <= N) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j];
                if (prod > max) {
                    max = prod;
                    coords.clear();
                    for (int k = 0; k < ADJ; ++k) coords.emplace_back(i+k, j);
                }
            }
            // diag down-right
            if (i + ADJ <= N && j + ADJ <= N) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j+k];
                if (prod > max) {
                    max = prod;
                    coords.clear();
                    for (int k = 0; k < ADJ; ++k) coords.emplace_back(i+k, j+k);
                }
            }
            // diag down-left
            if (i + ADJ <= N && j - ADJ + 1 >= 0) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j-k];
                if (prod > max) {
                    max = prod;
                    coords.clear();
                    for (int k = 0; k < ADJ; ++k) coords.emplace_back(i+k, j-k);
                }
            }
        }
    }
    return max;
}

void test_greatest_product() {
    init_matrix();
    std::vector<std::pair<int, int> > coords;
    int result = greatest_product(matrix, coords);
    assert(result == 43680); // 13*14*15*16
    std::cout << "test_greatest_product passed!\n";
}

int main() {
    test_greatest_product();
    std::cout << "All tests passed!\n";
    return 0;
}
