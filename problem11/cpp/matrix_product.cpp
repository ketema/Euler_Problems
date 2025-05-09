#include <iostream>
#include <fstream>
#include <vector>
#include <iomanip>

const int N = 20;
const int ADJ = 4;

typedef std::vector<std::vector<int> > Matrix;

// Reads the matrix from a file
bool read_matrix(const std::string& filename, Matrix& matrix) {
    std::ifstream file(filename);
    if (!file) return false;
    matrix.resize(N, std::vector<int>(N));
    for (int i = 0; i < N; ++i)
        for (int j = 0; j < N; ++j)
            if (!(file >> matrix[i][j])) return false;
    return true;
}

// Returns the greatest product of four adjacent numbers in any direction
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

void print_matrix_with_highlight(const Matrix& matrix, const std::vector<std::pair<int, int> >& coords) {
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            bool highlight = false;
            for (size_t idx = 0; idx < coords.size(); ++idx) {
                const std::pair<int, int>& p = coords[idx];
                if (p.first == i && p.second == j) highlight = true;
            }
            if (highlight)
                std::cout << "\033[31m" << std::setw(2) << std::setfill('0') << matrix[i][j] << "\033[0m ";
            else
                std::cout << std::setw(2) << std::setfill('0') << matrix[i][j] << " ";
        }
        std::cout << "\n";
    }
}

int main(int argc, char* argv[]) {
    std::string filename = argc > 1 ? argv[1] : "../matrix";
    Matrix matrix;
    if (!read_matrix(filename, matrix)) {
        std::cerr << "Failed to read matrix from " << filename << std::endl;
        return 1;
    }
    std::vector<std::pair<int, int> > coords;
    int max = greatest_product(matrix, coords);
    print_matrix_with_highlight(matrix, coords);
    std::cout << "Greatest product of four adjacent numbers: " << max << std::endl;
    return 0;
}
