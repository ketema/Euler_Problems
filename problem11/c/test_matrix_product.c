#include <stdio.h>
#include <assert.h>

#define N 4
#define ADJ 4

// Simple 4x4 test matrix for unit tests
int matrix[N][N] = {
    {1, 2, 3, 4},
    {5, 6, 7, 8},
    {9, 10, 11, 12},
    {13, 14, 15, 16}
};

// Copy of the logic from matrix_product.c, adapted for 4x4
int greatest_product(int coords[ADJ][2]) {
    int max = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            // right
            if (j + ADJ <= N) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i][j+k];
                if (prod > max) {
                    max = prod;
                    for (int k = 0; k < ADJ; ++k) { coords[k][0] = i; coords[k][1] = j+k; }
                }
            }
            // down
            if (i + ADJ <= N) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j];
                if (prod > max) {
                    max = prod;
                    for (int k = 0; k < ADJ; ++k) { coords[k][0] = i+k; coords[k][1] = j; }
                }
            }
            // diag down-right
            if (i + ADJ <= N && j + ADJ <= N) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j+k];
                if (prod > max) {
                    max = prod;
                    for (int k = 0; k < ADJ; ++k) { coords[k][0] = i+k; coords[k][1] = j+k; }
                }
            }
            // diag down-left
            if (i + ADJ <= N && j - ADJ + 1 >= 0) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j-k];
                if (prod > max) {
                    max = prod;
                    for (int k = 0; k < ADJ; ++k) { coords[k][0] = i+k; coords[k][1] = j-k; }
                }
            }
        }
    }
    return max;
}

void test_greatest_product() {
    int coords[ADJ][2];
    int result = greatest_product(coords);
    assert(result == 43680); // 13*14*15*16
    printf("test_greatest_product passed!\n");
}

int main() {
    test_greatest_product();
    printf("All tests passed!\n");
    return 0;
}
