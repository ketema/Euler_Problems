#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N 20
#define ADJ 4

int matrix[N][N];

// Reads the matrix from a file
int read_matrix(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) return 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            if (fscanf(file, "%d", &matrix[i][j]) != 1) {
                fclose(file);
                return 0;
            }
        }
    }
    fclose(file);
    return 1;
}

// Returns the greatest product of four adjacent numbers in any direction
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

void print_matrix_with_highlight(int coords[ADJ][2]) {
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            int highlight = 0;
            for (int k = 0; k < ADJ; ++k)
                if (coords[k][0] == i && coords[k][1] == j) highlight = 1;
            if (highlight)
                printf("\033[31m%02d\033[0m ", matrix[i][j]);
            else
                printf("%02d ", matrix[i][j]);
        }
        printf("\n");
    }
}

int main(int argc, char *argv[]) {
    const char *filename = argc > 1 ? argv[1] : "../matrix";
    if (!read_matrix(filename)) {
        fprintf(stderr, "Failed to read matrix from %s\n", filename);
        return 1;
    }
    int coords[ADJ][2];
    int max = greatest_product(coords);
    print_matrix_with_highlight(coords);
    printf("Greatest product of four adjacent numbers: %d\n", max);
    return 0;
}
