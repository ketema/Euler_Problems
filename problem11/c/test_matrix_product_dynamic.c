#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ADJ 4

#define ADJ 4

int **read_matrix(FILE *file, int *n_out) {
    int capacity = 32, n = 0;
    int **matrix = malloc(capacity * sizeof(int*));
    char line[4096];
    int width = -1;
    while (fgets(line, sizeof(line), file)) {
        int *row = malloc(1024 * sizeof(int));
        int count = 0;
        char *tok = strtok(line, " \t\n");
        while (tok) {
            row[count++] = atoi(tok);
            tok = strtok(NULL, " \t\n");
        }
        if (width == -1) width = count;
        if (count != width) {
            for (int i = 0; i < n; ++i) free(matrix[i]);
            free(matrix); free(row);
            *n_out = 0;
            return NULL;
        }
        matrix[n++] = row;
        if (n >= capacity) {
            capacity *= 2;
            matrix = realloc(matrix, capacity * sizeof(int*));
        }
    }
    if (n != width || n < ADJ) {
        for (int i = 0; i < n; ++i) free(matrix[i]);
        free(matrix);
        *n_out = 0;
        return NULL;
    }
    *n_out = n;
    return matrix;
}

int greatest_product(int **matrix, int n) {
    int max = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (j + ADJ <= n) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i][j+k];
                if (prod > max) max = prod;
            }
            if (i + ADJ <= n) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j];
                if (prod > max) max = prod;
            }
            if (i + ADJ <= n && j + ADJ <= n) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j+k];
                if (prod > max) max = prod;
            }
            if (i + ADJ <= n && j - ADJ + 1 >= 0) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j-k];
                if (prod > max) max = prod;
            }
        }
    }
    return max;
}

int dynamic_matrix_product(FILE *file, int *result) {
    int n = 0;
    int **matrix = read_matrix(file, &n);
    if (!matrix) return 0;
    *result = greatest_product(matrix, n);
    for (int i = 0; i < n; ++i) free(matrix[i]);
    free(matrix);
    return 1;
}

void test_matrix_too_small() {
    // 3x3 matrix, should fail
    const char *input = "01 02 03\n04 05 06\n07 08 09\n";
    FILE *f = fmemopen((void*)input, strlen(input), "r");
    int result = 0;
    int rc = dynamic_matrix_product(f, &result);
    fclose(f);
    if (rc == 0) {
        printf("PASS: Detected too-small matrix\n");
    } else {
        printf("FAIL: Did not detect too-small matrix\n");
    }
}

void test_matrix_valid() {
    // 4x4 matrix, should succeed
    const char *input = "01 02 03 04\n05 06 07 08\n09 10 11 12\n13 14 15 16\n";
    FILE *f = fmemopen((void*)input, strlen(input), "r");
    int result = 0;
    int rc = dynamic_matrix_product(f, &result);
    fclose(f);
    if (rc == 1) {
        printf("PASS: Correctly handled 4x4 matrix\n");
    } else {
        printf("FAIL: 4x4 matrix not handled\n");
    }
}

int main() {
    test_matrix_too_small();
    test_matrix_valid();
    return 0;
}
