#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ADJ 4

// Reads a dynamic matrix from a FILE*, returns pointer and sets n (size). Returns NULL on error.
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
        if (count != width) { // Not square
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
    if (n != width || n < ADJ) { // Must be square and at least ADJ
        for (int i = 0; i < n; ++i) free(matrix[i]);
        free(matrix);
        *n_out = 0;
        return NULL;
    }
    *n_out = n;
    return matrix;
}

// Returns the greatest product of four adjacent numbers in any direction
int greatest_product(int **matrix, int n, int coords[ADJ][2]) {
    int max = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            // right
            if (j + ADJ <= n) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i][j+k];
                if (prod > max) {
                    max = prod;
                    for (int k = 0; k < ADJ; ++k) { coords[k][0] = i; coords[k][1] = j+k; }
                }
            }
            // down
            if (i + ADJ <= n) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j];
                if (prod > max) {
                    max = prod;
                    for (int k = 0; k < ADJ; ++k) { coords[k][0] = i+k; coords[k][1] = j; }
                }
            }
            // diag down-right
            if (i + ADJ <= n && j + ADJ <= n) {
                int prod = 1;
                for (int k = 0; k < ADJ; ++k) prod *= matrix[i+k][j+k];
                if (prod > max) {
                    max = prod;
                    for (int k = 0; k < ADJ; ++k) { coords[k][0] = i+k; coords[k][1] = j+k; }
                }
            }
            // diag down-left
            if (i + ADJ <= n && j - ADJ + 1 >= 0) {
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

void print_matrix_with_highlight(int **matrix, int n, int coords[ADJ][2]) {
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
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
    FILE *file = NULL;
    const char *filename = NULL;
    if (argc > 1) {
        filename = argv[1];
        if (strcmp(filename, "-") == 0) {
            file = stdin;
        } else {
            file = fopen(filename, "r");
            if (!file) {
                fprintf(stderr, "Failed to open file: %s\n", filename);
                return 1;
            }
        }
    } else {
        filename = "matrix.txt";
        file = fopen(filename, "r");
        if (!file) {
            fprintf(stderr, "Failed to open file: %s\n", filename);
            return 1;
        }
    }
    int n = 0;
    int **matrix = read_matrix(file, &n);
    if (file != stdin) fclose(file);
    if (!matrix) {
        fprintf(stderr, "Error: Matrix must be square and at least %dx%d in size.\n", ADJ, ADJ);
        return 1;
    }
    int coords[ADJ][2];
    int max = greatest_product(matrix, n, coords);
    print_matrix_with_highlight(matrix, n, coords);
    printf("Greatest product of four adjacent numbers: %d\n", max);
    for (int i = 0; i < n; ++i) free(matrix[i]);
    free(matrix);
    return 0;
}
