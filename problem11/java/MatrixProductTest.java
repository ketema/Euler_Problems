public class MatrixProductTest {
    static final int N = 4;
    static final int ADJ = 4;
    static int[][] matrix = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12},
        {13, 14, 15, 16}
    };

    public static int greatestProduct(int[][] matrix, int[][] coords) {
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

    public static void testGreatestProduct() {
        int[][] coords = new int[ADJ][2];
        int result = greatestProduct(matrix, coords);
        if (result != 43680) {
            throw new AssertionError("Expected 43680, got " + result);
        }
        System.out.println("testGreatestProduct passed!");
    }

    public static void main(String[] args) {
        testGreatestProduct();
        System.out.println("All tests passed!");
    }
}
