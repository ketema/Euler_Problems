import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class MatrixProduct {
    static final int N = 20;
    static final int ADJ = 4;
    static int[][] matrix = new int[N][N];

    public static boolean readMatrix(String filename) {
        try (Scanner sc = new Scanner(new File(filename))) {
            for (int i = 0; i < N; ++i)
                for (int j = 0; j < N; ++j)
                    if (!sc.hasNextInt()) return false;
                    else matrix[i][j] = sc.nextInt();
            return true;
        } catch (FileNotFoundException e) {
            return false;
        }
    }

    public static int greatestProduct(int[][] coords) {
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

    public static void printMatrixWithHighlight(int[][] coords) {
        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                boolean highlight = false;
                for (int k = 0; k < ADJ; ++k)
                    if (coords[k][0] == i && coords[k][1] == j) highlight = true;
                if (highlight)
                    System.out.printf("\033[31m%02d\033[0m ", matrix[i][j]);
                else
                    System.out.printf("%02d ", matrix[i][j]);
            }
            System.out.println();
        }
    }

    public static void main(String[] args) {
        String filename = args.length > 0 ? args[0] : "../matrix";
        if (!readMatrix(filename)) {
            System.err.println("Failed to read matrix from " + filename);
            System.exit(1);
        }
        int[][] coords = new int[ADJ][2];
        int max = greatestProduct(coords);
        printMatrixWithHighlight(coords);
        System.out.println("Greatest product of four adjacent numbers: " + max);
    }
}
