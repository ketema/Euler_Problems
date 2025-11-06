import java.io.*;
import java.util.*;

/**
 * Project Euler Problem 18: Maximum Path Sum I
 *
 * WHY: Find maximum sum path through triangle using dynamic programming
 * EXPECTED: Efficient O(n²) solution using bottom-up DP approach
 */
public class MaximumPathSum {

    /**
     * Find maximum path sum through triangle using dynamic programming
     *
     * WHY: Bottom-up DP avoids exponential brute force
     * EXPECTED: Returns maximum sum, modifies triangle in-place
     *
     * @param triangle 2D array representing the triangle
     * @return maximum path sum from top to bottom
     */
    public static int findMaximumPathSum(int[][] triangle) {
        if (triangle == null || triangle.length == 0) {
            return 0;
        }

        int rows = triangle.length;

        // Work bottom-up: for each row from second-to-last to top
        for (int row = rows - 2; row >= 0; row--) {
            for (int col = 0; col < triangle[row].length; col++) {
                // Add maximum of two adjacent values from row below
                int leftChild = triangle[row + 1][col];
                int rightChild = triangle[row + 1][col + 1];
                triangle[row][col] += Math.max(leftChild, rightChild);
            }
        }

        // The top element now contains the maximum path sum
        return triangle[0][0];
    }

    /**
     * Parse triangle from text data
     *
     * @param lines lines of text containing triangle data
     * @return 2D array representing the triangle
     */
    public static int[][] parseTriangle(String[] lines) {
        int rows = lines.length;
        int[][] triangle = new int[rows][];

        for (int i = 0; i < rows; i++) {
            String[] numbers = lines[i].trim().split("\\s+");
            triangle[i] = new int[numbers.length];

            for (int j = 0; j < numbers.length; j++) {
                triangle[i][j] = Integer.parseInt(numbers[j]);
            }
        }

        return triangle;
    }

    /**
     * Read triangle from file
     *
     * @param filename name of file containing triangle data
     * @return 2D array representing the triangle
     */
    public static int[][] readTriangleFromFile(String filename) throws IOException {
        List<String> lines = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    lines.add(line);
                }
            }
        }

        return parseTriangle(lines.toArray(new String[0]));
    }

    /**
     * Print triangle for debugging
     */
    public static void printTriangle(int[][] triangle) {
        for (int[] row : triangle) {
            for (int num : row) {
                System.out.printf("%02d ", num);
            }
            System.out.println();
        }
    }

    public static void main(String[] args) {
        System.out.println("Solving Project Euler Problem 18...");
        System.out.println("Finding maximum path sum through triangle\n");

        try {
            // Try to read from file, otherwise use small example
            int[][] triangle;

            File file = new File("../triangle.txt");
            if (file.exists()) {
                triangle = readTriangleFromFile("../triangle.txt");
                System.out.println("Loaded triangle with " + triangle.length + " rows");
            } else {
                // Use small example from problem statement
                triangle = new int[][] {
                    {3},
                    {7, 4},
                    {2, 4, 6},
                    {8, 5, 9, 3}
                };
                System.out.println("Using small example (file not found)");
            }

            int result = findMaximumPathSum(triangle);
            System.out.println("\nAnswer: " + result);

        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
