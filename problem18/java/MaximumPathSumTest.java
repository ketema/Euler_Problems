import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for Project Euler Problem 18: Maximum Path Sum I
 * Following TDD/BDD methodology per AGENTS.md
 */
public class MaximumPathSumTest {

    @Test
    public void testSmallExample() {
        // Example from problem statement
        int[][] triangle = {
            {3},
            {7, 4},
            {2, 4, 6},
            {8, 5, 9, 3}
        };

        int result = MaximumPathSum.findMaximumPathSum(triangle);
        assertEquals(23, result, "Path should be 3 + 7 + 4 + 9 = 23");
    }

    @Test
    public void testSingleElement() {
        int[][] triangle = {{5}};
        assertEquals(5, MaximumPathSum.findMaximumPathSum(triangle));
    }

    @Test
    public void testTwoRows() {
        int[][] triangle = {
            {1},
            {2, 3}
        };
        // Path: 1 + 3 = 4
        assertEquals(4, MaximumPathSum.findMaximumPathSum(triangle));
    }

    @Test
    public void testThreeRows() {
        int[][] triangle = {
            {1},
            {2, 3},
            {4, 5, 6}
        };
        // Path: 1 + 3 + 6 = 10
        assertEquals(10, MaximumPathSum.findMaximumPathSum(triangle));
    }

    @Test
    public void testParseTriangle() {
        String[] lines = {
            "3",
            "7 4",
            "2 4 6"
        };

        int[][] triangle = MaximumPathSum.parseTriangle(lines);

        assertEquals(3, triangle.length);
        assertArrayEquals(new int[]{3}, triangle[0]);
        assertArrayEquals(new int[]{7, 4}, triangle[1]);
        assertArrayEquals(new int[]{2, 4, 6}, triangle[2]);
    }

    @Test
    public void testEmptyTriangle() {
        int[][] triangle = {};
        assertEquals(0, MaximumPathSum.findMaximumPathSum(triangle));
    }
}
