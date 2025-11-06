public class TestSmall {
    public static void main(String[] args) {
        int[][] triangle = {
            {3},
            {7, 4},
            {2, 4, 6},
            {8, 5, 9, 3}
        };
        int result = MaximumPathSum.findMaximumPathSum(triangle);
        System.out.println("Small example result: " + result);
        System.out.println("Expected: 23");
        System.out.println(result == 23 ? "✓ PASS" : "✗ FAIL");
    }
}
