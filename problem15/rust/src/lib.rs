/*
Project Euler Problem 15: Lattice Paths

WHY: Calculate number of paths through an n×n grid moving only right/down
EXPECTED: Efficient solution using dynamic programming or combinatorics
*/

/// Calculate number of lattice paths for an n×n grid using dynamic programming
///
/// WHY: Use DP to avoid factorial overflow and improve efficiency
/// EXPECTED: Returns exact count of paths, handles large grids efficiently
///
/// # Arguments
///
/// * `n` - Grid size (n×n grid)
///
/// # Returns
///
/// Number of unique paths from top-left to bottom-right
///
/// # Examples
///
/// ```
/// use euler_problem_15::count_lattice_paths;
/// assert_eq!(count_lattice_paths(2), 6);
/// assert_eq!(count_lattice_paths(3), 20);
/// ```
pub fn count_lattice_paths(n: usize) -> u64 {
    // Create DP table: dp[i][j] = number of paths to reach position (i, j)
    // Grid is (n+1) × (n+1) because we include the starting position
    let size = n + 1;
    let mut dp = vec![vec![0u64; size]; size];

    // Initialize: there's exactly 1 way to reach any cell in first row or column
    // (only by moving right or down respectively)
    for i in 0..size {
        dp[i][0] = 1;  // First column
        dp[0][i] = 1;  // First row
    }

    // Fill DP table using recurrence relation:
    // paths_to(i,j) = paths_to(i-1,j) + paths_to(i,j-1)
    for i in 1..size {
        for j in 1..size {
            dp[i][j] = dp[i - 1][j] + dp[i][j - 1];
        }
    }

    // Return paths to bottom-right corner
    dp[n][n]
}

/// Calculate lattice paths using combinatorial formula
///
/// WHY: Alternative method using binomial coefficient C(2n, n)
/// EXPECTED: Same result as DP approach, potentially more efficient for small n
///
/// For an n×n grid: C(2n, n) = (2n)! / (n! × n!)
/// We compute this iteratively to avoid overflow
pub fn count_lattice_paths_combinatorial(n: usize) -> u64 {
    if n == 0 {
        return 1;
    }

    // Calculate C(2n, n) = (2n)! / (n! * n!)
    // We compute this as: (2n × (2n-1) × ... × (n+1)) / (n × (n-1) × ... × 1)
    let mut result: u64 = 1;

    for i in 1..=n {
        // Multiply by (n + i) and divide by i to maintain precision
        result = result * (n + i) as u64 / i as u64;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lattice_paths_small_grids() {
        // 1×1 grid: 2 paths (RD or DR)
        assert_eq!(count_lattice_paths(1), 2);

        // 2×2 grid: 6 paths (from problem statement)
        assert_eq!(count_lattice_paths(2), 6);

        // 3×3 grid: C(6,3) = 20 paths
        assert_eq!(count_lattice_paths(3), 20);
    }

    #[test]
    fn test_lattice_paths_zero_grid() {
        // 0×0 grid: 1 path (stay at start)
        assert_eq!(count_lattice_paths(0), 1);
    }

    #[test]
    fn test_lattice_paths_consistency() {
        // Test that both methods give same results
        for n in 0..=10 {
            let dp_result = count_lattice_paths(n);
            let comb_result = count_lattice_paths_combinatorial(n);
            assert_eq!(
                dp_result, comb_result,
                "Mismatch for n={}: DP={}, Combinatorial={}",
                n, dp_result, comb_result
            );
        }
    }

    #[test]
    fn test_problem_solution_20x20() {
        // Main problem: 20×20 grid
        let result = count_lattice_paths(20);

        // Verify it's a reasonable large number
        assert!(result > 0, "Result should be positive");
        assert!(result > 1_000_000, "Result should be very large");

        println!("\nProblem 15 Solution: {} paths", result);
    }

    #[test]
    fn test_known_values() {
        // Test against known binomial coefficients
        // C(4,2) = 6 (for 2×2 grid)
        assert_eq!(count_lattice_paths(2), 6);

        // C(6,3) = 20 (for 3×3 grid)
        assert_eq!(count_lattice_paths(3), 20);

        // C(8,4) = 70 (for 4×4 grid)
        assert_eq!(count_lattice_paths(4), 70);

        // C(10,5) = 252 (for 5×5 grid)
        assert_eq!(count_lattice_paths(5), 252);
    }

    #[test]
    fn test_combinatorial_method() {
        // Test combinatorial method specifically
        assert_eq!(count_lattice_paths_combinatorial(2), 6);
        assert_eq!(count_lattice_paths_combinatorial(3), 20);
        assert_eq!(count_lattice_paths_combinatorial(10), count_lattice_paths(10));
    }
}
