#[cfg(test)]
mod tests {
    use problem12::{count_divisors, count_divisors_optimized};

    #[test]
    fn test_count_divisors_optimized_same_as_original_small() {
        // Test that optimized version gives same results as original
        assert_eq!(count_divisors_optimized(28), count_divisors(28));
        assert_eq!(count_divisors_optimized(28), 6);
    }

    #[test]
    fn test_count_divisors_optimized_same_as_original_medium() {
        assert_eq!(count_divisors_optimized(1000), count_divisors(1000));
    }

    #[test]
    fn test_count_divisors_optimized_same_as_original_large() {
        // Test the Project Euler answer
        assert_eq!(count_divisors_optimized(76576500), count_divisors(76576500));
        assert_eq!(count_divisors_optimized(76576500), 576);
    }

    #[test]
    fn test_count_divisors_optimized_edge_cases() {
        assert_eq!(count_divisors_optimized(1), count_divisors(1));
        assert_eq!(count_divisors_optimized(2), count_divisors(2));
        assert_eq!(count_divisors_optimized(3), count_divisors(3));
        assert_eq!(count_divisors_optimized(4), count_divisors(4));
    }

    #[test]
    fn test_count_divisors_optimized_prime_numbers() {
        // Prime numbers should have exactly 2 divisors
        assert_eq!(count_divisors_optimized(7), 2);
        assert_eq!(count_divisors_optimized(11), 2);
        assert_eq!(count_divisors_optimized(13), 2);
    }

    #[test]
    fn test_count_divisors_optimized_perfect_squares() {
        // Perfect squares: 4=2², 9=3², 16=4², 25=5²
        assert_eq!(count_divisors_optimized(4), count_divisors(4));
        assert_eq!(count_divisors_optimized(9), count_divisors(9));
        assert_eq!(count_divisors_optimized(16), count_divisors(16));
        assert_eq!(count_divisors_optimized(25), count_divisors(25));
    }
}
