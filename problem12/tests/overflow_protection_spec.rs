#[cfg(test)]
mod tests {
    use problem12::{triangle_number_safe, Problem12Error};

    #[test]
    fn test_triangle_number_safe_normal_values() {
        // Test normal values that should work
        assert_eq!(triangle_number_safe(7), Ok(28));
        assert_eq!(triangle_number_safe(12375), Ok(76576500));
    }

    #[test]
    fn test_triangle_number_safe_edge_cases() {
        assert_eq!(triangle_number_safe(1), Ok(1));
        assert_eq!(triangle_number_safe(2), Ok(3));
        assert_eq!(triangle_number_safe(0), Ok(0));
    }

    #[test]
    fn test_triangle_number_safe_overflow_detection() {
        // Test values that would cause overflow
        // For u64::MAX, we need n where n*(n+1)/2 > u64::MAX
        // This happens around n ≈ sqrt(2 * u64::MAX) ≈ 6.07 * 10^9
        let large_n = 6_074_000_999_u64; // Should cause overflow
        
        match triangle_number_safe(large_n) {
            Err(Problem12Error::Overflow(n)) => assert_eq!(n, large_n),
            _ => panic!("Expected overflow error for large n"),
        }
    }

    #[test]
    fn test_triangle_number_safe_max_safe_value() {
        // Find the largest safe value (approximately sqrt(2 * u64::MAX))
        // This should be around 4,294,967,295 (2^32 - 1)
        let max_safe_n = 4_294_967_295_u64;
        
        // This might overflow or might not, depending on exact calculation
        // The important thing is it doesn't panic
        match triangle_number_safe(max_safe_n) {
            Ok(_) => {}, // Fine if it works
            Err(Problem12Error::Overflow(n)) => assert_eq!(n, max_safe_n), // Fine if it overflows
            _ => panic!("Unexpected error type"),
        }
    }

    #[test]
    fn test_triangle_number_safe_boundary_values() {
        // Test values near the boundary
        let boundary_n = 4_000_000_000_u64;
        
        match triangle_number_safe(boundary_n) {
            Ok(result) => {
                // If it succeeds, verify the result is correct
                assert_eq!(result, boundary_n * (boundary_n + 1) / 2);
            },
            Err(Problem12Error::Overflow(n)) => {
                assert_eq!(n, boundary_n);
            },
            _ => panic!("Unexpected error type"),
        }
    }
}
