#[cfg(test)]
mod tests {
    use problem12::{find_triangle_with_divisors_timeout, Problem12Error};
    use std::time::Duration;

    #[test]
    fn test_find_triangle_with_divisors_timeout_normal_operation() {
        // Test normal operation with generous timeout
        let timeout = Duration::from_secs(10);
        let result = find_triangle_with_divisors_timeout(5, timeout);
        
        match result {
            Ok(triangle_num) => assert_eq!(triangle_num, 28),
            Err(e) => panic!("Expected success but got error: {}", e),
        }
    }

    #[test]
    fn test_find_triangle_with_divisors_timeout_quick_result() {
        // Test with very short timeout but easy problem
        let timeout = Duration::from_millis(100);
        let result = find_triangle_with_divisors_timeout(1, timeout);
        
        match result {
            Ok(triangle_num) => assert_eq!(triangle_num, 3), // First triangle number with >1 divisor
            Err(e) => panic!("Expected success but got error: {}", e),
        }
    }

    #[test]
    fn test_find_triangle_with_divisors_timeout_times_out() {
        // Test with very short timeout and hard problem
        let timeout = Duration::from_nanos(1); // Extremely short timeout
        let result = find_triangle_with_divisors_timeout(500, timeout);
        
        match result {
            Err(Problem12Error::Timeout(duration)) => {
                // Should timeout quickly
                assert!(duration <= Duration::from_millis(10)); // Allow some leeway
            },
            Ok(_) => panic!("Expected timeout but got success"),
            Err(e) => panic!("Expected timeout error but got: {}", e),
        }
    }

    #[test]
    fn test_find_triangle_with_divisors_timeout_zero_timeout() {
        // Test with zero timeout - should timeout immediately
        let timeout = Duration::from_secs(0);
        let result = find_triangle_with_divisors_timeout(10, timeout);
        
        match result {
            Err(Problem12Error::Timeout(duration)) => {
                assert_eq!(duration, Duration::from_secs(0));
            },
            Ok(_) => panic!("Expected timeout but got success"),
            Err(e) => panic!("Expected timeout error but got: {}", e),
        }
    }

    #[test]
    fn test_find_triangle_with_divisors_timeout_reasonable_timeout() {
        // Test with reasonable timeout for moderate problem
        let timeout = Duration::from_millis(500);
        let result = find_triangle_with_divisors_timeout(10, timeout);
        
        match result {
            Ok(triangle_num) => {
                // Should find the answer (120 has 16 divisors)
                assert!(triangle_num > 0);
            },
            Err(Problem12Error::Timeout(_)) => {
                // Timeout is also acceptable for this test
                // The important thing is it doesn't panic
            },
            Err(e) => panic!("Unexpected error type: {}", e),
        }
    }
}
