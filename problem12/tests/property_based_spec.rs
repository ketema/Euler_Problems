use proptest::prelude::*;
use problem12::{triangle_number, count_divisors, count_divisors_optimized, triangle_number_safe};

proptest! {
    #[test]
    fn test_triangle_number_formula_property(n in 1u64..10000) {
        // Property: triangle_number(n) = n*(n+1)/2
        let result = triangle_number(n);
        let expected = n * (n + 1) / 2;
        prop_assert_eq!(result, expected);
    }

    #[test]
    fn test_triangle_number_monotonic_property(n in 1u64..10000) {
        // Property: triangle numbers are strictly increasing
        let current = triangle_number(n);
        let next = triangle_number(n + 1);
        prop_assert!(next > current);
    }

    #[test]
    fn test_count_divisors_optimized_equals_original(n in 1u64..100000) {
        // Property: optimized version should give same results as original
        let original = count_divisors(n);
        let optimized = count_divisors_optimized(n);
        prop_assert_eq!(original, optimized);
    }

    #[test]
    fn test_count_divisors_minimum_property(n in 1u64..100000) {
        // Property: every number has at least 2 divisors (1 and itself), except 1
        let divisor_count = count_divisors(n);
        if n == 1 {
            prop_assert_eq!(divisor_count, 1);
        } else {
            prop_assert!(divisor_count >= 2);
        }
    }

    #[test]
    fn test_count_divisors_prime_property(n in 2u64..1000) {
        // Property: if n is prime, it should have exactly 2 divisors
        if is_prime(n) {
            let divisor_count = count_divisors(n);
            prop_assert_eq!(divisor_count, 2);
        }
    }

    #[test]
    fn test_count_divisors_perfect_square_property(n in 1u64..1000) {
        // Property: perfect squares have odd number of divisors
        let square = n * n;
        let divisor_count = count_divisors(square);
        prop_assert_eq!(divisor_count % 2, 1);
    }

    #[test]
    fn test_triangle_number_safe_consistency(n in 1u64..100000) {
        // Property: safe version should match unsafe version when no overflow
        match triangle_number_safe(n) {
            Ok(safe_result) => {
                let unsafe_result = triangle_number(n);
                prop_assert_eq!(safe_result, unsafe_result);
            },
            Err(_) => {
                // If safe version overflows, that's acceptable
                // The important thing is it doesn't panic
            }
        }
    }

    #[test]
    fn test_triangle_number_additive_property(n in 1u64..5000) {
        // Property: triangle_number(n) = triangle_number(n-1) + n
        if n > 1 {
            let current = triangle_number(n);
            let previous = triangle_number(n - 1);
            prop_assert_eq!(current, previous + n);
        }
    }

    #[test]
    fn test_count_divisors_multiplicative_property(a in 2u64..100, b in 2u64..100) {
        // Property: if gcd(a,b) = 1, then divisors(a*b) = divisors(a) * divisors(b)
        if gcd(a, b) == 1 {
            let divisors_a = count_divisors(a);
            let divisors_b = count_divisors(b);
            let divisors_ab = count_divisors(a * b);
            prop_assert_eq!(divisors_ab, divisors_a * divisors_b);
        }
    }

    #[test]
    fn test_triangle_number_bounds_property(n in 1u64..10000) {
        // Property: triangle_number(n) should be between n and n²
        let tri = triangle_number(n);
        prop_assert!(tri >= n);
        prop_assert!(tri <= n * n);
    }
}

// Helper functions for property tests
fn is_prime(n: u64) -> bool {
    if n < 2 { return false; }
    if n == 2 { return true; }
    if n % 2 == 0 { return false; }
    
    let sqrt_n = (n as f64).sqrt() as u64;
    for i in (3..=sqrt_n).step_by(2) {
        if n % i == 0 { return false; }
    }
    true
}

fn gcd(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a
}
