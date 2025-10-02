// Core logic for Project Euler Problem 12

use std::fmt;
use std::time::Duration;

/// Custom error types for Problem 12
#[derive(Debug, PartialEq)]
pub enum Problem12Error {
    InvalidInput(String),
    Overflow(u64),
    Timeout(Duration),
}

impl fmt::Display for Problem12Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Problem12Error::InvalidInput(input) => write!(f, "Invalid input: {}", input),
            Problem12Error::Overflow(value) => write!(f, "Overflow detected with value: {}", value),
            Problem12Error::Timeout(duration) => write!(f, "Operation timed out after {}s", duration.as_secs()),
        }
    }
}

impl std::error::Error for Problem12Error {}

/// Parses input string to u32, returning custom error on failure
pub fn parse_input(input: &str) -> Result<u32, Problem12Error> {
    let trimmed = input.trim();
    trimmed.parse::<u32>().map_err(|_| Problem12Error::InvalidInput(trimmed.to_string()))
}

/// Returns the nth triangle number.
pub fn triangle_number(n: u64) -> u64 {
    n * (n + 1) / 2
}

/// Returns the nth triangle number with overflow protection.
/// Uses checked arithmetic to detect overflow conditions.
pub fn triangle_number_safe(n: u64) -> Result<u64, Problem12Error> {
    // Check for potential overflow before calculation
    // For n*(n+1)/2, overflow occurs when n*(n+1) > 2*u64::MAX
    // This happens approximately when n > sqrt(2*u64::MAX) ≈ 6.07 * 10^9

    // Use checked multiplication to detect overflow
    match n.checked_mul(n + 1) {
        Some(product) => Ok(product / 2),
        None => Err(Problem12Error::Overflow(n)),
    }
}

/// Returns the number of divisors of n.
pub fn count_divisors(n: u64) -> u32 {
    if n == 1 {
        return 1;
    }
    let mut count = 0;
    let sqrt_n = (n as f64).sqrt() as u64;
    for i in 1..=sqrt_n {
        if n % i == 0 {
            count += 2; // i and n/i
        }
    }
    if sqrt_n * sqrt_n == n {
        count -= 1; // perfect square
    }
    count
}

/// Returns the number of divisors of n using prime factorization (optimized).
/// Uses the formula: if n = p1^a1 * p2^a2 * ... * pk^ak, then divisors = (a1+1)(a2+1)...(ak+1)
pub fn count_divisors_optimized(n: u64) -> u32 {
    if n == 1 {
        return 1;
    }

    let mut num = n;
    let mut divisor_count = 1;

    // Handle factor 2 separately for efficiency
    if num % 2 == 0 {
        let mut power = 0;
        while num % 2 == 0 {
            num /= 2;
            power += 1;
        }
        divisor_count *= power + 1;
    }

    // Check odd factors from 3 onwards
    let mut factor = 3;
    while factor * factor <= num {
        if num % factor == 0 {
            let mut power = 0;
            while num % factor == 0 {
                num /= factor;
                power += 1;
            }
            divisor_count *= power + 1;
        }
        factor += 2; // Only check odd numbers
    }

    // If num > 1, then it's a prime factor
    if num > 1 {
        divisor_count *= 2; // Prime has power 1, so divisors = 1 + 1 = 2
    }

    divisor_count
}

/// Finds the first triangle number with more than min_divisors divisors.
pub fn find_triangle_with_divisors(min_divisors: u32) -> u64 {
    let mut n = 1;
    loop {
        let tri = triangle_number(n);
        if count_divisors(tri) > min_divisors {
            return tri;
        }
        n += 1;
    }
}

/// Finds the first triangle number with more than min_divisors divisors, with timeout.
/// Returns Timeout error if the operation takes longer than the specified duration.
pub fn find_triangle_with_divisors_timeout(min_divisors: u32, timeout: Duration) -> Result<u64, Problem12Error> {
    use std::time::Instant;

    let start_time = Instant::now();

    // Check for zero timeout immediately
    if timeout.is_zero() {
        return Err(Problem12Error::Timeout(timeout));
    }

    let mut n = 1;
    loop {
        // Check timeout periodically (every 1000 iterations for efficiency)
        if n % 1000 == 0 {
            let elapsed = start_time.elapsed();
            if elapsed >= timeout {
                return Err(Problem12Error::Timeout(elapsed));
            }
        }

        let tri = triangle_number(n);
        if count_divisors_optimized(tri) > min_divisors {
            return Ok(tri);
        }
        n += 1;
    }
}
