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
