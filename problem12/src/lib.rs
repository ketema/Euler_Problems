// Core logic for Project Euler Problem 12

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
