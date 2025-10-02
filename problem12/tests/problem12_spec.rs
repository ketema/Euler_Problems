use rstest::rstest;
use speculate::speculate;
use problem12::{triangle_number, count_divisors, find_triangle_with_divisors};

speculate! {
    describe "triangle_number" {
        it "returns the 7th triangle number as 28" {
            assert_eq!(triangle_number(7), 28);
        }
    }

    describe "count_divisors" {
        it "returns 6 for 28" {
            assert_eq!(count_divisors(28), 6);
        }
        it "returns 1 for 1" {
            assert_eq!(count_divisors(1), 1);
        }
    }

    describe "find_triangle_with_divisors" {
        it "returns 28 for over 5 divisors" {
            assert_eq!(find_triangle_with_divisors(5), 28);
        }
        // The following test is slow, so it's commented out by default.
        // Uncomment to run the real Project Euler test.
        it "returns the correct triangle number for over 500 divisors" {
            assert_eq!(find_triangle_with_divisors(500), 76576500);
        }
    }
}
