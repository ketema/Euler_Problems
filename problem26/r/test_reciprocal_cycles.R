#!/usr/bin/env Rscript
# Test suite for Project Euler Problem 26: Reciprocal Cycles
# Following TDD methodology - RED phase
# Custom testing framework (testthat not available)

# Source the functions we're testing
source("reciprocal_cycles.R")

# Simple test framework
tests_run <- 0
tests_passed <- 0
tests_failed <- 0

assert_equal <- function(actual, expected, message) {
  tests_run <<- tests_run + 1
  if (isTRUE(all.equal(actual, expected))) {
    tests_passed <<- tests_passed + 1
    cat(sprintf("  ✓ %s\n", message))
  } else {
    tests_failed <<- tests_failed + 1
    cat(sprintf("  ✗ %s\n", message))
    cat(sprintf("    Expected: %s\n", expected))
    cat(sprintf("    Actual:   %s\n", actual))
  }
}

assert_true <- function(condition, message) {
  tests_run <<- tests_run + 1
  if (isTRUE(condition)) {
    tests_passed <<- tests_passed + 1
    cat(sprintf("  ✓ %s\n", message))
  } else {
    tests_failed <<- tests_failed + 1
    cat(sprintf("  ✗ %s\n", message))
  }
}

cat("Running R tests for Problem 26: Reciprocal Cycles\n")
cat("==================================================\n\n")

# Test cycle_length function
cat("Testing cycle_length()...\n")
assert_equal(cycle_length(2), 0, "1/2 = 0.5 (no cycle)")
assert_equal(cycle_length(3), 1, "1/3 = 0.333... (cycle length 1)")
assert_equal(cycle_length(4), 0, "1/4 = 0.25 (terminating)")
assert_equal(cycle_length(5), 0, "1/5 = 0.2 (terminating)")
assert_equal(cycle_length(6), 1, "1/6 = 0.1666... (cycle length 1)")
assert_equal(cycle_length(7), 6, "1/7 = 0.142857... (cycle length 6)")
assert_equal(cycle_length(8), 0, "1/8 = 0.125 (terminating)")
assert_equal(cycle_length(9), 1, "1/9 = 0.111... (cycle length 1)")
assert_equal(cycle_length(10), 0, "1/10 = 0.1 (terminating)")
assert_equal(cycle_length(11), 2, "1/11 = 0.090909... (cycle length 2)")
assert_equal(cycle_length(12), 1, "1/12 = 0.08333... (cycle length 1)")
assert_equal(cycle_length(13), 6, "1/13 = 0.076923... (cycle length 6)")

cat("\nTesting edge cases...\n")
assert_equal(cycle_length(1), 0, "1/1 = 1 (no cycle)")
assert_true(cycle_length(17) > 0, "1/17 has a cycle")
assert_true(cycle_length(19) > 0, "1/19 has a cycle")

cat("\nTesting find_longest_cycle()...\n")
result <- find_longest_cycle(10)
assert_equal(result$d, 7, "For d < 10, longest is 1/7")
assert_equal(result$length, 6, "1/7 has cycle length 6")

result <- find_longest_cycle(20)
assert_true(result$d >= 7, "For d < 20, d >= 7")
assert_true(result$length >= 6, "For d < 20, length >= 6")

cat("\nTesting solve()...\n")
result <- solve()
assert_true(is.numeric(result), "Result is numeric")
assert_true(result > 0, "Result is positive")
assert_true(result < 1000, "Result is less than 1000")

# Test that the result has a large cycle
cycle_len <- cycle_length(result)
assert_true(cycle_len > 900, "Result has cycle > 900")

cat("\n==================================================\n")
cat(sprintf("Tests run:    %d\n", tests_run))
cat(sprintf("Tests passed: %d\n", tests_passed))
cat(sprintf("Tests failed: %d\n", tests_failed))
cat("\n")

if (tests_failed == 0) {
  cat("✓ All tests passed!\n")
  quit(status = 0)
} else {
  cat("✗ Some tests failed!\n")
  quit(status = 1)
}
