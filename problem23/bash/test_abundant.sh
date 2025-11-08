#!/bin/bash
# Test suite for Project Euler Problem 23: Non-abundant Sums
# Following TDD methodology - RED phase

# Simple Bash test framework
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Source the functions we're testing
source "$(dirname "$0")/abundant.sh" 2>/dev/null || true

# Test helper functions
assert_equal() {
    local actual="$1"
    local expected="$2"
    local message="$3"

    TESTS_RUN=$((TESTS_RUN + 1))

    if [ "$actual" = "$expected" ]; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        echo "  ✓ $message"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        echo "  ✗ $message"
        echo "    Expected: $expected"
        echo "    Actual:   $actual"
    fi
}

assert_true() {
    local condition="$1"
    local message="$2"

    TESTS_RUN=$((TESTS_RUN + 1))

    if [ "$condition" = "true" ] || [ "$condition" = "0" ]; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        echo "  ✓ $message"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        echo "  ✗ $message"
    fi
}

echo "Running Bash tests for Problem 23: Non-abundant Sums"
echo "====================================================="
echo

# Test sum_of_divisors function
echo "Testing sum_of_divisors()..."
assert_equal "$(sum_of_divisors 1)" "0" "sum_of_divisors(1) = 0"
assert_equal "$(sum_of_divisors 12)" "16" "sum_of_divisors(12) = 16 (1+2+3+4+6)"
assert_equal "$(sum_of_divisors 28)" "28" "sum_of_divisors(28) = 28 (perfect number)"
assert_equal "$(sum_of_divisors 6)" "6" "sum_of_divisors(6) = 6 (1+2+3)"
assert_equal "$(sum_of_divisors 10)" "8" "sum_of_divisors(10) = 8 (1+2+5)"
assert_equal "$(sum_of_divisors 15)" "9" "sum_of_divisors(15) = 9 (1+3+5)"

echo
echo "Testing is_abundant()..."
# 12 is the smallest abundant number
assert_equal "$(is_abundant 12 && echo 'yes' || echo 'no')" "yes" "12 is abundant"
assert_equal "$(is_abundant 1 && echo 'yes' || echo 'no')" "no" "1 is not abundant"
assert_equal "$(is_abundant 10 && echo 'yes' || echo 'no')" "no" "10 is not abundant (deficient)"
assert_equal "$(is_abundant 6 && echo 'yes' || echo 'no')" "no" "6 is not abundant (perfect)"
assert_equal "$(is_abundant 28 && echo 'yes' || echo 'no')" "no" "28 is not abundant (perfect)"

# More abundant numbers
assert_equal "$(is_abundant 18 && echo 'yes' || echo 'no')" "yes" "18 is abundant (1+2+3+6+9=21)"
assert_equal "$(is_abundant 20 && echo 'yes' || echo 'no')" "yes" "20 is abundant (1+2+4+5+10=22)"
assert_equal "$(is_abundant 24 && echo 'yes' || echo 'no')" "yes" "24 is abundant"

echo
echo "Testing find_abundant_numbers()..."
# Find abundant numbers up to 30
result=$(find_abundant_numbers 30)
# Should include 12, 18, 20, 24, 30
echo "$result" | grep -q "12" && has_12="yes" || has_12="no"
echo "$result" | grep -q "18" && has_18="yes" || has_18="no"
echo "$result" | grep -q "24" && has_24="yes" || has_24="no"

assert_equal "$has_12" "yes" "Abundant list includes 12"
assert_equal "$has_18" "yes" "Abundant list includes 18"
assert_equal "$has_24" "yes" "Abundant list includes 24"

# 11 should not be in the list
echo "$result" | grep -q "^11$" && has_11="yes" || has_11="no"
assert_equal "$has_11" "no" "Abundant list does not include 11"

echo
echo "Testing can_be_sum_of_two_abundant()..."
# 24 = 12 + 12 (smallest sum of two abundant numbers)
assert_equal "$(can_be_sum_of_two_abundant 24 "12 18 20 24" && echo 'yes' || echo 'no')" "yes" "24 can be sum of two abundant"
assert_equal "$(can_be_sum_of_two_abundant 30 "12 18 20 24" && echo 'yes' || echo 'no')" "yes" "30 can be sum (12+18)"
assert_equal "$(can_be_sum_of_two_abundant 23 "12 18 20" && echo 'yes' || echo 'no')" "no" "23 cannot be sum"
assert_equal "$(can_be_sum_of_two_abundant 1 "12 18" && echo 'yes' || echo 'no')" "no" "1 cannot be sum"

echo
echo "Testing integration - small example..."
# For numbers 1-30, we know 24 onwards can be sums
# Numbers that CANNOT be sums: 1-23 except those that can be formed
# This is complex, so just test that function runs without error
result=$(solve_problem 50 2>&1)
exit_code=$?
assert_equal "$exit_code" "0" "solve_problem(50) runs without error"

echo
echo "===================================================="
echo "Tests run:    $TESTS_RUN"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo

if [ $TESTS_FAILED -eq 0 ]; then
    echo "✓ All tests passed!"
    exit 0
else
    echo "✗ Some tests failed!"
    exit 1
fi
