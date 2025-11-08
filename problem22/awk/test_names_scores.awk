#!/usr/bin/awk -f
# Test suite for Project Euler Problem 22: Names Scores
# Following TDD methodology - RED phase

BEGIN {
    # Simple test framework for AWK
    tests_run = 0
    tests_passed = 0
    tests_failed = 0

    print "Running AWK tests for Problem 22: Names Scores"
    print "================================================\n"

    # Test alphabetical_value function
    print "Testing alphabetical_value()..."
    assert_equal(alphabetical_value("A"), 1, "A should equal 1")
    assert_equal(alphabetical_value("Z"), 26, "Z should equal 26")
    assert_equal(alphabetical_value("ABC"), 6, "ABC should equal 6")
    assert_equal(alphabetical_value("COLIN"), 53, "COLIN should equal 53")
    assert_equal(alphabetical_value("MARY"), 57, "MARY should equal 57")
    assert_equal(alphabetical_value(""), 0, "Empty string should equal 0")

    # Test calculate_name_score function
    print "\nTesting calculate_name_score()..."
    assert_equal(calculate_name_score("COLIN", 1), 53, "COLIN at position 1 = 53")
    assert_equal(calculate_name_score("COLIN", 938), 49714, "COLIN at position 938 = 49714")
    assert_equal(calculate_name_score("A", 1), 1, "A at position 1 = 1")
    assert_equal(calculate_name_score("A", 100), 100, "A at position 100 = 100")

    # Test sorting and workflow
    print "\nTesting full workflow..."
    # Test with simple array
    names[1] = "MARY"
    names[2] = "ANNA"
    names[3] = "LISA"

    # After sorting: ANNA, LISA, MARY
    # ANNA = 30, pos 1, score = 30
    # LISA = 41, pos 2, score = 82
    # MARY = 57, pos 3, score = 171
    # Total = 283

    sorted_count = sort_names(names, sorted_names)
    assert_equal(sorted_count, 3, "Should have 3 sorted names")
    assert_equal(sorted_names[1], "ANNA", "First sorted name should be ANNA")
    assert_equal(sorted_names[2], "LISA", "Second sorted name should be LISA")
    assert_equal(sorted_names[3], "MARY", "Third sorted name should be MARY")

    total = calculate_total_score(sorted_names, sorted_count)
    assert_equal(total, 283, "Total score for MARY,ANNA,LISA should be 283")

    # Test edge cases
    print "\nTesting edge cases..."
    assert_equal(alphabetical_value("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), 351, "All letters = 351")
    assert_equal(calculate_name_score("A", 0), 0, "Position 0 gives score 0")

    # Print summary
    print "\n================================================"
    print "Tests run:    " tests_run
    print "Tests passed: " tests_passed
    print "Tests failed: " tests_failed

    if (tests_failed == 0) {
        print "\n✓ All tests passed!"
        exit 0
    } else {
        print "\n✗ Some tests failed!"
        exit 1
    }
}

# Test helper function
function assert_equal(actual, expected, message) {
    tests_run++
    if (actual == expected) {
        tests_passed++
        printf "  ✓ %s\n", message
    } else {
        tests_failed++
        printf "  ✗ %s\n", message
        printf "    Expected: %s\n", expected
        printf "    Actual:   %s\n", actual
    }
}

# Actual implementations (GREEN phase)

# Calculate alphabetical value of a name
function alphabetical_value(name,    i, char, value, sum) {
    sum = 0
    for (i = 1; i <= length(name); i++) {
        char = substr(name, i, 1)
        value = (index("ABCDEFGHIJKLMNOPQRSTUVWXYZ", toupper(char)))
        if (value > 0) {
            sum += value
        }
    }
    return sum
}

# Calculate score for a single name at given position
function calculate_name_score(name, position) {
    return alphabetical_value(name) * position
}

# Sort names alphabetically using insertion sort
function sort_names(input_array, output_array,    i, j, temp, count) {
    count = 0
    for (i in input_array) {
        output_array[++count] = input_array[i]
    }

    # Insertion sort
    for (i = 2; i <= count; i++) {
        temp = output_array[i]
        j = i - 1
        while (j > 0 && output_array[j] > temp) {
            output_array[j + 1] = output_array[j]
            j--
        }
        output_array[j + 1] = temp
    }

    return count
}

# Calculate total score for all names
function calculate_total_score(names_array, count,    i, total) {
    total = 0
    for (i = 1; i <= count; i++) {
        total += calculate_name_score(names_array[i], i)
    }
    return total
}
