#!/usr/bin/env lua
-- Test suite for Project Euler Problem 24: Lexicographic Permutations
-- Following TDD methodology - RED phase

-- Load the module
package.path = package.path .. ";./?.lua"
require("lexicographic_perm")

-- Simple test framework for Lua
local tests_run = 0
local tests_passed = 0
local tests_failed = 0

function assert_equal(actual, expected, message)
    tests_run = tests_run + 1
    if actual == expected then
        tests_passed = tests_passed + 1
        print(string.format("  ✓ %s", message))
    else
        tests_failed = tests_failed + 1
        print(string.format("  ✗ %s", message))
        print(string.format("    Expected: %s", tostring(expected)))
        print(string.format("    Actual:   %s", tostring(actual)))
    end
end

function assert_table_equal(actual, expected, message)
    tests_run = tests_run + 1
    local match = true

    if #actual ~= #expected then
        match = false
    else
        for i = 1, #actual do
            if actual[i] ~= expected[i] then
                match = false
                break
            end
        end
    end

    if match then
        tests_passed = tests_passed + 1
        print(string.format("  ✓ %s", message))
    else
        tests_failed = tests_failed + 1
        print(string.format("  ✗ %s", message))
        print(string.format("    Expected: %s", table.concat(expected, ", ")))
        print(string.format("    Actual:   %s", table.concat(actual, ", ")))
    end
end

print("Running Lua tests for Problem 24: Lexicographic Permutations")
print("=============================================================")
print()

-- Test factorial function
print("Testing factorial()...")
assert_equal(factorial(0), 1, "factorial(0) = 1")
assert_equal(factorial(1), 1, "factorial(1) = 1")
assert_equal(factorial(2), 2, "factorial(2) = 2")
assert_equal(factorial(3), 6, "factorial(3) = 6")
assert_equal(factorial(4), 24, "factorial(4) = 24")
assert_equal(factorial(5), 120, "factorial(5) = 120")
assert_equal(factorial(10), 3628800, "factorial(10) = 3,628,800")

print()
print("Testing nth_permutation()...")

-- Test with small example: permutations of {0, 1, 2}
-- Order: 012, 021, 102, 120, 201, 210
assert_table_equal(nth_permutation({0, 1, 2}, 1), {0, 1, 2}, "1st perm of {0,1,2} is 012")
assert_table_equal(nth_permutation({0, 1, 2}, 2), {0, 2, 1}, "2nd perm of {0,1,2} is 021")
assert_table_equal(nth_permutation({0, 1, 2}, 3), {1, 0, 2}, "3rd perm of {0,1,2} is 102")
assert_table_equal(nth_permutation({0, 1, 2}, 6), {2, 1, 0}, "6th perm of {0,1,2} is 210")

-- Test with {0, 1, 2, 3}
assert_table_equal(nth_permutation({0, 1, 2, 3}, 1), {0, 1, 2, 3}, "1st perm of {0,1,2,3}")
assert_table_equal(nth_permutation({0, 1, 2, 3}, 24), {3, 2, 1, 0}, "24th perm of {0,1,2,3}")

print()
print("Testing perm_to_string()...")
assert_equal(perm_to_string({0, 1, 2}), "012", "Convert {0,1,2} to '012'")
assert_equal(perm_to_string({2, 1, 0}), "210", "Convert {2,1,0} to '210'")
assert_equal(perm_to_string({0, 1, 2, 3, 4, 5, 6, 7, 8, 9}), "0123456789", "Convert full digits")

print()
print("Testing integration...")
-- The millionth permutation is a known answer
local digits = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
local result = nth_permutation(digits, 1000000)
local result_str = perm_to_string(result)

-- We can't know the answer without solving, but we can test properties
assert_equal(#result, 10, "Result has 10 digits")
assert_equal(type(result_str), "string", "Result converts to string")
assert_equal(#result_str, 10, "Result string has length 10")

print()
print("=============================================================")
print(string.format("Tests run:    %d", tests_run))
print(string.format("Tests passed: %d", tests_passed))
print(string.format("Tests failed: %d", tests_failed))
print()

if tests_failed == 0 then
    print("✓ All tests passed!")
    os.exit(0)
else
    print("✗ Some tests failed!")
    os.exit(1)
end
