// Test for Problem 24: Lexicographic Permutations
#include <iostream>
#include <string>
#include <cassert>
#include "lexicographic_permutations.h"

void test_brute_force_small() {
    // 6th permutation of "012" should be "210"
    std::string result = brute_force_permutation("012", 6);
    assert(result == "210");
    std::cout << "✓ Brute force small test passed: 6th perm of '012' = " << result << std::endl;
}

void test_mathematical_small() {
    // 6th permutation of "012" should be "210"
    std::string result = mathematical_permutation("012", 6);
    assert(result == "210");
    std::cout << "✓ Mathematical small test passed: 6th perm of '012' = " << result << std::endl;
}

void test_brute_force_large() {
    // 1,000,000th permutation of "0123456789"
    std::string result = brute_force_permutation("0123456789", 1000000);
    assert(result.length() == 10);
    std::cout << "✓ Brute force large test passed: " << result << std::endl;
}

void test_mathematical_large() {
    // 1,000,000th permutation of "0123456789"
    std::string result = mathematical_permutation("0123456789", 1000000);
    assert(result.length() == 10);
    std::cout << "✓ Mathematical large test passed: " << result << std::endl;
}

void test_both_agree() {
    // Both methods should produce identical results
    std::string bf = brute_force_permutation("0123456789", 1000000);
    std::string math = mathematical_permutation("0123456789", 1000000);
    assert(bf == math);
    std::cout << "✓ Both methods agree: " << bf << std::endl;
}

int main() {
    std::cout << "Running tests for Problem 24..." << std::endl;

    test_brute_force_small();
    test_mathematical_small();
    test_brute_force_large();
    test_mathematical_large();
    test_both_agree();

    std::cout << "\nAll tests passed!" << std::endl;
    return 0;
}
