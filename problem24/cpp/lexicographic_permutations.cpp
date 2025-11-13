// Problem 24: Lexicographic Permutations - Comparative Implementation
#include "lexicographic_permutations.h"
#include <algorithm>
#include <vector>

// Brute Force: Use STL next_permutation
// Time: O(k * n) where k=target position, n=string length
// Space: O(n)
std::string brute_force_permutation(const std::string& digits, int k) {
    std::string result = digits;
    for (int i = 1; i < k; ++i) {
        std::next_permutation(result.begin(), result.end());
    }
    return result;
}

// Mathematical: Factorial number system
// Time: O(n²) where n=string length
// Space: O(n)
std::string mathematical_permutation(const std::string& digits, int k) {
    std::vector<char> available(digits.begin(), digits.end());
    std::string result;
    int n = available.size();
    k--; // Convert to 0-indexed

    // Precompute factorials
    std::vector<long long> factorial(n);
    factorial[0] = 1;
    for (int i = 1; i < n; ++i) {
        factorial[i] = factorial[i-1] * i;
    }

    // Build permutation digit by digit
    for (int i = 0; i < n; ++i) {
        int index = k / factorial[n-1-i];
        result += available[index];
        available.erase(available.begin() + index);
        k %= factorial[n-1-i];
    }

    return result;
}
