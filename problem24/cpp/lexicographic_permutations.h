#ifndef LEXICOGRAPHIC_PERMUTATIONS_H
#define LEXICOGRAPHIC_PERMUTATIONS_H

#include <string>

// Brute force approach using std::next_permutation
std::string brute_force_permutation(const std::string& digits, int k);

// Mathematical approach using factorial number system
std::string mathematical_permutation(const std::string& digits, int k);

#endif
