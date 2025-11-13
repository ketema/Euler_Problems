// Benchmark comparison: Brute Force vs Mathematical Approach
#include <iostream>
#include <chrono>
#include <iomanip>
#include "lexicographic_permutations.h"

using namespace std::chrono;

double benchmark_brute_force(int iterations) {
    auto start = high_resolution_clock::now();

    for (int i = 0; i < iterations; ++i) {
        volatile std::string result = brute_force_permutation("0123456789", 1000000);
    }

    auto end = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(end - start);
    return duration.count() / 1000.0; // Convert to milliseconds
}

double benchmark_mathematical(int iterations) {
    auto start = high_resolution_clock::now();

    for (int i = 0; i < iterations; ++i) {
        volatile std::string result = mathematical_permutation("0123456789", 1000000);
    }

    auto end = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(end - start);
    return duration.count() / 1000.0; // Convert to milliseconds
}

int main() {
    const int ITERATIONS = 1000;

    std::cout << "╔════════════════════════════════════════════════════════════╗" << std::endl;
    std::cout << "║   Project Euler Problem 24: Algorithmic Comparison         ║" << std::endl;
    std::cout << "║   Lexicographic Permutations - Educational Case Study      ║" << std::endl;
    std::cout << "╚════════════════════════════════════════════════════════════╝" << std::endl;
    std::cout << std::endl;

    std::cout << "Problem: Find the 1,000,000th permutation of digits 0-9" << std::endl;
    std::cout << "Answer: " << mathematical_permutation("0123456789", 1000000) << std::endl;
    std::cout << std::endl;

    std::cout << "Running " << ITERATIONS << " iterations of each approach..." << std::endl;
    std::cout << std::endl;

    // Benchmark Brute Force
    std::cout << "⏱  Brute Force (std::next_permutation)..." << std::flush;
    double bf_time = benchmark_brute_force(ITERATIONS);
    double bf_avg = bf_time / ITERATIONS;
    std::cout << " Done!" << std::endl;

    // Benchmark Mathematical
    std::cout << "⏱  Mathematical (factorial number system)..." << std::flush;
    double math_time = benchmark_mathematical(ITERATIONS);
    double math_avg = math_time / ITERATIONS;
    std::cout << " Done!" << std::endl;

    std::cout << std::endl;
    std::cout << "╔════════════════════════════════════════════════════════════╗" << std::endl;
    std::cout << "║                      BENCHMARK RESULTS                     ║" << std::endl;
    std::cout << "╠════════════════════════════════════════════════════════════╣" << std::endl;
    std::cout << "║ Approach          │ Total (ms)  │ Avg (ms)   │ Speedup    ║" << std::endl;
    std::cout << "╠═══════════════════╪═════════════╪════════════╪════════════╣" << std::endl;

    std::cout << "║ Brute Force      │ "
              << std::setw(11) << std::fixed << std::setprecision(2) << bf_time
              << " │ "
              << std::setw(10) << std::fixed << std::setprecision(4) << bf_avg
              << " │ 1.00x      ║" << std::endl;

    std::cout << "║ Mathematical     │ "
              << std::setw(11) << std::fixed << std::setprecision(2) << math_time
              << " │ "
              << std::setw(10) << std::fixed << std::setprecision(4) << math_avg
              << " │ ";

    double speedup = bf_time / math_time;
    std::cout << std::setw(7) << std::fixed << std::setprecision(2) << speedup << "x ║" << std::endl;

    std::cout << "╚═══════════════════╧═════════════╧════════════╧════════════╝" << std::endl;
    std::cout << std::endl;

    std::cout << "📊 Analysis:" << std::endl;
    std::cout << "  • Brute Force: O(k·n) = O(1,000,000 × 10) = ~10M operations" << std::endl;
    std::cout << "  • Mathematical: O(n²) = O(10 × 10) = ~100 operations" << std::endl;
    std::cout << "  • Theoretical speedup: ~100,000x" << std::endl;
    std::cout << "  • Actual speedup: " << std::fixed << std::setprecision(0) << speedup << "x" << std::endl;
    std::cout << std::endl;

    if (speedup > 100) {
        std::cout << "🎓 Educational Insight:" << std::endl;
        std::cout << "  Mathematical approach is " << std::fixed << std::setprecision(0) << speedup
                  << "x faster!" << std::endl;
        std::cout << "  This demonstrates why algorithmic efficiency matters more" << std::endl;
        std::cout << "  than code brevity (~5 extra lines for " << std::fixed << std::setprecision(0)
                  << speedup << "x performance gain)." << std::endl;
    }

    return 0;
}
