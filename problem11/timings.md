# Problem 11: Execution Time Benchmarks

All solutions find the greatest product of four adjacent numbers in a 20x20 matrix.

**Expected Answer**: 70600674

**Methodology**: 
- 2 warmup runs (for JIT languages)
- 5 timed runs per implementation
- Nanosecond precision timing
- Statistical analysis (mean, median, std dev)

## Timing Results

| Language | Mean (ms) | Median (ms) | Std Dev (ms) | Min (ms) | Max (ms) | Status |
|----------|-----------|-------------|--------------|----------|----------|--------|
| C | 27.174 | 4.366 | 46.199 | 3.741 | 119.571 | ✅ Pass |
| C++ | 25.762 | 4.007 | 43.300 | 3.583 | 112.358 | ✅ Pass |
| C# | 250.683 | 251.730 | 2.367 | 246.813 | 253.036 | ✅ Pass |
| Fortran | 27.093 | 5.365 | 43.896 | 4.802 | 114.885 | ✅ Pass |
| Go | 32.261 | 6.053 | 53.306 | 4.728 | 138.866 | ✅ Pass |
| Haskell | 91.786 | 60.607 | 62.970 | 59.192 | 217.722 | ✅ Pass |
| Java | 54.428 | 54.509 | .928 | 52.822 | 55.727 | ✅ Pass |
| Julia | 384.896 | 384.770 | 4.929 | 379.223 | 391.056 | ✅ Pass |
| Kotlin | 636.430 | 775.940 | 196.574 | 394.948 | 822.185 | ✅ Pass |
| Perl | 16.949 | 16.196 | 1.601 | 15.249 | 19.798 | ✅ Pass |
| PHP | 39.360 | 39.114 | 1.063 | 38.123 | 41.260 | ✅ Pass |
| Python | 686.516 | 668.200 | 36.967 | 666.887 | 760.440 | ✅ Pass |
| Ruby | 44.668 | 44.790 | .707 | 43.444 | 45.651 | ✅ Pass |
| Rust | 31.058 | 4.603 | 53.309 | 3.910 | 137.674 | ✅ Pass |
| Scala | 1713.779 | 1717.481 | 16.143 | 1683.370 | 1731.583 | ✅ Pass |
| Swift | 30.444 | 7.056 | 47.228 | 6.205 | 124.899 | ✅ Pass |
| TypeScript | 28.177 | 28.006 | .309 | 27.934 | 28.769 | ✅ Pass |

## Performance Analysis

### 🏆 Fastest Languages (by Median Time)

1. **C++**: 4.007ms - Compiled, optimized, minimal overhead
2. **C**: 4.366ms - Compiled, direct memory access
3. **Rust**: 4.603ms - Compiled, zero-cost abstractions
4. **Fortran**: 5.365ms - Compiled, numerical computing optimized
5. **Go**: 6.053ms - Compiled, garbage collected
6. **Swift**: 7.056ms - Compiled, modern optimizations

### ⚡ Fast Interpreted Languages

7. **Perl**: 16.196ms - Interpreted, mature runtime
8. **TypeScript**: 28.006ms - JIT compiled via Node.js V8
9. **PHP**: 39.114ms - Interpreted, JIT compilation (PHP 8.4)
10. **Ruby**: 44.790ms - Interpreted, optimized runtime

### 🔄 JIT-Compiled Languages

11. **Java**: 54.509ms - JIT compiled, JVM overhead
12. **Haskell**: 60.607ms - Compiled, lazy evaluation

### 🐌 Slower Languages

13. **C#**: 251.730ms - JIT compiled, .NET runtime
14. **Julia**: 384.770ms - JIT compiled, first-run compilation
15. **Python**: 668.200ms - Interpreted, dynamic typing
16. **Kotlin**: 775.940ms - JIT compiled, JVM + Gradle overhead
17. **Scala**: 1717.481ms - JIT compiled, JVM + SBT overhead

### 📊 Key Observations

- **Compiled languages dominate**: C, C++, Rust, Fortran, Go, Swift all under 8ms median
- **First run penalty**: All compiled languages show ~100-140ms first run (cold start)
- **Perl is surprisingly fast**: 16ms median, faster than TypeScript and PHP
- **TypeScript performs well**: 28ms median, V8 JIT optimization effective
- **JIT warmup helps**: Java, C#, Kotlin show consistent times after warmup
- **Build tool overhead**: Kotlin and Scala suffer from Gradle/SBT startup costs
- **Julia slower than expected**: JIT compilation overhead even with warmup

### 📈 Statistical Notes

- **High std dev for compiled languages**: Due to cold start on first run (~100-140ms)
- **Low std dev for JIT languages**: Consistent performance after warmup
- **Median more representative**: Better metric than mean for compiled languages
- **TypeScript very consistent**: 0.309ms std dev, excellent V8 optimization

### ✅ All 16 Languages Pass

All implementations successfully benchmarked with correct output validation (70600674)

## Timing Precision Note

**Nanosecond precision** is the highest available with standard Unix tools (`date +%s%N`).
**Picosecond precision** is not available in standard timing tools - would require specialized hardware performance counters.
