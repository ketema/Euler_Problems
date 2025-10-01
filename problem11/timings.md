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
| C | 31.696 | 4.547 | 54.746 | 3.783 | 141.188 | ✅ Pass |
| C++ | 25.517 | 4.481 | 42.839 | 3.638 | 111.192 | ✅ Pass |
| C# | 248.422 | 250.581 | 5.876 | 239.870 | 256.408 | ✅ Pass |
| Fortran | - | - | - | - | - | ❌ Compile Fail |
| Go | 14.230 | 4.775 | 19.202 | 4.364 | 52.634 | ✅ Pass |
| Haskell | 93.864 | 60.589 | 68.611 | 56.588 | 231.054 | ✅ Pass |
| Java | 53.969 | 53.489 | 1.089 | 52.613 | 55.796 | ✅ Pass |
| Julia | - | - | - | - | - | ❌ Fail |
| Kotlin | 801.872 | 802.096 | 10.333 | 789.040 | 820.049 | ✅ Pass |
| Perl | - | - | - | - | - | ❌ Fail |
| PHP | 40.187 | 39.246 | 2.446 | 38.044 | 44.961 | ✅ Pass |
| Python | 672.405 | 669.478 | 5.608 | 668.529 | 683.457 | ✅ Pass |
| Ruby | - | - | - | - | - | ❌ Fail |
| Rust | 30.509 | 4.406 | 52.130 | 4.198 | 134.770 | ✅ Pass |
| Scala | 1697.931 | 1700.001 | 18.025 | 1679.517 | 1728.366 | ✅ Pass |
| Swift | 30.047 | 6.320 | 47.637 | 5.548 | 125.319 | ✅ Pass |
| TypeScript | - | - | - | - | - | ❌ Compile Fail |

## Performance Analysis

### Fastest Languages (by Median Time)

1. **C++**: 4.481ms - Compiled, optimized, minimal overhead
2. **Rust**: 4.406ms - Compiled, zero-cost abstractions
3. **C**: 4.547ms - Compiled, direct memory access
4. **Go**: 4.775ms - Compiled, garbage collected
5. **Swift**: 6.320ms - Compiled, modern optimizations

### Mid-Range Performance

6. **PHP**: 39.246ms - Interpreted, JIT compilation
7. **Java**: 53.489ms - JIT compiled, JVM overhead
8. **Haskell**: 60.589ms - Compiled, lazy evaluation

### Slower Languages

9. **C#**: 250.581ms - JIT compiled, .NET runtime
10. **Python**: 669.478ms - Interpreted, dynamic typing
11. **Kotlin**: 802.096ms - JIT compiled, JVM + Gradle overhead
12. **Scala**: 1700.001ms - JIT compiled, JVM + SBT overhead

### Key Observations

- **Compiled languages dominate**: C, C++, Rust, Go, Swift all under 7ms median
- **First run penalty**: All compiled languages show ~100-140ms first run (cold start)
- **JIT warmup helps**: Java, C#, Kotlin show consistent times after warmup
- **Build tool overhead**: Kotlin and Scala suffer from Gradle/SBT startup costs
- **Interpreted languages**: PHP surprisingly fast (~40ms), Python slower (~670ms)

### Statistical Notes

- High standard deviations for compiled languages due to cold start on first run
- Low standard deviations for JIT languages after warmup (consistent performance)
- Median is more representative than mean for compiled languages

### Failed Benchmarks

- **Fortran**: Compilation failed (needs investigation)
- **Julia**: Output validation failed (needs investigation)
- **Perl**: Output validation failed (needs investigation)
- **Ruby**: Output validation failed (needs investigation)
- **TypeScript**: Compilation failed (needs investigation)

## Timing Precision Note

**Nanosecond precision** is the highest available with standard Unix tools (`date +%s%N`).
**Picosecond precision** is not available in standard timing tools - would require specialized hardware performance counters.
