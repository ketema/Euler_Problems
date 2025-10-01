#!/bin/bash
# Benchmark all Problem 11 solutions with statistical rigor
# Records execution times with nanosecond precision and multiple runs

set -u  # Treat unset variables as errors

# Configuration
RESULTS_FILE="timings.md"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROBLEM_DIR="$SCRIPT_DIR"
EXPECTED_ANSWER="70600674"
NUM_RUNS=5
NUM_WARMUP=2

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "=== Problem 11: Benchmarking All Solutions ==="
echo "Configuration: $NUM_WARMUP warmup runs + $NUM_RUNS timed runs per language"
echo ""

# Initialize results file
cat > "$RESULTS_FILE" << 'EOF'
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
EOF

# Function to calculate statistics
calculate_stats() {
    local times_str="$1"
    IFS=',' read -ra times <<< "$times_str"
    local count=${#times[@]}

    # Sort times
    local sorted=($(printf '%s\n' "${times[@]}" | sort -n))

    # Calculate mean
    local sum=0
    for t in "${times[@]}"; do
        sum=$((sum + t))
    done
    local mean=$((sum / count))

    # Calculate median
    local median
    if (( count % 2 == 0 )); then
        local mid=$((count / 2))
        local mid_prev=$((mid - 1))
        median=$(( (sorted[mid_prev] + sorted[mid]) / 2 ))
    else
        median=${sorted[$((count / 2))]}
    fi

    # Calculate standard deviation
    local variance=0
    for t in "${times[@]}"; do
        local diff=$((t - mean))
        variance=$((variance + diff * diff))
    done
    variance=$((variance / count))
    local stddev=$(echo "scale=0; sqrt($variance)" | bc)

    # Min and max
    local min=${sorted[0]}
    local max_idx=$((count - 1))
    local max=${sorted[$max_idx]}

    # Convert to milliseconds with precision
    local mean_ms=$(echo "scale=3; $mean / 1000000" | bc)
    local median_ms=$(echo "scale=3; $median / 1000000" | bc)
    local stddev_ms=$(echo "scale=3; $stddev / 1000000" | bc)
    local min_ms=$(echo "scale=3; $min / 1000000" | bc)
    local max_ms=$(echo "scale=3; $max / 1000000" | bc)

    echo "$mean_ms|$median_ms|$stddev_ms|$min_ms|$max_ms"
}

# Function to time a command with multiple runs and validation
time_command() {
    local lang=$1
    local cmd=$2
    local dir=$3
    local needs_warmup=$4
    
    echo -e "${YELLOW}Benchmarking $lang...${NC}"
    
    cd "$PROBLEM_DIR/$dir" || return 1
    
    # Warmup runs for JIT languages
    if [[ "$needs_warmup" == "true" ]]; then
        echo -e "${BLUE}  Running $NUM_WARMUP warmup iterations...${NC}"
        for ((i=1; i<=NUM_WARMUP; i++)); do
            eval "$cmd" > /dev/null 2>&1 || true
        done
    fi
    
    # Timed runs
    local times_csv=""
    local all_passed=true

    echo -e "${BLUE}  Running $NUM_RUNS timed iterations...${NC}"
    for ((i=1; i<=NUM_RUNS; i++)); do
        local start=$(date +%s%N)
        local output=$(eval "$cmd" 2>&1)
        local end=$(date +%s%N)
        local elapsed_ns=$((end - start))

        # Validate output - check if it contains the expected answer
        if echo "$output" | grep -q "$EXPECTED_ANSWER"; then
            if [[ -z "$times_csv" ]]; then
                times_csv="$elapsed_ns"
            else
                times_csv="$times_csv,$elapsed_ns"
            fi
            echo -e "${GREEN}    Run $i: $(echo "scale=3; $elapsed_ns / 1000000" | bc)ms ✓${NC}"
        else
            echo -e "${RED}    Run $i: Output validation failed (expected $EXPECTED_ANSWER)${NC}"
            echo "    Output: $(echo "$output" | head -1)"
            all_passed=false
            break
        fi
    done

    if [[ "$all_passed" == "true" ]] && [[ -n "$times_csv" ]]; then
        local stats=$(calculate_stats "$times_csv")
        IFS='|' read -r mean median stddev min max <<< "$stats"

        echo -e "${GREEN}✓ $lang: Mean=${mean}ms, Median=${median}ms, StdDev=${stddev}ms${NC}"
        echo "| $lang | $mean | $median | $stddev | $min | $max | ✅ Pass |" >> "$PROBLEM_DIR/$RESULTS_FILE"
        return 0
    else
        echo -e "${RED}✗ $lang: Failed${NC}"
        echo "| $lang | - | - | - | - | - | ❌ Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
        return 1
    fi
}

# Compile and benchmark C
echo -e "${YELLOW}=== C ===${NC}"
cd "$PROBLEM_DIR/c"
if make clean > /dev/null 2>&1 && make > /dev/null 2>&1; then
    time_command "C" "./matrix_product matrix.txt" "c" "false"
else
    echo -e "${RED}✗ C: Compilation failed${NC}"
    echo "| C | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Compile and benchmark C++
echo -e "${YELLOW}=== C++ ===${NC}"
cd "$PROBLEM_DIR/cpp"
if make clean > /dev/null 2>&1 && make > /dev/null 2>&1; then
    time_command "C++" "./matrix_product matrix.txt" "cpp" "false"
else
    echo -e "${RED}✗ C++: Compilation failed${NC}"
    echo "| C++ | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Compile and benchmark C#
echo -e "${YELLOW}=== C# ===${NC}"
cd "$PROBLEM_DIR/csharp/MatrixProductApp"
if dotnet build -c Release > /dev/null 2>&1; then
    time_command "C#" "dotnet run -c Release --no-build matrix.txt" "csharp/MatrixProductApp" "true"
else
    echo -e "${RED}✗ C#: Compilation failed${NC}"
    echo "| C# | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Compile and benchmark Fortran
echo -e "${YELLOW}=== Fortran ===${NC}"
cd "$PROBLEM_DIR/fortran"
if make clean > /dev/null 2>&1 && make > /dev/null 2>&1; then
    time_command "Fortran" "./matrix_product matrix.txt" "fortran" "false"
else
    echo -e "${RED}✗ Fortran: Compilation failed${NC}"
    echo "| Fortran | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Compile and benchmark Go
echo -e "${YELLOW}=== Go ===${NC}"
cd "$PROBLEM_DIR/go"
if go build -o matrix_product . > /dev/null 2>&1; then
    time_command "Go" "./matrix_product matrix.txt" "go" "false"
else
    echo -e "${RED}✗ Go: Compilation failed${NC}"
    echo "| Go | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Compile and benchmark Haskell
echo -e "${YELLOW}=== Haskell ===${NC}"
cd "$PROBLEM_DIR/haskell"
if cabal clean > /dev/null 2>&1 && cabal build > /dev/null 2>&1; then
    time_command "Haskell" "cabal run MatrixProduct matrix.txt" "haskell" "false"
else
    echo -e "${RED}✗ Haskell: Compilation failed${NC}"
    echo "| Haskell | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Compile and benchmark Java
echo -e "${YELLOW}=== Java ===${NC}"
cd "$PROBLEM_DIR/java"
if javac MatrixProduct.java > /dev/null 2>&1; then
    time_command "Java" "java MatrixProduct matrix.txt" "java" "true"
else
    echo -e "${RED}✗ Java: Compilation failed${NC}"
    echo "| Java | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Benchmark Julia (JIT compiled)
echo -e "${YELLOW}=== Julia ===${NC}"
time_command "Julia" "julia matrix_product.jl matrix.txt" "julia" "true"

# Compile and benchmark Kotlin
echo -e "${YELLOW}=== Kotlin ===${NC}"
cd "$PROBLEM_DIR/kotlin"
export JAVA_HOME=/opt/homebrew/opt/openjdk@21
if gradle build > /dev/null 2>&1; then
    time_command "Kotlin" "gradle -q run --args='matrix.txt'" "kotlin" "true"
else
    echo -e "${RED}✗ Kotlin: Compilation failed${NC}"
    echo "| Kotlin | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Benchmark Perl (interpreted)
echo -e "${YELLOW}=== Perl ===${NC}"
time_command "Perl" "perl matrix_product.pl matrix.txt" "perl" "false"

# Benchmark PHP (interpreted)
echo -e "${YELLOW}=== PHP ===${NC}"
time_command "PHP" "/opt/homebrew/bin/php solution.php matrix.txt" "php" "false"

# Benchmark Python (interpreted)
echo -e "${YELLOW}=== Python ===${NC}"
cd "$PROBLEM_DIR/python"
time_command "Python" "poetry run python matrix_product.py matrix.txt" "python" "false"

# Benchmark Ruby (interpreted)
echo -e "${YELLOW}=== Ruby ===${NC}"
time_command "Ruby" "ruby matrix_product.rb matrix.txt" "ruby" "false"

# Compile and benchmark Rust
echo -e "${YELLOW}=== Rust ===${NC}"
cd "$PROBLEM_DIR/rust"
if cargo build --release > /dev/null 2>&1; then
    time_command "Rust" "./target/release/rust matrix.txt" "rust" "false"
else
    echo -e "${RED}✗ Rust: Compilation failed${NC}"
    echo "| Rust | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Compile and benchmark Scala
echo -e "${YELLOW}=== Scala ===${NC}"
cd "$PROBLEM_DIR/scala"
if sbt compile > /dev/null 2>&1; then
    time_command "Scala" "sbt -Dsbt.log.noformat=true 'run matrix.txt' 2>&1" "scala" "true"
else
    echo -e "${RED}✗ Scala: Compilation failed${NC}"
    echo "| Scala | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Compile and benchmark Swift
echo -e "${YELLOW}=== Swift ===${NC}"
cd "$PROBLEM_DIR/swift"
if xcrun swiftc -O MatrixProduct.swift main.swift -o matrix_product > /dev/null 2>&1; then
    time_command "Swift" "./matrix_product matrix.txt" "swift" "false"
else
    echo -e "${RED}✗ Swift: Compilation failed${NC}"
    echo "| Swift | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

# Benchmark TypeScript (via Node.js)
echo -e "${YELLOW}=== TypeScript ===${NC}"
cd "$PROBLEM_DIR/typescript"
if npm run build > /dev/null 2>&1; then
    time_command "TypeScript" "node dist/matrix_product.js matrix.txt" "typescript" "true"
else
    echo -e "${RED}✗ TypeScript: Compilation failed${NC}"
    echo "| TypeScript | - | - | - | - | - | ❌ Compile Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
fi

echo ""
echo -e "${GREEN}=== Benchmarking Complete ===${NC}"
echo "Results saved to: $RESULTS_FILE"
echo ""

