#!/bin/bash
# Benchmark all Problem 13 solutions
# Records execution times with nanosecond precision

RESULTS_FILE="timings.md"
PROBLEM_DIR="/Users/ketema/projects/Euler_Problems/problem13"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=== Problem 13: Benchmarking All Solutions ==="
echo ""

# Initialize results file
cat > "$RESULTS_FILE" << 'EOF'
# Problem 13: Execution Time Benchmarks

All solutions find the first ten digits of the sum of one hundred 50-digit numbers.

**Expected Answer**: 5537376230

## Timing Results

| Language | Time (seconds) | Time (nanoseconds) | Status |
|----------|----------------|-------------------|--------|
EOF

# Function to time a command and record results
time_command() {
    local language=$1
    local command=$2
    local directory=$3
    
    echo -e "${YELLOW}=== $language ===${NC}"
    cd "$PROBLEM_DIR/$directory"
    
    # Run command 5 times and take the best time
    local best_time=999999999
    local best_ns=999999999999
    local output=""
    local status="FAILED"
    
    for i in {1..5}; do
        local start_time=$(date +%s%N)
        local result=$(eval "$command" 2>&1)
        local exit_code=$?
        local end_time=$(date +%s%N)
        local elapsed_ns=$((end_time - start_time))
        local elapsed_s=$(echo "scale=6; $elapsed_ns / 1000000000" | bc -l)
        
        if [ $exit_code -eq 0 ]; then
            status="PASSED"
            output=$(echo "$result" | tail -1)
            
            local real_time=$elapsed_s
            local time_ns=$elapsed_ns
            
            if (( $(echo "$real_time < $best_time" | bc -l) )); then
                best_time=$real_time
                best_ns=$time_ns
            fi
        fi
    done
    
    # Verify output contains expected answer
    if [[ "$output" == *"5537376230"* ]]; then
        status="PASSED"
    else
        status="FAILED"
        echo -e "${RED}Expected answer not found in output: $output${NC}"
    fi
    
    echo "Best time: ${best_time}s (${best_ns}ns)"
    echo "Output: $output"
    echo "Status: $status"
    echo ""
    
    # Append to results file
    printf "| %-8s | %-14s | %-17s | %-6s |\n" "$language" "$best_time" "$best_ns" "$status" >> "$PROBLEM_DIR/$RESULTS_FILE"
}

# Benchmark Rust (compiled, optimized)
echo -e "${YELLOW}=== Rust (Release) ===${NC}"
cd "$PROBLEM_DIR/rust"
cargo build --release > /dev/null 2>&1
time_command "Rust" "./target/release/large_sum" "rust"

# Benchmark Python (interpreted)
echo -e "${YELLOW}=== Python ===${NC}"
cd "$PROBLEM_DIR/python"
time_command "Python" "python3 large_sum.py" "python"

# Benchmark Perl (interpreted)
echo -e "${YELLOW}=== Perl ===${NC}"
cd "$PROBLEM_DIR/perl"
time_command "Perl" "perl large_sum_cli.pl" "perl"

echo "=== Benchmarking Complete ==="
echo "Results saved to: $RESULTS_FILE"
