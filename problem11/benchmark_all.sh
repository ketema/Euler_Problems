#!/bin/bash
# Benchmark all Problem 11 solutions
# Records execution times with nanosecond precision

RESULTS_FILE="timings.md"
PROBLEM_DIR="/Users/ketema/projects/Euler_Problems/problem11"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=== Problem 11: Benchmarking All Solutions ==="
echo ""

# Initialize results file
cat > "$RESULTS_FILE" << 'EOF'
# Problem 11: Execution Time Benchmarks

All solutions find the greatest product of four adjacent numbers in a 20x20 matrix.

**Expected Answer**: 70600674

## Timing Results

| Language | Time (seconds) | Time (nanoseconds) | Status |
|----------|----------------|-------------------|--------|
EOF

# Function to time a command with nanosecond precision
time_command() {
    local lang=$1
    local cmd=$2
    local dir=$3
    
    echo -e "${YELLOW}Timing $lang...${NC}"
    
    cd "$PROBLEM_DIR/$dir"
    
    # Use bash built-in time with TIMEFORMAT for precision
    local start=$(date +%s%N)
    if eval "$cmd" > /dev/null 2>&1; then
        local end=$(date +%s%N)
        local elapsed_ns=$((end - start))
        local elapsed_s=$(echo "scale=9; $elapsed_ns / 1000000000" | bc)
        
        echo -e "${GREEN}✓ $lang: ${elapsed_s}s (${elapsed_ns}ns)${NC}"
        echo "| $lang | $elapsed_s | $elapsed_ns | ✅ Pass |" >> "$PROBLEM_DIR/$RESULTS_FILE"
        return 0
    else
        echo -e "${RED}✗ $lang: Failed${NC}"
        echo "| $lang | - | - | ❌ Fail |" >> "$PROBLEM_DIR/$RESULTS_FILE"
        return 1
    fi
}

# Compile and time C
echo -e "${YELLOW}=== C ===${NC}"
cd "$PROBLEM_DIR/c"
make clean > /dev/null 2>&1 || true
make > /dev/null 2>&1
time_command "C" "./matrix_product matrix.txt" "c"

# Compile and time C++
echo -e "${YELLOW}=== C++ ===${NC}"
cd "$PROBLEM_DIR/cpp"
make clean > /dev/null 2>&1 || true
make > /dev/null 2>&1
time_command "C++" "./matrix_product matrix.txt" "cpp"

# Compile and time C#
echo -e "${YELLOW}=== C# ===${NC}"
cd "$PROBLEM_DIR/csharp/MatrixProductApp"
dotnet build -c Release > /dev/null 2>&1
time_command "C#" "dotnet run -c Release --no-build matrix.txt" "csharp/MatrixProductApp"

# Compile and time Fortran
echo -e "${YELLOW}=== Fortran ===${NC}"
cd "$PROBLEM_DIR/fortran"
make clean > /dev/null 2>&1 || true
make > /dev/null 2>&1
time_command "Fortran" "./matrix_product matrix.txt" "fortran"

# Compile and time Go
echo -e "${YELLOW}=== Go ===${NC}"
cd "$PROBLEM_DIR/go"
go build -o matrix_product . > /dev/null 2>&1
time_command "Go" "./matrix_product matrix.txt" "go"

# Compile and time Haskell
echo -e "${YELLOW}=== Haskell ===${NC}"
cd "$PROBLEM_DIR/haskell"
cabal clean > /dev/null 2>&1 || true
cabal build > /dev/null 2>&1
time_command "Haskell" "cabal run MatrixProduct matrix.txt" "haskell"

# Compile and time Java
echo -e "${YELLOW}=== Java ===${NC}"
cd "$PROBLEM_DIR/java"
javac MatrixProduct.java > /dev/null 2>&1
time_command "Java" "java MatrixProduct matrix.txt" "java"

# Time Julia (interpreted but JIT compiled)
echo -e "${YELLOW}=== Julia ===${NC}"
time_command "Julia" "julia matrix_product.jl matrix.txt" "julia"

# Compile and time Kotlin
echo -e "${YELLOW}=== Kotlin ===${NC}"
cd "$PROBLEM_DIR/kotlin"
export JAVA_HOME=/opt/homebrew/opt/openjdk@21
gradle build > /dev/null 2>&1
time_command "Kotlin" "gradle -q run --args='matrix.txt'" "kotlin"

# Time Perl (interpreted)
echo -e "${YELLOW}=== Perl ===${NC}"
time_command "Perl" "perl matrix_product.pl matrix.txt" "perl"

# Time PHP (interpreted)
echo -e "${YELLOW}=== PHP ===${NC}"
time_command "PHP" "php solution.php matrix.txt" "php"

# Time Python (interpreted)
echo -e "${YELLOW}=== Python ===${NC}"
cd "$PROBLEM_DIR/python"
time_command "Python" "poetry run python matrix_product.py matrix.txt" "python"

# Time Ruby (interpreted)
echo -e "${YELLOW}=== Ruby ===${NC}"
time_command "Ruby" "ruby matrix_product.rb matrix.txt" "ruby"

# Compile and time Rust
echo -e "${YELLOW}=== Rust ===${NC}"
cd "$PROBLEM_DIR/rust"
cargo build --release > /dev/null 2>&1
time_command "Rust" "./target/release/matrix_product matrix.txt" "rust"

# Compile and time Scala
echo -e "${YELLOW}=== Scala ===${NC}"
cd "$PROBLEM_DIR/scala"
sbt compile > /dev/null 2>&1
time_command "Scala" "sbt -Dsbt.log.noformat=true 'run matrix.txt' 2>/dev/null | tail -1" "scala"

# Compile and time Swift
echo -e "${YELLOW}=== Swift ===${NC}"
cd "$PROBLEM_DIR/swift"
xcrun swiftc -O MatrixProduct.swift main.swift -o matrix_product > /dev/null 2>&1
time_command "Swift" "./matrix_product matrix.txt" "swift"

# Time TypeScript (via Node.js)
echo -e "${YELLOW}=== TypeScript ===${NC}"
cd "$PROBLEM_DIR/typescript"
npm run build > /dev/null 2>&1
time_command "TypeScript" "node dist/matrix_product.js matrix.txt" "typescript"

echo ""
echo -e "${GREEN}=== Benchmarking Complete ===${NC}"
echo "Results saved to: $RESULTS_FILE"
echo ""
echo "Summary:"
cat "$PROBLEM_DIR/$RESULTS_FILE"

