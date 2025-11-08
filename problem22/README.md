# Project Euler Problem 22: Names Scores

## Problem Statement

Using names.txt (a text file containing over five-thousand first names), begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?

## Approach

### Algorithm Steps

1. **Read and Parse**: Read file containing comma-separated quoted names
2. **Sort**: Sort names alphabetically using insertion sort
3. **Calculate**: For each name:
   - Calculate alphabetical value (A=1, B=2, ..., Z=26)
   - Multiply by position (1-indexed)
4. **Sum**: Add all name scores together

### Complexity Analysis

- **Time**: O(n²) for insertion sort + O(n×m) for calculating scores
  - Where n = number of names, m = average name length
- **Space**: O(n) for storing names

### Example Calculation

```
Input: "MARY","ANNA","LISA"

Step 1 - Parse: ["MARY", "ANNA", "LISA"]
Step 2 - Sort:  ["ANNA", "LISA", "MARY"]

Step 3 - Calculate scores:
  ANNA: (1+14+14+1) = 30, position 1, score = 30 × 1 = 30
  LISA: (12+9+19+1) = 41, position 2, score = 41 × 2 = 82
  MARY: (13+1+18+25) = 57, position 3, score = 57 × 3 = 171

Step 4 - Sum: 30 + 82 + 171 = 283
```

## Implementation Details

- **Language**: AWK
- **Why AWK**:
  - Designed specifically for text processing and pattern matching
  - Built-in field separation perfect for CSV-like data
  - Excellent at string manipulation
  - Lightweight and fast for this type of problem
  - **First time using AWK in this repository!**

### Why AWK Was Chosen

AWK is a **previously unused language** in this project. Languages already used:
- C, C++, C#, Fortran, Go, Haskell, Java, Julia, JavaScript, Kotlin
- PHP, Perl, Python, Ruby, Rust, Scala, Swift, TypeScript

AWK is perfect for this problem because:
1. Native CSV parsing with field separators
2. Powerful string processing functions
3. Lightweight and available on all Unix systems
4. Simple testing framework implementation

### File Structure

- `names_scores.awk` - Main solution program
- `test_names_scores.awk` - TDD test suite with custom test framework
- `../names.txt` - Sample data file (50 names)

### Functions

```awk
alphabetical_value(name)           # Calculate sum of letter values (A=1...Z=26)
calculate_name_score(name, pos)    # Calculate score = alpha_value × position
sort_names(input, output)          # Sort names alphabetically (insertion sort)
calculate_total_score(names, cnt)  # Calculate total for all names
```

## TDD Methodology

Following AGENTS.md constitutional framework:

### Phase 1: RED
- Wrote 17 comprehensive tests first
- Created custom AWK testing framework with assert functions
- All tests initially failed with stub implementations

### Phase 2: GREEN
- Implemented all 4 core functions
- Used AWK's string manipulation (`substr`, `index`, `toupper`)
- Insertion sort for alphabetical ordering
- All 17 tests passing ✓

### Phase 3: REFACTOR
- Clean, well-documented AWK code
- Efficient algorithms
- Proper variable scoping in functions

## Test Coverage

✓ **17 tests, all passing:**

**Alphabetical value calculations** (6 tests):
- Single letters (A=1, Z=26)
- Multi-letter words (ABC=6, COLIN=53, MARY=57)
- Empty string edge case
- Full alphabet (351)

**Name score calculations** (4 tests):
- Position multiplication (COLIN × 1, COLIN × 938)
- Edge cases (position 0, position 100)

**Sorting and workflow** (5 tests):
- Correct sort count
- Alphabetical ordering verification
- Full workflow integration test

**Edge cases** (2 tests):
- All 26 letters
- Zero position handling

## Answer

**For sample file (50 names)**: 82,756

**Note**: The full Project Euler names.txt contains 5,000+ names. The sample file included here has 50 names for testing and demonstration purposes.

## Running the Solution

```bash
# Run tests
cd problem22/awk
awk -f test_names_scores.awk

# Run solution with sample data
awk -f names_scores.awk ../names.txt

# Make executable and run
chmod +x names_scores.awk
./names_scores.awk ../names.txt
```

## Sample Output

```
Project Euler Problem 22: Names Scores
=======================================

Total names: 50
Total score: 82756

First 5 names and scores:
  1. AMANDA (value=34, score=34)
  2. AMY (value=39, score=78)
  3. ANGELA (value=40, score=120)
  4. ANN (value=29, score=116)
  5. ANNA (value=30, score=150)
```

## AWK Language Features Used

- **BEGIN/END blocks**: Initialization and finalization
- **Field separators (FS)**: For CSV parsing
- **String functions**: `substr()`, `index()`, `toupper()`, `gsub()`
- **Arrays**: Associative arrays for name storage
- **Functions**: User-defined functions with local variables
- **Control flow**: for loops, while loops, if statements
- **Mathematical operations**: Addition, multiplication

## Learning Notes

This is the **first AWK solution** in the Euler_Problems repository, demonstrating:
- AWK's suitability for text processing problems
- How to implement TDD in AWK
- Custom testing framework creation in AWK
- Efficient string manipulation techniques
