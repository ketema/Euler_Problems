# Project Euler Problem 22: Names Scores

## Problem Statement

Using names.txt (a 46KB text file containing over five-thousand first names), begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

**Question**: What is the total of all the name scores in the file?

## Approach

1. **Sort names alphabetically**: Convert comma-delimited single line to newline-separated names, sort, then process
2. **Calculate alphabetical value**: Sum position of each letter (A=1, B=2, ..., Z=26)
3. **Calculate name score**: Multiply alphabetical value by position in sorted list
4. **Sum all scores**: Accumulate total across all names

### Implementation Strategy

**UNIX Pipeline Approach**:
- Use `tr` to convert delimiters for line-based processing
- Use `sort` for alphabetical ordering
- Use `awk` for calculations (alphabetical value, position multiplication, running total)
- Process entirely in memory without modifying source file

**Time**: O(n log n) for sorting + O(n×m) for value calculation where n=names, m=avg name length
**Space**: O(n) for sorted list in memory

## Answer

**871198282**

**Verification**:
- COLIN (position 938, value 53) = 49714
- 5163 names processed
- All calculations performed in single pipeline
