# Project Euler Problem 17: Number Letter Counts

## Problem Statement

If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

**Question**: If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

**Note**: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

## Approach

1. **Convert numbers to words**: Implement English number naming rules
2. **Count letters**: Strip spaces/hyphens, count remaining characters
3. **Sum for range**: Apply to 1-1000

### English Number Rules

- 1-19: Special names (one, two, ..., nineteen)
- 20-99: Tens (twenty, thirty, ...) + units
- 100-999: Hundreds + "and" + remainder (if non-zero)
- 1000: "one thousand"

## Complexity

- **Time**: O(n) where n = 1000
- **Space**: O(1) for number-to-word conversion

## Answer

**21124**

**Verification**: All 12 tests pass, examples (342=23 letters, 115=20 letters) verified.
