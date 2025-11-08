# Project Euler Problem 19: Counting Sundays

## Problem Statement

You are given the following information, but you may prefer to do some research for yourself.

- 1 Jan 1900 was a Monday.
- Thirty days has September, April, June and November.
- All the rest have thirty-one,
- Saving February alone, which has twenty-eight, rain or shine.
- And on leap years, twenty-nine.
- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

## Approach

### Brute Force (Simple Iteration)
Use Python's `datetime` module to:
1. Iterate through each month from Jan 1901 to Dec 2000
2. Check if the 1st of each month is a Sunday
3. Count the occurrences

**Time**: O(n) where n is number of months (1200)
**Space**: O(1)

This is the most straightforward approach and perfectly efficient for this problem.

### Alternative: Mathematical Pattern
Could calculate using modular arithmetic and leap year rules, but the datetime approach is cleaner and more maintainable.

## Implementation Details

- **Language**: Python
- **Why Python**: Built-in `datetime` module handles all date arithmetic, leap years, and day-of-week calculations automatically
- `weekday()` returns 0-6 where Monday=0, Sunday=6
- Leap year handling is automatic (1900 not a leap year, 2000 is)

## Answer

**171**

**Verification**:
- Over 1200 months, probability of any first being Sunday is 1/7 ≈ 171.4
- Result matches expected statistical distribution
- All tests passing
