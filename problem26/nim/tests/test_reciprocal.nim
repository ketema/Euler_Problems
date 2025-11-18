import unittest
import ../src/reciprocal

suite "Reciprocal Cycle Tests":
  # Known cycles
  test "findCycleLength for 1/3":
    check findCycleLength(3) == 1

  test "findCycleLength for 1/7":
    check findCycleLength(7) == 6

  test "findCycleLength for 1/11":
    check findCycleLength(11) == 2

  test "findCycleLength for 1/6 (mixed factors 2×3)":
    # Regression test: 1/6 = 0.1666... has cycle length 1
    # Critical: Tests remainder collection fix (cycle-closing remainder)
    check findCycleLength(6) == 1

  test "findCycleLength for 1/12 (mixed factors 2²×3)":
    # Regression test: 1/12 = 0.08333... has cycle length 1
    # Tests optimization fix (12 should not be skipped despite multiple of 2)
    check findCycleLength(12) == 1

  # Terminating decimals
  test "findCycleLength for 1/2 (terminates)":
    check findCycleLength(2) == 0

  test "findCycleLength for 1/5 (terminates)":
    check findCycleLength(5) == 0

  # Edge cases
  test "findCycleLength for 1/0 (invalid)":
    check findCycleLength(0) == 0

  test "findCycleLength for 1/(-3) (negative)":
    check findCycleLength(-3) == 0

  test "findCycleLength for 1/1 (trivial)":
    check findCycleLength(1) == 0

  test "findCycleLength for 1/997 (large prime)":
    # Note: 997 is prime, cycle length is 166 (divides 996 = 997-1)
    check findCycleLength(997) == 166

  # Integration tests
  test "findLongestCycle under 10":
    let (d, length) = findLongestCycle(10)
    check d == 7
    check length == 6

  test "findLongestCycle under 1000 (answer validation)":
    let (d, length) = findLongestCycle(1000)
    check d == 983
    check length == 982
