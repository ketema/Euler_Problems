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
    check findCycleLength(997) == 996

  # Integration tests
  test "findLongestCycle under 10":
    let (d, length) = findLongestCycle(10)
    check d == 7
    check length == 6

  test "findLongestCycle under 1000 (answer validation)":
    let (d, length) = findLongestCycle(1000)
    check d == 983
    check length == 982
