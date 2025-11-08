# Project Euler Problem 26: Reciprocal Cycles
# R implementation following TDD methodology

#' Calculate the length of the recurring cycle in the decimal fraction of 1/d
#'
#' Uses long division to track remainders. When a remainder repeats,
#' we've found the cycle.
#'
#' @param d The denominator
#' @return The length of the recurring cycle (0 if terminating)
#' @examples
#' cycle_length(7)  # Returns 6 for 1/7 = 0.142857142857...
#' cycle_length(3)  # Returns 1 for 1/3 = 0.333...
#' cycle_length(4)  # Returns 0 for 1/4 = 0.25 (terminating)
cycle_length <- function(d) {
  if (d <= 1) {
    return(0)
  }

  # Check if it's a terminating decimal
  # A fraction 1/d terminates if d = 2^a * 5^b
  temp_d <- d
  while (temp_d %% 2 == 0) {
    temp_d <- temp_d / 2
  }
  while (temp_d %% 5 == 0) {
    temp_d <- temp_d / 5
  }
  if (temp_d == 1) {
    return(0)  # Terminating decimal
  }

  # Perform long division and track remainders
  remainders <- integer(0)
  remainder <- 1

  while (TRUE) {
    remainder <- remainder %% d

    if (remainder == 0) {
      return(0)  # Terminates
    }

    # Check if we've seen this remainder before
    position <- which(remainders == remainder)
    if (length(position) > 0) {
      # Found the cycle! Length is current position minus where we saw it
      return(length(remainders) - position[1] + 1)
    }

    # Record this remainder
    remainders <- c(remainders, remainder)

    # Move to next digit in long division
    remainder <- remainder * 10

    # Safety check: cycle can't be longer than d-1
    if (length(remainders) >= d) {
      return(length(remainders))
    }
  }
}

#' Find the value d < limit with the longest recurring cycle
#'
#' @param limit The upper bound (exclusive)
#' @return A list with 'd' (the value) and 'length' (cycle length)
#' @examples
#' find_longest_cycle(10)  # Returns list(d=7, length=6)
find_longest_cycle <- function(limit) {
  max_d <- 0
  max_length <- 0

  for (d in 2:(limit - 1)) {
    len <- cycle_length(d)
    if (len > max_length) {
      max_length <- len
      max_d <- d
    }
  }

  return(list(d = max_d, length = max_length))
}

#' Solve Problem 26: Find d < 1000 with longest cycle
#'
#' @return The value of d with the longest recurring cycle
#' @examples
#' solve()  # Returns the answer
solve <- function() {
  result <- find_longest_cycle(1000)
  return(result$d)
}

# If run as a script (not sourced), execute solve
if (sys.nframe() == 0) {
  result <- solve()
  cycle_len <- cycle_length(result)

  cat("\nProject Euler Problem 26: Reciprocal Cycles\n")
  cat("============================================\n\n")
  cat(sprintf("Finding d < 1000 with longest recurring cycle in 1/d\n\n"))
  cat(sprintf("Answer: d = %d\n", result))
  cat(sprintf("Cycle length: %d\n", cycle_len))
  cat(sprintf("\n1/%d has a %d-digit recurring cycle\n", result, cycle_len))
}
