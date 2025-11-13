defmodule FibonacciTest do
  use ExUnit.Case
  doctest Fibonacci

  @moduledoc """
  Test suite for Project Euler Problem 25: 1000-digit Fibonacci Number

  Tests validate edge cases and known Fibonacci sequence properties:
  - F_1 = 1 (first term, 1 digit)
  - F_2 = 1 (second term, 1 digit)
  - F_7 = 13 (first term with 2 digits)
  - F_12 = 144 (first term with 3 digits)
  - F_4782 = first term with 1000 digits
  """

  test "finds index of first Fibonacci term with 1 digit" do
    # F_1 = 1 is the first term with 1 digit
    assert Fibonacci.find_index_with_n_digits(1) == 1
  end

  test "finds index of first Fibonacci term with 2 digits" do
    # F_7 = 13 is the first term with 2 digits
    assert Fibonacci.find_index_with_n_digits(2) == 7
  end

  test "finds index of first Fibonacci term with 3 digits" do
    # F_12 = 144 is the first term with 3 digits (verified in problem statement)
    assert Fibonacci.find_index_with_n_digits(3) == 12
  end

  test "solves Project Euler Problem 25: first term with 1000 digits" do
    # The answer to Problem 25 is 4782
    assert Fibonacci.find_index_with_n_digits(1000) == 4782
  end
end
