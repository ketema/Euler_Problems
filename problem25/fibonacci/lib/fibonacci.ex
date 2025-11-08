defmodule Fibonacci do
  @moduledoc """
  Project Euler Problem 25: 1000-digit Fibonacci Number

  Find the index of the first term in the Fibonacci sequence to contain 1000 digits.
  """

  @doc """
  Calculate the nth Fibonacci number.
  F₁ = 1, F₂ = 1, Fₙ = Fₙ₋₁ + Fₙ₋₂

  Uses iterative approach with O(n) time and O(1) space.
  Elixir handles arbitrarily large integers natively.

  ## Examples

      iex> Fibonacci.fib_at(1)
      1

      iex> Fibonacci.fib_at(10)
      55

      iex> Fibonacci.fib_at(12)
      144
  """
  def fib_at(1), do: 1
  def fib_at(2), do: 1

  def fib_at(n) when n > 2 do
    fib_iterate(3, n, 1, 1)
  end

  defp fib_iterate(current, target, _prev, curr) when current > target, do: curr

  defp fib_iterate(current, target, prev, curr) do
    fib_iterate(current + 1, target, curr, prev + curr)
  end

  @doc """
  Count the number of digits in a number.

  ## Examples

      iex> Fibonacci.count_digits(1)
      1

      iex> Fibonacci.count_digits(144)
      3

      iex> Fibonacci.count_digits(12345)
      5
  """
  def count_digits(n) when n > 0 do
    n
    |> Integer.to_string()
    |> String.length()
  end

  @doc """
  Find the index of the first Fibonacci number with exactly n digits.

  Uses efficient iteration, checking digit count at each step.

  ## Examples

      iex> Fibonacci.first_fib_with_n_digits(1)
      1

      iex> Fibonacci.first_fib_with_n_digits(2)
      7

      iex> Fibonacci.first_fib_with_n_digits(3)
      12
  """
  def first_fib_with_n_digits(1), do: 1  # F₁ = 1 is the first 1-digit number

  def first_fib_with_n_digits(n) when n > 1 do
    # Start at F₂ (index 2), with prev=F₁=1, curr=F₂=1
    find_first_n_digit_fib(2, 1, 1, n)
  end

  # Iterate through Fibonacci sequence until we find one with n digits
  defp find_first_n_digit_fib(index, prev, curr, target_digits) do
    if count_digits(curr) >= target_digits do
      index
    else
      # Move to next Fibonacci number
      find_first_n_digit_fib(index + 1, curr, prev + curr, target_digits)
    end
  end

  @doc """
  Solve Problem 25: Find index of first Fibonacci number with 1000 digits.

  ## Examples

      iex> result = Fibonacci.solve()
      iex> result > 1000
      true
  """
  def solve do
    first_fib_with_n_digits(1000)
  end
end
