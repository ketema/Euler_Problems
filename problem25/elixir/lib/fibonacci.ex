defmodule Fibonacci do
  @moduledoc """
  Project Euler Problem 25: 1000-digit Fibonacci Number

  Find the index of the first term in the Fibonacci sequence to contain n digits.

  The Fibonacci sequence is defined by: F_n = F_{n-1} + F_{n-2}
  where F_1 = 1 and F_2 = 1.
  """

  @doc """
  Find the index of the first Fibonacci number containing at least n digits.

  Uses tail-recursive iteration with arbitrary-precision integers (native Elixir).
  Time complexity: O(result) where result is the returned index.
  Space complexity: O(1) due to tail call optimization.

  ## Examples

      iex> Fibonacci.find_index_with_n_digits(1)
      1

      iex> Fibonacci.find_index_with_n_digits(2)
      7

      iex> Fibonacci.find_index_with_n_digits(3)
      12

  """
  def find_index_with_n_digits(n) when n > 0 do
    # Start with F_1=1, F_2=1, checking from index 2
    # (F_1 has 1 digit, so if n=1, we return 1 immediately)
    if n == 1 do
      1
    else
      iterate(1, 1, 2, n)
    end
  end

  # Tail-recursive iteration: tracks (previous, current, index, target_digits)
  # Stops when current term has >= target_digits digits
  defp iterate(prev, curr, index, target_digits) do
    if digit_count(curr) >= target_digits do
      index
    else
      # Next iteration: curr becomes prev, prev+curr becomes curr, increment index
      iterate(curr, prev + curr, index + 1, target_digits)
    end
  end

  # Count digits using Elixir's Integer.digits/1
  # Returns length of digit list (works for arbitrary-precision integers)
  defp digit_count(n) do
    length(Integer.digits(n))
  end
end
