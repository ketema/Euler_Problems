defmodule FibonacciTest do
  use ExUnit.Case
  doctest Fibonacci

  describe "fib_at/1" do
    test "returns correct Fibonacci numbers" do
      assert Fibonacci.fib_at(1) == 1
      assert Fibonacci.fib_at(2) == 1
      assert Fibonacci.fib_at(3) == 2
      assert Fibonacci.fib_at(4) == 3
      assert Fibonacci.fib_at(5) == 5
      assert Fibonacci.fib_at(6) == 8
      assert Fibonacci.fib_at(7) == 13
      assert Fibonacci.fib_at(8) == 21
      assert Fibonacci.fib_at(9) == 34
      assert Fibonacci.fib_at(10) == 55
      assert Fibonacci.fib_at(12) == 144
    end
  end

  describe "count_digits/1" do
    test "counts digits correctly" do
      assert Fibonacci.count_digits(1) == 1
      assert Fibonacci.count_digits(9) == 1
      assert Fibonacci.count_digits(10) == 2
      assert Fibonacci.count_digits(99) == 2
      assert Fibonacci.count_digits(100) == 3
      assert Fibonacci.count_digits(999) == 3
      assert Fibonacci.count_digits(1000) == 4
      assert Fibonacci.count_digits(12345) == 5
    end
  end

  describe "first_fib_with_n_digits/1" do
    test "finds index of first Fibonacci number with n digits" do
      # F₁ = 1 (1 digit)
      assert Fibonacci.first_fib_with_n_digits(1) == 1

      # F₇ = 13 (2 digits)
      assert Fibonacci.first_fib_with_n_digits(2) == 7

      # F₁₂ = 144 (3 digits)
      assert Fibonacci.first_fib_with_n_digits(3) == 12
    end

    test "works for larger digit counts" do
      # These might take a moment but should be correct
      result = Fibonacci.first_fib_with_n_digits(4)
      assert result > 12
      assert Fibonacci.count_digits(Fibonacci.fib_at(result)) == 4
    end
  end

  describe "solve/0" do
    test "returns an integer" do
      result = Fibonacci.solve()
      assert is_integer(result)
      assert result > 1000
    end

    test "result is correct" do
      result = Fibonacci.solve()
      # The Fibonacci number at this index should have exactly 1000 digits
      fib_number = Fibonacci.fib_at(result)
      assert Fibonacci.count_digits(fib_number) == 1000

      # The previous Fibonacci number should have less than 1000 digits
      prev_fib = Fibonacci.fib_at(result - 1)
      assert Fibonacci.count_digits(prev_fib) < 1000
    end
  end
end
