#!/usr/bin/env ruby
# frozen_string_literal: true

require 'minitest/autorun'
require_relative 'number_letters'

=begin
Tests for Project Euler Problem 17: Number Letter Counts
Following TDD/BDD methodology per AGENTS.md
=end

class TestNumberLetters < Minitest::Test
  def test_single_digits
    assert_equal 'one', NumberLetters.number_to_words(1)
    assert_equal 'two', NumberLetters.number_to_words(2)
    assert_equal 'five', NumberLetters.number_to_words(5)
    assert_equal 'nine', NumberLetters.number_to_words(9)
  end

  def test_teens
    assert_equal 'ten', NumberLetters.number_to_words(10)
    assert_equal 'eleven', NumberLetters.number_to_words(11)
    assert_equal 'fifteen', NumberLetters.number_to_words(15)
    assert_equal 'nineteen', NumberLetters.number_to_words(19)
  end

  def test_tens
    assert_equal 'twenty', NumberLetters.number_to_words(20)
    assert_equal 'thirty', NumberLetters.number_to_words(30)
    assert_equal 'forty', NumberLetters.number_to_words(40)
    assert_equal 'ninety', NumberLetters.number_to_words(90)
  end

  def test_compound_numbers
    assert_equal 'twenty one', NumberLetters.number_to_words(21)
    assert_equal 'forty two', NumberLetters.number_to_words(42)
    assert_equal 'ninety nine', NumberLetters.number_to_words(99)
  end

  def test_hundreds
    assert_equal 'one hundred', NumberLetters.number_to_words(100)
    assert_equal 'two hundred', NumberLetters.number_to_words(200)
    assert_equal 'nine hundred', NumberLetters.number_to_words(900)
  end

  def test_hundreds_with_and
    # British usage: "and" after hundreds
    assert_equal 'one hundred and one', NumberLetters.number_to_words(101)
    assert_equal 'one hundred and fifteen', NumberLetters.number_to_words(115)
    assert_equal 'three hundred and forty two', NumberLetters.number_to_words(342)
  end

  def test_one_thousand
    assert_equal 'one thousand', NumberLetters.number_to_words(1000)
  end

  def test_letter_counting
    # Remove spaces and hyphens, count letters only
    assert_equal 3, NumberLetters.count_letters('one')
    assert_equal 3, NumberLetters.count_letters('two')
    assert_equal 5, NumberLetters.count_letters('three')
    assert_equal 4, NumberLetters.count_letters('four')
    assert_equal 4, NumberLetters.count_letters('five')
  end

  def test_problem_examples
    # 342 (three hundred and forty-two) = 23 letters
    assert_equal 23, NumberLetters.number_letter_count(342)

    # 115 (one hundred and fifteen) = 20 letters
    assert_equal 20, NumberLetters.number_letter_count(115)
  end

  def test_example_from_problem_statement
    # 1-5: one, two, three, four, five = 3+3+5+4+4 = 19 letters
    total = NumberLetters.total_letters(1, 5)
    assert_equal 19, total
  end

  def test_number_letter_counts_for_small_range
    # Verify individual counts for 1-5
    assert_equal 3, NumberLetters.number_letter_count(1)  # one
    assert_equal 3, NumberLetters.number_letter_count(2)  # two
    assert_equal 5, NumberLetters.number_letter_count(3)  # three
    assert_equal 4, NumberLetters.number_letter_count(4)  # four
    assert_equal 4, NumberLetters.number_letter_count(5)  # five
  end

  def test_solution_for_1_to_1000
    result = NumberLetters.total_letters(1, 1000)

    # Verify result is reasonable
    assert result > 0, 'Result should be positive'
    assert result > 10_000, 'Result should be large for 1000 numbers'
    assert result < 100_000, 'Result should be reasonable'

    puts "\n✓ Problem 17 Solution: #{result} letters"
  end
end
