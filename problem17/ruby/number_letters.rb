#!/usr/bin/env ruby
# frozen_string_literal: true

=begin
Project Euler Problem 17: Number Letter Counts

WHY: Count letters in numbers 1-1000 written as English words
EXPECTED: Correct conversion to words following British English rules
=end

class NumberLetters
  # English number words
  ONES = %w[zero one two three four five six seven eight nine ten
            eleven twelve thirteen fourteen fifteen sixteen seventeen
            eighteen nineteen].freeze

  TENS = %w[zero ten twenty thirty forty fifty sixty seventy eighty ninety].freeze

  HUNDRED = 'hundred'
  THOUSAND = 'thousand'
  AND = 'and'

  def self.number_to_words(n)
    return "one #{THOUSAND}" if n == 1000

    words = []

    # Handle hundreds
    if n >= 100
      hundreds_digit = n / 100
      words << ONES[hundreds_digit]
      words << HUNDRED
      n %= 100
      words << AND if n > 0  # British usage: "and" after hundreds
    end

    # Handle tens and units
    if n >= 20
      tens_digit = n / 10
      words << TENS[tens_digit]
      n %= 10
      words << ONES[n] if n > 0
    elsif n > 0
      words << ONES[n]
    end

    words.join(' ')
  end

  def self.count_letters(words)
    # Remove spaces and hyphens, count remaining letters
    words.gsub(/[^a-z]/i, '').length
  end

  def self.number_letter_count(n)
    count_letters(number_to_words(n))
  end

  def self.total_letters(start_num, end_num)
    (start_num..end_num).sum { |n| number_letter_count(n) }
  end
end

# Run if executed directly
if __FILE__ == $PROGRAM_NAME
  puts 'Solving Project Euler Problem 17...'
  puts 'Counting letters in numbers 1-1000 written as words'
  puts

  # Test examples from problem
  test_342 = NumberLetters.number_to_words(342)
  test_115 = NumberLetters.number_to_words(115)

  puts "342 = '#{test_342}' (#{NumberLetters.count_letters(test_342)} letters)"
  puts "115 = '#{test_115}' (#{NumberLetters.count_letters(test_115)} letters)"
  puts

  # Verify examples
  raise "342 should have 23 letters" unless NumberLetters.count_letters(test_342) == 23
  raise "115 should have 20 letters" unless NumberLetters.count_letters(test_115) == 20

  # Solve the problem
  result = NumberLetters.total_letters(1, 1000)
  puts "Answer: #{result} letters"
end
