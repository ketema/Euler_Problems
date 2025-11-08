# Project Euler Problem 28: Number Spiral Diagonals
# Crystal OOP implementation

class NumberSpiral
  def initialize(@size : Int32)
    raise ArgumentError.new("Size must be odd") unless @size.odd?
  end

  def diagonal_sum : Int64
    return 1_i64 if @size == 1

    sum = 1_i64
    value = 1_i64
    (2..@size).step(2).each do |layer|
      4.times { value += layer; sum += value }
    end
    sum
  end

  def self.solve(size : Int32 = 1001) : Int64
    new(size).diagonal_sum
  end
end

if ARGV.size > 0
  size = ARGV[0].to_i
  puts "Problem 28: Number Spiral Diagonals (#{size}×#{size})"
  puts "Answer: #{NumberSpiral.solve(size)}"
else
  puts "Problem 28: Number Spiral Diagonals (1001×1001)"
  puts "Answer: #{NumberSpiral.solve}"
end
