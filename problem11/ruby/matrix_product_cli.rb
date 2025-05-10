require_relative 'matrix_product'

# Reads a matrix from a file where each line is a row of space-separated integers.
def read_matrix(filename)
  File.readlines(filename).map { |line| line.split.map(&:to_i) }
end

# Prints the matrix, highlighting the winning sequence in red.
def print_matrix_with_highlight(matrix, coords)
  coord_set = coords.map { |c| c }.to_set
  matrix.each_with_index do |row, i|
    row.each_with_index do |val, j|
      if coord_set.include?([i, j])
        print "\e[31m%02d\e[0m " % val
      else
        print "%02d " % val
      end
    end
    puts
  end
end

if __FILE__ == $0
  require 'set'
  filename = ARGV[0] || 'matrix.txt'
  begin
    matrix = read_matrix(filename)
  rescue => e
    warn "Failed to read matrix from #{filename}: #{e}"
    exit 1
  end
  result, coords = greatest_product(matrix, 4)
  print_matrix_with_highlight(matrix, coords)
  puts "Greatest product of four adjacent numbers: #{result}"
end
