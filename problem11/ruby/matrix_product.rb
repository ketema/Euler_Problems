# Finds the greatest product of 'adj' adjacent numbers in a matrix.
def greatest_product(matrix, adj)
  rows = matrix.size
  cols = matrix[0].size
  max = 0
  best_coords = []
  rows.times do |i|
    cols.times do |j|
      # right
      if j + adj <= cols
        prod = 1
        coords = []
        adj.times { |k| prod *= matrix[i][j+k]; coords << [i, j+k] }
        if prod > max
          max = prod
          best_coords = coords
        end
      end
      # down
      if i + adj <= rows
        prod = 1
        coords = []
        adj.times { |k| prod *= matrix[i+k][j]; coords << [i+k, j] }
        if prod > max
          max = prod
          best_coords = coords
        end
      end
      # diag down-right
      if i + adj <= rows && j + adj <= cols
        prod = 1
        coords = []
        adj.times { |k| prod *= matrix[i+k][j+k]; coords << [i+k, j+k] }
        if prod > max
          max = prod
          best_coords = coords
        end
      end
      # diag down-left
      if i + adj <= rows && j - adj + 1 >= 0
        prod = 1
        coords = []
        adj.times { |k| prod *= matrix[i+k][j-k]; coords << [i+k, j-k] }
        if prod > max
          max = prod
          best_coords = coords
        end
      end
    end
  end
  [max, best_coords]
end
