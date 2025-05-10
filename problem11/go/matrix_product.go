package main

// GreatestProduct finds the greatest product of ADJ adjacent numbers in a matrix.
// It populates coords with the coordinates of the winning sequence.
func GreatestProduct(matrix [][]int, coords *[][2]int, ADJ int) int {
	rows := len(matrix)
	if rows == 0 {
		return 0
	}
	cols := len(matrix[0])
	max := 0
	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			// right
			if j+ADJ <= cols {
				prod := 1
				tmp := make([][2]int, ADJ)
				for k := 0; k < ADJ; k++ {
					prod *= matrix[i][j+k]
					tmp[k] = [2]int{i, j + k}
				}
				if prod > max {
					max = prod
					*coords = tmp
				}
			}
			// down
			if i+ADJ <= rows {
				prod := 1
				tmp := make([][2]int, ADJ)
				for k := 0; k < ADJ; k++ {
					prod *= matrix[i+k][j]
					tmp[k] = [2]int{i + k, j}
				}
				if prod > max {
					max = prod
					*coords = tmp
				}
			}
			// diag down-right
			if i+ADJ <= rows && j+ADJ <= cols {
				prod := 1
				tmp := make([][2]int, ADJ)
				for k := 0; k < ADJ; k++ {
					prod *= matrix[i+k][j+k]
					tmp[k] = [2]int{i + k, j + k}
				}
				if prod > max {
					max = prod
					*coords = tmp
				}
			}
			// diag down-left
			if i+ADJ <= rows && j-ADJ+1 >= 0 {
				prod := 1
				tmp := make([][2]int, ADJ)
				for k := 0; k < ADJ; k++ {
					prod *= matrix[i+k][j-k]
					tmp[k] = [2]int{i + k, j - k}
				}
				if prod > max {
					max = prod
					*coords = tmp
				}
			}
		}
	}
	return max
}
