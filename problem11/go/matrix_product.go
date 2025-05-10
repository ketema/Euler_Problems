package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func readMatrix(filename string) ([][]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	var matrix [][]int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.TrimSpace(line) == "" {
			continue
		}
		nums := strings.Fields(line)
		row := make([]int, len(nums))
		for i, s := range nums {
			row[i], _ = strconv.Atoi(s)
		}
		matrix = append(matrix, row)
	}
	return matrix, scanner.Err()
}

func printMatrix(matrix [][]int, coords [][2]int) {
	coordMap := make(map[[2]int]bool)
	for _, c := range coords {
		coordMap[c] = true
	}
	for i, row := range matrix {
		for j, val := range row {
			if coordMap[[2]int{i, j}] {
				fmt.Printf("\x1b[31m%02d\x1b[0m ", val)
			} else {
				fmt.Printf("%02d ", val)
			}
		}
		fmt.Println()
	}
}

func main() {
	matrix, err := readMatrix("../matrix.txt")
	if err != nil {
		fmt.Println("Error reading matrix:", err)
		os.Exit(1)
	}
	coords := make([][2]int, 4)
	maxProd := GreatestProduct(matrix, &coords, 4)
	printMatrix(matrix, coords)
	fmt.Printf("\nGreatest product of four adjacent numbers: %d\n", maxProd)
}

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
