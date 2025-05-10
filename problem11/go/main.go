package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// readMatrix reads a matrix from a text file where each line is a row of space-separated integers.
// func readMatrix(filename string) ([][]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var matrix [][]int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Fields(line)
		if len(parts) == 0 {
			continue
		}
		row := make([]int, len(parts))
		for i, p := range parts {
			num, err := strconv.Atoi(p)
			if err != nil {
				return nil, fmt.Errorf("invalid number %q: %v", p, err)
			}
			row[i] = num
		}
		matrix = append(matrix, row)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	if len(matrix) == 0 {
		return nil, fmt.Errorf("empty matrix")
	}
	cols := len(matrix[0])
	for _, row := range matrix {
		if len(row) != cols {
			return nil, fmt.Errorf("inconsistent row length")
		}
	}
	return matrix, nil
}

// printMatrixWithHighlight prints the matrix and highlights the winning sequence in red.
func printMatrixWithHighlight(matrix [][]int, coords [][2]int) {
	coordMap := make(map[[2]int]bool)
	for _, c := range coords {
		coordMap[c] = true
	}
	for i, row := range matrix {
		for j, val := range row {
			if coordMap[[2]int{i, j}] {
				fmt.Printf("\033[31m%02d\033[0m ", val)
			} else {
				fmt.Printf("%02d ", val)
			}
		}
		fmt.Println()
	}
}

// Ensure GreatestProduct is available from matrix_product.go
// func main() {
	filename := "matrix.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}
	matrix, err := readMatrix(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to read matrix from %s: %v\n", filename, err)
		os.Exit(1)
	}
	coords := make([][2]int, 0)
	max := GreatestProduct(matrix, &coords, 4)
	printMatrixWithHighlight(matrix, coords)
	fmt.Printf("Greatest product of four adjacent numbers: %d\n", max)
}
