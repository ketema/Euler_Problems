package main

import (
	"testing"
	"reflect"
)

func TestGreatestProduct(t *testing.T) {
	matrix := [][]int{
		{1, 2, 3, 4},
		{5, 6, 7, 8},
		{9, 10, 11, 12},
		{13, 14, 15, 16},
	}
	coords := make([][2]int, 0)
	result := GreatestProduct(matrix, &coords, 4)

	expected := 43680 // 13*14*15*16
	expectedCoords := [][2]int{{3, 0}, {3, 1}, {3, 2}, {3, 3}}

	if result != expected {
		t.Errorf("Expected product %d, got %d", expected, result)
	}
	if !reflect.DeepEqual(coords, expectedCoords) {
		t.Errorf("Expected coords %v, got %v", expectedCoords, coords)
	}
}
