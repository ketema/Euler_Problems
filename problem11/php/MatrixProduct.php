<?php

/**
 * Read a matrix from a text file where each line is a row of space-separated integers.
 *
 * @param string $filename Path to the matrix file
 * @return array 2D array representing the matrix
 */
function readMatrix(string $filename): array
{
    $matrix = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    
    foreach ($lines as $line) {
        $row = array_map('intval', preg_split('/\s+/', trim($line)));
        if (!empty($row)) {
            $matrix[] = $row;
        }
    }
    
    return $matrix;
}

/**
 * Find the maximum product of n consecutive numbers in the horizontal (right) direction.
 *
 * @param array $matrix 2D array of integers
 * @param int $n Number of consecutive elements
 * @return int Maximum product found
 */
function maxProductRight(array $matrix, int $n): int
{
    if (empty($matrix)) {
        return 0;
    }
    
    $maxProduct = 0;
    $rows = count($matrix);
    $cols = count($matrix[0] ?? []);
    
    if ($cols < $n) {
        return 0;
    }
    
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c <= $cols - $n; $c++) {
            $product = 1;
            for ($i = 0; $i < $n; $i++) {
                $product *= $matrix[$r][$c + $i];
            }
            $maxProduct = max($maxProduct, $product);
        }
    }
    
    return $maxProduct;
}

/**
 * Find the maximum product of n consecutive numbers in the vertical (down) direction.
 *
 * @param array $matrix 2D array of integers
 * @param int $n Number of consecutive elements
 * @return int Maximum product found
 */
function maxProductDown(array $matrix, int $n): int
{
    if (empty($matrix)) {
        return 0;
    }
    
    $maxProduct = 0;
    $rows = count($matrix);
    $cols = count($matrix[0] ?? []);
    
    if ($rows < $n) {
        return 0;
    }
    
    for ($r = 0; $r <= $rows - $n; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            $product = 1;
            for ($i = 0; $i < $n; $i++) {
                $product *= $matrix[$r + $i][$c];
            }
            $maxProduct = max($maxProduct, $product);
        }
    }
    
    return $maxProduct;
}

/**
 * Find the maximum product of n consecutive numbers in the diagonal down-right direction.
 *
 * @param array $matrix 2D array of integers
 * @param int $n Number of consecutive elements
 * @return int Maximum product found
 */
function maxProductDiagDownRight(array $matrix, int $n): int
{
    if (empty($matrix)) {
        return 0;
    }
    
    $maxProduct = 0;
    $rows = count($matrix);
    $cols = count($matrix[0] ?? []);
    
    if ($rows < $n || $cols < $n) {
        return 0;
    }
    
    for ($r = 0; $r <= $rows - $n; $r++) {
        for ($c = 0; $c <= $cols - $n; $c++) {
            $product = 1;
            for ($i = 0; $i < $n; $i++) {
                $product *= $matrix[$r + $i][$c + $i];
            }
            $maxProduct = max($maxProduct, $product);
        }
    }
    
    return $maxProduct;
}

/**
 * Find the maximum product of n consecutive numbers in the diagonal down-left direction.
 *
 * @param array $matrix 2D array of integers
 * @param int $n Number of consecutive elements
 * @return int Maximum product found
 */
function maxProductDiagDownLeft(array $matrix, int $n): int
{
    if (empty($matrix)) {
        return 0;
    }
    
    $maxProduct = 0;
    $rows = count($matrix);
    $cols = count($matrix[0] ?? []);
    
    if ($rows < $n || $cols < $n) {
        return 0;
    }
    
    for ($r = 0; $r <= $rows - $n; $r++) {
        for ($c = $n - 1; $c < $cols; $c++) {
            $product = 1;
            for ($i = 0; $i < $n; $i++) {
                $product *= $matrix[$r + $i][$c - $i];
            }
            $maxProduct = max($maxProduct, $product);
        }
    }
    
    return $maxProduct;
}

/**
 * Find the maximum product sequence across all directions and return coordinates.
 *
 * @param array $matrix 2D array of integers
 * @param int $n Number of consecutive elements
 * @return array [product, coordinates] where coordinates is array of [row, col] pairs
 */
function findMaxProductSequence(array $matrix, int $n): array
{
    if (empty($matrix)) {
        return [0, []];
    }
    
    $best = [0, []];
    $rows = count($matrix);
    $cols = count($matrix[0] ?? []);
    
    // Right direction
    if ($cols >= $n) {
        for ($r = 0; $r < $rows; $r++) {
            for ($c = 0; $c <= $cols - $n; $c++) {
                $coords = [];
                $product = 1;
                for ($i = 0; $i < $n; $i++) {
                    $coords[] = [$r, $c + $i];
                    $product *= $matrix[$r][$c + $i];
                }
                if ($product > $best[0]) {
                    $best = [$product, $coords];
                }
            }
        }
    }
    
    // Down direction
    if ($rows >= $n) {
        for ($r = 0; $r <= $rows - $n; $r++) {
            for ($c = 0; $c < $cols; $c++) {
                $coords = [];
                $product = 1;
                for ($i = 0; $i < $n; $i++) {
                    $coords[] = [$r + $i, $c];
                    $product *= $matrix[$r + $i][$c];
                }
                if ($product > $best[0]) {
                    $best = [$product, $coords];
                }
            }
        }
    }
    
    // Diagonal down-right
    if ($rows >= $n && $cols >= $n) {
        for ($r = 0; $r <= $rows - $n; $r++) {
            for ($c = 0; $c <= $cols - $n; $c++) {
                $coords = [];
                $product = 1;
                for ($i = 0; $i < $n; $i++) {
                    $coords[] = [$r + $i, $c + $i];
                    $product *= $matrix[$r + $i][$c + $i];
                }
                if ($product > $best[0]) {
                    $best = [$product, $coords];
                }
            }
        }
    }
    
    // Diagonal down-left
    if ($rows >= $n && $cols >= $n) {
        for ($r = 0; $r <= $rows - $n; $r++) {
            for ($c = $n - 1; $c < $cols; $c++) {
                $coords = [];
                $product = 1;
                for ($i = 0; $i < $n; $i++) {
                    $coords[] = [$r + $i, $c - $i];
                    $product *= $matrix[$r + $i][$c - $i];
                }
                if ($product > $best[0]) {
                    $best = [$product, $coords];
                }
            }
        }
    }
    
    return $best;
}

/**
 * Generate a colored matrix output with the winning sequence highlighted in red.
 *
 * @param array $matrix 2D array of integers
 * @param array $coords Array of [row, col] coordinate pairs to highlight
 * @return string Formatted matrix string with ANSI color codes
 */
function colorMatrixSequence(array $matrix, array $coords): string
{
    // Convert coords to a set for O(1) lookup
    $coordSet = [];
    foreach ($coords as $coord) {
        $key = $coord[0] . ',' . $coord[1];
        $coordSet[$key] = true;
    }
    
    $lines = [];
    foreach ($matrix as $r => $row) {
        $line = [];
        foreach ($row as $c => $val) {
            $key = $r . ',' . $c;
            if (isset($coordSet[$key])) {
                $line[] = "\033[31m" . sprintf("%02d", $val) . "\033[0m";
            } else {
                $line[] = sprintf("%02d", $val);
            }
        }
        $lines[] = implode(' ', $line);
    }
    
    return implode("\n", $lines);
}

/**
 * Find the greatest product of n adjacent numbers in any direction.
 *
 * @param array $matrix 2D array of integers
 * @param int $n Number of consecutive elements
 * @return int Maximum product found
 */
function greatestProductInMatrix(array $matrix, int $n): int
{
    return findMaxProductSequence($matrix, $n)[0];
}

