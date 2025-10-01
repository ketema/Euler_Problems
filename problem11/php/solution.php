<?php

require_once 'MatrixProduct.php';

// Get filename from command line or use default
$filename = $argc > 1 ? $argv[1] : 'matrix.txt';

// Read the matrix
$matrix = readMatrix($filename);

// Find the greatest product and its coordinates
[$product, $coords] = findMaxProductSequence($matrix, 4);

// Display the matrix with the winning sequence highlighted
echo colorMatrixSequence($matrix, $coords) . "\n\n";

// Display the result
echo "Greatest product of four adjacent numbers: $product\n";

