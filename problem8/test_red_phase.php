<?php

// Simple test to verify RED phase - current code should produce wrong results

require "StringChunker.php";

$chunker = new StringChunker();
$testString = "12345678901234567890";  // 20 characters
$chunkSize = 5;

$chunks = $chunker->chunk($testString, $chunkSize);

$expected_chunk_count = strlen($testString) - $chunkSize + 1;  // 20 - 5 + 1 = 16
$actual_chunk_count = sizeof($chunks);

echo "Test String Length: " . strlen($testString) . "\n";
echo "Chunk Size: $chunkSize\n";
echo "Expected number of chunks: $expected_chunk_count\n";
echo "Actual number of chunks: $actual_chunk_count\n";

if ($actual_chunk_count != $expected_chunk_count) {
    echo "\n✗ RED PHASE CONFIRMED - Test fails as expected\n";
    echo "  Difference: " . ($expected_chunk_count - $actual_chunk_count) . " chunks missing\n";
} else {
    echo "\n✓ Test passes - but it shouldn't yet!\n";
}

// Check first chunk
if (count($chunks) > 0) {
    $first_chunk = $chunks[0];
    echo "\nFirst chunk size: " . sizeof($first_chunk) . " (expected: $chunkSize)\n";
    echo "First chunk: " . implode('', $first_chunk) . "\n";

    if (sizeof($first_chunk) != $chunkSize) {
        echo "✗ First chunk has wrong size\n";
    }
}

