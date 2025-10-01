<?php

use PHPUnit\Framework\TestCase;

require_once 'MatrixProduct.php';

class MatrixProductTest extends TestCase
{
    private array $sampleMatrix;

    protected function setUp(): void
    {
        $this->sampleMatrix = [
            [8, 2, 22, 97],
            [49, 49, 99, 40],
            [81, 49, 31, 73],
            [52, 70, 95, 23]
        ];
    }

    public function testReadMatrix(): void
    {
        // Create a temporary test file
        $tmpFile = tempnam(sys_get_temp_dir(), 'matrix_test_');
        file_put_contents($tmpFile, "1 2 3\n4 5 6\n7 8 9\n");
        
        $matrix = readMatrix($tmpFile);
        
        $this->assertEquals([[1, 2, 3], [4, 5, 6], [7, 8, 9]], $matrix);
        
        unlink($tmpFile);
    }

    public function testMaxProductRight(): void
    {
        // Test horizontal (right) products
        $matrix = [
            [1, 2, 3, 4],
            [5, 6, 7, 8],
            [9, 10, 11, 12],
            [13, 14, 15, 16]
        ];
        
        $result = maxProductRight($matrix, 4);
        
        // Last row: 13 * 14 * 15 * 16 = 43680
        $this->assertEquals(43680, $result);
    }

    public function testMaxProductDown(): void
    {
        // Test vertical (down) products
        $matrix = [
            [1, 2, 3, 4],
            [5, 6, 7, 8],
            [9, 10, 11, 12],
            [13, 14, 15, 16]
        ];
        
        $result = maxProductDown($matrix, 4);
        
        // Last column: 4 * 8 * 12 * 16 = 6144
        $this->assertEquals(6144, $result);
    }

    public function testMaxProductDiagDownRight(): void
    {
        // Test diagonal down-right products
        $matrix = [
            [1, 0, 0, 4],
            [0, 6, 7, 0],
            [0, 10, 11, 0],
            [13, 0, 0, 16]
        ];
        
        $result = maxProductDiagDownRight($matrix, 4);
        
        // Main diagonal: 1 * 6 * 11 * 16 = 1056
        $this->assertEquals(1056, $result);
    }

    public function testMaxProductDiagDownLeft(): void
    {
        // Test diagonal down-left products
        $matrix = [
            [0, 0, 0, 4],
            [0, 0, 7, 0],
            [0, 10, 0, 0],
            [13, 0, 0, 0]
        ];
        
        $result = maxProductDiagDownLeft($matrix, 4);
        
        // Anti-diagonal: 4 * 7 * 10 * 13 = 3640
        $this->assertEquals(3640, $result);
    }

    public function testGreatestProductInMatrix(): void
    {
        // Test that it returns the maximum across all directions
        $matrix = [
            [1, 2, 3, 4],
            [5, 6, 7, 8],
            [9, 10, 11, 12],
            [13, 14, 15, 16]
        ];
        
        $result = greatestProductInMatrix($matrix, 4);
        
        // Should be the last row (right direction): 43680
        $this->assertEquals(43680, $result);
    }

    public function testFindMaxProductSequence(): void
    {
        // Test that it returns both product and coordinates
        $matrix = [
            [1, 2, 3, 4],
            [5, 6, 7, 8],
            [9, 10, 11, 12],
            [13, 14, 15, 16]
        ];
        
        [$product, $coords] = findMaxProductSequence($matrix, 4);
        
        // Should be the last row
        $this->assertEquals(43680, $product);
        $this->assertEquals([[3, 0], [3, 1], [3, 2], [3, 3]], $coords);
    }

    public function testColorMatrixSequence(): void
    {
        // Test that it produces colored output
        $matrix = [
            [1, 2, 3, 4],
            [5, 6, 7, 8],
            [9, 10, 11, 12],
            [13, 14, 15, 16]
        ];
        $coords = [[3, 0], [3, 1], [3, 2], [3, 3]];
        
        $output = colorMatrixSequence($matrix, $coords);
        
        // Check that the output contains ANSI red codes for the sequence
        $this->assertStringContainsString("\033[31m13\033[0m", $output);
        $this->assertStringContainsString("\033[31m14\033[0m", $output);
        $this->assertStringContainsString("\033[31m15\033[0m", $output);
        $this->assertStringContainsString("\033[31m16\033[0m", $output);
        
        // Check that non-sequence values are not colored
        $this->assertStringContainsString("01", $output);
        $this->assertStringNotContainsString("\033[31m01\033[0m", $output);
    }

    public function testEdgeCaseEmptyMatrix(): void
    {
        $matrix = [];
        
        $result = maxProductRight($matrix, 4);
        
        $this->assertEquals(0, $result);
    }

    public function testEdgeCaseSmallMatrix(): void
    {
        // Matrix smaller than n
        $matrix = [[1, 2], [3, 4]];
        
        $result = maxProductRight($matrix, 4);
        
        $this->assertEquals(0, $result);
    }
}

