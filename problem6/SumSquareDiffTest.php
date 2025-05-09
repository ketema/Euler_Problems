<?php

require "SumSquareDiff.php";

class SumSquareDiffTest
    extends PHPUnit_Framework_TestCase
{
    public function setup()
    {
        $this->fixture = new SumSquareDiff();
    }

    /**
     * @dataProvider provideSumSquares
     */
    public function test_sum_of_squares( $sum, array $range )
    {
        $squareSum = $this->fixture->sumSquares( $range );

        $this->assertEquals( $sum, $squareSum );
    }

    /**
     * @dataProvider provideSquareSum
     */
    public function test_square_of_sum( $result, array $range )
    {
        $squareSum = $this->fixture->squareSum( $range );
        $this->assertEquals( $result, $squareSum );
    }

    /**
     * @depends test_sum_of_squares
     * @depends test_square_of_sum
     * @dataProvider provideDiffs
     */
    public function test_return_diff_sum_square( $expected, array $range)
    {
        $this->assertEquals($expected,
            $this->fixture->squareSum($range) - $this->fixture->sumSquares($range)
        );
    }

    public function provideDiffs()
    {
        return array(
            array( 2640, range(1,10) ),
        );
    }

    public function provideSumSquares()
    {
        return array(
            array( 385, range(1,10) ),
            array( 50 , range(3,5) ),
        );
    }

    public function provideSquareSum()
    {
        return array(
            array( 3025, range(1,10) ),
            array( 9, range(1,2) ),
            array( 625 , range( 3,7 ) ),
        );
    }

        
}

?>
