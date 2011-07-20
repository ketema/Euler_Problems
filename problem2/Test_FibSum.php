<?php

include 'FibSum.php';

class FibSumTest
    extends PHPUnit_Framework_TestCase
{
    
    protected function setUp()
    {
        $this->fixture = new FibSum();
    }

    public function test_basic()
    {
        $fibSum = new FibSum();
        $this->assertTrue( $fibSum instanceof FibSum ,
            'this is not a FibSum object' );
    }

    /**
     * This test verifies that the sumFibs method
     * returns the correct sum for the number of
     * terms in the fib sequence
     *
     * @dataProvider fibvalsum
     */
    public function test_fibSum( $terms, $sum )
    {
        $this->assertTrue( $this->fixture->sumFibs( $terms ) == $sum ,
            'Sorry your math is wrong' );
    }

    /** 
     * This provider returns "terms" and "sum"
     * so for a fib sequence that has 3 terms, i.e.
     * 1,2,3 then the sum is 6.
     */
    public function fibvalsum()
    {
        return array(
            array( 3, 6 ),
            array( 4, 11),
            array( 5, 19),
            array( 6, 32)
        );
    }

    /**
     * This test verifies that the fib method returns the correct
     * terms in the fibonacci sequence.
     *
     * @dataProvider fibs
     */
    public function test_fib( $terms, $expected )
    {
        $this->assertTrue(
            $this->fixture->fib( $terms ) == $expected,
            'Did not get the expected array'
        );
    }

    /**
     * This provider returns "terms" and the correct fib array
     */
    public function fibs()
    {
        return array(
            array( 3, array(1,2,3) ),
            array( 4, array(1,2,3,5) ),
            array( 5, array(1,2,3,5,8) ),
            array( 6, array(1,2,3,5,8,13) ),
            array( 7, array(1,2,3,5,8,13,21) ),
            array( 8, array(1,2,3,5,8,13,21,34) ),
            array( 9, array(1,2,3,5,8,13,21,34,55) ),
            array(10, array(1,2,3,5,8,13,21,34,55,89) )
        );
    }
}
?>
