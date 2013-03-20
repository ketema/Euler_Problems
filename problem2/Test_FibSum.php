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

    /**
     * Now that I know I can generate a fib sequence correctly
     * and that I can get back the correct sum of an arbitrary
     * sequence length, I need to test that I can filter out
     * a sequence down to its even elements only
     */
    public function test_get_evens()
    {
        $fibArr = $this->fixture->fib( 10 );
        
        $this->assertTrue( method_exists( $this->fixture, 'getEvens' ) );

        $fibArr = $this->fixture->getEvens( $fibArr );

        foreach( $fibArr as $fibNum )
        {
            $this->assertEquals( $fibNum % 2 , 0, 
                "Fibonacci Number $fibNum is not divisible by 2" );
        }
    }

    /**
     * Now that we have verified that we can filter the fib array
     * and only get even elements, we need to verify that we can 
     * add those elements up for every even fib number up to an
     * arbitrary sequence length that does not contain a term that
     * exceeds an arbitrary threshold.
     *
     * @dataProvider threshSums
     */
    public function test_threshSum( $threshold, $sum )
    {
        $this->assertTrue( method_exists( $this->fixture, 'threshSum' ) );
        $this->assertTrue( $this->fixture->threshSum( $threshold ) == $sum ,
            'Sorry your math is wrong'
        );
    }

    /**
     * reference the fib function above. for threshold 8 meaning no
     * term in the sequence is greater than 8, the sum of the even
     * terms of that sequence is 10. 1,2,3,5,8 breaks down to 2 and 8
     * which when summed = 10.
     */ 
    public function threshSums()
    {
        return array(
            array( 8, 10 ),
            array( 10, 10 ),
            array( 89, 44 ),
            array( 95, 44 ),
        );
    }

}
?>
