<?php

include 'FibSum.php';

class Test_FibSum
    extends PHPUnit_Framework_TestCase
{
    
    public function setup()
    {
        $this->fixture = new FibSum();
    }

    public function test_basic()
    {
        $fibSum = new FibSum();
        $this->assertTrue( $fibSum instanceof FibSum ,
            'this is not a FibSum object' );
    }

    public function fibVals()
    {
        return array(
            array( 3, 6 ),
            array( 4, 11),
            array( 5, 19),
            array( 6, 32)
        );
    }

    /**
     * @dataProvider fibVals
     */
    public function test_fib( $terms, $sum )
    {
        $this->assertTrue( $this->fixture->sumFibs( $terms ) == $sum ,
            'Sorry your math is wrong' );
    }
}
?>
