<?php

class FibSum
{
    public $fibs = array( 1, 2 );

    public function sumFibs( $terms )
    {
        return array_sum( $this->fib( $terms ) );
    }

    public function fib( $terms = 3 )
    {
        if ( $terms < 3 ) { $terms = 3; };
        
        for( $i = 2; $i < $terms; $i++ )
        {
            $this->fibs[$i] = $this->fibs[$i - 1] + $this->fibs[$i - 2] ; 
        }

        return $this->fibs;
    }

    public function sumThreshold( $threshold = 4000000 )
    {
        $terms = 3;
        while( $this->fib( $terms )[$terms - 1]  <= $threshold )
        {
            array_filter( 
        } 
    }
}

?>
