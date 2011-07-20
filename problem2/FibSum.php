<?php

class FibSum
{

    public function sumFibs( $terms )
    {
        return array_sum( $this->fib( $terms ) );
    }

    public function fib( $terms = 3 )
    {
        $fibs = array(1,2);
        if ( $terms < 3 ) { $terms = 3; };
        
        for( $i = 2; $i < $terms; $i++ )
        {
            $fibs[$i] = $fibs[$i - 1] + $fibs[$i - 2]; 
        }

        return $fibs;
    }

    public function sumThreshold( $threshold = 4000000 )
    {
        return null;
        /*
        $terms = 3;
        while( $this->fib( $terms )[$terms - 1]  <= $threshold )
        {
            array_filter( );
        } 
        */
    }
}

?>
