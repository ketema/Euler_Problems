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

    public function getEvens( $fibArr )
    {
        return array_filter( $fibArr, function( $element )
            {
                return( !($element & 1) );
            }
        );

    }

    public function threshSum( $threshold )
    {
        //Need to get a fib sequence where the last term is not greater
        //than the $threshold
        $terms = 3;
        $fibs = array();
        do{
            $fibs = $this->fib( $terms );
            $terms++;
        }while( array_pop( $fibs ) <= $threshold );
        
        return array_sum( $this->getEvens( $fibs ) );
    }
}

?>
