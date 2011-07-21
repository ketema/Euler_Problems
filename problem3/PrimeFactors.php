<?php

class PrimeFactors
{
    public function getFactors( $numeral )
    {
        $factors = array();

        for( $i = 1; $i <= $numeral; $i++ )
        {
            if ( $numeral % $i == 0 )
            {
                $factors[] = $i;
            }
        }
           return $factors;
    }

    public function getPrimeFactors( $numeral )
    {
        $primes = array( 1 );
        $factors = $this->getFactors( $numeral );

        foreach( $factors as $factor )
        {
            if ( count( $this->getFactors( $factor ) ) == 2 )
            {
                $primes[] = $factor;
            }
        }
        return $primes;
    }

    public function getLargestPrime( $numeral )
    {
        return max(
            $this->getPrimeFactors( $numeral )
        );
    }
}
