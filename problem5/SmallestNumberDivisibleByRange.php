<?php

require_once __DIR__."/../problem3/PrimeFactors.php";

class SmallestNumberDivisibleByRange
{
    public function getPrimeFactorsForNumeralsInRange( array $range )
    {
        $primes = new PrimeFactors(20);
        $rangePrimes = array();

        foreach( $range as $numeral )
        {
            $rangePrimes[] = $primes->getPrimeFactors( $numeral );
        }

        return $rangePrimes;
    }

    public function getDistinctPrimeFactorsForRange( array $range )
    {
        $distinctPrimes = array();
        foreach( $this->getPrimeFactorsForNumeralsInRange( $range ) as $primes )
        {
            $distinctPrimes = array_merge( $distinctPrimes, $primes );
        }
        return array_values( array_unique( $distinctPrimes ) );
    }

    public function getDistinctRangeFactorsProduct( array $range )
    {
        return array_product( $this->getDistinctPrimeFactorsForRange( $range ) );
    }

}
