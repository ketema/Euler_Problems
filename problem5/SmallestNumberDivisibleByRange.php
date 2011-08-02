<?php

require_once __DIR__."/../problem3/PrimeFactors.php";

class SmallestNumberDivisibleByRange
{
    public function getPrimeFactorsForNumeralsInRange( array $range )
    {
        $primes = new PrimeFactors( max($range) );
        $rangePrimes = array();

        foreach( $range as $numeral )
        {
            $rangePrimes[] = $primes->getPrimeFactors( $numeral );
        }

        return $rangePrimes;
    }

    public function getMaxPrimeOccurenceForRange( array $range )
    {
        $primeOccurrences = array();
        $primeFactors = $this->getPrimeFactorsForNumeralsInRange( $range );

        foreach( $primeFactors as $factors )
        {
            $localOccurrences = array();
            foreach( $factors as $factor )
            {
                $localOccurrences[$factor] = array_key_exists($factor, $localOccurrences) ?
                    $localOccurrences[$factor] + 1 : 1;
                
            }
            foreach( $localOccurrences as $factor => $occurrence )
            {
                if( ! array_key_exists($factor, $primeOccurrences ) )
                {
                    $primeOccurrences[$factor] = $occurrence;
                }
                if( $occurrence > $primeOccurrences[$factor] )
                {
                    $primeOccurrences[$factor] = $occurrence;
                }
            }
        }
        return $primeOccurrences;
    }

    public function getPrimeFactorsForRange( array $range )
    {
        $rangePrimes = array();
        foreach( $this->getPrimeFactorsForNumeralsInRange( $range ) as $primes )
        {
            $rangePrimes = array_merge( $rangePrimes, $primes );
        }
        return $rangePrimes;
    }

    public function getRangeSmallestDivisible( array $range )
    {
        $product = 0;
        $primeOccurrences = $this->getMaxPrimeOccurenceForRange( $range );
        foreach( $primeOccurrences as $prime => $maxOccurrence )
        {
            $prime = gmp_init( $prime );
            if( $product === 0 ) { $product = gmp_pow($prime, $maxOccurrence); }
            
            $product = gmp_mul( $product, gmp_pow($prime, $maxOccurrence) );
        }
        return $product;
    }

}
