<?php

class PrimeFactors
{
    public $primes = array();

    public function __construct( $primeLimit =  1000000 )
    {
        $this->primes = $this->computePrimes( $primeLimit );
        $this->primes[] = 1;
    }

    /**
     * After a lot of research on how to find factors efficiently I found
     * that pre-computing the prime numbers and having them cached then 
     * using a factorization method (there are many) was the accpeted 
     * method of finding factors efficiently.  So the task then became
     * well how to cache the prime numbers, or generate them on the fly
     * fast.  Searching for this revealed a function written in java using
     * a bitset operator.  I then looked for this object for php.  I want 
     * to give credit to Alexander Veremyev author of the bitset extension
     * for php.
     */
    public function computePrimes($limit)
    {
        $primes = bitset_fill($limit);
        bitset_excl($primes, 0);
        bitset_excl($primes, 1);

        for ( $i = 0; $i*$i < $limit; $i++ )
        {
            if (bitset_in($primes, $i))
            {
                for ( $j = $i*$i; $j < $limit; $j += $i )
                {
                    bitset_excl($primes, $j);
                }
            }
        }

        return bitset_to_array( $primes );
    }

    /**
     * The First version of this function "worked" using simple modulus
     * but it sucked for big numbers as it just took too long. For my
     * second attempt I am going to use the bitset function to use
     * prime factorization method which I found on 
     * http://www.math.com/school/subject1/lessons/S1U3L1DP.html
     * You can write any composite number as a product of prime factors. This 
     * is called prime factorization. To find the prime factors of a number, 
     * you divide the number by the smallest possible prime number and work up 
     * the list of prime numbers until the result is itself a prime number. 
     * Let's use this method to find the prime factors of 168. Since 168 is 
     * even, we start by dividing it by the smallest prime number, 2. 168 
     * divided by 2 is 84.
     *
     * 84 divided by 2 is 42. 42 divided by 2 is 21. Since 21 is not divisible 
     * by 2, we try dividing by 3, the next biggest prime number. We find that 
     * 21 divided by 3 equals 7, and 7 is a prime number. We know 168 is now 
     * fully factored. We simply list the divisors to write the factors of 168.
     *
     * 168 ÷ 2 = 84
     * 84 ÷ 2 = 42
     * 42 ÷ 2 = 21
     * 21 ÷ 3 = 7 Prime number
     * prime factors = 2 × 2 × 2 × 3 × 7
     *
     * To check the answer, multiply these factors and make 
     * sure they equal 168. 
     */
    public function getPrimeFactors( $numeral )
    {
       $factors = array( 1 );

       if( $this->isPrime($numeral) || $numeral === 1 )
       {
           $factors[] = $numeral;
           return array_unique( $factors );
       }
           
       //Need to find the smallest prime integer that will divide 
       //into $numeral evenly.  If the $numeral is even this is 2.

       $divisor = $this->getSmallestPrimeDivisor( $numeral ); 
       $factors[] = $divisor;

       do{
           $quotient = $numeral / $divisor;
           $factors[] = $divisor;
           $numeral = $quotient;
           $divisor = $this->getSmallestPrimeDivisor( $numeral );
       }
       while( ! $this->isPrime($quotient) );
       
       $factors[] = $quotient;

      $factors = array_values(array_unique( $factors ));
      sort($factors); 

      return $factors;
    }

    public function isPrime( $numeral )
    {
        return in_array( $numeral, $this->primes );
    }

    public function getSmallestPrimeDivisor( $numeral )
    {
        if (! $numeral & 1 ) { return 2; }

        foreach( $this->primes as $prime )
        {
            if ( $numeral % $prime == 0 )
            {
                return $prime;
            }
        }
        return $numeral;
    }

    public function getLargestPrime( $numeral )
    {
        return max(
            $this->getPrimeFactors( $numeral )
        );
    }
}
$obj = new PrimeFactors();
echo "The largest prime factor of 600851475143 is " .
    $obj->getLargestPrime(600851475143) . "\n";
