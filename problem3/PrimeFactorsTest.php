<?php

include 'PrimeFactors.php';

class PrimeFactorsTest
    extends PHPUnit_Framework_TestCase
{
    public function setUp()
    {
        $this->fixture = new PrimeFactors(20000);
    }

    /**
     * @dataProvider provideSmallestPrimeDivisor
     */
    public function test_getSmallestPrimeDivisor( $numeral, $primeDivisor )
    {
        $calculated = $this->fixture->getSmallestPrimeDivisor( $numeral );

        $this->assertEquals( $primeDivisor, $calculated );
    }

    public function provideSmallestPrimeDivisor()
    {
        return array(
            array( 4, 2 ),
            array( 21, 3),
            array( 25, 5),
            array( 301, 7 ),
            array(13195, 5),
            array( 52417, 23 ),
        );
    }

    /**
     * This will test the bitset function to ensure it returns
     * all the prime numbers up to the given limit
     *
     * @dataProvider providePrimes
     */
    public function test_computePrimes( $limit, $primes )
    {
        $this->assertEquals(
            $this->fixture->computePrimes( $limit ), $primes
        );
    }

    public function providePrimes()
    {
        return array(
            array( 100, array(2,3,5,7,11,13,17,19,23,29,31,37,41,43,
            47,53,59,61,67,71,73,79,83,89,97)
        )
        );
    }

    /**
     * @dataProvider providePrimes
     */
    public function test_isPrime( $limit, $numerals )
    {
        foreach( $numerals as $numeral )
        {
            $fixture = new PrimeFactors( $limit );
            $this->assertTrue( $fixture->isPrime($numeral),
                "$numeral is not prime\n" );
        }
    }

    /**
     * OK, so now we need to get just the prime factors of any number.
     * We can use the factors function and then just filter out the primes
     * which are defined as only being divisible by 1 and itself
     *
     * Update. Although my approach worked, it was horribly slow for large
     * numbers.  I am now using a method called Prime Factorization and 
     * pre-computing the prime numbers with a bitset object so that 
     * the factors can simply be looked up rather than tested with
     * slow modulus operator.
     *
     * @dataProvider providePrimeFactors
     */
    public function test_getPrimeFactors( $numeral, $primeFactors )
    {
        $primes = $this->fixture->getPrimeFactors( $numeral );
        $this->assertEquals(
            $primes, $primeFactors,
            "Those are not the prime factors of $numeral"
        );
    }

    public function providePrimeFactors()
    {
        return array(
            array( 21, array( 1, 3, 7 ) ),
            array( 1, array( 1 ) ),
            array( 3, array( 1, 3 ) ),
            array( 5, array( 1, 5 ) ),
            array( 13195, array( 1, 5, 7, 13, 29 ) ),
            array( 6008110, array(1,2,5,31,19381 ) ),
            array( 12, array(1,2,2,3) ),
            array( 20, array(1,2,2,5) ),
            array( 100, array(1,2,2,5,5) ),
        );
    }

    /**
     * next step is to take the largest prime factor from the list of prime
     * factors for any given $number
     *
     * @dataProvider provideLargestPrimeFactor
     **/
    public function test_getLargestPrimeFactor( $numeral, $largest )
    {
        $largestPrime = $this->fixture->getLargestPrime( $numeral );
        $this->assertEquals(
            $largestPrime, $largest,
            "That is not the largest Prime Factor of $numeral"
        );
    }

    public function provideLargestPrimeFactor()
    {
        return array(
            array( 13195, 29 ),
            array( 5, 5 ),
            array( 15000, 5 ),
            array( 6008110, 19381 ),
        );
    }

}
