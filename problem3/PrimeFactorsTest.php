<?php

include 'PrimeFactors.php';

class PrimeFactorsTest
    extends PHPUnit_Framework_TestCase
{
    public function setUp()
    {
        $this->fixture = new PrimeFactors( 6500 );
    }

    /**
     * First thing I needed to do in order to write tests for this problem
     * was to refresh myself on the exact definition of composite and 
     * prime numbers.  A quick google:
     * A Composite Number can be divided evenly by numbers other than
     * 1 or itself.  Example: 9 can be divided evenly by 1, 3 and 9,
     * so 9 is a composite number.  If it is not a Composite Number 
     * it is called a Prime Number Example: 7 can only be divided evenly by 
     * 1 and 7, so it is not composite. It must be a prime number.
     * For the first test I just want to make sure I can get all the 
     * factors of any arbitrary number. I will use a dataprovider returning
     * the number and an array of its factors.
     *
     * @dataProvider provideFactors
     */
    public function test_getFactors( $numeral, $factors )
    {
        $calculated = $this->fixture->getFactors( $numeral );
        $this->assertTrue(
            $calculated == $factors ,
            implode($calculated, ',') . " Those are not the factors of $numeral"
        );
    }

    public function provideFactors()
    {
        return array(
            array( 1, array( 1 ) ),
            array( 2, array( 1, 2 ) ),
            array( 4, array( 1, 2, 4 ) ),
            array( 7, array( 1, 7 ) ),
            array( 9, array( 1, 3, 9 ) ),
            array( 21, array( 1, 3, 7, 21 ) ),
            array( 100, array( 1, 2, 4, 5, 10, 20, 25, 50, 100 ) ),
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
        $this->assertTrue(
            $this->fixture->computePrimes( 100 ) == $primes,
            'The returned list of primes does not match'
        );
    }

    public function providePrimes()
    {
        return array(
            array( 100, array(2,3,5,7,11,13,17,19,23,29,31,37,41,43,
                         47,53,59,61,67,71,73,79,83,89,97),
            )
        );
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
        $this->assertTrue(
            $primes == $primeFactors,
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
        $this->assertTrue(
            $largestPrime == $largest,
            "That is not the largest Prime Factor of $numeral"
        );
    }

    public function provideLargestPrimeFactor()
    {
        return array(
            array( 13195, 29 ),
            array( 5, 5 ),
            array( 15000, 5 ),
        );
    }

}
