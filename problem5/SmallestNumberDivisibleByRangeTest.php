<?php

require "SmallestNumberDivisibleByRange.php";

class SmallestNumberDivisibleByRangeTest
    extends PHPUnit_Framework_TestCase
{
    public function setup()
    {
        $this->fixture = new SmallestNumberDivisibleByRange();
    }

    /**
     * Some googling reveled that an efficient method of solving
     * this type of problem is by using sets.  Find the set of
     * prime factors for all the numbers in the range, then 
     * multiply them together to get the smallest number that
     * is divisible by all the numbers in the range.  Going to
     * test this on some ranges I can do by hand, and use the 
     * PrimeFactors code I have already written for problem3"
     */

    public function test_get_all_prime_factors_for_range()
    {
        $rangePrimes = array( 
            array(1),       //1
            array(1,2),     //2
            array(1,3),     //3
            array(1,2),     //4
            array(1,5),     //5
            array(1,2,3),   //6
            array(1,7),     //7
            array(1,2),     //8
            array(1,3),     //9
            array(1,2,5),   //10
            array(1,11),    //11
            array(1,2,3),   //12
            array(1,13),    //13
            array(1,2,7),   //14
            array(1,3,5),   //15
            array(1,2),     //16
            array(1,17),    //17
            array(1,2,3),   //18
            array(1,19),    //19
            array(1,2,5)    //20
        );

        $this->assertEquals( $rangePrimes, 
            $this->fixture->getPrimeFactorsForNumeralsInRange( range( 1, 20) )
        );
    }

    public function test_get_distinct_prime_factors_for_range()
    {
        $distinctPrimes = array( 1,2,3,5,7,11,13,17,19 );

        $this->assertEquals( $distinctPrimes,
            $this->fixture->getDistinctPrimeFactorsForRange( range( 1, 20 ) )
        );
    }

    public function test_product_of_distinct_prime_factors_of_range_is_smallest_divisible_number_by_each_numeral_in_range()
    {
        $product = $this->fixture->getDistinctRangeFactorsProduct( range( 1, 20 ) );

        for( $i = $product; $i > 0; $i-- )
        {
            foreach( range( 1, 20 ) as $numeral )
            {
                $this->assertTrue( $product % $numeral == 0,
                    "$product is not evenly divisible by $numeral"
                );
            }
        }
    }
}
