<?php

require "SmallestNumberDivisibleByRange.php";

class SmallestNumberDivisibleByRangeTest
    extends PHPUnit_Framework_TestCase
{
    public function setup()
    {
        $this->fixture = new SmallestNumberDivisibleByRange();
    }

    public function test_get_all_prime_factors_for_range()
    {
        $rangePrimes = array( 
            array(1),        //1
            array(1,2),      //2
            array(1,3),      //3
            array(1,2,2),    //4
            array(1,5),      //5
            array(1,2,3),    //6
            array(1,7),      //7
            array(1,2,2,2),  //8
            array(1,3,3),    //9
            array(1,2,5),    //10
            array(1,11),     //11
            array(1,2,2,3),  //12
            array(1,13),     //13
            array(1,2,7),    //14
            array(1,3,5),    //15
            array(1,2,2,2,2),//16
            array(1,17),     //17
            array(1,2,3,3),  //18
            array(1,19),     //19
            array(1,2,2,5)   //20
        );

        $this->assertEquals( $rangePrimes, 
            $this->fixture->getPrimeFactorsForNumeralsInRange( range( 1, 20) )
        );
    }

    public function test_get_max_occurrences_of_prime_factor_for_range()
    {
        $distinctPrimeOccurrences = array( 
            '1' => 1,
            '2' => 4,
            '3' => 2,
            '5' => 1,
            '7' => 1,
            '11' => 1,
            '13' => 1,
            '17' => 1,
            '19' => 1 );

        $this->assertEquals( $distinctPrimeOccurrences,
            $this->fixture->getMaxPrimeOccurenceForRange( range( 1, 20 ) )
        );
    }

    public function test_get_product_of_prime_factors_for_range()
    {
       $this->assertEquals( "232792560",
            gmp_strval($this->fixture->getRangeSmallestDivisible( range(1,20) ) ) ); 
    }

    /**
     * @dataProvider provideRanges
     */
    public function test_answer_is_smallest_divisible_number_by_each_numeral_in_range( $answer, $range, $expectation )
    {
        $evenlyDivisibleByAll = false;

        for( $i = gmp_intval("$answer") - 1; $i > max($range); $i-- )
        {
            foreach( $range as $numeral )
            {
                $numeral = gmp_init( $numeral );
                if ( 0 != gmp_intval( gmp_mod( $i, $numeral ) ) )
                {
                    break(2);
                }
                $evenlyDivisibleByAll = true;
            }
           if( $evenlyDivisibleByAll ) { break; } 
        }
        $this->assertFalse( $evenlyDivisibleByAll, "$i was divisible by all numbers in range" );
    }

    public function provideRanges()
    {
        $fixture = new SmallestNumberDivisibleByRange();

        return array(
            array( "6" , array( 2,3 ) , true ),
            array( "20", array( 2,3,4), false ), //18
            array( $fixture->getRangeSmallestDivisible(range(1,20)), range(1,20), true),
        ); 
    }

}
