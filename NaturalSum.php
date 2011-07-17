<?php

Class NaturalSum
{

    public $threshold;
    public $multiples;

    public function __construct( $threshold = 1000,
                                 $multiples = array( 3, 5 )
                               )
    {
        $this->threshold = $threshold;
    }

    public function findNaturals( $threshold = 1000 )
    {
        $naturals = array();

        for( $i = 0; $i < $threshold; $i++ )
        {
            if( !($i % 3) or !($i % 5) )
            {
              $naturals[] = $i; 
            }
        }

        return $naturals;
    }

    public function sum( $threshold )
    {
        return array_sum( $this->findNaturals( $threshold ) );
    }
}

$naturalSum = new NaturalSum();
print_r( "The sum of all the Natural numbers divisible by 3 or 5, less than 1000 is:".
    $naturalSum->sum(1000)."\n" );

?>
