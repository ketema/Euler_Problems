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
        $this->multiples = $multiples;
    }

    public function findNaturals( $threshold = 1000 )
    {
        $naturals = array();

        for( $i = 0; $i <= $threshold; $i++ )
        {
            if( !($i % 3) or !($i % 5) )
            {
              $naturals[] = $i; 
            }
        }

        return $naturals;
    }
}

?>
