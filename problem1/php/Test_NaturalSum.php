<?php
use PHPUnit\Framework\TestCase;

include 'NaturalSum.php';

Class Test_NaturalSum
    extends TestCase
{
    protected function setUp(): void
    {
        parent::setUp();
        $this->fixture = new NaturalSum();
    }
        
    public function testBasic()
    {
        $natural = new NaturalSum();
        $this->assertTrue( $natural instanceof NaturalSum,
            'This is not a Natural object' );
    }

    public function testGetNaturalNumberSumBelowThreshold()
    {
        $this->assertObjectHasAttribute( 'threshold', $this->fixture );
        $this->assertTrue( method_exists( $this->fixture, 'findNaturals'),
            'Object is missing "findNaturals method"' );
        $this->assertTrue( is_array( $this->fixture->findNaturals() ) ,
            'findnaturals method did not return an array' );
        $this->assertTrue( $this->fixture->findNaturals(10) == array( 0,3,5,6,9 ) ,
            'Uhm those are not the right numbers' ) ;
        $this->assertTrue( method_exists( $this->fixture, 'sum' ),
            'method "sum" does not exist' );
        $this->assertTrue( $this->fixture->sum(10) == 23,
            'Sorry your addition is wrong' );
    }
}
