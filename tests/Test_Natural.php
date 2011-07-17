<?php

Class Test_Natural
    extends PhpUnit_Test_Case
{
    function testBasic()
    {
        $natural = new Natural();
        $this->assertTrue( $natural isInstanceOf Natural,
            'This is not a Natural object' );
}
