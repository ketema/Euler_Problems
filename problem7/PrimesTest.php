<?php

require "Primes.php";

class PrimesTest
    extends PHPUnit_Framework_TestCase
{
    public function setup()
    {
        $this->fixture = new Primes();
    }

    public function testSQLite3()
    {
        $this->assertFileExists("primes.db", "DatabaseFile Does not exist");
        $primesDb = new SQLite3('primes.db');
        $this->assertTrue( $primesDb instanceof SQLite3,
            "Object is not an SQLite3 object" );
    }

}
