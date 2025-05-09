<?php

require_once dirname(__FILE__)."/../problem3/PrimeFactors.php";

$primes = new PrimeFactors( 1000000 );

echo "The 10001st prime number is " . $primes->primes[10000] . "\n";


