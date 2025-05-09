<?php

require "PrimeFactors.php";

$obj = new PrimeFactors();
echo "The largest prime factor of 600851475143 is " .
    $obj->getLargestPrime(600851475143) . "\n";

?>
