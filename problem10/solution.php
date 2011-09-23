<?php

require_once dirname(__FILE__)."/../problem3/PrimeFactors.php";

$primes = new PrimeFactors(2000000);

echo array_sum($primes->primes);
