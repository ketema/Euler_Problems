<?php

include "SmallestNumberDivisibleByRange.php";

$obj = new SmallestNumberDivisiblebyRange();
$ans = $obj->getRangeSmallestDivisible( range(1,20) );

echo "The smallest number divisible by all numbers in the range 1 to 20 is: ".gmp_strval($ans)."\n";
