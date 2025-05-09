<?php

include "SumSquareDiff.php";

$obj = new SumSquareDiff();
$diff = $obj->squareSum( range(1,100) ) - $obj->sumSquares( range(1,100) );

echo "The difference between the sum of the squares of the first one hundred
    natural numbers and the square of the sum is: $diff\n";

?>
