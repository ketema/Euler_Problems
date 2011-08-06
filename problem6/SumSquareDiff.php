<?php

class SumSquareDiff
{

    public function sumSquares( array $range )
    {
        $squareSum = 0;

        foreach( $range  as $num )
        {
            $squareSum += pow($num,2);
        }
        return $squareSum;
    }

    public function squareSum( array $range )
    {
        return pow( array_sum($range),2);
    }
}

?>
