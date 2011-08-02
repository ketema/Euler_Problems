<?php

class Palindrome
{
    public function isPalindrome( $numeral )
    {

        $numeralString = str_split($numeral);
        $reverseNumeralString = array_reverse( str_split("$numeral") );
        $diff = array_diff_assoc( $numeralString, $reverseNumeralString ) ;

        if ( empty( $diff ) )
        {
            return true;
        }

        return false;
    } 

    public function generateProducts()
    {
        $products = array();

        for( $i = 100; $i <= 999; $i++ )
        {
            for( $j = 100; $j <= 999; $j++ )
            {
                $products[] = $i * $j;
            }
        }
        return $products;
    }

    public function get3DigitProductPalindromes()
    {
        $palindromes = array();

        foreach( $this->generateProducts() as $product )
        {
            if( $this->isPalindrome( $product ) )
            {
                $palindromes[] = $product;
            }
        }

        return $palindromes;
    } 

    public function getMaxPalindrome()
    {
        return max( $this->get3DigitProductPalindromes() );
    }

}


?>
