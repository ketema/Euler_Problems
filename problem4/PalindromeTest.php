<?php

require "Palindrome.php";

class PalidromeTest
    extends PHPUnit_Framework_TestCase
{
    public function setup()
    {
        $this->fixture = new Palindrome();
    }

    public function test_isPalindrome()
    {
        $this->assertTrue( $this->fixture->isPalindrome(1001) );
    }

    public function test_max_product_of_3_digit_numbers()
    {
            $this->assertEquals(998001, max( $this->fixture->generateProducts() ) ); 
    }

    /**
     * @dataProvider providePalindromes
     */
    public function test_get_all_palindromes_from_3_digit_numbers( $palindrome )
    {
        $this->assertTrue( $this->fixture->isPalindrome( $palindrome ), "$palindrome is not a palindrome" );
    }

    public function providePalindromes()
    {
        $fixture = new Palindrome();
        $arrayOfPalindromes = array();
        $palindromes = $fixture->get3DigitProductPalindromes();
        foreach( $palindromes as $palindrome )
        {
            $arrayOfPalindromes[] = (array) $palindrome;
        }

        return $arrayOfPalindromes;
    }

}

?>
