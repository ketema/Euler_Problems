class Palindrome
    isPalindrome: (numeral) ->
        reverseNumeralString = numeral.toString().split("").reverse().join("")
        numeral.toString() == reverseNumeralString

    generateProducts: () ->
        products = []
        for num1 in [100..999]
            for num2 in [100..999]
                products.push(num1 * num2)
        products

    get3DigitProductPalindromes: () ->
        palindromes = []
        for product in this.generateProducts() 
            palindromes.push(product) if this.isPalindrome(product)
        palindromes

    getMaxPalindrome: () ->
        Math.max.apply(null,this.get3DigitProductPalindromes())

fixture = new Palindrome

describe 'Palindrome', ->
    it 'should verify 1001 is a palindrome', ->
        expect(fixture.isPalindrome('1001')).toEqual true
        expect(fixture.isPalindrome('1234')).toEqual false

    it 'should verify 998001 is the max product of 3 digit numbers', ->
        expect(fixture.generateProducts().pop()).toEqual 998001
    
    it 'should return only palindromes from the products', (palindrome) ->
       expect(fixture.isPalindrome(number)).toEqual true for number in fixture.get3DigitProductPalindromes()

console.log('Max palindrome from product of 3 digit numbers is %s', fixture.getMaxPalindrome())
