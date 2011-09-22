class PythagoreanTriplet
    constructor: (@sum = 0) ->
        @findSquares(@sum)
        [@a,@b,@c] = @findTriplet()

    a: 0
    b: 0
    c: 0
    squares: []

    verify: () ->
        #this function will verify that a < b < c
        #that a + b + c = @sum
        # and that a^2 + b^2 = c^2
        return false if @sum is null
        return false if (@a + @b + @c) != @sum
        return false if Math.pow(@a, 2) + Math.pow(@b,2) != Math.pow(@c,2)
        return true unless ! ( (@a < @b < @c) or (0 == @a == @b == @c) )

    isSquare: (x) ->
        #Function to determine if a number is a perfect square
        #At first I tried to convert a C function I found on 
        #StackOverFlow, but although I got it to compile it
        #did not pass unit tests.  Then I decide to forego the
        #super efficient math and only use the parts I understood

        #If the number is negative, odd, when ANDed with 7 (111) and the
        #result = 5 (101), or ANDed with 11 (1011) and result is 8 (100)
        #then its not a perfect square 
        return false if( x < 0 || (x & 2) || ((x & 7) == 5) || ((x & 11) == 8) )
        #0 is a perfect square
        return true if( x == 0 )
        #If one of those quick answers did not work, then find the sqrt and determine
        #if it is an integer. At first I used a regex looking for a period, but then
        #I thought what if i got back a float like 5.0? So I settled on comparing ceil
        #and floor.
        y = Math.sqrt(x)
        return true if Math.ceil(y) == Math.floor(y)

        return false

    findSquares: (sum = @sum) ->
        @sum = sum
        @squares = []
        for n in [0..Math.pow(@sum,2)]
            @squares.push(n) if @isSquare(n)

        return @squares

    findTriplet: () ->
        for x in @squares
            for y in @squares
                @a = Math.sqrt(x)
                @b = Math.sqrt(y)
                @c = Math.sqrt(x + y)
                return [@a, @b, @c] if @verify()

        return [@a, @b, @c]

fixture = new PythagoreanTriplet()

describe 'PythagoreanTriplet', ->
    it 'should have a property called sum', ->
        expect(fixture.sum).toNotEqual null

    it 'should have properties a,b,c', ->
        expect(fixture.a).toNotEqual null
        expect(fixture.b).toNotEqual null
        expect(fixture.c).toNotEqual null


describe 'Object Actions', ->
    it 'should be able to verify if a number is a square', ->
        expect(fixture.isSquare(0)).toEqual(true)
        expect(fixture.isSquare(1)).toEqual(true)
        expect(fixture.isSquare(2)).toEqual(false)
        expect(fixture.isSquare(3)).toEqual(false)
        expect(fixture.isSquare(4)).toEqual(true)
        expect(fixture.isSquare(25)).toEqual(true)
        expect(fixture.isSquare(-5)).toEqual(false)
        expect(fixture.isSquare(9)).toEqual(true)
        expect(fixture.isSquare(123456)).toEqual(false)

    it 'should be able to find all the perfect squares in a range', ->
        expect(fixture.findSquares(7)).toEqual [0,1,4,9,16,25,36,49]
        expect(fixture.findSquares(10)).toEqual [0,1,4,9,16,25,36,49,64,81,100]

    it 'should be able to verify that a<b<c or that 0=a=b=c', ->
        expect(fixture.a < fixture.b < fixture.c or
            (fixture.a == 0 and fixture.b == 0 and fixture.c == 0)
        ).toEqual true

    it 'should be able to verify a^2+b^2==c^2', ->
        expect(Math.pow(fixture.a, 2) +
            Math.pow(fixture.b, 2) == Math.pow(fixture.c, 2)
        ).toEqual true

    it 'should be able to verify itself', ->
        fixture = new PythagoreanTriplet(12)
        expect(fixture.verify()).toEqual true

describe 'find the pythagorean triplet such that a + b + c = 1000', ->
    it 'should be able to verify itself with 1000', ->
        fixture = new PythagoreanTriplet(1000)
        [a,b,c] = fixture.findTriplet()
        expect(fixture.verify()).toEqual true
        console.log("\nThe triplet is: %d, %d, %d",a,b,c)
        console.log("\nThe product of a,b,c is: %d", a*b*c)
