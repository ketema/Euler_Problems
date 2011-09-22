class PythagoreanTriplet
    constructor: (@sum = 0) ->

    a: 0
    b: 0
    c: 0
    
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


    findTriplet: (sum = @sum) ->
        #had to go to Wikipedia.
        #Generating Triples using a Square
        #Start with any square number n. Express that number in the form 
        #x(x + 2y), then y^2 will produce another square such that n + y^2 = z^2. For instance:
        #let n = 9, 1(1 + 8) = 9, (8 / 2)^2 = 16, and 9 + 16 = 25.
        #let n = 36, 2(2 + 16) = 36, (16 / 2)^2 = 64, and 36 + 64 = 100.
        #This works because x(x + 2y) = x^2 + 2xy. 
        #If we add y^2, our expression becomes x^2 + 2xy + y^2, which factors into the form (x + y)^2.
        @a = (n for n in [1..Math.pow(@sum,2)])
            for m in [Math.pow(@sum,2)..1]
                break if !@isSquare(n)
                continue if !@isSquare(m)
                [@a,@b,@c] = [Math.sqrt(n), Math.sqrt(m), @sum - @a - @b]
                console.log(@a,@b,@c)
                break if @verify()

        return [@a,@b,@c]

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
        expect(fixture.isSquare(25)).toEqual(true)
        expect(fixture.isSquare(-5)).toEqual(false)
        expect(fixture.isSquare(9)).toEqual(true)
        expect(fixture.isSquare(123456)).toEqual(false)

    it 'should be able to verify that a<b<c or that 0=a=b=c', ->
        expect(fixture.a < fixture.b < fixture.c or
            (fixture.a == 0 and fixture.b == 0 and fixture.c == 0)
        ).toEqual true

    it 'should be able to verify a^2+b^2==c^2', ->
        expect(Math.pow(fixture.a, 2) +
            Math.pow(fixture.b, 2) == Math.pow(fixture.c, 2)
        ).toEqual true

    it 'should be able to verify itself', ->
        fixture = new PythagoreanTriplet(0)
        expect(fixture.verify()).toEqual true

describe 'find the pythagorean triplet such that a + b + c = 1000', ->
    it 'should be able to verify itself with 1000', ->
        fixture = new PythagoreanTriplet(1000)
        [a,b,c] = fixture.findTriplet()
        expect(fixture.verify()).toEqual true
        console.log("\n%d, %d, %d",a,b,c)
