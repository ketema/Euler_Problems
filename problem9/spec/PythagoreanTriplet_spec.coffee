class PythagoreanTriplet
    constructor: (@sum) ->

    a: 0
    b: 0
    c: 0
    
    #Function to determine if a number is a perfect square
    #At first I tried to convert a C function I found on 
    #StackOverFlow, but although I got it to compile it
    #did not pass unit tests.  Then I decide to forego the
    #super efficient math and only use the parts I understood
    isSquare: (x) ->
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

fixture = new PythagoreanTriplet(1000)

describe 'PythagoreanTriplet', ->
    it 'should have a property called sum', ->
        expect(fixture.sum).toNotEqual null

    it 'should have properties a,b,c', ->
        expect(fixture.a).toNotEqual null

    it 'should have properties a,b,c', ->
        expect(fixture.b).toNotEqual null

    it 'should have properties a,b,c', ->
        expect(fixture.c).toNotEqual null

    it 'should be able to verify that a<b<c or that 0=a=b=c', ->
        expect(fixture.a < fixture.b < fixture.c or
            (fixture.a == 0 and fixture.b == 0 and fixture.c == 0)
        ).toEqual true

    it 'should be able to verify a^2+b^2==c^2', ->
        expect(Math.pow(fixture.a, 2) +
            Math.pow(fixture.b, 2) == Math.pow(fixture.c, 2)
        ).toEqual true

describe 'Object Actions', ->
    it 'should be able to verify if a number is a square', ->
        expect(fixture.isSquare(0)).toEqual(true)
        expect(fixture.isSquare(25)).toEqual(true)
        expect(fixture.isSquare(-5)).toEqual(false)
        expect(fixture.isSquare(9)).toEqual(true)
        expect(fixture.isSquare(123456)).toEqual(false)
