class PythagoreanTriplet
    constructor: (@sum) ->

    a: 0
    b: 0
    c: 0

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
