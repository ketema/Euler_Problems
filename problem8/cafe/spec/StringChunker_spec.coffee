class StringChunker
    chunk: (str, chunkSize) ->
        throw 'Chunk Size > length' if chunkSize > str.length

        arr = str.split("")
        
        if arr.length > chunkSize
            chunks = (for index in [0..arr.length - (chunkSize+1)]
                arr.slice(index, index+chunkSize))
        else
            chunks = arr.slice(0,chunkSize) 
        
        chunks = (for chunk in chunks
            chunk = (for str in chunk
                str = parseInt(str)))
        
        chunks

    sum:(arr) ->
        arr.reduce((a,b) ->
            a+b
        )
       
    product:(arr) ->
        arr.reduce((a, b) ->
            a*b
        ) 
        
    getMaxProd:(arr) ->
        Math.max.apply(null, arr)

fixture = new StringChunker

bigString = '7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450'

testStrings = ['12345','123456', bigString]
invalidStr = '1234'

chunkSize = 5

products = fixture.product(fixture.chunk(bigString,chunkSize))

describe 'StringChunker', ->

    it 'Should throw an Exception when length < chunkSize', ->
        expect( ->
            fixture.chunk(invalidStr,chunkSize)
        ).toThrow( new Error('Chunk Size > length'))

    it 'Should take a string and return an array', ->
        expect(typeof fixture.chunk(testStr,chunkSize)).toEqual(typeof []) for testStr in testStrings

    it 'Should have returned an array with length <= the string length', ->
        expect(fixture.chunk(testStr,chunkSize).length <= testStr.length).toBeTruthy() for testStr in testStrings

    it 'Should verify the length of each chunk is chunkSize', ->
        expect(chunk.length).toEqual(chunkSize) for chunk in  fixture.chunk(bigString,chunkSize)

    it 'Should verify the first chunk is [7,3,1,6,7]', ->
        expect(fixture.chunk(bigString,chunkSize)[0]).toEqual([7,3,1,6,7])

    it 'Should be able to calc sum of a chunk', ->
        expect(fixture.sum(fixture.chunk(bigString,chunkSize)[0])).toEqual(24)

    it 'Should be able to calc product of a chunk', ->
        expect(fixture.product(fixture.chunk(bigString,chunkSize)[0])).toEqual(882)

    it 'Should be able to find the max product of all the chunks', ->
        expect(fixture.getMaxProd(products)).toEqual(120)
