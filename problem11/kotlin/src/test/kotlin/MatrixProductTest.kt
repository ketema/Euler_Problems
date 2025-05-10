package org.euler.problem11

import kotlin.test.Test
import kotlin.test.assertEquals

class MatrixProductTest {
    @Test
    fun testGreatestProduct4x4() {
        val matrix = listOf(
            listOf(1, 2, 3, 4),
            listOf(5, 6, 7, 8),
            listOf(9, 10, 11, 12),
            listOf(13, 14, 15, 16)
        )
        val (result, coords) = greatestProduct(matrix, 4)
        val expected = 13 * 14 * 15 * 16
        val expectedCoords = listOf(Pair(3,0), Pair(3,1), Pair(3,2), Pair(3,3))
        assertEquals(expected, result)
        assertEquals(expectedCoords, coords)
    }
}
