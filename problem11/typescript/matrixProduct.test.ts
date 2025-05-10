import { describe, it, expect, beforeAll } from 'vitest';

let greatestProduct: typeof import('./matrixProduct').greatestProduct;
type Coord = import('./matrixProduct').Coord;
type Matrix = import('./matrixProduct').Matrix;

const N = 4;
const ADJ = 4;

const matrix: Matrix = [
    [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9, 10, 11, 12],
    [13, 14, 15, 16]
];

beforeAll(async () => {
    const mod = await import('./matrixProduct');
    greatestProduct = mod.greatestProduct;
});

describe('greatestProduct', () => {
    it('finds the greatest product in a 4x4 matrix', () => {
        const coords: Coord[] = [];
        const result = greatestProduct(matrix, coords, 4);
        expect(result).toBe(43680); // 13*14*15*16
        expect(coords).toEqual([
            [3, 0], [3, 1], [3, 2], [3, 3]
        ]);
    });
});
