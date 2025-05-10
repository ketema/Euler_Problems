import fs from 'node:fs';


export type Coord = [number, number];
export type Matrix = number[][];

export function readMatrix(filename: string): Matrix | null {
    try {
        const lines = fs.readFileSync(filename, 'utf-8').trim().split(/\r?\n/);
        if (lines.length === 0) return null;
        const matrix = lines.map(line => line.trim().split(/\s+/).map(Number));
        const numCols = matrix[0].length;
        if (matrix.some(row => row.length !== numCols)) return null;
        return matrix;
    } catch {
        return null;
    }
}

export function greatestProduct(matrix: Matrix, coords: Coord[], ADJ: number = 4): number {
    const numRows = matrix.length;
    const numCols = matrix[0]?.length || 0;
    let max = 0;
    for (let i = 0; i < numRows; ++i) {
        for (let j = 0; j < numCols; ++j) {
            // right
            if (j + ADJ <= numCols) {
                let prod = 1;
                for (let k = 0; k < ADJ; ++k) prod *= matrix[i][j + k];
                if (prod > max) {
                    max = prod;
                    coords.length = 0;
                    for (let k = 0; k < ADJ; ++k) coords.push([i, j + k]);
                }
            }
            // down
            if (i + ADJ <= numRows) {
                let prod = 1;
                for (let k = 0; k < ADJ; ++k) prod *= matrix[i + k][j];
                if (prod > max) {
                    max = prod;
                    coords.length = 0;
                    for (let k = 0; k < ADJ; ++k) coords.push([i + k, j]);
                }
            }
            // diag down-right
            if (i + ADJ <= numRows && j + ADJ <= numCols) {
                let prod = 1;
                for (let k = 0; k < ADJ; ++k) prod *= matrix[i + k][j + k];
                if (prod > max) {
                    max = prod;
                    coords.length = 0;
                    for (let k = 0; k < ADJ; ++k) coords.push([i + k, j + k]);
                }
            }
            // diag down-left
            if (i + ADJ <= numRows && j - ADJ + 1 >= 0) {
                let prod = 1;
                for (let k = 0; k < ADJ; ++k) prod *= matrix[i + k][j - k];
                if (prod > max) {
                    max = prod;
                    coords.length = 0;
                    for (let k = 0; k < ADJ; ++k) coords.push([i + k, j - k]);
                }
            }
        }
    }
    return max;
}

export function printMatrixWithHighlight(matrix: Matrix, coords: Coord[]): void {
    const coordSet = new Set(coords.map(([i, j]) => `${i},${j}`));
    for (let i = 0; i < matrix.length; ++i) {
        let rowStr = '';
        for (let j = 0; j < matrix[i].length; ++j) {
            let valStr = matrix[i][j].toString().padStart(2, '0') + ' ';
            if (coordSet.has(`${i},${j}`)) {
                rowStr += `\x1b[31m${valStr}\x1b[0m`;
            } else {
                rowStr += valStr;
            }
        }
        console.log(rowStr);
    }
}

/**
 * The main function.
 */
export function main() {
    const matrix = readMatrix('../matrix.txt');
    if (!matrix) {
        console.error('Failed to read matrix');
        process.exit(1);
    }
    const coords: Coord[] = [];
    const maxProd = greatestProduct(matrix, coords, 4);
    printMatrixWithHighlight(matrix, coords);
    console.log(`\nGreatest product of four adjacent numbers: ${maxProd}`);
}

main();
// If ts-node is not available, use 'npx ts-node matrixProduct.ts' or compile with tsc and run with node.
