def read_matrix(filename):
    with open(filename) as f:
        return [list(map(int, line.strip().split())) for line in f if line.strip()]

def greatest_product_in_matrix(matrix, n):
    max_product = 0
    rows = len(matrix)
    cols = len(matrix[0]) if rows else 0
    for r in range(rows):
        for c in range(cols):
            # Horizontal
            if c + n <= cols:
                product = 1
                for i in range(n):
                    product *= matrix[r][c+i]
                max_product = max(max_product, product)
            # Vertical
            if r + n <= rows:
                product = 1
                for i in range(n):
                    product *= matrix[r+i][c]
                max_product = max(max_product, product)
            # Diagonal
            if r + n <= rows and c + n <= cols:
                product = 1
                for i in range(n):
                    product *= matrix[r+i][c+i]
                max_product = max(max_product, product)
            # Anti-diagonal
            if r + n <= rows and c - n + 1 >= 0:
                product = 1
                for i in range(n):
                    product *= matrix[r+i][c-i]
                max_product = max(max_product, product)
    return max_product

def main():
    import sys
    if len(sys.argv) < 2:
        print("Usage: python matrix_product.py <matrix_file>")
        sys.exit(1)
    matrix = read_matrix(sys.argv[1])
    result = greatest_product_in_matrix(matrix, 4)
    print(f"Greatest product of four adjacent numbers: {result}")

if __name__ == "__main__":
    main()
