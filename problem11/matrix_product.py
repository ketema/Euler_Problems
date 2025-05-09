def read_matrix(filename):
    with open(filename) as f:
        return [list(map(int, line.strip().split())) for line in f if line.strip()]

def max_product_right(matrix, n):
    max_product = 0
    rows = len(matrix)
    cols = len(matrix[0]) if rows else 0
    for r in range(rows):
        for c in range(cols - n + 1):
            product = 1
            for i in range(n):
                product *= matrix[r][c+i]
            max_product = max(max_product, product)
    return max_product

def max_product_down(matrix, n):
    max_product = 0
    rows = len(matrix)
    cols = len(matrix[0]) if rows else 0
    for r in range(rows - n + 1):
        for c in range(cols):
            product = 1
            for i in range(n):
                product *= matrix[r+i][c]
            max_product = max(max_product, product)
    return max_product

def max_product_diag_down_right(matrix, n):
    max_product = 0
    rows = len(matrix)
    cols = len(matrix[0]) if rows else 0
    for r in range(rows - n + 1):
        for c in range(cols - n + 1):
            product = 1
            for i in range(n):
                product *= matrix[r+i][c+i]
            max_product = max(max_product, product)
    return max_product

def max_product_diag_down_left(matrix, n):
    max_product = 0
    rows = len(matrix)
    cols = len(matrix[0]) if rows else 0
    for r in range(rows - n + 1):
        for c in range(n - 1, cols):
            product = 1
            for i in range(n):
                product *= matrix[r+i][c-i]
            max_product = max(max_product, product)
    return max_product

def find_max_product_sequence(matrix, n):
    best = (0, [])
    rows = len(matrix)
    cols = len(matrix[0]) if rows else 0
    # Right
    for r in range(rows):
        for c in range(cols - n + 1):
            coords = [(r, c+i) for i in range(n)]
            prod = 1
            for i in range(n):
                prod *= matrix[r][c+i]
            if prod > best[0]:
                best = (prod, coords)
    # Down
    for r in range(rows - n + 1):
        for c in range(cols):
            coords = [(r+i, c) for i in range(n)]
            prod = 1
            for i in range(n):
                prod *= matrix[r+i][c]
            if prod > best[0]:
                best = (prod, coords)
    # Diagonal down-right
    for r in range(rows - n + 1):
        for c in range(cols - n + 1):
            coords = [(r+i, c+i) for i in range(n)]
            prod = 1
            for i in range(n):
                prod *= matrix[r+i][c+i]
            if prod > best[0]:
                best = (prod, coords)
    # Diagonal down-left
    for r in range(rows - n + 1):
        for c in range(n - 1, cols):
            coords = [(r+i, c-i) for i in range(n)]
            prod = 1
            for i in range(n):
                prod *= matrix[r+i][c-i]
            if prod > best[0]:
                best = (prod, coords)
    return best

def color_matrix_sequence(matrix, coords):
    coords_set = set(coords)
    lines = []
    for r, row in enumerate(matrix):
        line = []
        for c, val in enumerate(row):
            if (r, c) in coords_set:
                line.append(f"\033[31m{val:02}\033[0m")  # Red
            else:
                line.append(f"{val:02}")
        lines.append(' '.join(line))
    return '\n'.join(lines)

def greatest_product_in_matrix(matrix, n):
    return find_max_product_sequence(matrix, n)[0]

def main():
    import sys
    if len(sys.argv) < 2:
        print("Usage: python matrix_product.py <matrix_file>")
        sys.exit(1)
    matrix = read_matrix(sys.argv[1])
    product, coords = find_max_product_sequence(matrix, 4)
    print(color_matrix_sequence(matrix, coords))
    print(f"Greatest product of four adjacent numbers: {product}")

if __name__ == "__main__":
    main()
