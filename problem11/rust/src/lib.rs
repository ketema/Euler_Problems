pub fn greatest_product(matrix: &Vec<Vec<i32>>, adj: usize) -> (i32, Vec<(usize, usize)>) {
    let rows = matrix.len();
    let cols = if rows > 0 { matrix[0].len() } else { 0 };
    let mut max = 0;
    let mut best_coords = Vec::new();
    for i in 0..rows {
        for j in 0..cols {
            // right
            if j + adj <= cols {
                let mut prod = 1;
                let mut coords = Vec::new();
                for k in 0..adj {
                    prod *= matrix[i][j + k];
                    coords.push((i, j + k));
                }
                if prod > max {
                    max = prod;
                    best_coords = coords;
                }
            }
            // down
            if i + adj <= rows {
                let mut prod = 1;
                let mut coords = Vec::new();
                for k in 0..adj {
                    prod *= matrix[i + k][j];
                    coords.push((i + k, j));
                }
                if prod > max {
                    max = prod;
                    best_coords = coords;
                }
            }
            // diag down-right
            if i + adj <= rows && j + adj <= cols {
                let mut prod = 1;
                let mut coords = Vec::new();
                for k in 0..adj {
                    prod *= matrix[i + k][j + k];
                    coords.push((i + k, j + k));
                }
                if prod > max {
                    max = prod;
                    best_coords = coords;
                }
            }
            // diag down-left
            if i + adj <= rows && j >= adj - 1 {
                let mut prod = 1;
                let mut coords = Vec::new();
                for k in 0..adj {
                    prod *= matrix[i + k][j - k];
                    coords.push((i + k, j - k));
                }
                if prod > max {
                    max = prod;
                    best_coords = coords;
                }
            }
        }
    }
    (max, best_coords)
}
