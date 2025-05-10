use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use rust::greatest_product;

fn read_matrix(filename: &str) -> io::Result<Vec<Vec<i32>>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut matrix = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let row: Vec<i32> = line
            .split_whitespace()
            .map(|s| s.parse().unwrap_or(0))
            .collect();
        if !row.is_empty() {
            matrix.push(row);
        }
    }
    Ok(matrix)
}

fn print_matrix_with_highlight(matrix: &Vec<Vec<i32>>, coords: &Vec<(usize, usize)>) {
    use std::collections::HashSet;
    let coord_set: HashSet<_> = coords.iter().cloned().collect();
    for (i, row) in matrix.iter().enumerate() {
        for (j, val) in row.iter().enumerate() {
            if coord_set.contains(&(i, j)) {
                print!("\x1b[31m{:02}\x1b[0m ", val);
            } else {
                print!("{:02} ", val);
            }
        }
        println!("");
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = if args.len() > 1 { &args[1] } else { "../matrix.txt" };
    let matrix = match read_matrix(filename) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Failed to read matrix from {}: {}", filename, e);
            std::process::exit(1);
        }
    };
    let (max, coords) = greatest_product(&matrix, 4);
    print_matrix_with_highlight(&matrix, &coords);
    println!("Greatest product of four adjacent numbers: {}", max);
}
