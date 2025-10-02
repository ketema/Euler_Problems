use large_sum::solve_problem13;
use std::fs;
use std::path::Path;

fn main() {
    let numbers_file = Path::new("../numbers.txt");
    
    let input_text = match fs::read_to_string(numbers_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading numbers.txt: {}", e);
            std::process::exit(1);
        }
    };

    match solve_problem13(&input_text) {
        Ok(result) => println!("First ten digits of the sum: {}", result),
        Err(e) => {
            eprintln!("Error solving problem: {}", e);
            std::process::exit(1);
        }
    }
}
