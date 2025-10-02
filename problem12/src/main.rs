use std::io;
use problem12::parse_input;

fn main() {
    println!("Enter the minimum number of divisors:");
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Failed to read input");
    let min_divisors = match parse_input(&input) {
        Ok(num) => num,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    let result = problem12::find_triangle_with_divisors(min_divisors);
    println!("The first triangle number with over {} divisors is: {}", min_divisors, result);
}
