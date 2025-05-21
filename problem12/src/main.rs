use std::io;

fn parse_input(input: &str) -> Result<u32, String> {
    input.trim().parse::<u32>().map_err(|_| "Please enter a valid numeric value.".to_string())
}

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
