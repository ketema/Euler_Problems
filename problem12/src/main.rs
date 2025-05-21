mod lib;

fn main() {
    let result = lib::find_triangle_with_divisors(500);
    println!("The first triangle number with over 500 divisors is: {}", result);
}
