/*
Project Euler Problem 15: Lattice Paths - CLI Runner
*/

use euler_problem_15::{count_lattice_paths, count_lattice_paths_combinatorial};

fn main() {
    println!("Solving Project Euler Problem 15...");
    println!("Calculating lattice paths through a 20×20 grid\n");

    // Solve using both methods
    let dp_result = count_lattice_paths(20);
    let comb_result = count_lattice_paths_combinatorial(20);

    println!("Dynamic Programming approach: {}", dp_result);
    println!("Combinatorial approach:       {}", comb_result);

    assert_eq!(dp_result, comb_result, "Methods should agree!");

    println!("\nAnswer: {}", dp_result);
}
