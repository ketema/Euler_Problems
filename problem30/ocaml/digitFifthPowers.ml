(* Project Euler Problem 30: Digit Fifth Powers *)
(* Purely functional implementation in OCaml *)
(* Following TDD: This module makes the tests pass (GREEN phase) *)

(* Extract digits from a number into a list *)
(* Uses tail recursion for efficiency *)
let digits n =
  let rec digits_acc n acc =
    if n < 10 then n :: acc
    else digits_acc (n / 10) ((n mod 10) :: acc)
  in
  if n = 0 then [0]
  else digits_acc n []

(* Calculate fifth power of a number *)
(* Pure function - no side effects *)
let fifth_power n =
  n * n * n * n * n

(* Sum of fifth powers of a list of digits *)
(* Uses fold with accumulator - single pass, no intermediate list *)
let sum_of_fifth_powers digits_list =
  List.fold_left (fun acc d -> acc + fifth_power d) 0 digits_list

(* Check if a number equals the sum of fifth powers of its digits *)
(* Combines pure functions compositionally *)
(* Excludes single digits (1-9) as they are not sums *)
let is_digit_fifth_power n =
  n >= 10 && n = sum_of_fifth_powers (digits n)

(* Upper bound for search: 6 * 9^5 = 354,294 *)
(* Constant binding - referentially transparent *)
let upper_bound = 6 * (fifth_power 9)

(* Find all digit fifth powers in a range [start, end] *)
(* Uses tail recursion with accumulator for efficiency *)
let find_digit_fifth_powers start end_val =
  let rec find_acc current acc =
    if current > end_val then List.rev acc
    else if is_digit_fifth_power current then
      find_acc (current + 1) (current :: acc)
    else
      find_acc (current + 1) acc
  in
  find_acc start []

(* Solve the problem: find sum of all digit fifth powers *)
(* Excludes single digits (1-9) as they are not sums *)
(* Returns sum of all valid numbers from 10 to upper_bound *)
let solve () =
  let valid_numbers = find_digit_fifth_powers 10 upper_bound in
  List.fold_left (+) 0 valid_numbers
