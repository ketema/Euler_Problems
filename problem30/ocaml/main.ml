(* Main program to solve Problem 30 *)
let () =
  let answer = DigitFifthPowers.solve () in
  let numbers = DigitFifthPowers.find_digit_fifth_powers 10 DigitFifthPowers.upper_bound in
  Printf.printf "PROBLEM 30: DIGIT FIFTH POWERS\n";
  Printf.printf "================================\n\n";
  Printf.printf "Numbers that equal sum of fifth powers of their digits:\n";
  List.iter (fun n ->
    let digs = DigitFifthPowers.digits n in
    let sum = DigitFifthPowers.sum_of_fifth_powers digs in
    Printf.printf "  %d (digits: %s, sum: %d)\n"
      n
      (String.concat " + " (List.map (fun d -> Printf.sprintf "%d^5" d) digs))
      sum
  ) numbers;
  Printf.printf "\nANSWER: %d\n" answer
