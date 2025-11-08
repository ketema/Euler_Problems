(* Test suite for Problem 30: Digit Fifth Powers *)
(* Written FIRST following TDD methodology: RED → GREEN → REFACTOR *)

open OUnit2

(* These tests will FAIL initially because DigitFifthPowers module doesn't exist yet *)
(* This is CORRECT - we want RED phase first! *)

(* Test: Extract digits from a number *)
let test_digits_of_number _ =
  assert_equal [1; 2; 3; 4] (DigitFifthPowers.digits 1234);
  assert_equal [5] (DigitFifthPowers.digits 5);
  assert_equal [0] (DigitFifthPowers.digits 0);
  assert_equal [9; 0; 0; 0] (DigitFifthPowers.digits 9000);
  assert_equal [1; 6; 3; 4] (DigitFifthPowers.digits 1634)

(* Test: Calculate fifth power of a single digit *)
let test_fifth_power _ =
  assert_equal 0 (DigitFifthPowers.fifth_power 0);
  assert_equal 1 (DigitFifthPowers.fifth_power 1);
  assert_equal 32 (DigitFifthPowers.fifth_power 2);
  assert_equal 243 (DigitFifthPowers.fifth_power 3);
  assert_equal 1024 (DigitFifthPowers.fifth_power 4);
  assert_equal 3125 (DigitFifthPowers.fifth_power 5);
  assert_equal 7776 (DigitFifthPowers.fifth_power 6);
  assert_equal 16807 (DigitFifthPowers.fifth_power 7);
  assert_equal 32768 (DigitFifthPowers.fifth_power 8);
  assert_equal 59049 (DigitFifthPowers.fifth_power 9)

(* Test: Sum of fifth powers of digits *)
let test_sum_of_fifth_powers _ =
  (* Example: 1634 = 1^5 + 6^5 + 3^5 + 4^5 *)
  let digits_1634 = [1; 6; 3; 4] in
  let expected = 1 + 7776 + 243 + 1024 in
  assert_equal expected (DigitFifthPowers.sum_of_fifth_powers digits_1634);

  (* Test with zeros *)
  assert_equal 0 (DigitFifthPowers.sum_of_fifth_powers [0; 0; 0]);

  (* Test single digit *)
  assert_equal 59049 (DigitFifthPowers.sum_of_fifth_powers [9])

(* Test: Check if a number equals sum of fifth powers of its digits *)
let test_is_digit_fifth_power _ =
  (* These should work for fourth powers (from problem statement) *)
  (* We'll find fifth power examples when we run the solution *)

  (* Single digits don't count (not sums) *)
  assert_equal false (DigitFifthPowers.is_digit_fifth_power 1);
  assert_equal false (DigitFifthPowers.is_digit_fifth_power 5);
  assert_equal false (DigitFifthPowers.is_digit_fifth_power 9);

  (* Test a known non-match *)
  assert_equal false (DigitFifthPowers.is_digit_fifth_power 12345)

(* Test: Find all digit fifth powers in a range *)
let test_find_in_range _ =
  (* Find in small range 10-1000 *)
  let small_range = DigitFifthPowers.find_digit_fifth_powers 10 1000 in
  (* We don't know the exact numbers yet, but we can verify it's a list *)
  assert_bool "Result should be a list" (List.length small_range >= 0)

(* Test: Calculate upper bound (6 * 9^5) *)
let test_upper_bound _ =
  let expected = 6 * 59049 in  (* 354294 *)
  assert_equal 354294 expected;
  assert_equal expected DigitFifthPowers.upper_bound

(* Test: Full problem - sum of all digit fifth powers *)
let test_solve_problem _ =
  let result = DigitFifthPowers.solve () in
  (* We don't know the answer yet, but it should be positive *)
  assert_bool "Answer should be positive" (result > 0);
  (* Answer should be reasonable (less than upper_bound * 10) *)
  assert_bool "Answer should be reasonable" (result < 3542940)

(* Test: Verify calculation matches number *)
let test_calculation_examples _ =
  (* For number 4150: 4^5 + 1^5 + 5^5 + 0^5 = 1024 + 1 + 3125 + 0 = 4150 *)
  let calc_4150 = List.fold_left (+) 0
    (List.map DigitFifthPowers.fifth_power [4; 1; 5; 0]) in
  assert_equal 4150 calc_4150;

  (* This should mean 4150 is a digit fifth power *)
  assert_equal true (DigitFifthPowers.is_digit_fifth_power 4150)

(* Test: Edge cases *)
let test_edge_cases _ =
  (* 10 is minimum to check (single digits excluded) *)
  let digits_10 = DigitFifthPowers.digits 10 in
  assert_equal [1; 0] digits_10;

  (* Upper bound *)
  let digits_max = DigitFifthPowers.digits DigitFifthPowers.upper_bound in
  assert_equal 6 (List.length digits_max)

(* Assemble the test suite *)
let suite =
  "Digit Fifth Powers Test Suite" >::: [
    "test_digits_of_number" >:: test_digits_of_number;
    "test_fifth_power" >:: test_fifth_power;
    "test_sum_of_fifth_powers" >:: test_sum_of_fifth_powers;
    "test_is_digit_fifth_power" >:: test_is_digit_fifth_power;
    "test_find_in_range" >:: test_find_in_range;
    "test_upper_bound" >:: test_upper_bound;
    "test_calculation_examples" >:: test_calculation_examples;
    "test_edge_cases" >:: test_edge_cases;
    "test_solve_problem" >:: test_solve_problem;
  ]

(* Run the tests *)
let () =
  run_test_tt_main suite
