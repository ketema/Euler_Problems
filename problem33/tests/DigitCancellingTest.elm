module DigitCancellingTest exposing (..)

import Expect
import Test exposing (..)
import DigitCancelling exposing (..)


{-| Test suite for Euler Problem 33: Digit Cancelling Fractions

The fraction 49/98 is curious: incorrectly "cancelling" the 9s gives 49/98 = 4/8.
We seek all fractions where this "cancellation" happens to give the correct result.

Requirements:
- REQ-001: Detect digit-cancelling fractions
- REQ-002: Filter trivial cases (trailing zeros)
- REQ-003: Find exactly 4 such fractions
- REQ-004: Multiply all 4 and reduce to lowest terms
- REQ-005: Return denominator of reduced product

-}
suite : Test
suite =
    describe "Digit Cancelling Fractions"
        [ describe "REQ-001: Detect digit-cancelling fractions"
            [ test "TC-001: 49/98 IS a digit-cancelling fraction" <|
                \_ ->
                    isDigitCancelling { numerator = 49, denominator = 98 }
                        |> Expect.equal True
                        |> Expect.onFail
                            """
TC-001 FAILED: 49/98 must be detected as digit-cancelling

Requirement: REQ-001 (Detect digit-cancelling fractions)

Expected: isDigitCancelling { numerator = 49, denominator = 98 } = True
    The fraction 49/98 when you "cancel" the 9s becomes 4/8.
    4/8 = 0.5 and 49/98 = 0.5, so the cancellation preserves value.

Guidance: Fraction must have a shared digit between numerator and denominator.
    After removing the shared digit, the resulting fraction must equal the original.
    For 49/98: shared digit is 9, remaining digits form 4/8, and 4/8 = 49/98.
"""
            , test "TC-006: 16/64 IS a digit-cancelling fraction" <|
                \_ ->
                    isDigitCancelling { numerator = 16, denominator = 64 }
                        |> Expect.equal True
                        |> Expect.onFail
                            """
TC-006 FAILED: 16/64 must be detected as digit-cancelling

Requirement: REQ-001 (Detect digit-cancelling fractions)

Expected: isDigitCancelling { numerator = 16, denominator = 64 } = True
    Cancelling the 6s gives 1/4.
    1/4 = 0.25 and 16/64 = 0.25.

Guidance: The shared digit (6) appears in both numerator and denominator.
    Removing it and comparing values must show equality.
"""
            , test "TC-007: 19/95 IS a digit-cancelling fraction" <|
                \_ ->
                    isDigitCancelling { numerator = 19, denominator = 95 }
                        |> Expect.equal True
                        |> Expect.onFail
                            """
TC-007 FAILED: 19/95 must be detected as digit-cancelling

Requirement: REQ-001 (Detect digit-cancelling fractions)

Expected: isDigitCancelling { numerator = 19, denominator = 95 } = True
    Cancelling the 9s gives 1/5.
    1/5 = 0.2 and 19/95 = 0.2.

Guidance: Check all digit positions for shared digits and verify value preservation.
"""
            , test "TC-008: 26/65 IS a digit-cancelling fraction" <|
                \_ ->
                    isDigitCancelling { numerator = 26, denominator = 65 }
                        |> Expect.equal True
                        |> Expect.onFail
                            """
TC-008 FAILED: 26/65 must be detected as digit-cancelling

Requirement: REQ-001 (Detect digit-cancelling fractions)

Expected: isDigitCancelling { numerator = 26, denominator = 65 } = True
    Cancelling the 6s gives 2/5.
    2/5 = 0.4 and 26/65 = 0.4.

Guidance: The cancelled fraction must equal the original fraction's value.
"""
            ]
        , describe "REQ-002: Filter trivial cases"
            [ test "TC-002: 30/50 is NOT digit-cancelling (trivial - trailing zeros)" <|
                \_ ->
                    isDigitCancelling { numerator = 30, denominator = 50 }
                        |> Expect.equal False
                        |> Expect.onFail
                            """
TC-002 FAILED: 30/50 must NOT be detected as digit-cancelling

Requirement: REQ-002 (Filter trivial cases with trailing zeros)

Expected: isDigitCancelling { numerator = 30, denominator = 50 } = False
    Although cancelling zeros gives 3/5 = 30/50, this is trivial.
    Fractions where both end in 0 are excluded from consideration.

Guidance: Any fraction where both numerator and denominator end in zero
    must be rejected as trivial, even if cancellation would preserve value.
"""
            , test "TC-009: 10/20 is NOT digit-cancelling (trivial - trailing zeros)" <|
                \_ ->
                    isDigitCancelling { numerator = 10, denominator = 20 }
                        |> Expect.equal False
                        |> Expect.onFail
                            """
TC-009 FAILED: 10/20 must NOT be detected as digit-cancelling

Requirement: REQ-002 (Filter trivial cases with trailing zeros)

Expected: isDigitCancelling { numerator = 10, denominator = 20 } = False
    Trivial case where both numbers end in 0.

Guidance: Trailing zero cancellations are always trivial and must be excluded.
"""
            ]
        , describe "REQ-001: Reject non-digit-cancelling fractions"
            [ test "TC-003: 12/34 is NOT digit-cancelling (no valid cancellation)" <|
                \_ ->
                    isDigitCancelling { numerator = 12, denominator = 34 }
                        |> Expect.equal False
                        |> Expect.onFail
                            """
TC-003 FAILED: 12/34 must NOT be detected as digit-cancelling

Requirement: REQ-001 (Detect digit-cancelling fractions)

Expected: isDigitCancelling { numerator = 12, denominator = 34 } = False
    No shared digits exist between 12 and 34.
    Digits of 12: {1, 2}, Digits of 34: {3, 4}, intersection is empty.

Guidance: Without shared digits, no cancellation is possible.
    The function must return False when no digit can be cancelled.
"""
            , test "TC-010: 23/46 is NOT digit-cancelling (cancellation changes value)" <|
                \_ ->
                    isDigitCancelling { numerator = 23, denominator = 46 }
                        |> Expect.equal False
                        |> Expect.onFail
                            """
TC-010 FAILED: 23/46 must NOT be detected as digit-cancelling

Requirement: REQ-001 (Detect digit-cancelling fractions)

Expected: isDigitCancelling { numerator = 23, denominator = 46 } = False
    23/46 = 0.5, but there's no digit cancellation that preserves this.
    No shared digit between 23 and 46 allows valid cancellation.

Guidance: Even if a fraction has a nice value, it must have a digit
    that when cancelled preserves that value.
"""
            , test "TC-011: 13/39 is NOT digit-cancelling (shared 3, but 1/9 != 13/39)" <|
                \_ ->
                    isDigitCancelling { numerator = 13, denominator = 39 }
                        |> Expect.equal False
                        |> Expect.onFail
                            """
TC-011 FAILED: 13/39 must NOT be detected as digit-cancelling

Requirement: REQ-001 (Detect digit-cancelling fractions)

Expected: isDigitCancelling { numerator = 13, denominator = 39 } = False
    Shared digit is 3. Cancelling gives 1/9.
    But 1/9 = 0.111... while 13/39 = 0.333...
    The cancellation does NOT preserve the value.

Guidance: Having a shared digit is necessary but not sufficient.
    The cancelled fraction must EQUAL the original fraction.
"""
            ]
        , describe "REQ-003: Find exactly 4 fractions"
            [ test "TC-004: Exactly 4 digit-cancelling fractions exist" <|
                \_ ->
                    findAllDigitCancellingFractions
                        |> List.length
                        |> Expect.equal 4
                        |> Expect.onFail
                            """
TC-004 FAILED: Must find exactly 4 digit-cancelling fractions

Requirement: REQ-003 (Find exactly 4 such fractions)

Expected: List.length findAllDigitCancellingFractions = 4
    The problem states there are exactly 4 non-trivial digit-cancelling
    fractions with 2-digit numerator and denominator where n < d.

Guidance: Search space is all fractions n/d where:
    - 10 <= n < d <= 99 (two-digit numbers, proper fraction)
    - Not trivial (both ending in 0)
    - Digit cancellation preserves fraction value
"""
            , test "TC-012: Found fractions include 49/98" <|
                \_ ->
                    findAllDigitCancellingFractions
                        |> List.any (\f -> f.numerator == 49 && f.denominator == 98)
                        |> Expect.equal True
                        |> Expect.onFail
                            """
TC-012 FAILED: 49/98 must be in the list of found fractions

Requirement: REQ-003 (Find all digit-cancelling fractions)

Expected: findAllDigitCancellingFractions contains { numerator = 49, denominator = 98 }
    This is the example given in the problem statement.

Guidance: The search must cover the full range and correctly identify
    49/98 as a digit-cancelling fraction.
"""
            , test "TC-013: Found fractions include all 4 known fractions" <|
                \_ ->
                    let
                        found =
                            findAllDigitCancellingFractions

                        contains n d =
                            List.any (\f -> f.numerator == n && f.denominator == d) found

                        allPresent =
                            contains 16 64 && contains 19 95 && contains 26 65 && contains 49 98
                    in
                    allPresent
                        |> Expect.equal True
                        |> Expect.onFail
                            """
TC-013 FAILED: All 4 known digit-cancelling fractions must be found

Requirement: REQ-003 (Find exactly 4 such fractions)

Expected: findAllDigitCancellingFractions contains:
    - 16/64 (cancels to 1/4)
    - 19/95 (cancels to 1/5)
    - 26/65 (cancels to 2/5)
    - 49/98 (cancels to 4/8)

Guidance: Verify the search algorithm correctly identifies all four.
    Each must pass the digit-cancelling test.
"""
            ]
        , describe "REQ-004 & REQ-005: Product and final answer"
            [ test "TC-005: Final answer denominator = 100" <|
                \_ ->
                    solve
                        |> Expect.equal 100
                        |> Expect.onFail
                            """
TC-005 FAILED: solve must return 100

Requirement: REQ-005 (Return denominator of reduced product)

Expected: solve = 100
    Product of all 4 fractions: (16/64) * (19/95) * (26/65) * (49/98)
    = (16*19*26*49) / (64*95*65*98)
    = 387296 / 38729600
    = 1/100 (after reducing to lowest terms)
    The denominator is 100.

Guidance: Multiply all 4 found fractions together, then reduce
    the result to lowest terms using GCD. Return the denominator.
"""
            , test "TC-014: Product numerator/denominator reduces correctly" <|
                \_ ->
                    let
                        -- 16/64 * 19/95 * 26/65 * 49/98
                        -- = (16*19*26*49) / (64*95*65*98)
                        productNumerator =
                            16 * 19 * 26 * 49

                        productDenominator =
                            64 * 95 * 65 * 98

                        -- GCD should reduce this to 1/100
                        expectedRatio =
                            toFloat productNumerator / toFloat productDenominator
                    in
                    expectedRatio
                        |> Expect.within (Expect.Absolute 0.0000001) 0.01
                        |> Expect.onFail
                            """
TC-014 FAILED: Product of fractions must equal 1/100

Requirement: REQ-004 (Multiply all 4 and reduce to lowest terms)

Expected: Product ratio = 0.01 (which is 1/100)
    (16*19*26*49) / (64*95*65*98) = 387296 / 38729600 = 1/100

Guidance: This test validates the mathematical relationship.
    The implementation must correctly multiply and reduce.
"""
            ]
        , describe "Edge cases and constraints"
            [ test "TC-015: Fraction must have n < d (proper fraction)" <|
                \_ ->
                    -- 98/49 should NOT be found (improper fraction)
                    findAllDigitCancellingFractions
                        |> List.any (\f -> f.numerator >= f.denominator)
                        |> Expect.equal False
                        |> Expect.onFail
                            """
TC-015 FAILED: All fractions must be proper (numerator < denominator)

Requirement: REQ-003 (Two-digit fractions where n < d)

Expected: No fraction in findAllDigitCancellingFractions has n >= d
    We only consider proper fractions (less than 1).

Guidance: The search must enforce n < d constraint.
"""
            , test "TC-016: All found fractions are two-digit" <|
                \_ ->
                    findAllDigitCancellingFractions
                        |> List.all (\f -> f.numerator >= 10 && f.numerator <= 99 && f.denominator >= 10 && f.denominator <= 99)
                        |> Expect.equal True
                        |> Expect.onFail
                            """
TC-016 FAILED: All fractions must have two-digit numerator and denominator

Requirement: REQ-003 (Two-digit fractions)

Expected: All fractions have 10 <= numerator <= 99 and 10 <= denominator <= 99

Guidance: Search space is strictly two-digit numbers.
"""
            ]
        ]
