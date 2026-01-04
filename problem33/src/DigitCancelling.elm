module DigitCancelling exposing
    ( Fraction
    , findAllDigitCancellingFractions
    , isDigitCancelling
    , solve
    )

{-| Module for solving Project Euler Problem 33: Digit Cancelling Fractions

A digit-cancelling fraction is one where removing a shared digit from both
numerator and denominator gives a fraction equal to the original.

Example: 49/98 - removing the shared digit 9 gives 4/8 = 1/2 = 49/98

-}


{-| Represents a fraction with numerator and denominator
-}
type alias Fraction =
    { numerator : Int
    , denominator : Int
    }


{-| Get the digits of a two-digit number as (tens, units)
-}
digits : Int -> ( Int, Int )
digits n =
    ( n // 10, modBy 10 n )


{-| Check if a fraction is trivial (both numerator and denominator end in 0)
-}
isTrivial : Fraction -> Bool
isTrivial fraction =
    modBy 10 fraction.numerator == 0 && modBy 10 fraction.denominator == 0


{-| Check if two fractions are equal (cross-multiply to avoid floating point)
-}
fractionsEqual : Fraction -> Fraction -> Bool
fractionsEqual f1 f2 =
    f1.numerator * f2.denominator == f2.numerator * f1.denominator


{-| Try to cancel a shared digit and return the resulting fraction if valid.
Returns Nothing if no valid cancellation exists.
-}
tryCancelDigit : Fraction -> Maybe Fraction
tryCancelDigit fraction =
    let
        ( n1, n2 ) =
            digits fraction.numerator

        ( d1, d2 ) =
            digits fraction.denominator
    in
    -- Try all 4 combinations of digit positions for shared digit
    if n1 == d1 && d2 /= 0 then
        -- Cancel first digit of both: use n2/d2
        Just { numerator = n2, denominator = d2 }

    else if n1 == d2 && d1 /= 0 then
        -- Cancel n's first digit with d's second digit: use n2/d1
        Just { numerator = n2, denominator = d1 }

    else if n2 == d1 && d2 /= 0 then
        -- Cancel n's second digit with d's first digit: use n1/d2
        Just { numerator = n1, denominator = d2 }

    else if n2 == d2 && d1 /= 0 then
        -- Cancel second digit of both: use n1/d1
        Just { numerator = n1, denominator = d1 }

    else
        Nothing


{-| Check if a fraction is a non-trivial digit-cancelling fraction.

A fraction is digit-cancelling if:

1.  It has a shared digit between numerator and denominator
2.  After removing the shared digit, the resulting fraction equals the original
3.  It is not trivial (both ending in 0)

-}
isDigitCancelling : Fraction -> Bool
isDigitCancelling fraction =
    if isTrivial fraction then
        False

    else
        case tryCancelDigit fraction of
            Just cancelled ->
                fractionsEqual fraction cancelled

            Nothing ->
                False


{-| Find all digit-cancelling fractions in the search space.

Search space: all fractions n/d where 10 <= n < d <= 99 and not trivial.

-}
findAllDigitCancellingFractions : List Fraction
findAllDigitCancellingFractions =
    List.range 10 99
        |> List.concatMap
            (\n ->
                List.range (n + 1) 99
                    |> List.map (\d -> { numerator = n, denominator = d })
            )
        |> List.filter isDigitCancelling


{-| Calculate the greatest common divisor of two numbers
-}
gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (modBy b a)


{-| Multiply two fractions
-}
multiplyFractions : Fraction -> Fraction -> Fraction
multiplyFractions f1 f2 =
    { numerator = f1.numerator * f2.numerator
    , denominator = f1.denominator * f2.denominator
    }


{-| Reduce a fraction to lowest terms
-}
reduceFraction : Fraction -> Fraction
reduceFraction fraction =
    let
        divisor =
            gcd fraction.numerator fraction.denominator
    in
    { numerator = fraction.numerator // divisor
    , denominator = fraction.denominator // divisor
    }


{-| Solve Project Euler Problem 33.

Multiply all 4 digit-cancelling fractions together, reduce to lowest terms,
and return the denominator.

Expected answer: 100

-}
solve : Int
solve =
    findAllDigitCancellingFractions
        |> List.foldl multiplyFractions { numerator = 1, denominator = 1 }
        |> reduceFraction
        |> .denominator
