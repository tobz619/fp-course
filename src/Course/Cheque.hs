{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh
          [ const "",
            const "un",
            const "do",
            const "tre",
            const "quattuor",
            const "quin",
            const "sex",
            const "septen",
            const "octo",
            \q -> if "n" `isPrefixOf` q then "novem" else "noven"
          ]
      postillion ::
        List Chars
      postillion =
        listh
          [ "vigintillion",
            "trigintillion",
            "quadragintillion",
            "quinquagintillion",
            "sexagintillion",
            "septuagintillion",
            "octogintillion",
            "nonagintillion",
            "centillion",
            "decicentillion",
            "viginticentillion",
            "trigintacentillion",
            "quadragintacentillion",
            "quinquagintacentillion",
            "sexagintacentillion",
            "septuagintacentillion",
            "octogintacentillion",
            "nonagintacentillion",
            "ducentillion",
            "deciducentillion",
            "vigintiducentillion",
            "trigintaducentillion",
            "quadragintaducentillion",
            "quinquagintaducentillion",
            "sexagintaducentillion",
            "septuagintaducentillion",
            "octogintaducentillion",
            "nonagintaducentillion",
            "trecentillion",
            "decitrecentillion",
            "vigintitrecentillion",
            "trigintatrecentillion",
            "quadragintatrecentillion",
            "quinquagintatrecentillion",
            "sexagintatrecentillion",
            "septuagintatrecentillion",
            "octogintatrecentillion",
            "nonagintatrecentillion",
            "quadringentillion",
            "deciquadringentillion",
            "vigintiquadringentillion",
            "trigintaquadringentillion",
            "quadragintaquadringentillion",
            "quinquagintaquadringentillion",
            "sexagintaquadringentillion",
            "septuagintaquadringentillion",
            "octogintaquadringentillion",
            "nonagintaquadringentillion",
            "quingentillion",
            "deciquingentillion",
            "vigintiquingentillion",
            "trigintaquingentillion",
            "quadragintaquingentillion",
            "quinquagintaquingentillion",
            "sexagintaquingentillion",
            "septuagintaquingentillion",
            "octogintaquingentillion",
            "nonagintaquingentillion",
            "sescentillion",
            "decisescentillion",
            "vigintisescentillion",
            "trigintasescentillion",
            "quadragintasescentillion",
            "quinquagintasescentillion",
            "sexagintasescentillion",
            "septuagintasescentillion",
            "octogintasescentillion",
            "nonagintasescentillion",
            "septingentillion",
            "deciseptingentillion",
            "vigintiseptingentillion",
            "trigintaseptingentillion",
            "quadragintaseptingentillion",
            "quinquagintaseptingentillion",
            "sexagintaseptingentillion",
            "septuagintaseptingentillion",
            "octogintaseptingentillion",
            "nonagintaseptingentillion",
            "octingentillion",
            "decioctingentillion",
            "vigintioctingentillion",
            "trigintaoctingentillion",
            "quadragintaoctingentillion",
            "quinquagintaoctingentillion",
            "sexagintaoctingentillion",
            "septuagintaoctingentillion",
            "octogintaoctingentillion",
            "nonagintaoctingentillion",
            "nongentillion",
            "decinongentillion",
            "vigintinongentillion",
            "trigintanongentillion",
            "quadragintanongentillion",
            "quinquagintanongentillion",
            "sexagintanongentillion",
            "septuagintanongentillion",
            "octogintanongentillion",
            "nonagintanongentillion"
          ]
   in listh
        [ "",
          "thousand",
          "million",
          "billion",
          "trillion",
          "quadrillion",
          "quintillion",
          "sextillion",
          "septillion",
          "octillion",
          "nonillion",
          "decillion",
          "undecillion",
          "duodecillion",
          "tredecillion",
          "quattuordecillion",
          "quindecillion",
          "sexdecillion",
          "septendecillion",
          "octodecillion",
          "novemdecillion"
        ]
        ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit ::
  Digit ->
  Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3
  = D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq)

-- Possibly convert a character to a digit.
fromChar ::
  Char ->
  Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars ->
  Chars
dollars amt = dollarProcess ds ++ " dollars and " ++ centProcess cents ++ " cents"
  where
    (ds, cents) = span (/= '.') amt
    dollarProcess "" = "zero"
    dollarProcess x = sayDollar (filter isDigit x)
    centProcess "" = "zero"
    centProcess x = sayCents (filter isDigit x)


splitEveryThree :: Chars -> List Chars
splitEveryThree Nil = Nil
splitEveryThree xs = ys :. splitEveryThree zs
  where (ys, zs) = (take 3 xs, drop 3 xs)

splitReverse :: List Char -> List (List Char)
splitReverse = (reverse <$>) . splitEveryThree . reverse

buildDigits :: List Char -> List (Optional Digit3)
buildDigits Nil = Nil
buildDigits (x :. Nil) = (D1 <$> fromChar x) :. Nil
buildDigits (d1 :. d2 :. Nil) = (D2 <$> fromChar d1 <*> fromChar d2) :. Nil
buildDigits (d1 :. d2 :. d3 :. ds) =
  buildDigits ds ++ pure ( D3
      <$> fromChar d1
      <*> fromChar d2
      <*> fromChar d3
  )

showGroups :: Digit3 -> Chars
showGroups (D1 Zero) = ""
showGroups (D1 d) = showDigit d
showGroups (D2 d1 d2)
  | d1 == Zero = showGroups (D1 d2) 
  | d1 == One = showTeens d2
  | d2 == Zero = showTy d1
  | otherwise = showTy d1 ++ " " ++ showDigit d2
  where showTy Zero = ""
        showTy Two = "twenty"
        showTy Three = "thirty"
        showTy Five = "fifty"
        showTy Eight = "eighty"
        showTy x = showDigit x ++ "ty"

showGroups (D3 d1 d2 d3) 
  | d1 == Zero = " and " ++ showGroups (D2 d2 d3)
  | d2 == Zero && d3 == Zero = showDigit d1 ++ " hundred"
  | otherwise = showDigit d1 ++ " hundred and " ++ showGroups (D2 d2 d3)

showTeens :: Digit -> Chars
showTeens Zero = "ten"
showTeens One = "eleven"
showTeens Two = "twelve"
showTeens Three = "thirteen"
showTeens Five = "fifteen"
showTeens Eight = "eighteen"
showTeens x = showDigit x ++ "teen"

mkLists :: Chars -> List Chars
mkLists inp = fullOr (listh ["0"]) $ let 
              vs = splitReverse $ inp
              conv = sequence $ flatMap buildDigits vs
           in (showGroups <$>)  <$> conv

zipSuffixes nums = zipWith k nums illion
  where k n "" = n
        k n s  = n ++ " " ++ s 

sayDollar ls = unwords $ reverse (zipSuffixes (mkLists ls))
  where intersperse _ Nil = Nil
        intersperse s (x :. xs) = x :. prependToAll s xs
        prependToAll _ Nil = Nil
        prependToAll s (y :. ys) = y :. s :. prependToAll y ys

sayCents (d1 :. Nil) = sayDollar (d1 :. '0' :. Nil)
sayCents (d1 :. d2 :. Nil) = sayDollar (d1 :. d2 :. Nil)
sayCents _ = "Wtf"