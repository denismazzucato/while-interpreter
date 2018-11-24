module SemTree (tests) where

import Lib
import DataStructure
import Parse

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

tests = [whileFact, repeatFact, forFact]

whileFact = testCase "while factorial semantic tree" (assertEqual "" expected result)
  where
    expected =
      Composition (Assignment "x" (Numeral (5)))
      $ Composition (Assignment "y" (Numeral 1))
      $ (While (Not (ABExp Equal (Variable "x") (Numeral 1)))
        $ Composition (Assignment "y" (AExp Mul (Variable "y") (Variable "x")))
        $ Assignment "x" (AExp Sub (Variable "x") (Numeral 1))
      )
    result =
      parseString "x := 5; y := 1; while not x == 1 do { y := y * x; x := x - 1 }"

repeatFact = testCase "repeat until factorial semantic tree" (assertEqual "" expected result)
  where
    expected =
      Composition (Assignment "x" (Numeral (5)))
      $ Composition (Assignment "y" (Numeral 1))
      $ (Repeat (Composition
          (Assignment "y" (AExp Mul (Variable "y") (Variable "x")))
          (Assignment "x" (AExp Sub (Variable "x") (Numeral 1))))
          (ABExp Equal (Variable "x") (Numeral 1))
      )
    result =
      parseString "x := 5; y := 1; repeat { y := y * x; x := x - 1 } until x == 1"

forFact = testCase "for factorial semantic tree" (assertEqual "" expected result)
  where
    expected =
      Composition (Assignment "y" (Numeral 1))
      (For "i" (Numeral 1) (Numeral 6) (Assignment "y" (AExp Mul (Variable "y") (Variable "i"))))
    result =
      parseString "y := 1; for i := 1 to 6 do y := y * i"
