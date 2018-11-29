module Loops (tests) where

import Lib
import IntWrapper
import DataStructure

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

tests = [whileFact, repeatFact, repeatSSFact, forFact]

whileFact = testCase "while factorial interpretation" (assertEqual "" expected result)
  where
    expected = State [("x", I 1), ("y", I 120)]
    result =
      interpret "x := 5; y := 1; while not x == 1 do { y := y * x; x := x - 1 }" "State []"

repeatFact = testCase "repeat until factorial interpretation" (assertEqual "" expected result)
  where
    expected = State [("x", I 1), ("y", I 120)]
    result =
      interpret "x := 5; y := 1; repeat { y := y * x; x := x - 1 } until x == 1" "State []"

repeatSSFact = testCase "repeat until factorial interpretation, Syntactic Sugar definition" (assertEqual "" expected result)
  where
    expected = State [("x", I 1), ("y", I 120)]
    result =
      interpret "x := 5; y := 1; repeatSS { y := y * x; x := x - 1 } until x == 1" "State []"


forFact = testCase "for factorial interpretation" (assertEqual "" expected result)
  where
    expected = State [("i", I 6), ("y", I 120)]
    result =
      interpret "y := 1; for i := 1 to 6 do y := y * i" "State []"
