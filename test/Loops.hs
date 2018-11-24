module Loops (tests) where

import Lib
import DataStructure

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

tests = [whileFact, repeatFact, forFact]

whileFact = testCase "while factorial interpretation" (assertEqual "" expected result)
  where
    expected = Valid (Map.fromList [("y", 120), ("x", 1)])
    result =
      interpret "x := 5; y := 1; while not x == 1 do { y := y * x; x := x - 1 }"

repeatFact = testCase "repeat until factorial interpretation" (assertEqual "" expected result)
  where
    expected = Valid (Map.fromList [("y", 120), ("x", 1)])
    result =
      interpret "x := 5; y := 1; repeat { y := y * x; x := x - 1 } until x == 1"

forFact = testCase "for factorial interpretation" (assertEqual "" expected result)
  where
    expected = Valid (Map.fromList [("y", 120), ("i", 6)])
    result =
      interpret "y := 1; for i := 1 to 6 do y := y * i"
