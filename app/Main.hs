module Main where

import Lib
import Semantic
import DataStructure

main :: IO ()
main = (putStr . show . interpret) program

program = "x := 5; y := 1; while not x == 1 do { y := y * x; x := x - 1 }"