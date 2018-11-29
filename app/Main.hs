module Main where

import Lib
import Semantic
import DataStructure

main :: IO ()
main = putStr $ show $ interpret program initialState

program = "while not x == 1 do { y := y * x; x := x - 1 }"
initialState = "State [(\"x\", I 5), (\"y\", Unknown)]"