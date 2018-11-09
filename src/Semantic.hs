module Semantic
  (
    Stm(..),
    semFunction
  ) where

import DataStructure
import Fixpoint

data Stm =
  Assignment Var AExp
  | Skip
  | Composition Stm Stm
  | If BExp Stm Stm
  | While BExp Stm
  deriving (Show)

-- semantic function
semFunction :: Stm -> State -> State -- Table 4.1
semFunction (Assignment x a) = updateState x a
semFunction (Skip) = id
semFunction (Composition s0 s1) = semFunction s1 . semFunction s0
semFunction (If b s0 s1) = cond (evalBExp b) (semFunction s0) (semFunction s1)
semFunction (While b s) = fix f
  where f = \g -> cond (evalBExp b) (g . semFunction(s)) id

cond ::
  (State -> Bool) -> -- b
  (State -> State) -> -- s0
  (State -> State) -> -- s1
  State -> -- s
  State
cond b s0 s1 s
  | b s = s0 s
  | otherwise = s1 s