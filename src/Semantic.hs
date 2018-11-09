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
  | Repeat Stm BExp
  | For AExp AExp Stm
  deriving (Show)

-- semantic function
semFunction :: Stm -> State -> State -- Table 4.1
semFunction (Assignment x a) = updateState x a
semFunction (Skip) = id
semFunction (Composition s0 s1) = semFunction s1 . semFunction s0
semFunction (If b s0 s1) = cond (evalBExp b) (semFunction s0) (semFunction s1)
semFunction (While b s) = fix f
  where f = \g -> cond (evalBExp b) (g . semFunction(s)) id

semFunction (Repeat s b) = fix f
  where f = \g -> (cond (evalBExp b) id g) . (semFunction s)

-- syntactic sugar
semFunction (For a0 a1 s) =
  cond
  (evalBExp $ ABExp Smaller a0 a1) -- condition
  (semFunction $ Composition s (For a0 a1 s)) -- then
  (semFunction Skip) -- else

cond ::
  (State -> Bool) -> -- b
  (State -> State) -> -- s0
  (State -> State) -> -- s1
  State -> -- s
  State
cond b s0 s1 s
  | b s = s0 s
  | otherwise = s1 s