module Semantic (semFunction) where

import DataStructure
import Fixpoint
import State.Update
import EvalBExp

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
semFunction (RepeatSS s b) = semFunction $ Composition s (While (Not b) s)
semFunction (For x a0 a1 s) = semFunction $ Composition (Assignment x a0) (
    If (smaller x a1) (Composition s (recCall x a0 a1 s)) Skip
  )
    where
      recCall x a0 a1 s = For x (AExp Sum a0 (Numeral 1)) a1 s
      smaller x a1 = ABExp Smaller (Variable x) a1

-- auxiliary conditional
cond ::
  (State -> Bool) -> -- b
  (State -> State) -> -- s0
  (State -> State) -> -- s1
  State -> -- s
  State
cond b s0 s1 s
  | b s = s0 s
  | otherwise = s1 s
