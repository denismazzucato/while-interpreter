module Semantic (semFunction) where

import DataStructure
import UtilityFunctions
import Fixpoint
import State.Update
import EvalBExp

-- semantic function
semFunction :: Stm -> State -> State -- Table 4.1
semFunction (Assignment x a) = updateState x a
semFunction (Skip) = id
semFunction (Composition s0 s1) = semFunction s1 . semFunction s0
semFunction (If b s0 s1) = extract. cond (evalBExp b, Def. semFunction s0, Def. semFunction s1)
semFunction (While b s) = fix f
  where f = \g -> cond (evalBExp b, g .semFunction s, Def)
semFunction (Repeat s b) = fix f
  where f = \g -> cond (evalBExp b, Def, g) . semFunction s

-- syntactic sugar
semFunction (RepeatSS s b) = semFunction $ Composition s (While (Not b) s)
semFunction (For x a0 a1 s) = semFunction $ Composition (Assignment x a0)
  $ If (smaller x a1) (Composition s (recCall x a0 a1 s)) Skip
    where
      recCall x a0 a1 = For x (AExp Sum a0 (Numeral 1)) a1
      smaller x a1 = ABExp Smaller (Variable x) a1
