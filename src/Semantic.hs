module Semantic (semFunction) where

import DataStructure
import UtilityFunctions
import Fixpoint
import State.Update
import EvalBExp

-- semantic function
semFunction :: Stm -> State -> Partial State -- Table 4.1
semFunction (Assignment x a) = pure . (updateState x a)
semFunction (Skip) = pure -- like identity function
semFunction (Composition s0 s1) = comp (semFunction s1) (semFunction s0)
semFunction (If b s0 s1) = cond (evalBExp b, semFunction s0, semFunction s1)
semFunction (While b s) = fix f
  where f = \g -> cond (evalBExp b, comp g (semFunction s), pure)
semFunction (Repeat s b) = fix f
  where f = \g -> comp (cond (evalBExp b, pure, g)) (semFunction s)

-- syntactic sugar
semFunction (RepeatSS s b) = semFunction $ Composition s (While (Not b) s)
semFunction (For x a0 a1 s) = semFunction $ Composition (Assignment x a0)
  $ If (smaller x a1) (Composition s (recCall x a0 a1 s)) Skip
    where
      recCall x a0 a1 = For x (AExp Sum a0 (Numeral 1)) a1
      smaller x a1 = ABExp Smaller (Variable x) a1
