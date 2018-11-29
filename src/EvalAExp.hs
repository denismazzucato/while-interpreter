module EvalAExp
  (
    evalAExp
  ) where

import DataStructure
import State.Lookup (lookupState)

-- while
evalAExp :: AExp -> State -> Int -- Table 1.1
evalAExp (Numeral n) = const n
evalAExp (Variable v) = lookupState v
evalAExp (AExp Sum a0 a1) = \s -> (evalAExp a0 s) + (evalAExp a1 s)
evalAExp (AExp Sub a0 a1) = \s -> (evalAExp a0 s) - (evalAExp a1 s)
evalAExp (AExp Mul a0 a1) = \s -> (evalAExp a0 s) * (evalAExp a1 s)
