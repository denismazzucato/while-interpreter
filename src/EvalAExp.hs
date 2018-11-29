module EvalAExp
  (
    evalAExp
  ) where

import DataStructure
import IntWrapper
import State.Lookup (lookupState)

-- while
evalAExp :: AExp -> State -> Integer -- Table 1.1
evalAExp (Numeral n) = const n
evalAExp (Variable v) = intWrapper2Int . lookupState v
evalAExp (AExp Sum a0 a1) = \s -> (evalAExp a0 s) + (evalAExp a1 s)
evalAExp (AExp Sub a0 a1) = \s -> (evalAExp a0 s) - (evalAExp a1 s)
evalAExp (AExp Mul a0 a1) = \s -> (evalAExp a0 s) * (evalAExp a1 s)
