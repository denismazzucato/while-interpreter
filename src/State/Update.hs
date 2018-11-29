module State.Update (updateState) where

import DataStructure
import EvalAExp (evalAExp)
import IntWrapper
import qualified Data.Map as Map

updateState :: Var -> AExp -> State -> State -- substituions in State
updateState x aExp (State s) =
  State $ [(x, v)] ++ filter (\(key, value) -> key /= x) s
  -- add first new value and delete old value
    where v = int2IntWrapper $ evalAExp aExp (State s)
