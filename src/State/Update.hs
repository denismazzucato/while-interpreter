module State.Update (updateState) where

import DataStructure
import EvalAExp (evalAExp)
import IntWrapper
import qualified Data.Map as Map

updateState :: Var -> AExp -> State -> State -- substituions in State
updateState x aExp s = Map.insert x v s
  where v = int2IntWrapper $ evalAExp aExp s
